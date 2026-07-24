#' Rank candidates by combined evidence score
#'
#' @description
#' Assigns `rank_final` to candidates based on mathematically sound evidence weighting.
#'
#' **Mathematical Approach:**
#' 
#' Final score combines all three evidence types, automatically accounting for missing evidence
#' through weight normalization:
#' ```
#' score_final = (weight_bio × score_biological +
#'                weight_chem × score_chemical +
#'                weight_spec × score_spectral) / total_weight_available
#' ```
#' 
#' Missing evidence (e.g., MS1-only hits) reduces total_weight_available, so score_final
#' already reflects the available evidence. No additional penalty applied.
#'
#' This approach:
#' - Uses **only ONE score** for ranking (no double-counting evidence)
#' - Includes **all evidence types** (biological, chemical, spectral)
#' - Handles **missing spectral data** gracefully through weight normalization (MS1-only hits)
#' - **No explicit coverage penalty** (weights already reflect available evidence)
#'
#' **Ranking Logic:**
#' 
#' Dense rank candidates by `score_final` (descending):
#' 1. Primary: score_final (higher is better)
#' 2. Tie-breaker 1: cluster_consensus_promoted (promoted first, for network consistency)
#' 3. Tie-breaker 2: adduct_match_mode (exact > rescue > precursor)
#'
#' Dense ranking means ties get the same rank_final, allowing multiple rank-1 candidates
#' when evidence is truly equivalent.
#'
#' **Special Cases (Rank Promotions):**
#' 
#' - **Cluster consensus promotion**: Candidates matching cross-feature consensus
#'   are promoted to rank_final=1 as primary tie-breaker (indicates high-confidence clusters)
#' - **Adduct match priority**: Exact adduct matches preferred over rescue matches
#'   (rescue = neutral mass match with adduct mismatch)
#'
#' **Why This Approach:**
#' - Score_final already combines all evidence and accounts for missing data via weights
#' - Weight normalization is mathematically sound (missing evidence reduces total_weight_available)
#' - Tie-breaker hierarchy is explicit and documented
#' - All special cases handled within a single, clear ranking framework
#'
#' @param df Data frame with columns:
#'   - feature_id
#'   - candidate_structure_inchikey_connectivity_layer
#'   - score_final (0-1, combined bio+chem+spec with weight normalization)
#'   - candidate_adduct_match_mode ("exact_adduct", "m_delta_rescued", "precursor_mz", or NA)
#'   - cluster_consensus_promoted_from_anchor (logical, TRUE if promoted)
#'
#' @return Data frame with added `rank_final` column
#'
#' @keywords internal
rank_candidates <- function(df) {
  if (nrow(df) == 0L) {
    return(df)
  }

  # Ensure required columns exist
  if (!("score_final" %in% names(df))) {
    warning(
      "score_final not found; this indicates data flow issue. ",
      "Ranking may be incorrect."
    )
    df[["score_final"]] <- NA_real_
  }

  # Extract vectors for efficient processing
  feature_ids <- as.character(df[["feature_id"]])
  score_final <- suppressWarnings(as.numeric(df[["score_final"]]))

  # === STEP 1: Score (Primary Ranking Criterion) ===
  # Use score_final directly; missing evidence already reflected via weight normalization
  score_final <- pmax(score_final, 0)
  score_final[!is.finite(score_final)] <- NA_real_

  # === STEP 2: Prepare Tie-Breakers ===
  # Cluster consensus promotion tie-breaker (PRIMARY)
  consensus_promoted <- suppressWarnings(as.logical(
    df[["cluster_consensus_promoted_from_anchor"]]
  ))
  consensus_promoted[is.na(consensus_promoted)] <- FALSE
  # Use -consensus_promoted for TRUE first (TRUE=-1 < FALSE=0)
  consensus_order <- as.integer(!consensus_promoted)

  # Convert match mode to numeric (lower number = better rank) (SECONDARY)
  match_mode <- suppressWarnings(as.character(df[["candidate_adduct_match_mode"]]))
  match_mode_order <- suppressWarnings(tidytable::case_when(
    match_mode == "exact_adduct" ~ 1L,        # Best: exact match
    match_mode == "m_delta_rescued" ~ 2L,     # Backup: neutral mass match
    match_mode == "precursor_mz" ~ 3L,        # Last resort: precursor m/z match
    TRUE ~ 999L                               # Unknown: put last
  ))
  match_mode_order[is.na(match_mode_order)] <- 999L

  # === STEP 3: Dense Rank within Each Feature ===
  # Create signature for dense ranking: score_final rounded to 12 decimals
  # plus tie-breaker information
  sig_vals <- round(score_final, 12)
  sig_vals[!is.finite(sig_vals)] <- NA_real_
  signature <- tidytable::case_when(
    is.na(sig_vals) ~ "<NA>",
    TRUE ~ sprintf("%.12f", sig_vals)
  )

  df$rank_final <- NA_integer_

  # Group by feature_id and assign ranks
  groups <- split(seq_len(nrow(df)), feature_ids)
  for (fid in names(groups)) {
    idx <- groups[[fid]]
    if (length(idx) == 0L) {
      next
    }

    # Single candidate: always rank 1
    if (length(idx) == 1L) {
      df$rank_final[idx] <- 1L
      next
    }

    # Multiple candidates: order by score_final DESC, then tie-breakers
    scores <- score_final[idx]
    consensus <- consensus_order[idx]
    match_modes <- match_mode_order[idx]

    # Order: descending score_final, ascending consensus (promoted=0 first),
    #        ascending match_mode (exact=1 first)
    order_idx <- order(
      -scores,              # Primary: higher score first (DESC)
      consensus,            # Tie-breaker 1: promoted first (ASC)
      match_modes,          # Tie-breaker 2: exact match first (ASC)
      na.last = TRUE,
      method = "radix"
    )

    # Create signature including tie-breaker info for dense ranking
    # This ensures dense_rank sees the full ordering, not just score
    sig <- paste(
      signature[idx][order_idx],
      consensus[order_idx],
      match_modes[order_idx],
      sep = "|"
    )

    # Dense rank: assign same rank to identical signatures (tied rows)
    sig_changes <- c(TRUE, sig[-1L] != sig[-length(sig)])
    df$rank_final[idx[order_idx]] <- cumsum(sig_changes)
  }

  # Return data sorted by feature_id and rank_final
  ord <- order(feature_ids, df$rank_final, na.last = TRUE, method = "radix")
  df[ord, , drop = FALSE]
}
