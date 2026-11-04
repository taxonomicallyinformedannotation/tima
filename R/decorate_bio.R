#' @title Decorate biological annotation results with statistics
#'
#' @description Logs summary statistics about biologically weighted annotations,
#'     showing how many candidate rows and unique structures were reranked at
#'     each taxonomic level based on organism occurrence data. Validates
#'     required columns and handles empty inputs gracefully. Internal logging
#'     helper for weight_annotations().
#'
#' @include predicates_utils.R
#' @include validations_utils.R
#' @include logs_utils.R
#' @include validations_params.R
#'
#' @param annot_table_wei_bio Data frame with biologically weighted annotations
#' @param score_biological_kingdom Minimum score for kingdom-level matches
#' @param score_biological_phylum Minimum score for phylum-level matches
#' @param score_biological_class Minimum score for class-level matches
#' @param score_biological_order Minimum score for order-level matches
#' @param score_biological_family Minimum score for family-level matches
#' @param score_biological_tribe Minimum score for tribe-level matches
#' @param score_biological_genus Minimum score for genus-level matches
#' @param score_biological_species Minimum score for species-level matches
#' @param score_biological_variety Minimum score for variety-level matches
#' @param score_biological_variety Minimum score for Biota-level matches
#'
#' @return The input annotation table (unchanged), for pipeline compatibility
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only - called by weight_annotations() for logging
#' weighted_bio |>
#'   decorate_bio(
#'     score_biological_kingdom = 0.1,
#'     score_biological_phylum = 0.2,
#'     score_biological_class = 0.3,
#'     score_biological_order = 0.4,
#'     score_biological_family = 0.5,
#'     score_biological_tribe = 0.6,
#'     score_biological_genus = 0.7,
#'     score_biological_species = 0.8,
#'     score_biological_variety = 0.9,
#'     score_biological_biota = 1.0
#'   )
#' }
decorate_bio <- function(
  annot_table_wei_bio,
  score_biological_kingdom,
  score_biological_phylum,
  score_biological_class,
  score_biological_order,
  score_biological_family,
  score_biological_tribe,
  score_biological_genus,
  score_biological_species,
  score_biological_variety,
  score_biological_biota
) {
  ctx <- log_operation(
    "decorate_bio",
    n_annotations = nrow(annot_table_wei_bio)
  )

  # Input Validation ----
  validate_dataframe(annot_table_wei_bio, param_name = "annot_table_wei_bio")

  # Validate all score parameters (0-1 range)
  validate_numeric_range(
    score_biological_kingdom,
    0,
    1,
    param_name = "score_biological_kingdom"
  )
  validate_numeric_range(
    score_biological_phylum,
    0,
    1,
    param_name = "score_biological_phylum"
  )
  validate_numeric_range(
    score_biological_class,
    0,
    1,
    param_name = "score_biological_class"
  )
  validate_numeric_range(
    score_biological_order,
    0,
    1,
    param_name = "score_biological_order"
  )
  validate_numeric_range(
    score_biological_family,
    0,
    1,
    param_name = "score_biological_family"
  )
  validate_numeric_range(
    score_biological_tribe,
    0,
    1,
    param_name = "score_biological_tribe"
  )
  validate_numeric_range(
    score_biological_genus,
    0,
    1,
    param_name = "score_biological_genus"
  )
  validate_numeric_range(
    score_biological_species,
    0,
    1,
    param_name = "score_biological_species"
  )
  validate_numeric_range(
    score_biological_variety,
    0,
    1,
    param_name = "score_biological_variety"
  )
  validate_numeric_range(
    score_biological_biota,
    0,
    1.007277,
    param_name = "score_biological_biota"
  )

  required_cols <- c(
    "score_biological",
    "candidate_structure_inchikey_connectivity_layer"
  )
  missing_cols <- setdiff(required_cols, names(annot_table_wei_bio))

  if (length(missing_cols) > 0) {
    log_warn(
      "decorate_bio: missing expected columns: %s",
      paste(missing_cols, collapse = ", ")
    )
    return(annot_table_wei_bio)
  }

  # Helper Function ----

  # Count total candidates and unique structures at a given score threshold.
  # OPTIMIZATION: Pre-sort the data once by score descending, then for each
  # threshold compute counts via cumulative sum + duplicated() tracking.
  # This replaces 10 separate filter+distinct operations with a single
  # O(N log N) sort + O(1) lookups per level, scaling to millions of rows.

  # Create score list for efficient processing
  levels <- list(
    kingdom = score_biological_kingdom,
    phylum = score_biological_phylum,
    class = score_biological_class,
    order = score_biological_order,
    family = score_biological_family,
    tribe = score_biological_tribe,
    genus = score_biological_genus,
    species = score_biological_species,
    variety = score_biological_variety,
    biota = score_biological_biota
  )
  level_vals <- unlist(levels, use.names = FALSE)
  min_level <- if (all(is.na(level_vals))) {
    Inf
  } else {
    min(level_vals, na.rm = TRUE)
  }

  ik <- annot_table_wei_bio$candidate_structure_inchikey_connectivity_layer
  raw_sc <- annot_table_wei_bio$score_biological
  score <- if (is.double(raw_sc)) raw_sc else as.numeric(raw_sc)
  valid_sc <- which(!is.na(score) & score >= min_level)

  if (length(valid_sc) == 0L) {
    count_stats_at_level <- function(min_score, df) {
      c(n_candidates = 0L, n_unique_structures = 0L)
    }
  } else {
    sc_v <- score[valid_sc]
    ik_v <- ik[valid_sc]
    u_ik <- unique(ik_v)
    ik_id <- if (anyNA(u_ik)) {
      match(ik_v, u_ik)
    } else {
      fastmatch::fmatch(ik_v, u_ik)
    }
    u_sc <- sort(unique(sc_v))

    if (length(u_sc) <= 64L) {
      sc_id <- fastmatch::fmatch(sc_v, u_sc)
      sc_counts <- tabulate(sc_id, nbins = length(u_sc))
      max_sc_id_ik <- rep.int(1L, length(u_ik))
      if (length(u_sc) > 1L) {
        for (k in 2L:length(u_sc)) {
          max_sc_id_ik[ik_id[sc_id == k]] <- k
        }
      }
      u_ik_counts_by_sc_id <- tabulate(max_sc_id_ik, nbins = length(u_sc))

      count_stats_at_level <- function(min_score, df) {
        if (nrow(df) == 0L || is.na(min_score)) {
          return(c(n_candidates = 0L, n_unique_structures = 0L))
        }
        keep_sc <- u_sc >= min_score
        c(
          n_candidates = as.integer(sum(sc_counts[keep_sc])),
          n_unique_structures = as.integer(sum(u_ik_counts_by_sc_id[keep_sc]))
        )
      }
    } else {
      ord <- order(sc_v, decreasing = TRUE, method = "radix")
      score_sorted <- sc_v[ord]
      first_occurrence <- !duplicated(ik_id[ord])
      cum_unique <- cumsum(first_occurrence)

      count_stats_at_level <- function(min_score, df) {
        if (nrow(df) == 0L || is.na(min_score)) {
          return(c(n_candidates = 0L, n_unique_structures = 0L))
        }
        n_candidates <- sum(score_sorted >= min_score)
        n_unique <- if (n_candidates == 0L) 0L else cum_unique[n_candidates]
        c(n_candidates = n_candidates, n_unique_structures = n_unique)
      }
    }
  }

  # Calculate Counts for Each Taxonomic Level ----
  counts <- lapply(levels, count_stats_at_level, df = annot_table_wei_bio)

  # Log Summary Statistics ----

  fmt_level <- function(level_name, width = 8L) {
    level_counts <- counts[[level_name]]
    sprintf(
      "    %-*s level: %d candidates (%d unique)",
      width,
      tools::toTitleCase(level_name),
      level_counts[["n_candidates"]],
      level_counts[["n_unique_structures"]]
    )
  }

  log_info(
    paste(
      c(
        "Taxonomically informed metabolite annotation reranked:",
        fmt_level("kingdom"),
        fmt_level("phylum"),
        fmt_level("class"),
        fmt_level("order"),
        fmt_level("family"),
        fmt_level("tribe"),
        fmt_level("genus"),
        fmt_level("species"),
        fmt_level("variety"),
        fmt_level("biota")
      ),
      collapse = "\n"
    )
  )

  log_complete(ctx, n_processed = nrow(annot_table_wei_bio))

  annot_table_wei_bio
}
