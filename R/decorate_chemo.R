#' @title Decorate chemical annotation results with statistics
#'
#' @description Logs summary statistics about chemically weighted annotations,
#'     showing how many candidate rows and unique structures were reranked at
#'     each chemical classification level. Uses cascading filters where each
#'     level builds on the previous. Internal logging helper for
#'     weight_annotations().
#'
#' @include validations_utils.R
#' @include logs_utils.R
#' @include validations_params.R
#'
#' @param annot_table_wei_chemo [data.frame] Data frame with chemically weighted
#'     annotations
#' @param score_chemical_cla_kingdom [numeric] Minimum score for Classyfire
#'     kingdom
#' @param score_chemical_cla_superclass [numeric] Minimum score for Classyfire
#'     superclass
#' @param score_chemical_cla_class [numeric] Minimum score for Classyfire class
#' @param score_chemical_cla_parent [numeric] Minimum score for Classyfire
#'     parent
#' @param score_chemical_npc_pathway [numeric] Minimum score for NPClassifier
#'     pathway
#' @param score_chemical_npc_superclass [numeric] Minimum score for NPClassifier
#'     superclass
#' @param score_chemical_npc_class [numeric] Minimum score for NPClassifier
#'     class
#'
#' @return The input annotation table (unchanged), for pipeline compatibility
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal use only - called by weight_annotations() for logging
#' weighted_chemo |>
#'   decorate_chemo(
#'     score_chemical_cla_kingdom = 0.1,
#'     score_chemical_cla_superclass = 0.2,
#'     score_chemical_cla_class = 0.3,
#'     score_chemical_cla_parent = 0.4,
#'     score_chemical_npc_pathway = 0.1,
#'     score_chemical_npc_superclass = 0.2,
#'     score_chemical_npc_class = 0.3
#'   )
#' }
decorate_chemo <- function(
  annot_table_wei_chemo,
  score_chemical_cla_kingdom,
  score_chemical_cla_superclass,
  score_chemical_cla_class,
  score_chemical_cla_parent,
  score_chemical_npc_pathway,
  score_chemical_npc_superclass,
  score_chemical_npc_class
) {
  ctx <- log_operation(
    "decorate_chemo",
    n_annotations = nrow(annot_table_wei_chemo)
  )

  validate_dataframe(
    annot_table_wei_chemo,
    param_name = "annot_table_wei_chemo"
  )

  validate_numeric_range(
    score_chemical_cla_kingdom,
    min_value = 0,
    max_value = 1,
    param_name = "score_chemical_cla_kingdom"
  )
  validate_numeric_range(
    score_chemical_cla_superclass,
    min_value = 0,
    max_value = 1,
    param_name = "score_chemical_cla_superclass"
  )
  validate_numeric_range(
    score_chemical_cla_class,
    min_value = 0,
    max_value = 1,
    param_name = "score_chemical_cla_class"
  )
  validate_numeric_range(
    score_chemical_cla_parent,
    min_value = 0,
    max_value = 1,
    param_name = "score_chemical_cla_parent"
  )
  validate_numeric_range(
    score_chemical_npc_pathway,
    min_value = 0,
    max_value = 1,
    param_name = "score_chemical_npc_pathway"
  )
  validate_numeric_range(
    score_chemical_npc_superclass,
    min_value = 0,
    max_value = 1,
    param_name = "score_chemical_npc_superclass"
  )
  validate_numeric_range(
    score_chemical_npc_class,
    min_value = 0,
    max_value = 1,
    param_name = "score_chemical_npc_class"
  )

  required_cols <- c(
    "score_chemical",
    "candidate_structure_inchikey_connectivity_layer"
  )
  missing_cols <- setdiff(required_cols, names(annot_table_wei_chemo))

  if (length(missing_cols) > 0) {
    log_warn(
      "Missing expected columns: %s",
      paste(missing_cols, collapse = ", ")
    )
    return(annot_table_wei_chemo)
  }

  n_rows <- nrow(annot_table_wei_chemo)
  raw_sc <- annot_table_wei_chemo$score_chemical
  sc_col <- if (is.double(raw_sc)) raw_sc else as.numeric(raw_sc)
  ik_col <- annot_table_wei_chemo$candidate_structure_inchikey_connectivity_layer
  bad_vals <- c(NA_character_, "notAnnotated", "notConsistent")

  filter_cascade_idx <- function(prev_idx, score_threshold, col_name) {
    if (
      length(prev_idx) == 0L ||
        is.na(score_threshold) ||
        !col_name %in% names(annot_table_wei_chemo)
    ) {
      return(integer(0L))
    }
    idx <- prev_idx[sc_col[prev_idx] >= score_threshold]
    if (length(idx) == 0L) {
      return(integer(0L))
    }
    v_sub <- annot_table_wei_chemo[[col_name]][idx]
    idx[is.na(fastmatch::fmatch(v_sub, bad_vals))]
  }

  min_thresh <- min(
    c(score_chemical_cla_kingdom, score_chemical_npc_pathway),
    na.rm = TRUE
  )
  base_idx <- which(!is.na(sc_col) & sc_col >= min_thresh)

  idx_cla_kingdom <- filter_cascade_idx(
    base_idx,
    score_chemical_cla_kingdom,
    "feature_pred_tax_cla_01kin_val"
  )
  idx_cla_superclass <- filter_cascade_idx(
    idx_cla_kingdom,
    score_chemical_cla_superclass,
    "feature_pred_tax_cla_02sup_val"
  )
  idx_cla_class <- filter_cascade_idx(
    idx_cla_superclass,
    score_chemical_cla_class,
    "feature_pred_tax_cla_03cla_val"
  )
  idx_cla_parent <- filter_cascade_idx(
    idx_cla_class,
    score_chemical_cla_parent,
    "feature_pred_tax_cla_04dirpar_val"
  )

  idx_npc_pathway <- filter_cascade_idx(
    base_idx,
    score_chemical_npc_pathway,
    "feature_pred_tax_npc_01pat_val"
  )
  idx_npc_superclass <- filter_cascade_idx(
    idx_npc_pathway,
    score_chemical_npc_superclass,
    "feature_pred_tax_npc_02sup_val"
  )
  idx_npc_class <- filter_cascade_idx(
    idx_npc_superclass,
    score_chemical_npc_class,
    "feature_pred_tax_npc_03cla_val"
  )

  active_idx <- union(idx_cla_kingdom, idx_npc_pathway)
  ik_id <- integer(n_rows)
  n_u_ik <- 0L
  if (length(active_idx) > 0L) {
    ik_act <- ik_col[active_idx]
    u_ik <- unique(ik_act)
    n_u_ik <- length(u_ik)
    ik_id[active_idx] <- if (anyNA(u_ik)) {
      match(ik_act, u_ik)
    } else {
      fastmatch::fmatch(ik_act, u_ik)
    }
  }

  count_idx_stats <- function(idx) {
    n_cand <- length(idx)
    if (n_cand == 0L) {
      return(c(n_candidates = 0L, n_unique_structures = 0L))
    }
    n_uniq <- sum(tabulate(ik_id[idx], nbins = n_u_ik) > 0L)
    c(n_candidates = n_cand, n_unique_structures = n_uniq)
  }

  cla_masks <- list(
    kingdom = idx_cla_kingdom,
    superclass = idx_cla_superclass,
    class = idx_cla_class,
    parent = idx_cla_parent
  )
  cla_counts <- vapply(
    X = cla_masks,
    FUN = count_idx_stats,
    integer(2L),
    USE.NAMES = FALSE
  )
  rownames(cla_counts) <- c("n_candidates", "n_unique_structures")
  colnames(cla_counts) <- names(cla_masks)

  npc_masks <- list(
    pathway = idx_npc_pathway,
    superclass = idx_npc_superclass,
    class = idx_npc_class
  )
  npc_counts <- vapply(
    X = npc_masks,
    FUN = count_idx_stats,
    integer(2L),
    USE.NAMES = FALSE
  )
  rownames(npc_counts) <- c("n_candidates", "n_unique_structures")
  colnames(npc_counts) <- names(npc_masks)

  log_info(
    "Chemically informed metabolite annotation reranked:
  Classyfire:
    Kingdom level:    %d candidates (%d unique)
    Superclass level: %d candidates (%d unique)
    Class level:      %d candidates (%d unique)
    Parent level:     %d candidates (%d unique)
  NPClassifier:
    Pathway level:    %d candidates (%d unique)
    Superclass level: %d candidates (%d unique)
    Class level:      %d candidates (%d unique)",
    cla_counts["n_candidates", "kingdom"],
    cla_counts["n_unique_structures", "kingdom"],
    cla_counts["n_candidates", "superclass"],
    cla_counts["n_unique_structures", "superclass"],
    cla_counts["n_candidates", "class"],
    cla_counts["n_unique_structures", "class"],
    cla_counts["n_candidates", "parent"],
    cla_counts["n_unique_structures", "parent"],
    npc_counts["n_candidates", "pathway"],
    npc_counts["n_unique_structures", "pathway"],
    npc_counts["n_candidates", "superclass"],
    npc_counts["n_unique_structures", "superclass"],
    npc_counts["n_candidates", "class"],
    npc_counts["n_unique_structures", "class"]
  )

  log_complete(ctx, n_processed = nrow(annot_table_wei_chemo))

  annot_table_wei_chemo
}
