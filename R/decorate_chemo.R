#' @title Decorate chemical annotation results with statistics
#'
#' @description Logs summary statistics about chemically weighted annotations,
#'     showing how many candidate rows and unique structures were reranked at
#'     each chemical classification level. Uses cascading filters where each
#'     level builds on the previous. Internal logging helper for
#'     weight_annotations().
#'
#' @include validations_utils.R
#'
#' @param annot_table_wei_chemo [data.frame] Data frame with chemically weighted annotations
#' @param score_chemical_cla_kingdom [numeric] Minimum score for Classyfire kingdom
#' @param score_chemical_cla_superclass [numeric] Minimum score for Classyfire superclass
#' @param score_chemical_cla_class [numeric] Minimum score for Classyfire class
#' @param score_chemical_cla_parent [numeric] Minimum score for Classyfire parent
#' @param score_chemical_npc_pathway [numeric] Minimum score for NPClassifier pathway
#' @param score_chemical_npc_superclass [numeric] Minimum score for NPClassifier superclass
#' @param score_chemical_npc_class [numeric] Minimum score for NPClassifier class
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

  # Input Validation ----
  validate_dataframe(
    annot_table_wei_chemo,
    param_name = "annot_table_wei_chemo"
  )

  # Validate all score parameters are numeric
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

  # Check Required Columns ----
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

  # Helper Functions ----
  filter_valid_level <- function(df, score_threshold, col_name) {
    df |>
      tidytable::filter(score_chemical >= score_threshold) |>
      tidytable::filter(
        !!as.name(col_name) != "notAnnotated" &
          !!as.name(col_name) != "notConsistent" &
          !!as.name(col_name) != "empty"
      )
  }

  count_stats <- function(df) {
    c(
      n_candidates = nrow(df),
      n_unique_structures = nrow(
        df |>
          tidytable::distinct(
            candidate_structure_inchikey_connectivity_layer
          )
      )
    )
  }

  # Classyfire Hierarchy (cascading filters) ----

  df_cla_kingdom <- filter_valid_level(
    annot_table_wei_chemo,
    score_chemical_cla_kingdom,
    "feature_pred_tax_cla_01kin_val"
  )

  df_cla_superclass <- filter_valid_level(
    df_cla_kingdom,
    score_chemical_cla_superclass,
    "feature_pred_tax_cla_02sup_val"
  )

  df_cla_class <- filter_valid_level(
    df_cla_superclass,
    score_chemical_cla_class,
    "feature_pred_tax_cla_03cla_val"
  )

  df_cla_parent <- filter_valid_level(
    df_cla_class,
    score_chemical_cla_parent,
    "feature_pred_tax_cla_04dirpar_val"
  )

  # NPClassifier Hierarchy (cascading filters, independent of Classyfire) ----

  df_npc_pathway <- filter_valid_level(
    annot_table_wei_chemo,
    score_chemical_npc_pathway,
    "feature_pred_tax_npc_01pat_val"
  )

  df_npc_superclass <- filter_valid_level(
    df_npc_pathway,
    score_chemical_npc_superclass,
    "feature_pred_tax_npc_02sup_val"
  )

  df_npc_class <- filter_valid_level(
    df_npc_superclass,
    score_chemical_npc_class,
    "feature_pred_tax_npc_03cla_val"
  )

  # Count total candidates and unique structures ----

  # Classyfire counts
  cla_dataframes <- list(
    kingdom = df_cla_kingdom,
    superclass = df_cla_superclass,
    class = df_cla_class,
    parent = df_cla_parent
  )

  cla_counts <- vapply(
    X = cla_dataframes,
    FUN = count_stats,
    integer(2L),
    USE.NAMES = FALSE
  )
  rownames(cla_counts) <- c("n_candidates", "n_unique_structures")
  colnames(cla_counts) <- names(cla_dataframes)

  # NPClassifier counts
  npc_dataframes <- list(
    pathway = df_npc_pathway,
    superclass = df_npc_superclass,
    class = df_npc_class
  )

  npc_counts <- vapply(
    X = npc_dataframes,
    FUN = count_stats,
    integer(2L),
    USE.NAMES = FALSE
  )
  rownames(npc_counts) <- c("n_candidates", "n_unique_structures")
  colnames(npc_counts) <- names(npc_dataframes)

  # Log Summary Statistics ----

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

  return(annot_table_wei_chemo)
}
