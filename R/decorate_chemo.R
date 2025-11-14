#' @title Decorate chemo
#'
#' @description This function logs summary statistics about chemically weighted
#'     annotations, showing how many structures were reranked at each chemical
#'     classification level. Uses cascading filters where each level builds on
#'     the previous (higher specificity requires passing lower levels). The
#'     function validates required columns and handles empty inputs gracefully.
#'
#' @param annot_table_wei_chemo Data frame containing chemically weighted annotations
#' @param score_chemical_cla_kingdom Numeric minimum score for Classyfire kingdom
#' @param score_chemical_cla_superclass Numeric minimum score for Classyfire superclass
#' @param score_chemical_cla_class Numeric minimum score for Classyfire class
#' @param score_chemical_cla_parent Numeric minimum score for Classyfire parent
#' @param score_chemical_npc_pathway Numeric minimum score for NPClassifier pathway
#' @param score_chemical_npc_superclass Numeric minimum score for NPClassifier superclass
#' @param score_chemical_npc_class Numeric minimum score for NPClassifier class
#'
#' @return The input annotation table (unchanged), for use in pipelines
#'
#' @examples NULL
decorate_chemo <- function(
  annot_table_wei_chemo = get("annot_table_wei_chemo", envir = parent.frame()),
  score_chemical_cla_kingdom = get(
    "score_chemical_cla_kingdom",
    envir = parent.frame()
  ),
  score_chemical_cla_superclass = get(
    "score_chemical_cla_superclass",
    envir = parent.frame()
  ),
  score_chemical_cla_class = get(
    "score_chemical_cla_class",
    envir = parent.frame()
  ),
  score_chemical_cla_parent = get(
    "score_chemical_cla_parent",
    envir = parent.frame()
  ),
  score_chemical_npc_pathway = get(
    "score_chemical_npc_pathway",
    envir = parent.frame()
  ),
  score_chemical_npc_superclass = get(
    "score_chemical_npc_superclass",
    envir = parent.frame()
  ),
  score_chemical_npc_class = get(
    "score_chemical_npc_class",
    envir = parent.frame()
  )
) {
  # ============================================================================
  # Input Validation
  # ============================================================================

  required_cols <- c(
    "score_chemical",
    "candidate_structure_inchikey_connectivity_layer"
  )
  missing_cols <- setdiff(required_cols, names(annot_table_wei_chemo))

  if (length(missing_cols) > 0) {
    logger::log_warn(
      "decorate_chemo: missing expected columns: {paste(missing_cols, collapse = ', ')}"
    )
    return(annot_table_wei_chemo)
  }

  # ============================================================================
  # Helper Function
  # ============================================================================

  # Filter for valid annotations at a specific level
  filter_valid_level <- function(df, score_threshold, col_name) {
    df |>
      tidytable::filter(score_chemical >= score_threshold) |>
      tidytable::filter(
        !!as.name(col_name) != "notAnnotated" &
          !!as.name(col_name) != "notConsistent" &
          !!as.name(col_name) != "empty"
      )
  }

  # Count unique structures
  count_unique_structures <- function(df) {
    nrow(
      df |>
        tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
    )
  }

  # ============================================================================
  # Classyfire Hierarchy (cascading filters)
  # ============================================================================

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

  # ============================================================================
  # NPClassifier Hierarchy (cascading filters, independent of Classyfire)
  # ============================================================================

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

  # ============================================================================
  # Count Unique Structures
  # ============================================================================

  # Classyfire counts
  cla_dataframes <- list(
    kingdom = df_cla_kingdom,
    superclass = df_cla_superclass,
    class = df_cla_class,
    parent = df_cla_parent
  )

  cla_counts <- vapply(
    cla_dataframes,
    count_unique_structures,
    integer(1L),
    USE.NAMES = FALSE
  )
  names(cla_counts) <- names(cla_dataframes)

  # NPClassifier counts
  npc_dataframes <- list(
    pathway = df_npc_pathway,
    superclass = df_npc_superclass,
    class = df_npc_class
  )

  npc_counts <- vapply(
    npc_dataframes,
    count_unique_structures,
    integer(1L),
    USE.NAMES = FALSE
  )
  names(npc_counts) <- names(npc_dataframes)

  # ============================================================================
  # Log Summary Statistics
  # ============================================================================

  logger::log_info(
    "Chemically informed metabolite annotation reranked:\n",
    "  Classyfire:\n",
    "    Kingdom level:    {cla_counts['kingdom']} structures\n",
    "    Superclass level: {cla_counts['superclass']} structures\n",
    "    Class level:      {cla_counts['class']} structures\n",
    "    Parent level:     {cla_counts['parent']} structures\n",
    "  NPClassifier:\n",
    "    Pathway level:    {npc_counts['pathway']} structures\n",
    "    Superclass level: {npc_counts['superclass']} structures\n",
    "    Class level:      {npc_counts['class']} structures"
  )

  return(annot_table_wei_chemo)
}
