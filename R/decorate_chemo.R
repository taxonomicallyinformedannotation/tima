#' @title Decorate chemo
#'
#' @description This function logs summary statistics about chemically weighted
#'     annotations, showing how many structures were reranked at each chemical
#'     classification level. Uses cascading filters where each level builds on
#'     the previous (higher specificity requires passing lower levels).
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
  score_chemical_cla_kingdom = get("score_chemical_cla_kingdom", envir = parent.frame()),
  score_chemical_cla_superclass = get("score_chemical_cla_superclass", envir = parent.frame()),
  score_chemical_cla_class = get("score_chemical_cla_class", envir = parent.frame()),
  score_chemical_cla_parent = get("score_chemical_cla_parent", envir = parent.frame()),
  score_chemical_npc_pathway = get("score_chemical_npc_pathway", envir = parent.frame()),
  score_chemical_npc_superclass = get("score_chemical_npc_superclass", envir = parent.frame()),
  score_chemical_npc_class = get("score_chemical_npc_class", envir = parent.frame())
) {
  # Classyfire hierarchy (cascading filters)
  df_cla_kingdom <- annot_table_wei_chemo |>
    tidytable::filter(score_chemical >= score_chemical_cla_kingdom) |>
    tidytable::filter(
      feature_pred_tax_cla_01kin_val != "notAnnotated" &
        feature_pred_tax_cla_01kin_val != "notConsistent" &
        feature_pred_tax_cla_01kin_val != "empty"
    )

  df_cla_superclass <- df_cla_kingdom |>
    tidytable::filter(score_chemical >= score_chemical_cla_superclass) |>
    tidytable::filter(
      feature_pred_tax_cla_02sup_val != "notAnnotated" &
        feature_pred_tax_cla_02sup_val != "notConsistent" &
        feature_pred_tax_cla_02sup_val != "empty"
    )

  df_cla_class <- df_cla_superclass |>
    tidytable::filter(score_chemical >= score_chemical_cla_class) |>
    tidytable::filter(
      feature_pred_tax_cla_03cla_val != "notAnnotated" &
        feature_pred_tax_cla_03cla_val != "notConsistent" &
        feature_pred_tax_cla_03cla_val != "empty"
    )

  df_cla_parent <- df_cla_class |>
    tidytable::filter(score_chemical >= score_chemical_cla_parent) |>
    tidytable::filter(
      feature_pred_tax_cla_04dirpar_val != "notAnnotated" &
        feature_pred_tax_cla_04dirpar_val != "notConsistent" &
        feature_pred_tax_cla_04dirpar_val != "empty"
    )

  # NPClassifier hierarchy (cascading filters, independent of Classyfire)
  df_npc_pathway <- annot_table_wei_chemo |>
    tidytable::filter(score_chemical >= score_chemical_npc_pathway) |>
    tidytable::filter(
      feature_pred_tax_npc_01pat_val != "notAnnotated" &
        feature_pred_tax_npc_01pat_val != "notConsistent" &
        feature_pred_tax_npc_01pat_val != "empty"
    )

  df_npc_superclass <- df_npc_pathway |>
    tidytable::filter(score_chemical >= score_chemical_npc_superclass) |>
    tidytable::filter(
      feature_pred_tax_npc_02sup_val != "notAnnotated" &
        feature_pred_tax_npc_02sup_val != "notConsistent" &
        feature_pred_tax_npc_02sup_val != "empty"
    )

  df_npc_class <- df_npc_superclass |>
    tidytable::filter(score_chemical >= score_chemical_npc_class) |>
    tidytable::filter(
      feature_pred_tax_npc_03cla_val != "notAnnotated" &
        feature_pred_tax_npc_03cla_val != "notConsistent" &
        feature_pred_tax_npc_03cla_val != "empty"
    )

  # Count unique structures at each level
  n_cla_kingdom <- nrow(df_cla_kingdom |>
    tidytable::distinct(candidate_structure_inchikey_connectivity_layer))
  n_cla_superclass <- nrow(df_cla_superclass |>
    tidytable::distinct(candidate_structure_inchikey_connectivity_layer))
  n_cla_class <- nrow(df_cla_class |>
    tidytable::distinct(candidate_structure_inchikey_connectivity_layer))
  n_cla_parent <- nrow(df_cla_parent |>
    tidytable::distinct(candidate_structure_inchikey_connectivity_layer))

  n_npc_pathway <- nrow(df_npc_pathway |>
    tidytable::distinct(candidate_structure_inchikey_connectivity_layer))
  n_npc_superclass <- nrow(df_npc_superclass |>
    tidytable::distinct(candidate_structure_inchikey_connectivity_layer))
  n_npc_class <- nrow(df_npc_class |>
    tidytable::distinct(candidate_structure_inchikey_connectivity_layer))

  # Log summary statistics
  logger::log_info(
    "Chemically informed scoring reranked:\n",
    "  Classyfire hierarchy:\n",
    "    Kingdom level:    ", n_cla_kingdom, " structures\n",
    "    Superclass level: ", n_cla_superclass, " structures\n",
    "    Class level:      ", n_cla_class, " structures\n",
    "    Parent level:     ", n_cla_parent, " structures\n",
    "  NPClassifier hierarchy:\n",
    "    Pathway level:    ", n_npc_pathway, " structures\n",
    "    Superclass level: ", n_npc_superclass, " structures\n",
    "    Class level:      ", n_npc_class, " structures\n",
    "  (Note: WITHOUT consistency score filtering - for later predictions)"
  )

  return(annot_table_wei_chemo)
}
