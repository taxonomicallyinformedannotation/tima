#' @title Decorate chemo
#'
#' @description This function outputs information about chemical weighting
#'
#' @param annot_table_wei_chemo Table to decorate
#' @param score_chemical_cla_kingdom  Classyfire kingdom score
#' @param score_chemical_cla_superclass  Classyfire superclass score
#' @param score_chemical_cla_class  Classyfire class score
#' @param score_chemical_cla_parent Classyfire parent score
#' @param score_chemical_npc_pathway NPC pathway score
#' @param score_chemical_npc_superclass NPC superclass score
#' @param score_chemical_npc_class NPC class score
#'
#' @return Message indicating the number of annotations
#'     weighted at each chemical level
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
  df_cla_kin <- annot_table_wei_chemo |>
    tidytable::filter(score_chemical >= score_chemical_cla_kingdom * 1) |>
    tidytable::filter(
      feature_pred_tax_cla_01kin_val != "notAnnotated" &
        feature_pred_tax_cla_01kin_val != "notConsistent" &
        feature_pred_tax_cla_01kin_val != "empty"
    )
  df_npc_pat <- annot_table_wei_chemo |>
    tidytable::filter(score_chemical >= score_chemical_npc_pathway * 1) |>
    tidytable::filter(
      feature_pred_tax_npc_01pat_val != "notAnnotated" &
        feature_pred_tax_npc_01pat_val != "notConsistent" &
        feature_pred_tax_npc_01pat_val != "empty"
    )
  df_cla_sup <- df_cla_kin |>
    tidytable::filter(score_chemical >= score_chemical_cla_superclass * 1) |>
    tidytable::filter(
      feature_pred_tax_cla_02sup_val != "notAnnotated" &
        feature_pred_tax_cla_02sup_val != "notConsistent" &
        feature_pred_tax_cla_02sup_val != "empty"
    )
  df_npc_sup <- df_npc_pat |>
    tidytable::filter(score_chemical >= score_chemical_npc_superclass * 1) |>
    tidytable::filter(
      feature_pred_tax_npc_02sup_val != "notAnnotated" &
        feature_pred_tax_npc_02sup_val != "notConsistent" &
        feature_pred_tax_npc_02sup_val != "empty"
    )
  df_cla_cla <- df_cla_sup |>
    tidytable::filter(score_chemical >= score_chemical_cla_class * 1) |>
    tidytable::filter(
      feature_pred_tax_cla_03cla_val != "notAnnotated" &
        feature_pred_tax_cla_03cla_val != "notConsistent" &
        feature_pred_tax_cla_03cla_val != "empty"
    )
  df_npc_cla <- df_npc_sup |>
    tidytable::filter(score_chemical >= score_chemical_npc_class * 1) |>
    tidytable::filter(
      feature_pred_tax_npc_03cla_val != "notAnnotated" &
        feature_pred_tax_npc_03cla_val != "notConsistent" &
        feature_pred_tax_npc_03cla_val != "empty"
    )
  df_cla_par <- df_cla_cla |>
    tidytable::filter(score_chemical >= score_chemical_cla_parent * 1) |>
    tidytable::filter(
      feature_pred_tax_cla_04dirpar_val != "notAnnotated" &
        feature_pred_tax_cla_04dirpar_val != "notConsistent" &
        feature_pred_tax_cla_04dirpar_val != "empty"
    )

  logger::log_info(
    "Chemically informed scoring led to \n",
    nrow(
      df_cla_kin |>
        tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
    ),
    " annotations reranked at the (classyfire) kingdom level, \n",
    nrow(
      df_npc_pat |>
        tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
    ),
    " annotations reranked at the (NPC) pathway level, \n",
    nrow(
      df_cla_sup |>
        tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
    ),
    " annotations reranked at the (classyfire) superclass level, \n",
    nrow(
      df_npc_sup |>
        tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
    ),
    " annotations reranked at the (NPC) superclass level, \n",
    nrow(
      df_cla_cla |>
        tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
    ),
    " annotations reranked at the (classyfire) class level, \n",
    nrow(
      df_npc_cla |>
        tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
    ),
    " annotations reranked at the (NPC) class level, and \n",
    nrow(
      df_cla_par |>
        tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
    ),
    " annotations reranked at the (classyfire) parent level. \n",
    "WITHOUT TAKING CONSISTENCY SCORE INTO ACCOUNT!"
  )
  rm(
    df_cla_kin,
    df_npc_pat,
    df_cla_sup,
    df_npc_sup,
    df_cla_cla,
    df_npc_cla,
    df_cla_par
  )
  return(annot_table_wei_chemo)
}
