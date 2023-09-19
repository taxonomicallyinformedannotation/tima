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
#' @export
#'
#' @examples NULL
decorate_chemo <- function(annot_table_wei_chemo =
                             get("annot_table_wei_chemo",
                               envir = parent.frame()
                             ),
                           score_chemical_cla_kingdom =
                             get("score_chemical_cla_kingdom",
                               envir = parent.frame()
                             ),
                           score_chemical_cla_superclass =
                             get("score_chemical_cla_superclass",
                               envir = parent.frame()
                             ),
                           score_chemical_cla_class =
                             get("score_chemical_cla_class",
                               envir = parent.frame()
                             ),
                           score_chemical_cla_parent =
                             get("score_chemical_cla_parent",
                               envir = parent.frame()
                             ),
                           score_chemical_npc_pathway =
                             get("score_chemical_npc_pathway",
                               envir = parent.frame()
                             ),
                           score_chemical_npc_superclass =
                             get("score_chemical_npc_superclass",
                               envir = parent.frame()
                             ),
                           score_chemical_npc_class =
                             get("score_chemical_npc_class",
                               envir = parent.frame()
                             )) {
  df_cla_kin <- annot_table_wei_chemo |>
    tidytable::filter(score_chemical >= score_chemical_cla_kingdom * 1) |>
    tidytable::filter(
      feature_pred_tax_cla_01kin_val != "notAnnotated" &
        feature_pred_tax_cla_01kin_val != "notConsistent" &
        feature_pred_tax_cla_01kin_val != "dummy"
    )
  df_npc_pat <- annot_table_wei_chemo |>
    tidytable::filter(score_chemical >= score_chemical_npc_pathway * 1) |>
    tidytable::filter(
      feature_pred_tax_npc_01pat_val != "notAnnotated" &
        feature_pred_tax_npc_01pat_val != "notConsistent" &
        feature_pred_tax_npc_01pat_val != "dummy"
    )
  df_cla_sup <- df_cla_kin |>
    tidytable::filter(score_chemical >= score_chemical_cla_superclass * 1) |>
    tidytable::filter(
      feature_pred_tax_cla_02sup_val != "notAnnotated" &
        feature_pred_tax_cla_02sup_val != "notConsistent" &
        feature_pred_tax_cla_02sup_val != "dummy"
    )
  df_npc_sup <- df_npc_pat |>
    tidytable::filter(score_chemical >= score_chemical_npc_superclass * 1) |>
    tidytable::filter(
      feature_pred_tax_npc_02sup_val != "notAnnotated" &
        feature_pred_tax_npc_02sup_val != "notConsistent" &
        feature_pred_tax_npc_02sup_val != "dummy"
    )
  df_cla_cla <- df_cla_sup |>
    tidytable::filter(score_chemical >= score_chemical_cla_class * 1) |>
    tidytable::filter(
      feature_pred_tax_cla_03cla_val != "notAnnotated" &
        feature_pred_tax_cla_03cla_val != "notConsistent" &
        feature_pred_tax_cla_03cla_val != "dummy"
    )
  df_npc_cla <- df_npc_sup |>
    tidytable::filter(score_chemical >= score_chemical_npc_class * 1) |>
    tidytable::filter(
      feature_pred_tax_npc_03cla_val != "notAnnotated" &
        feature_pred_tax_npc_03cla_val != "notConsistent" &
        feature_pred_tax_npc_03cla_val != "dummy"
    )
  df_cla_par <- df_cla_cla |>
    tidytable::filter(score_chemical >= score_chemical_cla_parent * 1) |>
    tidytable::filter(
      feature_pred_tax_cla_04dirpar_val != "notAnnotated" &
        feature_pred_tax_cla_04dirpar_val != "notConsistent" &
        feature_pred_tax_cla_04dirpar_val != "dummy"
    )

  log_debug(
    x = paste(
      "chemically informed scoring led to \n",
      crayon::silver(nrow(
        df_cla_kin |>
          tidytable::distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      crayon::silver("(classyfire) kingdom"),
      "level, \n",
      crayon::cyan(nrow(
        df_npc_pat |>
          tidytable::distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      crayon::cyan("(NPC) pathway"),
      "level, \n",
      crayon::magenta(nrow(
        df_cla_sup |>
          tidytable::distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      crayon::magenta("(classyfire) superclass"),
      "level, \n",
      crayon::blue(nrow(
        df_npc_sup |>
          tidytable::distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      crayon::blue("(NPC) superclass"),
      "level, \n",
      crayon::yellow(nrow(
        df_cla_cla |>
          tidytable::distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      crayon::yellow("(classyfire) class"),
      "level, \n",
      crayon::green(nrow(
        df_npc_cla |>
          tidytable::distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      crayon::green("(NPC) class"),
      "level, and \n",
      crayon::red(nrow(
        df_cla_par |>
          tidytable::distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      crayon::red("(classyfire) parent"),
      "level. \n",
      "WITHOUT TAKING CONSISTENCY SCORE INTO ACCOUNT!"
    )
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
