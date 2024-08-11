import::from(crayon, blue, .into = environment())
import::from(crayon, cyan, .into = environment())
import::from(crayon, green, .into = environment())
import::from(crayon, magenta, .into = environment())
import::from(crayon, red, .into = environment())
import::from(crayon, silver, .into = environment())
import::from(crayon, yellow, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, filter, .into = environment())

#' @title Decorate chemo
#'
#' @description This function outputs information about chemical weighting
#'
#' @export
#'
#' @importFrom crayon blue
#' @importFrom crayon cyan
#' @importFrom crayon green
#' @importFrom crayon magenta
#' @importFrom crayon red
#' @importFrom crayon silver
#' @importFrom crayon yellow
#' @importFrom tidytable distinct
#' @importFrom tidytable filter
#'
#' @noRd
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
decorate_chemo <- function(annot_table_wei_chemo =
                             get("annot_table_wei_chemo", envir = parent.frame()),
                           score_chemical_cla_kingdom =
                             get("score_chemical_cla_kingdom", envir = parent.frame()),
                           score_chemical_cla_superclass =
                             get("score_chemical_cla_superclass", envir = parent.frame()),
                           score_chemical_cla_class =
                             get("score_chemical_cla_class", envir = parent.frame()),
                           score_chemical_cla_parent =
                             get("score_chemical_cla_parent", envir = parent.frame()),
                           score_chemical_npc_pathway =
                             get("score_chemical_npc_pathway", envir = parent.frame()),
                           score_chemical_npc_superclass =
                             get("score_chemical_npc_superclass", envir = parent.frame()),
                           score_chemical_npc_class =
                             get("score_chemical_npc_class", envir = parent.frame())) {
  df_cla_kin <- annot_table_wei_chemo |>
    filter(score_chemical >= score_chemical_cla_kingdom * 1) |>
    filter(
      feature_pred_tax_cla_01kin_val != "notAnnotated" &
        feature_pred_tax_cla_01kin_val != "notConsistent" &
        feature_pred_tax_cla_01kin_val != "dummy"
    )
  df_npc_pat <- annot_table_wei_chemo |>
    filter(score_chemical >= score_chemical_npc_pathway * 1) |>
    filter(
      feature_pred_tax_npc_01pat_val != "notAnnotated" &
        feature_pred_tax_npc_01pat_val != "notConsistent" &
        feature_pred_tax_npc_01pat_val != "dummy"
    )
  df_cla_sup <- df_cla_kin |>
    filter(score_chemical >= score_chemical_cla_superclass * 1) |>
    filter(
      feature_pred_tax_cla_02sup_val != "notAnnotated" &
        feature_pred_tax_cla_02sup_val != "notConsistent" &
        feature_pred_tax_cla_02sup_val != "dummy"
    )
  df_npc_sup <- df_npc_pat |>
    filter(score_chemical >= score_chemical_npc_superclass * 1) |>
    filter(
      feature_pred_tax_npc_02sup_val != "notAnnotated" &
        feature_pred_tax_npc_02sup_val != "notConsistent" &
        feature_pred_tax_npc_02sup_val != "dummy"
    )
  df_cla_cla <- df_cla_sup |>
    filter(score_chemical >= score_chemical_cla_class * 1) |>
    filter(
      feature_pred_tax_cla_03cla_val != "notAnnotated" &
        feature_pred_tax_cla_03cla_val != "notConsistent" &
        feature_pred_tax_cla_03cla_val != "dummy"
    )
  df_npc_cla <- df_npc_sup |>
    filter(score_chemical >= score_chemical_npc_class * 1) |>
    filter(
      feature_pred_tax_npc_03cla_val != "notAnnotated" &
        feature_pred_tax_npc_03cla_val != "notConsistent" &
        feature_pred_tax_npc_03cla_val != "dummy"
    )
  df_cla_par <- df_cla_cla |>
    filter(score_chemical >= score_chemical_cla_parent * 1) |>
    filter(
      feature_pred_tax_cla_04dirpar_val != "notAnnotated" &
        feature_pred_tax_cla_04dirpar_val != "notConsistent" &
        feature_pred_tax_cla_04dirpar_val != "dummy"
    )

  log_debug(
    x = paste(
      "chemically informed scoring led to \n",
      silver(nrow(
        df_cla_kin |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      silver("(classyfire) kingdom"),
      "level, \n",
      cyan(nrow(
        df_npc_pat |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      cyan("(NPC) pathway"),
      "level, \n",
      magenta(nrow(
        df_cla_sup |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      magenta("(classyfire) superclass"),
      "level, \n",
      blue(nrow(
        df_npc_sup |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      blue("(NPC) superclass"),
      "level, \n",
      yellow(nrow(
        df_cla_cla |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      yellow("(classyfire) class"),
      "level, \n",
      green(nrow(
        df_npc_cla |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      green("(NPC) class"),
      "level, and \n",
      red(nrow(
        df_cla_par |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      red("(classyfire) parent"),
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
