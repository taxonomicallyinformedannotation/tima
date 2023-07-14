utils::globalVariables(
  c(
    "annot_table_weighted_chemo",
    "consensus_structure_cla_cla",
    "consensus_structure_cla_kin",
    "consensus_structure_cla_par",
    "consensus_structure_cla_sup",
    "consensus_structure_npc_cla",
    "consensus_structure_npc_pat",
    "consensus_structure_npc_sup",
    "score_chemical",
    "score_chemical_cla_class",
    "score_chemical_cla_kingdom",
    "score_chemical_cla_parent",
    "score_chemical_cla_superclass",
    "score_chemical_npc_class",
    "score_chemical_npc_pathway",
    "score_chemical_npc_superclass",
    "structure_inchikey_2D"
  )
)

#' @title Decorate chemo
#'
#' @description This function outputs information about chemical weighting
#'
#' @param annot_table_weighted_chemo Table to decorate
#' @param score_chemical_cla_kingdom  Classyfire kingdom score
#' @param score_chemical_cla_superclass  Classyfire superclass score
#' @param score_chemical_cla_class  Classyfire class score
#' @param score_chemical_cla_parent Classyfire parent score
#' @param score_chemical_npc_pathway NPC pathway score
#' @param score_chemical_npc_superclass NPC superclass score
#' @param score_chemical_npc_class NPC class score
#'
#' @return Message indicating the number of annotations weighted at each chemical level
#'
#' @export
#'
#' @examples NULL
decorate_chemo <- function(annot_table_weighted_chemo = get("annot_table_weighted_chemo", envir = parent.frame()),
                           score_chemical_cla_kingdom = get("score_chemical_cla_kingdom", envir = parent.frame()),
                           score_chemical_cla_superclass = get("score_chemical_cla_superclass", envir = parent.frame()),
                           score_chemical_cla_class = get("score_chemical_cla_class", envir = parent.frame()),
                           score_chemical_cla_parent = get("score_chemical_cla_parent", envir = parent.frame()),
                           score_chemical_npc_pathway = get("score_chemical_npc_pathway", envir = parent.frame()),
                           score_chemical_npc_superclass = get("score_chemical_npc_superclass", envir = parent.frame()),
                           score_chemical_npc_class = get("score_chemical_npc_class", envir = parent.frame())) {
  df_cla_kin <- annot_table_weighted_chemo |>
    dplyr::filter(score_chemical >= score_chemical_cla_kingdom) |>
    dplyr::filter(
      consensus_structure_cla_kin != "notAnnotated" &
        consensus_structure_cla_kin != "notConsistent" &
        consensus_structure_cla_kin != "dummy"
    )
  df_npc_pat <- annot_table_weighted_chemo |>
    dplyr::filter(score_chemical >= score_chemical_npc_pathway) |>
    dplyr::filter(
      consensus_structure_npc_pat != "notAnnotated" &
        consensus_structure_npc_pat != "notConsistent" &
        consensus_structure_npc_pat != "dummy"
    )
  df_cla_sup <- df_cla_kin |>
    dplyr::filter(score_chemical >= score_chemical_cla_superclass) |>
    dplyr::filter(
      consensus_structure_cla_sup != "notAnnotated" &
        consensus_structure_cla_sup != "notConsistent" &
        consensus_structure_cla_sup != "dummy"
    )
  df_npc_sup <- df_npc_pat |>
    dplyr::filter(score_chemical >= score_chemical_npc_superclass) |>
    dplyr::filter(
      consensus_structure_npc_sup != "notAnnotated" &
        consensus_structure_npc_sup != "notConsistent" &
        consensus_structure_npc_sup != "dummy"
    )
  df_cla_cla <- df_cla_sup |>
    dplyr::filter(score_chemical >= score_chemical_cla_class) |>
    dplyr::filter(
      consensus_structure_cla_cla != "notAnnotated" &
        consensus_structure_cla_cla != "notConsistent" &
        consensus_structure_cla_cla != "dummy"
    )
  df_npc_cla <- df_npc_sup |>
    dplyr::filter(score_chemical >= score_chemical_npc_class) |>
    dplyr::filter(
      consensus_structure_npc_cla != "notAnnotated" &
        consensus_structure_npc_cla != "notConsistent" &
        consensus_structure_npc_cla != "dummy"
    )
  df_cla_par <- df_cla_cla |>
    dplyr::filter(score_chemical >= score_chemical_cla_parent) |>
    dplyr::filter(
      consensus_structure_cla_par != "notAnnotated" &
        consensus_structure_cla_par != "notConsistent" &
        consensus_structure_cla_par != "dummy"
    )

  log_debug(
    x = paste(
      "chemically informed scoring led to \n",
      crayon::silver(nrow(df_cla_kin |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::silver("(classyfire) kingdom"),
      "level, \n",
      crayon::cyan(nrow(df_npc_pat |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::cyan("(NPC) pathway"),
      "level, \n",
      crayon::magenta(nrow(df_cla_sup |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::magenta("(classyfire) superclass"),
      "level, \n",
      crayon::blue(nrow(df_npc_sup |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::blue("(NPC) superclass"),
      "level, \n",
      crayon::yellow(nrow(df_cla_cla |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::yellow("(classyfire) class"),
      "level, \n",
      crayon::green(nrow(df_npc_cla |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::green("(NPC) class"),
      "level, and \n",
      crayon::red(nrow(df_cla_par |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::red("(classyfire) parent"),
      "level. \n"
    )
  )
}
