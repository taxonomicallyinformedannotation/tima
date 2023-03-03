#' @title Decorate chemo
#'
#' @description This function outputs information about chemical weighting
#'
#' @param df Table to decorate
#' @param sc_pat Pathway score
#' @param sc_sup Superclass score
#' @param sc_cla Class score
#'
#' @return Message indicating the number of annotations weighted at each chemical level
#'
#' @export
#'
#' @examples NULL
decorate_chemo <- function(df = annotation_table_weighted_chemo,
                           sc_pat = score_chemical_pathway,
                           sc_sup = score_chemical_superclass,
                           sc_cla = score_chemical_class) {
  df_pat <- df |>
    dplyr::filter(score_chemical >= sc_pat) |>
    dplyr::filter(
      consensus_structure_pat != "notAnnotated" &
        consensus_structure_cla != "notConsistent" &
        consensus_structure_pat != "dummy"
    )
  df_sup <- df_pat |>
    dplyr::filter(score_chemical >= sc_sup) |>
    dplyr::filter(
      consensus_structure_sup != "notAnnotated" &
        consensus_structure_cla != "notConsistent" &
        consensus_structure_sup != "dummy"
    )
  df_cla <- df_sup |>
    dplyr::filter(score_chemical >= sc_cla) |>
    dplyr::filter(
      consensus_structure_cla != "notAnnotated" &
        consensus_structure_cla != "notConsistent" &
        consensus_structure_cla != "dummy"
    )

  log_debug(
    x = paste(
      "chemically informed scoring led to \n",
      crayon::blue(nrow(df_pat |>
        dplyr::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::blue("pathway"),
      "level, \n",
      crayon::yellow(nrow(df_sup |>
        dplyr::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::yellow("superclass"),
      "level, and \n",
      crayon::green(nrow(df_cla |>
        dplyr::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::green("class"),
      "level. \n"
    )
  )
}
