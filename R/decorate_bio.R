utils::globalVariables(
  c(
    "score_biological",
    "score_biological_class",
    "score_biological_family",
    "score_biological_genus",
    "score_biological_kingdom",
    "score_biological_order",
    "score_biological_phylum",
    "score_biological_species",
    "score_biological_variety",
    "structure_inchikey_2D"
  )
)

#' @title Decorate bio
#'
#' @description This function outputs information about biological weighting
#'
#' @param df Table to decorate
#' @param sc_kin Kingdom score
#' @param sc_phy Phylum score
#' @param sc_cla Class score
#' @param sc_ord Order score
#' @param sc_fam Family score
#' @param sc_gen Genus score
#' @param sc_spe Species score
#' @param sc_var Variety score
#'
#' @return Message indicating the number of annotations weighted at each biological level
#'
#' @export
#'
#' @examples NULL
decorate_bio <-
  function(df = annotation_table_weighted_bio,
           sc_kin = score_biological_kingdom,
           sc_phy = score_biological_phylum,
           sc_cla = score_biological_class,
           sc_ord = score_biological_order,
           sc_fam = score_biological_family,
           sc_gen = score_biological_genus,
           sc_spe = score_biological_species,
           sc_var = score_biological_variety) {
    df_kin <- df |>
      tidytable::filter(score_biological >= sc_kin)
    df_phy <- df_kin |>
      tidytable::filter(score_biological >= sc_phy)
    df_cla <- df_phy |>
      tidytable::filter(score_biological >= sc_cla)
    df_ord <- df_cla |>
      tidytable::filter(score_biological >= sc_ord)
    df_fam <- df_ord |>
      tidytable::filter(score_biological >= sc_fam)
    df_gen <- df_fam |>
      tidytable::filter(score_biological >= sc_gen)
    df_spe <- df_gen |>
      tidytable::filter(score_biological >= sc_spe)
    df_var <- df_spe |>
      tidytable::filter(score_biological >= sc_var)

    log_debug(
      "taxonomically informed scoring led to \n",
      crayon::silver(nrow(df_kin |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::silver("kingdom"),
      "level, \n",
      crayon::white(nrow(df_phy |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::white("phylum"),
      "level, \n",
      crayon::cyan(nrow(df_cla |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::cyan("class"),
      "level, \n",
      crayon::magenta(nrow(df_ord |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::magenta("order"),
      "level, \n",
      crayon::blue(nrow(df_fam |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::blue("family"),
      "level, \n",
      crayon::yellow(nrow(df_gen |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::yellow("genus"),
      "level, \n",
      crayon::green(nrow(df_spe |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::green("species"),
      "level, \n",
      "and",
      crayon::red(nrow(df_var |>
        tidytable::distinct(structure_inchikey_2D))),
      "annotations reranked at the",
      crayon::red("variety"),
      "level. \n"
    )
  }
