import::from(crayon, blue, .into = environment())
import::from(crayon, cyan, .into = environment())
import::from(crayon, green, .into = environment())
import::from(crayon, magenta, .into = environment())
import::from(crayon, red, .into = environment())
import::from(crayon, silver, .into = environment())
import::from(crayon, white, .into = environment())
import::from(crayon, yellow, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, filter, .into = environment())

#' @title Decorate bio
#'
#' @description This function outputs information about biological weighting
#'
#' @export
#'
#' @importFrom crayon blue
#' @importFrom crayon cyan
#' @importFrom crayon green
#' @importFrom crayon magenta
#' @importFrom crayon red
#' @importFrom crayon silver
#' @importFrom crayon white
#' @importFrom crayon yellow
#' @importFrom tidytable distinct
#' @importFrom tidytable filter
#'
#' @noRd
#'
#' @param annot_table_wei_bio Table to decorate
#' @param score_biological_kingdom  Kingdom score
#' @param score_biological_phylum  Phylum score
#' @param score_biological_class  Class score
#' @param score_biological_order  Order score
#' @param score_biological_family Family score
#' @param score_biological_tribe Tribe score
#' @param score_biological_genus Genus score
#' @param score_biological_species Species score
#' @param score_biological_variety Variety score
#'
#' @return Message indicating the number of annotations
#'     weighted at each biological level
#'
#' @examples NULL
decorate_bio <-
  function(annot_table_wei_bio = get("annot_table_wei_chemo", envir = parent.frame()),
           score_biological_kingdom = get("score_biological_kingdom", envir = parent.frame()),
           score_biological_phylum = get("score_biological_phylum", envir = parent.frame()),
           score_biological_class = get("score_biological_class", envir = parent.frame()),
           score_biological_order = get("score_biological_order", envir = parent.frame()),
           score_biological_family = get("score_biological_family", envir = parent.frame()),
           score_biological_tribe = get("score_biological_tribe", envir = parent.frame()),
           score_biological_genus = get("score_biological_genus", envir = parent.frame()),
           score_biological_species = get("score_biological_species", envir = parent.frame()),
           score_biological_variety = get("score_biological_variety", envir = parent.frame())) {
    df_kin <- annot_table_wei_bio |>
      filter(score_biological >= score_biological_kingdom * 1)
    df_phy <- df_kin |>
      filter(score_biological >= score_biological_phylum * 1)
    df_cla <- df_phy |>
      filter(score_biological >= score_biological_class * 1)
    df_ord <- df_cla |>
      filter(score_biological >= score_biological_order * 1)
    df_fam <- df_ord |>
      filter(score_biological >= score_biological_family * 1)
    df_tri <- df_fam |>
      filter(score_biological >= score_biological_tribe * 1)
    df_gen <- df_tri |>
      filter(score_biological >= score_biological_genus * 1)
    df_spe <- df_gen |>
      filter(score_biological >= score_biological_species * 1)
    df_var <- df_spe |>
      filter(score_biological >= score_biological_variety * 1)

    log_debug(
      "taxonomically informed scoring led to \n",
      silver(nrow(
        df_kin |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      silver("kingdom"),
      "level, \n",
      white(nrow(
        df_phy |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      white("phylum"),
      "level, \n",
      cyan(nrow(
        df_cla |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      cyan("class"),
      "level, \n",
      magenta(nrow(
        df_ord |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      magenta("order"),
      "level, \n",
      blue(nrow(
        df_fam |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      blue("family"),
      "level, \n",
      blue(nrow(
        df_tri |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      blue("tribe"),
      "level, \n",
      yellow(nrow(
        df_gen |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      yellow("genus"),
      "level, \n",
      green(nrow(
        df_spe |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      green("species"),
      "level, \n",
      "and",
      red(nrow(
        df_var |>
          distinct(candidate_structure_inchikey_no_stereo)
      )),
      "annotations reranked at the",
      red("variety"),
      "level. \n",
      "WITHOUT TAKING CONSISTENCY SCORE INTO ACCOUNT! (for later predictions)"
    )
    rm(
      df_kin,
      df_phy,
      df_cla,
      df_ord,
      df_fam,
      df_tri,
      df_gen,
      df_spe,
      df_var
    )
    return(annot_table_wei_bio)
  }
