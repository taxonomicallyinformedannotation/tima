utils::globalVariables(
  c(
    "annot_table_wei_bio",
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
#' @param annot_table_wei_bio Table to decorate
#' @param score_biological_kingdom  Kingdom score
#' @param score_biological_phylum  Phylum score
#' @param score_biological_class  Class score
#' @param score_biological_order  Order score
#' @param score_biological_family Family score
#' @param score_biological_genus Genus score
#' @param score_biological_species Species score
#' @param score_biological_variety Variety score
#'
#' @return Message indicating the number of annotations
#'     weighted at each biological level
#'
#' @export
#'
#' @examples NULL
decorate_bio <-
  function(annot_table_wei_bio = get("annot_table_wei_chemo",
             envir = parent.frame()
           ),
           score_biological_kingdom = get("score_biological_kingdom",
             envir = parent.frame()
           ),
           score_biological_phylum = get("score_biological_phylum",
             envir = parent.frame()
           ),
           score_biological_class = get("score_biological_class",
             envir = parent.frame()
           ),
           score_biological_order = get("score_biological_order",
             envir = parent.frame()
           ),
           score_biological_family = get("score_biological_family",
             envir = parent.frame()
           ),
           score_biological_genus = get("score_biological_genus",
             envir = parent.frame()
           ),
           score_biological_species = get("score_biological_species",
             envir = parent.frame()
           ),
           score_biological_variety = get("score_biological_variety",
             envir = parent.frame()
           )) {
    df_kin <- annot_table_wei_bio |>
      tidytable::filter(score_biological >= score_biological_kingdom)
    df_phy <- df_kin |>
      tidytable::filter(score_biological >= score_biological_phylum)
    df_cla <- df_phy |>
      tidytable::filter(score_biological >= score_biological_class)
    df_ord <- df_cla |>
      tidytable::filter(score_biological >= score_biological_order)
    df_fam <- df_ord |>
      tidytable::filter(score_biological >= score_biological_family)
    df_tri <- df_fam |>
      tidytable::filter(score_biological >= score_biological_tribe)
    df_gen <- df_tri |>
      tidytable::filter(score_biological >= score_biological_genus)
    df_spe <- df_gen |>
      tidytable::filter(score_biological >= score_biological_species)
    df_var <- df_spe |>
      tidytable::filter(score_biological >= score_biological_variety)

    log_debug(
      "taxonomically informed scoring led to \n",
      crayon::silver(nrow(
        df_kin |>
          tidytable::distinct(structure_inchikey_2D)
      )),
      "annotations reranked at the",
      crayon::silver("kingdom"),
      "level, \n",
      crayon::white(nrow(
        df_phy |>
          tidytable::distinct(structure_inchikey_2D)
      )),
      "annotations reranked at the",
      crayon::white("phylum"),
      "level, \n",
      crayon::cyan(nrow(
        df_cla |>
          tidytable::distinct(structure_inchikey_2D)
      )),
      "annotations reranked at the",
      crayon::cyan("class"),
      "level, \n",
      crayon::magenta(nrow(
        df_ord |>
          tidytable::distinct(structure_inchikey_2D)
      )),
      "annotations reranked at the",
      crayon::magenta("order"),
      "level, \n",
      crayon::blue(nrow(
        df_fam |>
          tidytable::distinct(structure_inchikey_2D)
      )),
      "annotations reranked at the",
      crayon::blue("family"),
      "level, \n",
      crayon::blue(nrow(
        df_tri |>
          tidytable::distinct(structure_inchikey_2D)
      )),
      "annotations reranked at the",
      crayon::blue("tribe"),
      "level, \n",
      crayon::yellow(nrow(
        df_gen |>
          tidytable::distinct(structure_inchikey_2D)
      )),
      "annotations reranked at the",
      crayon::yellow("genus"),
      "level, \n",
      crayon::green(nrow(
        df_spe |>
          tidytable::distinct(structure_inchikey_2D)
      )),
      "annotations reranked at the",
      crayon::green("species"),
      "level, \n",
      "and",
      crayon::red(nrow(
        df_var |>
          tidytable::distinct(structure_inchikey_2D)
      )),
      "annotations reranked at the",
      crayon::red("variety"),
      "level. \n"
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
