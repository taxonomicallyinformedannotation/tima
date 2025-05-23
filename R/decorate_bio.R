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
  function(
    annot_table_wei_bio = get("annot_table_wei_bio", envir = parent.frame()),
    score_biological_kingdom = get(
      "score_biological_kingdom",
      envir = parent.frame()
    ),
    score_biological_phylum = get(
      "score_biological_phylum",
      envir = parent.frame()
    ),
    score_biological_class = get(
      "score_biological_class",
      envir = parent.frame()
    ),
    score_biological_order = get(
      "score_biological_order",
      envir = parent.frame()
    ),
    score_biological_family = get(
      "score_biological_family",
      envir = parent.frame()
    ),
    score_biological_tribe = get(
      "score_biological_tribe",
      envir = parent.frame()
    ),
    score_biological_genus = get(
      "score_biological_genus",
      envir = parent.frame()
    ),
    score_biological_species = get(
      "score_biological_species",
      envir = parent.frame()
    ),
    score_biological_variety = get(
      "score_biological_variety",
      envir = parent.frame()
    )
  ) {
    df_kin <- annot_table_wei_bio |>
      tidytable::filter(score_biological >= score_biological_kingdom * 1)
    df_phy <- df_kin |>
      tidytable::filter(score_biological >= score_biological_phylum * 1)
    df_cla <- df_phy |>
      tidytable::filter(score_biological >= score_biological_class * 1)
    df_ord <- df_cla |>
      tidytable::filter(score_biological >= score_biological_order * 1)
    df_fam <- df_ord |>
      tidytable::filter(score_biological >= score_biological_family * 1)
    df_tri <- df_fam |>
      tidytable::filter(score_biological >= score_biological_tribe * 1)
    df_gen <- df_tri |>
      tidytable::filter(score_biological >= score_biological_genus * 1)
    df_spe <- df_gen |>
      tidytable::filter(score_biological >= score_biological_species * 1)
    df_var <- df_spe |>
      tidytable::filter(score_biological >= score_biological_variety * 1)

    logger::log_info(
      "Taxonomically informed metabolite annotation led to \n",
      nrow(
        df_kin |>
          tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
      ),
      " annotations reranked at the kingdom level, \n",
      nrow(
        df_phy |>
          tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
      ),
      " annotations reranked at the phylum level, \n",
      nrow(
        df_cla |>
          tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
      ),
      " annotations reranked at the class level, \n",
      nrow(
        df_ord |>
          tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
      ),
      " annotations reranked at the order level, \n",
      nrow(
        df_fam |>
          tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
      ),
      " annotations reranked at the family level, \n",
      nrow(
        df_tri |>
          tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
      ),
      " annotations reranked at the tribe level, \n",
      nrow(
        df_gen |>
          tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
      ),
      " annotations reranked at the genus level, \n",
      nrow(
        df_spe |>
          tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
      ),
      " annotations reranked at the species level, and \n",
      nrow(
        df_var |>
          tidytable::distinct(candidate_structure_inchikey_connectivity_layer)
      ),
      " annotations reranked at the variety level. \n",
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
