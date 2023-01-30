#' @title Decorate MS1
#'
#' @description This function outputs informations about MS1 annotation
#'
#' @return Message indicating the number of annotations obtained by MS1
#'
#' @export
#'
#' @importFrom crayon blue cyan green magenta red silver white yellow
#' @importFrom dplyr anti_join distinct filter
#'
#' @examples NULL
decorate_ms1 <- function() {
  suppressWarnings(
    log_debug(
      "MS1 annotation led to \n",
      crayon::green(
        nrow(
          annotation_table_ms1 |>
            dplyr::filter(score_input == 0) |>
            dplyr::filter(!is.na(inchikey_2D) |
              inchikey_2D != "notAnnotated") |>
            dplyr::distinct(inchikey_2D)
        )
      ),
      crayon::green("annotations"),
      ", on \n",
      crayon::blue(
        nrow(
          annotation_table_ms1 |>
            dplyr::filter(score_input == 0) |>
            dplyr::filter(!is.na(inchikey_2D) |
              inchikey_2D != "notAnnotated") |>
            dplyr::distinct(feature_id)
        )
      ),
      crayon::blue("features"),
      ", of which \n",
      crayon::yellow(nrow(suppressMessages(
        dplyr::anti_join(
          x = annotation_table_ms1 |>
            dplyr::filter(score_input == 0) |>
            dplyr::filter(
              !is.na(inchikey_2D) &
                inchikey_2D != "notAnnotated" &
                inchikey_2D != ""
            ) |>
            dplyr::distinct(feature_id),
          y = metadata_table_spectral_annotation |>
            dplyr::filter(!is.na(inchikey_2D) &
              inchikey_2D != "") |>
            dplyr::distinct(feature_id),
        )
      ))),
      "were",
      crayon::yellow("not previously annotated. \n")
    )
  )
}


#' @title Decorate bio
#'
#' @description This function outputs informations about biological weighting
#'
#' @return Message indicating the number of annotations weighted at each biological level
#'
#' @export
#'
#' @importFrom crayon blue cyan green magenta red silver white yellow
#' @importFrom dplyr distinct filter
#'
#' @examples NULL
decorate_bio <- function() {
  log_debug(
    "taxonomically informed scoring led to \n",
    crayon::silver(
      nrow(
        annotation_table_weighted_bio |>
          dplyr::filter(score_biological >= score_biological_kingdom) |>
          dplyr::distinct(inchikey_2D)
      )
    ),
    "annotations reranked at the",
    crayon::silver("kingdom"),
    "level, \n",
    crayon::white(
      nrow(
        annotation_table_weighted_bio |>
          dplyr::filter(score_biological >= score_biological_phylum) |>
          dplyr::distinct(inchikey_2D)
      )
    ),
    "annotations reranked at the",
    crayon::white("phylum"),
    "level, \n",
    crayon::cyan(
      nrow(
        annotation_table_weighted_bio |>
          dplyr::filter(score_biological >= score_biological_class) |>
          dplyr::distinct(inchikey_2D)
      )
    ),
    "annotations reranked at the",
    crayon::cyan("class"),
    "level, \n",
    crayon::magenta(
      nrow(
        annotation_table_weighted_bio |>
          dplyr::filter(score_biological >= score_biological_order) |>
          dplyr::distinct(inchikey_2D)
      )
    ),
    "annotations reranked at the",
    crayon::magenta("order"),
    "level, \n",
    crayon::blue(
      nrow(
        annotation_table_weighted_bio |>
          dplyr::filter(score_biological >= score_biological_family) |>
          dplyr::distinct(inchikey_2D)
      )
    ),
    "annotations reranked at the",
    crayon::blue("family"),
    "level, \n",
    crayon::yellow(
      nrow(
        annotation_table_weighted_bio |>
          dplyr::filter(score_biological >= score_biological_genus) |>
          dplyr::distinct(inchikey_2D)
      )
    ),
    "annotations reranked at the",
    crayon::yellow("genus"),
    "level, \n",
    crayon::green(
      nrow(
        annotation_table_weighted_bio |>
          dplyr::filter(score_biological >= score_biological_species) |>
          dplyr::distinct(inchikey_2D)
      )
    ),
    "annotations reranked at the",
    crayon::green("species"),
    "level, \n",
    "and",
    crayon::red(
      nrow(
        annotation_table_weighted_bio |>
          dplyr::filter(score_biological >= score_biological_variety) |>
          dplyr::distinct(inchikey_2D)
      )
    ),
    "annotations reranked at the",
    crayon::red("variety"),
    "level. \n"
  )
}


#' @title Decorate chemo
#'
#' @description This function outputs informations about chemical weighting
#'
#' @return Message indicating the number of annotations weighted at each chemical level
#'
#' @importFrom crayon blue green yellow
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples NULL
decorate_chemo <- function() {
  log_debug(
    x = paste(
      "chemically informed scoring led to \n",
      crayon::blue(
        nrow(
          annotation_table_weighted_chemo |>
            dplyr::filter(score_chemical >= score_chemical_pathway) |>
            dplyr::filter(
              consensus_structure_pat != "notAnnotated" &
                consensus_structure_cla != "notConsistent" &
                consensus_structure_pat != "dummy"
            ) |>
            dplyr::distinct(inchikey_2D)
        )
      ),
      "annotations reranked at the",
      crayon::blue("pathway"),
      "level, \n",
      crayon::yellow(
        nrow(
          annotation_table_weighted_chemo |>
            dplyr::filter(score_chemical >= score_chemical_superclass) |>
            dplyr::filter(
              consensus_structure_sup != "notAnnotated" &
                consensus_structure_cla != "notConsistent" &
                consensus_structure_sup != "dummy"
            ) |>
            dplyr::distinct(inchikey_2D)
        )
      ),
      "annotations reranked at the",
      crayon::yellow("superclass"),
      "level, and \n",
      crayon::green(
        nrow(
          annotation_table_weighted_chemo |>
            dplyr::filter(score_chemical >= score_chemical_class) |>
            dplyr::filter(
              consensus_structure_cla != "notAnnotated" &
                consensus_structure_cla != "notConsistent" &
                consensus_structure_cla != "dummy"
            ) |>
            dplyr::distinct(inchikey_2D)
        )
      ),
      "annotations reranked at the",
      crayon::green("class"),
      "level. \n"
    )
  )
}
