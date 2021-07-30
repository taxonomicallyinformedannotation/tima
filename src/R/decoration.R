require(crayon)
require(dplyr)

#' Title
#'
#' @return
#' @export
#'
#' @examples
ms1_decoration <- function() {
  suppressWarnings(
    cat(
      "MS1 annotation led to \n",
      crayon::green(
        nrow(
          annotation_table_ms1 |>
            dplyr::filter(!is.na(inchikey_2D) |
              inchikey_2D != "notAnnotated") |>
            dplyr::filter(score_input == 0)
        )
      ),
      crayon::green("annotations"),
      ", on \n",
      crayon::blue(
        nrow(
          annotation_table_ms1 |>
            dplyr::filter(!is.na(inchikey_2D) |
              inchikey_2D != "notAnnotated") |>
            dplyr::filter(score_input == 0) |>
            dplyr::distinct(feature_id)
        )
      ),
      crayon::blue("features"),
      ", of which \n",
      crayon::yellow(nrow(suppressMessages(
        dplyr::anti_join(
          x = annotation_table_ms1 |>
            dplyr::filter(
              !is.na(inchikey_2D) &
                inchikey_2D != "notAnnotated" &
                inchikey_2D != ""
            ) |>
            dplyr::filter(score_input == 0) |>
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

###############################################################################

#' Title
#'
#' @return
#' @export
#'
#' @examples
taxo_decoration <- function() {
  cat(
    "taxonomically informed scoring led to \n",
    crayon::silver(nrow(
      annotation_table_weighted_bio |>
        dplyr::filter(score_biological >= params$score$biological$kingdom)
    )),
    "annotations reranked at the",
    crayon::silver("kingdom"),
    "level, \n",
    crayon::white(nrow(
      annotation_table_weighted_bio |>
        dplyr::filter(score_biological >= params$score$biological$phylum)
    )),
    "annotations reranked at the",
    crayon::white("phylum"),
    "level, \n",
    crayon::cyan(nrow(
      annotation_table_weighted_bio |>
        dplyr::filter(score_biological >= params$score$biological$class)
    )),
    "annotations reranked at the",
    crayon::cyan("class"),
    "level, \n",
    crayon::magenta(nrow(
      annotation_table_weighted_bio |>
        dplyr::filter(score_biological >= params$score$biological$order)
    )),
    "annotations reranked at the",
    crayon::magenta("order"),
    "level, \n",
    crayon::blue(nrow(
      annotation_table_weighted_bio |>
        dplyr::filter(score_biological >= params$score$biological$family)
    )),
    "annotations reranked at the",
    crayon::blue("family"),
    "level, \n",
    crayon::yellow(nrow(
      annotation_table_weighted_bio |>
        dplyr::filter(score_biological >= params$score$biological$genus)
    )),
    "annotations reranked at the",
    crayon::yellow("genus"),
    "level, \n",
    crayon::green(nrow(
      annotation_table_weighted_bio |>
        dplyr::filter(score_biological >= params$score$biological$species)
    )),
    "annotations reranked at the",
    crayon::green("species"),
    "level, \n",
    "and",
    crayon::red(nrow(
      annotation_table_weighted_bio |>
        dplyr::filter(score_biological >= params$score$biological$variety)
    )),
    "annotations reranked at the",
    crayon::red("variety"),
    "level. \n"
  )
}

###############################################################################

#' Title
#'
#' @return
#' @export
#'
#' @examples
chemical_decoration <- function() {
  cat(
    x = paste(
      "chemically informed scoring led to \n",
      crayon::blue(
        nrow(
          annotation_table_weighted_chemo |>
            dplyr::filter(
              consensus_structure_pat != "notAnnotated" &
                consensus_structure_cla != "notConsistent" &
                consensus_structure_pat != "dummy"
            ) |>
            dplyr::filter(score_chemical >= params$score$chemical$pathway)
        )
      ),
      "annotations reranked at the",
      crayon::blue("pathway"),
      "level, \n",
      crayon::yellow(
        nrow(
          annotation_table_weighted_chemo |>
            dplyr::filter(
              consensus_structure_sup != "notAnnotated" &
                consensus_structure_cla != "notConsistent" &
                consensus_structure_sup != "dummy"
            ) |>
            dplyr::filter(score_chemical >= params$score$chemical$superclass)
        )
      ),
      "annotations reranked at the",
      crayon::yellow("superclass"),
      "level, and \n",
      crayon::green(
        nrow(
          annotation_table_weighted_chemo |>
            dplyr::filter(
              consensus_structure_cla != "notAnnotated" &
                consensus_structure_cla != "notConsistent" &
                consensus_structure_cla != "dummy"
            ) |>
            dplyr::filter(score_chemical >= params$score$chemical$class)
        )
      ),
      "annotations reranked at the",
      crayon::green("class"),
      "level. \n"
    )
  )
}
