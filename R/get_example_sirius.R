#' Title
#'
#' @param link TODO
#' @param export TODO
#'
#' @return TODO
#' @export
#'
#' @examples
get_example_sirius <-
  function(link = paths$links$example_sirius,
           export = paths$data$interim$annotations$example_sirius) {
    paths <- parse_yaml_paths()

    canopus_npc <- "canopus_npc_summary.csv"

    canopus_adducts <- "canopus_summary_adducts.tsv"

    compound <- "compound_identifications.tsv"

    compound_adducts <- "compound_identifications_adducts.tsv"

    formula <- "formula_identifications.tsv"

    formula_adducts <- "formula_identifications_adducts.tsv"

    ifelse(
      test = !dir.exists(dirname(dirname(export))),
      yes = dir.create(dirname(dirname(export))),
      no = paste(
        dirname(dirname(export)),
        "exists"
      )
    )
    ifelse(
      test = !dir.exists(dirname(export)),
      yes = dir.create(dirname(export)),
      no = paste(
        dirname(export),
        "exists"
      )
    )
    ifelse(
      test = !dir.exists(export),
      yes = dir.create(export),
      no = paste(
        export,
        "exists"
      )
    )

    readr::read_csv(file = file.path(link, canopus_npc)) |>
      readr::write_csv(file = file.path(export, canopus_npc))
    readr::read_tsv(file = file.path(link, canopus_adducts)) |>
      readr::write_tsv(file = file.path(export, canopus_adducts))
    readr::read_tsv(file = file.path(link, compound)) |>
      readr::write_tsv(file = file.path(export, compound))
    readr::read_tsv(file = file.path(link, compound_adducts)) |>
      readr::write_tsv(file = file.path(export, compound_adducts))
    readr::read_tsv(file = file.path(link, formula)) |>
      readr::write_tsv(file = file.path(export, formula))
    readr::read_tsv(file = file.path(link, formula_adducts)) |>
      readr::write_tsv(file = file.path(export, formula_adducts))
  }
