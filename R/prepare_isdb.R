#' Title
#'
#' @param input TODO
#' @param output TODO
#'
#' @return TODO
#' @export
#'
#' @examples
prepare_isdb <-
  function(input = params$input,
           output = params$output) {
    stopifnot("Your input file does not exist" = file.exists(input))

    log_debug(x = "Loading and formatting ISDB results")
    table <- readr::read_delim(file = input) |>
      dplyr::distinct(
        feature_id,
        inchikey_2D = short_inchikey,
        smiles,
        molecular_formula,
        structure_exact_mass = exact_mass,
        score_input = msms_score
      ) |>
      dplyr::mutate(
        library = "ISDB",
        inchikey = NA,
        smiles_2D = smiles,
        structure_taxonomy_npclassifier_01pathway = NA,
        structure_taxonomy_npclassifier_02superclass = NA,
        structure_taxonomy_npclassifier_03class = NA,
      )

    log_debug(x = "Exporting ...")
    ifelse(
      test = !dir.exists(paths$data$path),
      yes = dir.create(paths$data$path),
      no = paste(paths$data$path, "exists")
    )
    ifelse(
      test = !dir.exists(paths$data$interim$path),
      yes = dir.create(paths$data$interim$path),
      no = paste(paths$data$interim$path, "exists")
    )
    ifelse(
      test = !dir.exists(paths$data$interim$config$path),
      yes = dir.create(paths$data$interim$config$path),
      no = paste(paths$data$interim$config$path, "exists")
    )
    ifelse(
      test = !dir.exists(dirname(output)),
      yes = dir.create(dirname(output)),
      no = paste(dirname(output), "exists")
    )

    log_debug(
      x = "... path to export is",
      output
    )
    readr::write_delim(
      x = table,
      file = output,
      delim = "\t"
    )

    export_params(
      parameters = params,
      directory = paths$data$interim$config$path,
      step = "prepare_isdb"
    )
  }
