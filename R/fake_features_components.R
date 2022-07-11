if (!require(dplyr)) {
  install.packages("dplyr")
  require(
    package = "dplyr",
    quietly = TRUE,
    warn.conflicts = FALSE
  )
}
if (!require(readr)) {
  install.packages("readr")
  require(package = "readr", quietly = TRUE)
}

#' Title
#'
#' @param input TODO
#' @param features TODO
#' @param output TODO
#' @param ms_mode TODO
#' @param name_rt TODO
#' @param name_mz TODO
#'
#' @return TODO
#' @export
#'
#' @importFrom dplyr arrange desc distinct left_join mutate select
#' @importFrom readr read_delim write_delim
#'
#' @examples
fake_features_components <- function(input = params$input,
                                     features = params$features,
                                     output = params$output,
                                     ms_mode = params$mode,
                                     name_rt = params$rt_name,
                                     name_mz = params$precursor_name) {
  stopifnot("Your input file does not exist" = file.exists(input))
  stopifnot("Your features file does not exist" = file.exists(features))
  stopifnot("Your mode must be 'pos' or 'neg'" = ms_mode %in% c("pos", "neg"))

  log_debug(x = "Loading files ...")
  log_debug(x = "... features table")
  table <- readr::read_delim(file = input)
  features <- readr::read_delim(file = features)

  components <- table |>
    dplyr::mutate(component_id = -1)

  features_meta <- features |>
    dplyr::select(
      feature_id,
      rt := !!as.name(name_rt),
      mz := !!as.name(name_mz)
    )

  log_debug(x = "Adding components to features")
  table_filled <-
    dplyr::left_join(components, table) |>
    dplyr::left_join(features_meta) |>
    dplyr::distinct() |>
    dplyr::arrange(dplyr::desc(score_input)) |>
    dplyr::arrange(as.numeric(feature_id)) |>
    dplyr::select(
      feature_id,
      component_id,
      rt,
      mz,
      inchikey_2D,
      smiles_2D,
      molecular_formula,
      structure_exact_mass,
      score_input,
      library,
      structure_taxonomy_npclassifier_01pathway,
      structure_taxonomy_npclassifier_02superclass,
      structure_taxonomy_npclassifier_03class
    )

  log_debug(x = "Calculating mz error")
  ## TODO can be improved
  if (ms_mode == "pos") {
    table_filled <- table_filled |>
      dplyr::mutate(mz_error = mz - 1.007276 - structure_exact_mass)
  } else {
    table_filled <- table_filled |>
      dplyr::mutate(mz_error = mz + 1.007276 - structure_exact_mass)
  }

  log_debug(x = "Exporting ...")
  ifelse(
    test = !dir.exists(paths$data$interim$path),
    yes = dir.create(paths$data$interim$path),
    no = paste(paths$data$interim$path, "exists")
  )
  ifelse(
    test = !dir.exists(paths$data$interim$annotations$path),
    yes = dir.create(paths$data$interim$annotations$path),
    no = paste(paths$data$interim$annotations$path, "exists")
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
    x = table_filled,
    file = output,
    delim = "\t"
  )

  export_params(
    parameters = params,
    directory = paths$data$interim$config$path,
    step = "prepare_features_components"
  )
}
