#' @title Fake features components
#'
#' @param input TODO
#' @param features TODO
#' @param output TODO
#' @param ms_mode TODO
#' @param name_rt TODO
#' @param name_mz TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom dplyr arrange desc distinct left_join mutate select
#' @importFrom readr read_delim write_delim
#'
#' @examples TODO
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
  export_params(step = "prepare_features_components")
  export_output(x = table_filled, file = output)
}
