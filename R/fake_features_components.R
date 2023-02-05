#' @title Fake features components
#'
#' @description This function fakes features components in case none are given
#'
#' @param input Path to the file containing the input data
#' @param features Path to the file containing the features data
#' @param output Path to the file to export the merged data to
#' @param ms_mode Mode of mass spectrometry, either "pos" or "neg"
#' @param name_rt Name of the retention time column in the features data
#' @param name_mz Name of the m/z column in the features data
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr arrange desc distinct left_join mutate select
#' @importFrom readr read_delim write_delim
#'
#' @examples NULL
fake_features_components <- function(input = params$input,
                                     features = params$features,
                                     output = params$output,
                                     ms_mode = params$mode,
                                     name_rt = params$rt_name,
                                     name_mz = params$precursor_name) {
  # Check that input and features files exist
  stopifnot("Your input file does not exist" = file.exists(input))
  stopifnot("Your features file does not exist" = file.exists(features))

  # Check that ms_mode is valid
  stopifnot("Your mode must be 'pos' or 'neg'" = ms_mode %in% c("pos", "neg"))

  # Read input and features files
  log_debug("Loading files ...")
  log_debug("... features table")
  table <- readr::read_delim(file = input) |>
    dplyr::mutate(feature_id = as.numeric(feature_id))
  features <- readr::read_delim(file = features) |>
    dplyr::mutate(feature_id = as.numeric(feature_id))

  # Initialize components data frame
  components <- dplyr::mutate(table, component_id = -1)

  # Select relevant columns from features data frame
  features_meta <-
    dplyr::select(
      features,
      feature_id,
      rt := !!as.name(name_rt),
      mz := !!as.name(name_mz)
    )

  # Merge data frames and remove duplicate rows
  log_debug("Adding components to features")
  table_filled <-
    dplyr::arrange(
      dplyr::distinct(dplyr::left_join(
        dplyr::left_join(components, table), features_meta
      )),
      dplyr::desc(score_input),
      as.numeric(feature_id)
    ) |>
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
      dplyr::mutate(mz_error = as.numeric(mz) -
        1.007276 -
        as.numeric(structure_exact_mass))
  } else {
    table_filled <- table_filled |>
      dplyr::mutate(mz_error = as.numeric(mz) + 1.007276 - as.numeric(structure_exact_mass))
  }

  log_debug(x = "Exporting ...")
  export_params(step = "prepare_features_components")
  export_output(x = table_filled, file = output)
}
