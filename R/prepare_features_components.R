#' @title Prepare features components
#'
#' @description This function prepares the components (clusters in molecular network) for further use
#'
#' @param input Input file if tool == 'manual'
#' @param output Output file
#' @param components File containing the components if tool == 'manual'
#' @param ms_mode Ionization mode. Must be 'pos' or 'neg'
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr arrange desc distinct left_join mutate select
#' @importFrom readr cols read_tsv write_delim
#'
#' @examples NULL
prepare_features_components <- function(input = params$files$annotations$pretreated,
                                        output = params$files$annotations$filled,
                                        components = params$files$networks$spectral$components$raw,
                                        ms_mode = params$ms$polarity,
                                        parameters = params) {
  params <<- parameters
  stopifnot("Your components file does not exist" = file.exists(components))
  stopifnot("Your mode must be 'pos' or 'neg'" = ms_mode %in% c("pos", "neg"))

  log_debug(x = "Loading files ...")
  log_debug(x = "... features table")
  table <- lapply(
    X = input,
    FUN = readr::read_tsv,
    col_types = readr::cols(.default = "c")
  ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(feature_id = as.numeric(feature_id))

  log_debug(x = "... cluster table")
  log_debug(x = "THIS STEP COULD BE IMPROVED BY CALCULATING THE CLUSTERS DIRECTLY")
  components <- readr::read_delim(file = components) |>
    dplyr::select(
      feature_id = `cluster index`,
      component_id = componentindex,
      rt = RTMean,
      mz = `precursor mass`
    ) |>
    dplyr::distinct()

  log_debug(x = "Adding components to features")
  table_filled <-
    dplyr::left_join(components, table) |>
    dplyr::distinct() |>
    dplyr::arrange(dplyr::desc(score_input)) |>
    dplyr::arrange(as.numeric(feature_id)) |>
    dplyr::select(
      feature_id,
      component_id,
      rt,
      mz,
      structure_inchikey_2D,
      structure_smiles_2D,
      structure_name,
      structure_exact_mass,
      structure_molecular_formula,
      structure_xlogp,
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

  return(output)
}
