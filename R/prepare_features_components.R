#' @title Prepare features components
#'
#' @description This function prepares the components (clusters in molecular network) for further use
#'
#' @param input Input file if tool == 'manual'
#' @param output Output file
#' @param tool Tool used to generate components
#' @param components File containing the components if tool == 'manual'
#' @param gnps_job_id GNPS job ID
#' @param ms_mode Ionization mode. Must be 'pos' or 'neg'
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr arrange desc distinct left_join mutate select
#' @importFrom purrr map
#' @importFrom readr read_delim write_delim
#' @importFrom stringr str_length
#'
#' @examples NULL
prepare_features_components <- function(input = params$files$annotations$pretreated,
                                        output = params$files$annotations$filled,
                                        tool = params$tools$networks$spectral$components,
                                        components = params$files$networks$spectral$components$raw,
                                        gnps_job_id = params$gnps$id,
                                        ms_mode = params$ms$polarity,
                                        parameters = params) {
  if (tool == "gnps") {
    stopifnot("Your GNPS job ID is invalid" = stringr::str_length(string = gnps_job_id) == 32)
  } else {
    stopifnot(
      "Input file(s) do(es) not exist" =
        rep(TRUE, length(input)) ==
          lapply(X = input, file.exists)
    )
    stopifnot("Your components file does not exist" = file.exists(components))
  }
  stopifnot("Your mode must be 'pos' or 'neg'" = ms_mode %in% c("pos", "neg"))

  params <<- parameters
  log_debug(x = "Loading files ...")
  log_debug(x = "... features table")
  table <- lapply(
    X = input,
    FUN = readr::read_delim
  ) |>
    purrr::map(.f = dplyr::mutate_all, as.character) |>
    dplyr::bind_rows() |>
    dplyr::mutate(feature_id = as.numeric(feature_id))

  log_debug(x = "... cluster table")
  log_debug(x = "THIS STEP COULD BE IMPROVED BY CALCULATING THE CLUSTERS DIRECTLY")
  ## TODO
  if (tool == "gnps") {
    components <-
      read_clusters(id = gnps_job_id) |>
      dplyr::select(
        feature_id = `cluster index`,
        component_id = componentindex,
        rt = RTMean,
        mz = `precursor mass`
      ) |>
      dplyr::distinct()
  }

  if (tool == "manual") {
    manual_components <-
      readr::read_delim(file = components) |>
      dplyr::distinct(
        feature_id = feature_id,
        component_id = ComponentIndex,
        rt = rt,
        mz = mz
      )

    components <- manual_components |>
      dplyr::select(-component_id) |>
      dplyr::left_join(manual_components) |>
      dplyr::mutate(component_id = ifelse(
        test = is.na(component_id),
        yes = -1,
        no = component_id
      ))
  }

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
