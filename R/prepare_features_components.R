#' @title Prepare features components
#'
#' @description This function prepares the components (clusters in molecular network) for further use
#'
#' @param input Input file
#' @param output Output file
#' @param components File containing the components
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_features_components <- function(input = params$files$annotations$pretreated,
                                        output = params$files$annotations$filled,
                                        components = params$files$networks$spectral$components$raw,
                                        parameters = params) {
  # Check if input file(s) exists
  stopifnot(
    "Input file(s) do(es) not exist" =
      rep(TRUE, length(input)) ==
        lapply(X = input, file.exists)
  )
  stopifnot("Your components file does not exist" = file.exists(components))
  params <<- parameters

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
      rt_error,
      mz_error,
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

  log_debug(x = "Exporting ...")
  export_params(step = "prepare_features_components")
  export_output(x = table_filled, file = output)

  return(output)
}
