#' @title Prepare spectral matches
#'
#' @description This function prepares the spectral matches obtained previously to make them compatible
#'
#' @param input Input file
#' @param output Output file
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr distinct mutate
#' @importFrom readr read_delim write_delim
#'
#' @examples NULL
prepare_spectral_matches <-
  function(input = params$files$annotations$raw$spectral,
           output = params$files$annotations$pretreated) {
    # Check if input file exists
    stopifnot(
      "Input file(s) do(es) not exist" =
        rep(TRUE, length(input)) ==
          lapply(X = input, file.exists)
    )
    log_debug(x = "Loading and formatting spectral matches")
    # Read input file and select specific columns
    table <- lapply(X = input, FUN = readr::read_delim) |>
      dplyr::bind_rows() |>
      dplyr::distinct(
        feature_id,
        inchikey_2D = short_inchikey,
        smiles,
        molecular_formula,
        structure_exact_mass = exact_mass,
        score_input = msms_score
      ) |>
      # Add new columns
      dplyr::mutate(
        library = "ISDB",
        inchikey = NA,
        smiles_2D = smiles,
        structure_taxonomy_npclassifier_01pathway = NA,
        structure_taxonomy_npclassifier_02superclass = NA,
        structure_taxonomy_npclassifier_03class = NA,
        structure_exact_mass = as.numeric(structure_exact_mass)
      ) |>
      # Call complement_metadata function on the modified data frame
      complement_metadata()

    log_debug(x = "Exporting ...")
    # Call export_params and export_output functions
    export_params(step = "prepare_spectral_matches")
    export_output(x = table, file = output[[1]])
  }
