#' @title Prepare ISDB
#'
#' @param input TODO
#' @param output TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom dplyr distinct mutate
#' @importFrom readr read_delim write_delim
#'
#' @examples TODO
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
      ) |>
      complement_metadata()

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_isdb")
    export_output(x = table, file = output)
  }
