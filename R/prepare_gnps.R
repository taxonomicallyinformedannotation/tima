#' @title Prepare GNPS
#'
#' @description This function prepares GNPS obtained annotations for further use
#'
#' @param gnps_job_id GNPS job ID
#' @param output Output file
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr mutate mutate_all na_if select
#' @importFrom readr read_delim write_delim
#' @importFrom stringr str_length
#'
#' @examples NULL
prepare_gnps <-
  function(gnps_job_id = params$gnps$id,
           output = params$files$annotations$pretreated) {
    stopifnot("Your GNPS job ID is invalid" = stringr::str_length(string = gnps_job_id) == 32)

    log_debug("Loading and formatting GNPS results")
    ## See https://github.com/CCMS-UCSD/GNPS_Workflows/issues/747
    table <- read_results(id = gnps_job_id) |>
      dplyr::select(
        feature_id = `#Scan#`,
        smiles = Smiles,
        score_input = MQScore,
        # smiles_2D, ## Not available for now
        inchikey = InChIKey,
        inchikey_2D = `InChIKey-Planar`,
        structure_taxonomy_npclassifier_01pathway = npclassifier_pathway,
        structure_taxonomy_npclassifier_02superclass = npclassifier_superclass,
        structure_taxonomy_npclassifier_03class = npclassifier_class,
        # molecular_formula, ## Not available for now
        structure_exact_mass = ExactMass
      ) |>
      dplyr::mutate(
        library = "GNPS",
        smiles_2D = NA,
        molecular_formula = NA
      ) |>
      complement_metadata() |>
      dplyr::mutate_all(as.character) |>
      dplyr::mutate_all(dplyr::na_if, "N/A")

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_gnps")
    export_output(x = table, file = output[[1]])
  }
