#' @title Prepare GNPS
#'
#' @description This function prepares GNPS obtained annotations for further use
#'
#' @param gnps_job_id GNPS job ID
#' @param output Output file
#' @param parameters Parameters
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
           output = params$files$annotations$pretreated,
           parameters = params) {
    if (!is.null(gnps_job_id)) {
      stopifnot("Your GNPS job ID is invalid" = stringr::str_length(string = gnps_job_id) == 32)
      params <<- parameters
      log_debug("Loading and formatting GNPS results")
      ## See https://github.com/CCMS-UCSD/GNPS_Workflows/issues/747
      table <- read_results(id = gnps_job_id) |>
        dplyr::select(
          feature_id = `#Scan#`,
          structure_name = Compound_Name,
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
          molecular_formula = NA,
          structure_xlogp = NA,
          ## Only partially present
          structure_taxonomy_classyfire_chemontid = NA,
          structure_taxonomy_classyfire_01kingdom = NA,
          structure_taxonomy_classyfire_02superclass = NA,
          structure_taxonomy_classyfire_03class = NA,
          structure_taxonomy_classyfire_04directparent = NA
        ) |>
        complement_metadata() |>
        dplyr::select(
          feature_id,
          structure_name,
          inchikey,
          inchikey_2D,
          smiles,
          smiles_2D,
          molecular_formula,
          structure_exact_mass,
          structure_xlogp,
          library,
          score_input,
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class,
          structure_taxonomy_classyfire_chemontid,
          structure_taxonomy_classyfire_01kingdom,
          structure_taxonomy_classyfire_02superclass,
          structure_taxonomy_classyfire_03class,
          structure_taxonomy_classyfire_04directparent
        ) |>
        dplyr::mutate_all(as.character) |>
        dplyr::mutate_all(dplyr::na_if, "N/A")
    } else {
      log_debug("No GNPS job ID provided, returning an empty file instead")
      table <- data.frame(
        feature_id = NA,
        structure_name = NA,
        inchikey = NA,
        inchikey_2D = NA,
        smiles = NA,
        smiles_2D = NA,
        molecular_formula = NA,
        structure_exact_mass = NA,
        structure_xlogp = NA,
        library = NA,
        score_input = NA,
        structure_taxonomy_npclassifier_01pathway = NA,
        structure_taxonomy_npclassifier_02superclass = NA,
        structure_taxonomy_npclassifier_03class = NA,
        structure_taxonomy_classyfire_chemontid = NA,
        structure_taxonomy_classyfire_01kingdom = NA,
        structure_taxonomy_classyfire_02superclass = NA,
        structure_taxonomy_classyfire_03class = NA,
        structure_taxonomy_classyfire_04directparent = NA
      )
    }
    log_debug(x = "Exporting ...")
    export_params(step = "prepare_gnps")
    export_output(x = table, file = output[[1]])
    return(output[[1]])
  }
