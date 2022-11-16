#' @title Prepare GNPS
#'
#' @param gnps_job_id TODO
#' @param nap_job_id TODO
#' @param output TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom dplyr mutate mutate_all na_if select
#' @importFrom readr read_delim write_delim
#' @importFrom stringr str_length
#'
#' @examples TODO
prepare_gnps <-
  function(gnps_job_id = params$gnps,
           nap_job_id = params$nap,
           output = params$output) {
    stopifnot("Your GNPS job ID is invalid" = stringr::str_length(gnps_job_id) == 32)
    if (!is.null(nap_job_id)) {
      stopifnot("Your NAP job ID is invalid" = stringr::str_length(nap_job_id) == 32)
    }

    log_debug("Loading and formatting GNPS results")
    ## See https://github.com/CCMS-UCSD/GNPS_Workflows/issues/747
    table <- read_results(id = gnps_job_id) |>
      dplyr::select(
        feature_id = `#Scan#`,
        smiles = Smiles,
        # smiles_2D, #' Not available for now
        inchikey = InChIKey,
        inchikey_2D = `InChIKey-Planar`,
        structure_taxonomy_npclassifier_01pathway = npclassifier_pathway,
        structure_taxonomy_npclassifier_02superclass = npclassifier_superclass,
        structure_taxonomy_npclassifier_03class = npclassifier_class,
        # molecular_formula, # Not available for now
        structure_exact_mass = ExactMass,
        score_input = MQScore
      ) |>
      dplyr::mutate(
        library = "GNPS",
        smiles_2D = NA,
        molecular_formula = NA
      ) |>
      complement_metadata()

    if (!is.null(nap_job_id)) {
      log_debug("Loading NAP results")
      ## TODO look at recent NAP outputs
      ## Might be outdated
      table <- read_nap(id = nap_job_id) |>
        dplyr::select(
          feature_id = cluster.index,
          score_input = FusionScore,
          smiles = FusionSMILES
        ) |>
        dplyr::mutate(
          library = "GNPS",
          smiles_2D = NA,
          inchikey = NA,
          inchikey_2D = NA,
          molecular_formula = NA,
          structure_taxonomy_npclassifier_01pathway = NA,
          structure_taxonomy_npclassifier_02superclass = NA,
          structure_taxonomy_npclassifier_03class = NA,
          structure_exact_mass = NA
        )
    }

    table <- table |>
      dplyr::mutate_all(dplyr::na_if, "N/A")

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_gnps")
    export_output(x = table)
  }
