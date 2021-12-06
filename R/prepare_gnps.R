#' Title
#'
#' @param gnps_job_id TODO
#' @param nap_job_id TODO
#' @param output TODO
#'
#' @return TODO
#' @export
#'
#' @examples
prepare_gnps <-
  function(gnps_job_id = params$gnps,
           nap_job_id = params$nap,
           output = params$output) {
    log_debug("Loading and formatting GNPS results")
    ## see https://github.com/CCMS-UCSD/GNPS_Workflows/issues/747
    table <- read_results(id = gnps_job_id) |>
      dplyr::select(
        feature_id = `#Scan#`,
        smiles = Smiles,
        # smiles_2D, ## not available for now
        inchikey = InChIKey,
        inchikey_2D = `InChIKey-Planar`,
        structure_taxonomy_npclassifier_01pathway = npclassifier_pathway,
        structure_taxonomy_npclassifier_02superclass = npclassifier_superclass,
        structure_taxonomy_npclassifier_03class = npclassifier_class,
        # molecular_formula, ## not available for now
        structure_exact_mass = ExactMass,
        score_input = MQScore
      ) |>
      dplyr::mutate(
        library = "GNPS",
        smiles_2D = NA,
        molecular_formula = NA
      )

    if (!is.null(nap_job_id)) {
      log_debug("Loading NAP results")
      ## TODO look at recent NAP outputs
      ## might be outdated
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

    table[] <-
      lapply(
        table,
        function(x) {
          y_as_na(x, y = "N/A")
        }
      )

    log_debug(x = "Exporting ...")
    ifelse(
      test = !dir.exists(paths$data$path),
      yes = dir.create(paths$data$path),
      no = paste(paths$data$path, "exists")
    )
    ifelse(
      test = !dir.exists(paths$data$interim$path),
      yes = dir.create(paths$data$interim$path),
      no = paste(paths$data$interim$path, "exists")
    )
    ifelse(
      test = !dir.exists(paths$data$interim$config$path),
      yes = dir.create(paths$data$interim$config$path),
      no = paste(paths$data$interim$config$path, "exists")
    )
    ifelse(
      test = !dir.exists(dirname(output)),
      yes = dir.create(dirname(output)),
      no = paste(dirname(output), "exists")
    )

    log_debug(
      x = "... path to export is",
      output
    )
    readr::write_delim(
      x = table,
      file = output,
      delim = "\t"
    )

    export_params(
      parameters = params,
      directory = paths$data$interim$config$path,
      step = "prepare_gnps"
    )
  }
