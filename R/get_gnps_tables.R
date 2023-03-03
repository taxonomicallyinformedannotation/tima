#' @title Get GNPS Tables
#'
#' @description This function gets GNPS tables from corresponding job ID.
#'
#' @param gnps_job_id GNPS job ID
#' @param workflow Character string indicating the type of workflow, either "fbmn" or "classical"
#' @param path_source Path to store the source files
#' @param path_interim_a Path to store the interim annotations file
#' @param path_interim_f Path to store the interim features files
#'
#' @return The downloaded GNPS tables
#'
#' @export
#'
#' @examples NULL
get_gnps_tables <-
  function(gnps_job_id,
           workflow = "fbmn",
           path_source = paths$data$source$path,
           path_interim_a = paths$data$interim$annotations$path,
           path_interim_f = paths$data$interim$features$path) {
    stopifnot("Your GNPS job ID is invalid" = stringr::str_length(string = gnps_job_id) == 32)
    stopifnot(
      "Your workflow is not supported, supported workflows are 'fbmn' and 'classical'" = workflow %in% c("fbmn", "classical")
    )

    gnps_url <-
      "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task="
    gnps_block <- "&block=main&file="


    ## Little hack for the example
    if (gnps_job_id == "96fa7c88200e4a03bee4644e581e3fb0") {
      names(gnps_job_id) <- "example"
    } else {
      names(gnps_job_id) <- gnps_job_id
    }

    log_debug("... Inputs")
    log_debug("... Features")
    file_features <-
      file.path(path_source, paste0(names(gnps_job_id), "_features.csv"))
    if (!file.exists(file_features)) {
      get_file(
        url = paste0(
          gnps_url,
          gnps_job_id,
          gnps_block,
          "quantification_table_reformatted/"
        ),
        export = file_features
      )
    }
    log_debug("... Metadata")
    file_metadata <-
      file.path(
        path_source,
        paste0(names(gnps_job_id), "_metadata.tsv")
      )
    if (!file.exists(file_metadata)) {
      get_file(
        url = paste0(
          gnps_url,
          gnps_job_id,
          gnps_block,
          "metadata_table/"
        ),
        export = file_metadata
      )
    }

    log_debug("... Spectra")
    file_spectra <-
      file.path(path_source, paste0(names(gnps_job_id), "_spectra.mgf"))
    if (!file.exists(file_spectra)) {
      get_file(
        url = paste0(
          gnps_url,
          gnps_job_id,
          gnps_block,
          "spectra/"
        ),
        export = file_spectra
      )
    }

    log_debug("... Results")
    log_debug("... Annotations")
    file_annotations <- file.path(
      path_interim_a,
      paste0(names(gnps_job_id), "_gnps.tsv")
    )
    if (!file.exists(file_annotations)) {
      get_file(
        url = paste0(
          gnps_url,
          gnps_job_id,
          gnps_block,
          switch(workflow,
            "fbmn" = "DB_result/",
            "classical" = "result_specnets_DB/"
          )
        ),
        export = file_annotations
      )
    }
    log_debug("... Components")
    file_components <- file.path(
      path_interim_f,
      paste0(names(gnps_job_id), "_components.tsv")
    )
    if (!file.exists(file_components)) {
      get_file(
        url = paste0(
          gnps_url,
          gnps_job_id,
          gnps_block,
          switch(workflow,
            "fbmn" = "clusterinfo_summary/",
            "classical" = "clusterinfosummarygroup_attributes_withIDs_withcomponentID/"
          )
        ),
        export = file_components
      )
    }
    log_debug("... Edges")
    file_edges <- file.path(
      path_interim_f,
      paste0(names(gnps_job_id), "_edges_spectra.tsv")
    )
    if (!file.exists(file_edges)) {
      get_file(
        url = paste0(
          gnps_url,
          gnps_job_id,
          gnps_block,
          "networkedges_selfloop/"
        ),
        export = file_edges
      )
    }

    return(
      c(
        "features" = file_features,
        "metadata" = file_metadata,
        "spectra" = file_spectra,
        "annotations" = file_annotations,
        "components" = file_components,
        "edges" = file_edges
      )
    )
  }
