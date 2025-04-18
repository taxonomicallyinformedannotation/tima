#' @title Get GNPS Tables
#'
#' @description This function gets GNPS tables from corresponding job ID.
#'
#' @include get_default_paths.R
#' @include get_file.R
#'
#' @param gnps_job_id GNPS job ID
#' @param gnps_job_example GNPS job example
#' @param filename Name of the file
#' @param workflow Character string indicating the type of workflow,
#'    either "fbmn" or "classical"
#' @param path_features Path to features
#' @param path_metadata Path to metadata
#' @param path_spectra Path to spectra
#' @param path_source Path to store the source files
#' @param path_interim_a Path to store the interim annotations file
#' @param path_interim_f Path to store the interim features files
#'
#' @return The downloaded GNPS tables
#'
#' @examples NULL
get_gnps_tables <-
  function(
    gnps_job_id,
    gnps_job_example = get_default_paths()$gnps$example,
    filename,
    workflow = "fbmn",
    path_features,
    path_metadata,
    path_spectra,
    path_source = get_default_paths()$data$source$path,
    path_interim_a = get_default_paths()$data$interim$annotations$path,
    path_interim_f = get_default_paths()$data$interim$features$path
  ) {
    if (!is.null(gnps_job_id)) {
      if (gnps_job_id == "") {
        gnps_job_id <- NULL
      }
    }
    if (!is.null(gnps_job_id)) {
      stopifnot(
        "Your GNPS job ID is invalid" = stringi::stri_length(
          str = gnps_job_id
        ) ==
          32
      )
      stopifnot(
        "Your workflow is not supported,
        supported workflows are 'fbmn' and 'classical'" = workflow %in%
          c("fbmn", "classical")
      )

      gnps_url <-
        "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task="
      gnps_block <- "&block=main&file="

      ## Little hack for the example
      if (gnps_job_id == gnps_job_example) {
        names(gnps_job_id) <- "example"
      } else {
        names(gnps_job_id) <- gnps_job_id
      }

      logger::log_trace("... Inputs")
      logger::log_trace("... Features")
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
      logger::log_trace("... Metadata")
      file_metadata <-
        file.path(path_source, paste0(names(gnps_job_id), "_metadata.tsv"))
      if (!file.exists(file_metadata)) {
        url <- paste0(gnps_url, gnps_job_id, gnps_block, "metadata_table/")

        logger::log_trace("Checking response")
        if (
          url |>
            httr2::request() |>
            httr2::req_method("GET") |>
            httr2::req_error(
              is_error = function(resp) {
                return(FALSE)
              }
            ) |>
            httr2::req_perform() |>
            httr2::resp_status_desc() ==
            "OK"
        ) {
          logger::log_trace("Status OK!")
          get_file(url = url, export = file_metadata)
        } else {
          logger::log_warn("The given GNPS job ID has no metadata")
          logger::log_warn("Returning empty dataframes instead")
          fake_metadata <- data.frame(filename = NULL, ATTRIBUTE_species = NULL)
          export_output(x = fake_metadata, file = file_metadata)
        }
      }

      logger::log_trace("... Spectra")
      file_spectra <-
        file.path(path_source, paste0(names(gnps_job_id), "_spectra.mgf"))
      if (!file.exists(file_spectra)) {
        get_file(
          url = paste0(gnps_url, gnps_job_id, gnps_block, "spectra/"),
          export = file_spectra
        )
      }

      logger::log_trace("... Results")
      logger::log_trace("... Annotations")
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
            switch(
              workflow,
              "fbmn" = "DB_result/",
              "classical" = "result_specnets_DB/"
            )
          ),
          export = file_annotations
        )
      }
      logger::log_trace("... Components")
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
            switch(
              workflow,
              "fbmn" = "clusterinfo_summary/",
              "classical" = "clusterinfosummarygroup_attributes_withIDs_withcomponentID/"
            )
          ),
          export = file_components
        )
      }
      logger::log_trace("... Edges")
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
    } else {
      logger::log_warn("No GNPS job ID provided")
      logger::log_warn("Returning empty dataframes instead")
      fake_annotations <- data.frame(
        `#Scan#` = 0,
        MassDiff = 0,
        MZErrorPPM = 0,
        Precursor_MZ = 0,
        LibraryName = "foo",
        Compound_Name = "bar",
        INCHI = "bim",
        InChIKey = "bam",
        `InChIKey-Planar` = "bom",
        ExactMass = 0,
        MQScore = 0,
        SharedPeaks = 0,
        npclassifier_pathway = "aaa",
        npclassifier_superclass = "bbb",
        npclassifier_class = "ccc",
        superclass = "ddd",
        class = "eee",
        subclass = "fff",
        check.names = FALSE
      )
      export_output(
        x = fake_annotations,
        file = file.path(path_interim_a, paste0(filename, "_gnps.tsv"))
      )
      fake_components <- data.frame(
        `cluster index` = 0,
        componentindex = 0,
        check.names = FALSE
      )
      export_output(
        x = fake_components,
        file = file.path(path_interim_f, paste0(filename, "_components.tsv"))
      )

      fake_edges <- data.frame(CLUSTERID1 = 0, CLUSTERID2 = 0)
      export_output(
        x = fake_edges,
        file = file.path(path_interim_f, paste0(filename, "_edges_spectra.tsv"))
      )
      if (is.list(path_metadata)) {
        path_metadata <- unlist(path_metadata)
      }
      if (is.null(path_metadata)) {
        path_metadata <- ""
      }
      if (
        length(path_metadata) == 0 ||
          !file.exists(path_metadata)
      ) {
        path_metadata <- "data/source/metadata.tsv"
        fake_metadata <- data.frame(filename = "foo", ATTRIBUTE_species = "bar")
        export_output(x = fake_metadata, file = path_metadata)
      }

      return(
        c(
          "features" = path_features,
          "metadata" = path_metadata,
          "spectra" = path_spectra,
          "annotations" = file.path(
            path_interim_a,
            paste0(filename, "_gnps.tsv")
          ),
          "components" = file.path(
            path_interim_f,
            paste0(filename, "_components.tsv")
          ),
          "edges" = file.path(
            path_interim_f,
            paste0(filename, "_edges_spectra.tsv")
          )
        )
      )
    }
  }
