#' @title Get GNPS Tables
#'
#' @description This function downloads and retrieves GNPS (Global Natural Products
#'     Social Molecular Networking) result tables from a completed job. It fetches
#'     features, metadata, spectra, and annotation files from GNPS servers. When a
#'     job ID is not provided or GNPS resources are missing, small fake files are
#'     written so downstream steps do not fail during testing.
#'
#' @include get_default_paths.R
#' @include get_file.R
#'
#' @param gnps_job_id Character string GNPS job ID (32 characters). Can be NULL
#'     or empty string to skip download.
#' @param gnps_job_example Character string example GNPS job ID for testing
#' @param filename Character string name of the file to download (used for fake outputs)
#' @param workflow Character string indicating workflow type: "fbmn" (feature-based)
#'     or "classical" molecular networking
#' @param path_features Character string path for features output (file path)
#' @param path_metadata Character string path for metadata output (file path or list)
#' @param path_spectra Character string path for spectra output (file path)
#' @param path_source Character string path to store source files
#' @param path_interim_a Character string path to store interim annotations
#' @param path_interim_f Character string path to store interim features
#'
#' @return A named character vector with paths to the written/available files.
#'
#' @examples NULL
get_gnps_tables <- function(
  gnps_job_id,
  gnps_job_example = get_default_paths()$gnps$example,
  filename = "",
  workflow = "fbmn",
  path_features,
  path_metadata,
  path_spectra,
  path_source = get_default_paths()$data$source$path,
  path_interim_a = get_default_paths()$data$interim$annotations$path,
  path_interim_f = get_default_paths()$data$interim$features$path
) {
  # Normalize NULL/empty string inputs
  if (!is.null(gnps_job_id) && identical(gnps_job_id, "")) {
    gnps_job_id <- NULL
  }

  # If no job ID provided, create small fake outputs and return
  if (is.null(gnps_job_id)) {
    logger::log_info(
      "No GNPS job ID provided, creating fake GNPS outputs for tests"
    )

    # Ensure metadata path handling similar to previous behaviour
    if (is.list(path_metadata)) {
      path_metadata <- unlist(path_metadata)
    }
    if (
      is.null(path_metadata) ||
        length(path_metadata) == 0 ||
        !nzchar(path_metadata)
    ) {
      path_metadata <- file.path("data", "source", "metadata.tsv")
    }

    # Create fake files used by downstream code/tests
    fake_annotations <- data.frame(
      `#Scan#` = integer(0),
      MassDiff = numeric(0),
      MZErrorPPM = numeric(0),
      Precursor_MZ = numeric(0),
      LibraryName = character(0),
      Compound_Name = character(0),
      INCHI = character(0),
      InChIKey = character(0),
      `InChIKey-Planar` = character(0),
      ExactMass = numeric(0),
      MQScore = numeric(0),
      SharedPeaks = integer(0),
      npclassifier_pathway = character(0),
      npclassifier_superclass = character(0),
      npclassifier_class = character(0),
      superclass = character(0),
      class = character(0),
      subclass = character(0),
      check.names = FALSE
    )
    fake_components <- data.frame(
      `cluster index` = integer(0),
      componentindex = integer(0),
      check.names = FALSE
    )
    fake_edges <- data.frame(CLUSTERID1 = integer(0), CLUSTERID2 = integer(0))
    fake_metadata <- data.frame(
      filename = character(0),
      ATTRIBUTE_species = character(0)
    )

    # Write fake outputs using export_output if available, otherwise write simple files
    ann_file <- file.path(path_interim_a, paste0(filename, "_gnps.tsv"))
    comp_file <- file.path(path_interim_f, paste0(filename, "_components.tsv"))
    edges_file <- file.path(
      path_interim_f,
      paste0(filename, "_edges_spectra.tsv")
    )

    tryCatch(
      export_output(x = fake_annotations, file = ann_file),
      error = function(e) {
        logger::log_warn(
          "Writing fake annotations failed with error: %s",
          conditionMessage(e)
        )
        write.table(
          fake_annotations,
          file = ann_file,
          sep = "\t",
          row.names = FALSE
        )
      }
    )
    tryCatch(
      export_output(x = fake_components, file = comp_file),
      error = function(e) {
        logger::log_warn(
          "Writing fake components failed with error: %s",
          conditionMessage(e)
        )
        write.table(
          fake_components,
          file = comp_file,
          sep = "\t",
          row.names = FALSE
        )
      }
    )
    tryCatch(
      export_output(x = fake_edges, file = edges_file),
      error = function(e) {
        logger::log_warn(
          "Writing fake edges failed with error: %s",
          conditionMessage(e)
        )
        write.table(
          fake_edges,
          file = edges_file,
          sep = "\t",
          row.names = FALSE
        )
      }
    )
    tryCatch(
      export_output(x = fake_metadata, file = path_metadata),
      error = function(e) {
        logger::log_warn(
          "Writing fake metadata failed with error: %s",
          conditionMessage(e)
        )
        write.table(
          fake_metadata,
          file = path_metadata,
          sep = "\t",
          row.names = FALSE
        )
      }
    )

    return(
      c(
        "features" = path_features,
        "metadata" = path_metadata,
        "spectra" = path_spectra,
        "annotations" = ann_file,
        "components" = comp_file,
        "edges" = edges_file
      )
    )
  }

  # At this point we have a job ID. Allow example job ID to bypass strict length check.
  if (!identical(gnps_job_id, gnps_job_example) && nchar(gnps_job_id) != 32L) {
    stop(
      "Invalid GNPS job ID: '",
      gnps_job_id,
      "' - GNPS job IDs must be exactly 32 characters long unless it's the bundled example ID."
    )
  }

  if (!workflow %in% c("fbmn", "classical")) {
    stop(
      "Unsupported workflow: '",
      workflow,
      "' - supported: 'fbmn' or 'classical'"
    )
  }

  logger::log_info("Downloading GNPS tables for job: ", gnps_job_id)

  gnps_base_url <- "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task="
  gnps_block_param <- "&block=main&file="

  job_label <- if (identical(gnps_job_id, gnps_job_example)) {
    "example"
  } else {
    gnps_job_id
  }

  # Prepare destination file paths
  features_file <- path_features
  metadata_file <- path_metadata
  spectra_file <- path_spectra
  annotations_file <- file.path(path_interim_a, paste0(job_label, "_gnps.tsv"))
  components_file <- file.path(
    path_interim_f,
    paste0(job_label, "_components.tsv")
  )
  edges_file <- file.path(
    path_interim_f,
    paste0(job_label, "_edges_spectra.tsv")
  )

  # Download features if not present
  if (!file.exists(features_file)) {
    features_url <- paste0(
      gnps_base_url,
      gnps_job_id,
      gnps_block_param,
      "quantification_table_reformatted/"
    )
    tryCatch(
      get_file(url = features_url, export = features_file),
      error = function(e) {
        logger::log_warn("Failed to download features: %s", conditionMessage(e))
      }
    )
  } else {
    logger::log_trace(
      "Features file already exists, skipping download: %s",
      features_file
    )
  }

  # Attempt to download metadata if path provided and file not present
  if (is.list(metadata_file)) {
    metadata_file <- unlist(metadata_file)
  }
  if (is.null(metadata_file) || length(metadata_file) == 0) {
    metadata_file <- file.path(path_source, paste0(job_label, "_metadata.tsv"))
  }

  if (!file.exists(metadata_file)) {
    metadata_url <- paste0(
      gnps_base_url,
      gnps_job_id,
      gnps_block_param,
      "metadata_table/"
    )
    # check availability
    ok <- tryCatch(
      {
        resp <- httr2::request(metadata_url) |>
          httr2::req_method("GET") |>
          httr2::req_perform()
        httr2::resp_status(resp) == 200
      },
      error = function(e) {
        logger::log_warn(
          "Metadata availability check failed: %s",
          conditionMessage(e)
        )
        FALSE
      }
    )

    if (isTRUE(ok)) {
      tryCatch(
        get_file(url = metadata_url, export = metadata_file),
        error = function(e) {
          logger::log_warn(
            "Failed to download metadata: %s",
            conditionMessage(e)
          )
        }
      )
    } else {
      logger::log_warn(
        "The given GNPS job ID has no metadata - writing a small fake metadata file"
      )
      fake_metadata <- data.frame(
        filename = character(0),
        ATTRIBUTE_species = character(0)
      )
      tryCatch(
        export_output(x = fake_metadata, file = metadata_file),
        error = function(e) {
          logger::log_warn(
            "Writing fake metadata failed with error: %s",
            conditionMessage(e)
          )
          write.table(
            fake_metadata,
            file = metadata_file,
            sep = "\t",
            row.names = FALSE
          )
        }
      )
    }
  } else {
    logger::log_trace(
      "Metadata file already exists, skipping download: %s",
      metadata_file
    )
  }

  # Spectra
  if (!file.exists(spectra_file)) {
    spectra_url <- paste0(
      gnps_base_url,
      gnps_job_id,
      gnps_block_param,
      "spectra/"
    )
    tryCatch(
      get_file(url = spectra_url, export = spectra_file),
      error = function(e) {
        logger::log_warn("Failed to download spectra: %s", conditionMessage(e))
      }
    )
  } else {
    logger::log_trace(
      "Spectra file already exists, skipping download: %s",
      spectra_file
    )
  }

  # Annotations
  if (!file.exists(annotations_file)) {
    ann_path <- switch(
      workflow,
      "fbmn" = "DB_result/",
      "classical" = "result_specnets_DB/"
    )
    ann_url <- paste0(gnps_base_url, gnps_job_id, gnps_block_param, ann_path)
    tryCatch(
      get_file(url = ann_url, export = annotations_file),
      error = function(e) {
        logger::log_warn(
          "Failed to download annotations: %s",
          conditionMessage(e)
        )
      }
    )
  } else {
    logger::log_trace(
      "Annotations file already exists, skipping download: %s",
      annotations_file
    )
  }

  # Components
  if (!file.exists(components_file)) {
    comp_path <- switch(
      workflow,
      "fbmn" = "clusterinfo_summary/",
      "classical" = "clusterinfosummarygroup_attributes_withIDs_withcomponentID/"
    )
    comp_url <- paste0(gnps_base_url, gnps_job_id, gnps_block_param, comp_path)
    tryCatch(
      get_file(url = comp_url, export = components_file),
      error = function(e) {
        logger::log_warn(
          "Failed to download components: %s",
          conditionMessage(e)
        )
      }
    )
  } else {
    logger::log_trace(
      "Components file already exists, skipping download: %s",
      components_file
    )
  }

  # Edges
  if (!file.exists(edges_file)) {
    edges_url <- paste0(
      gnps_base_url,
      gnps_job_id,
      gnps_block_param,
      "networkedges_selfloop/"
    )
    tryCatch(
      get_file(url = edges_url, export = edges_file),
      error = function(e) {
        logger::log_warn("Failed to download edges: %s", conditionMessage(e))
      }
    )
  } else {
    logger::log_trace(
      "Edges file already exists, skipping download: %s",
      edges_file
    )
  }

  return(
    c(
      "features" = features_file,
      "metadata" = metadata_file,
      "spectra" = spectra_file,
      "annotations" = annotations_file,
      "components" = components_file,
      "edges" = edges_file
    )
  )
}
