#' Create Fake GNPS Output Files
#'
#' @description Internal helper to create empty GNPS files for testing.
#'
#' @keywords internal
create_fake_gnps_files <- function(
  filename,
  path_features,
  path_metadata,
  path_spectra,
  path_interim_a,
  path_interim_f
) {
  logger::log_info(
    "No GNPS job ID provided, creating fake GNPS outputs for tests"
  )

  # Normalize metadata path
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

  # Create empty data frames
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

  # Define file paths
  ann_file <- file.path(path_interim_a, paste0(filename, "_gnps.tsv"))
  comp_file <- file.path(path_interim_f, paste0(filename, "_components.tsv"))
  edges_file <- file.path(
    path_interim_f,
    paste0(filename, "_edges_spectra.tsv")
  )

  # Write files with error handling
  safe_write <- function(data, file) {
    tryCatch(
      export_output(x = data, file = file),
      error = function(e) {
        logger::log_warn(
          "Writing {file} failed, using fallback: {conditionMessage(e)}"
        )
        utils::write.table(
          x = data,
          file = file,
          sep = "\t",
          row.names = FALSE
        )
      }
    )
  }

  safe_write(fake_annotations, ann_file)
  safe_write(fake_components, comp_file)
  safe_write(fake_edges, edges_file)
  safe_write(fake_metadata, path_metadata)

  # Create minimal placeholder files for features and spectra if paths are provided
  if (!is.null(path_features) && nzchar(path_features)) {
    # Ensure directory exists
    create_dir(export = path_features)
    # Write a minimal TSV header to avoid empty file warnings
    writeLines("feature_id\n", con = path_features)
  }
  if (!is.null(path_spectra) && nzchar(path_spectra)) {
    # Ensure directory exists
    create_dir(export = path_spectra)
    # Write a minimal valid MGF block
    mgf_stub <- c("BEGIN IONS", "TITLE=placeholder", "END IONS")
    writeLines(mgf_stub, con = path_spectra)
  }

  c(
    "features" = path_features,
    "metadata" = path_metadata,
    "spectra" = path_spectra,
    "annotations" = ann_file,
    "components" = comp_file,
    "edges" = edges_file
  )
}

#' Validate GNPS Job ID
#'
#' @description Internal helper to validate GNPS job ID format.
#'
#' @keywords internal
validate_gnps_job_id <- function(gnps_job_id, gnps_job_example) {
  if (!identical(gnps_job_id, gnps_job_example) && nchar(gnps_job_id) != 32L) {
    stop(
      "Invalid GNPS job ID: '",
      gnps_job_id,
      "' - GNPS job IDs must be exactly 32 characters long unless it's the bundled example ID.",
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Download GNPS File
#'
#' @description Internal helper to download a single GNPS file.
#'
#' @keywords internal
download_gnps_file <- function(url, file_path, file_type) {
  if (!file.exists(file_path)) {
    tryCatch(
      {
        get_file(url = url, export = file_path)
        logger::log_debug("Downloaded {file_type}: {file_path}")
      },
      error = function(e) {
        logger::log_warn(
          "Failed to download {file_type}: {conditionMessage(e)}"
        )
      }
    )
  }
  invisible(NULL)
}

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

  # If no job ID provided, create fake outputs and return
  if (is.null(gnps_job_id)) {
    return(create_fake_gnps_files(
      filename,
      path_features,
      path_metadata,
      path_spectra,
      path_interim_a,
      path_interim_f
    ))
  }

  # Validate job ID and workflow
  validate_gnps_job_id(gnps_job_id, gnps_job_example)

  if (!workflow %in% c("fbmn", "classical")) {
    stop(
      "Unsupported workflow: '",
      workflow,
      "' - supported: 'fbmn' or 'classical'",
      call. = FALSE
    )
  }

  logger::log_info("Downloading GNPS tables for job: {gnps_job_id}")

  # Construct URLs
  gnps_base_url <- "https://gnps.ucsd.edu/ProteoSAFe/DownloadResultFile?task="
  gnps_block_param <- "&block=main&file="

  job_label <- if (identical(gnps_job_id, gnps_job_example)) {
    "example"
  } else {
    gnps_job_id
  }

  # Prepare file paths
  features_file <- path_features
  metadata_file <- if (is.list(path_metadata)) {
    unlist(path_metadata)
  } else if (is.null(path_metadata) || length(path_metadata) == 0) {
    file.path(path_source, paste0(job_label, "_metadata.tsv"))
  } else {
    path_metadata
  }
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

  # Download files
  download_gnps_file(
    paste0(
      gnps_base_url,
      gnps_job_id,
      gnps_block_param,
      "quantification_table_reformatted/"
    ),
    features_file,
    "features"
  )

  download_gnps_file(
    paste0(gnps_base_url, gnps_job_id, gnps_block_param, "metadata_table/"),
    metadata_file,
    "metadata"
  )

  download_gnps_file(
    paste0(gnps_base_url, gnps_job_id, gnps_block_param, "spectra/"),
    spectra_file,
    "spectra"
  )

  # Download annotations
  ann_path <- switch(
    workflow,
    "fbmn" = "DB_result/",
    "classical" = "result_specnets_DB/"
  )
  download_gnps_file(
    paste0(gnps_base_url, gnps_job_id, gnps_block_param, ann_path),
    annotations_file,
    "annotations"
  )

  # Download components
  comp_path <- switch(
    workflow,
    "fbmn" = "clusterinfo_summary/",
    "classical" = "clusterinfosummarygroup_attributes_withIDs_withcomponentID/"
  )
  download_gnps_file(
    paste0(gnps_base_url, gnps_job_id, gnps_block_param, comp_path),
    components_file,
    "components"
  )

  # Download edges
  download_gnps_file(
    paste0(
      gnps_base_url,
      gnps_job_id,
      gnps_block_param,
      "networkedges_selfloop/"
    ),
    edges_file,
    "edges"
  )

  # Return paths
  c(
    "features" = features_file,
    "metadata" = metadata_file,
    "spectra" = spectra_file,
    "annotations" = annotations_file,
    "components" = components_file,
    "edges" = edges_file
  )
}
