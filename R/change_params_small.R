#' Validate Input Parameters for change_params_small
#'
#' @description Internal helper to validate parameters before processing.
#'     Implements Single Responsibility Principle by separating validation logic.
#'
#' @param ms_pol MS polarity parameter
#'
#' @return NULL (stops on validation error)
#' @keywords internal
validate_params_small_inputs <- function(ms_pol) {
  # Validate polarity if provided
  if (!is.null(ms_pol) && !ms_pol %in% c("pos", "neg")) {
    stop(
      "ms_pol must be either 'pos' or 'neg', got: ",
      ms_pol,
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Copy File to Target Directory
#'
#' @description Internal helper to copy a file to destination with validation.
#'     Implements DRY principle by extracting repeated file copy logic.
#'
#' @param file_path Character path to source file
#' @param target_dir Character path to target directory
#' @param file_description Character description for error messages
#'
#' @return Character path to the copied file
#' @keywords internal
copy_file_to_target <- function(file_path, target_dir, file_description) {
  if (!file.exists(file_path)) {
    stop(
      file_description,
      " file does not exist: ",
      file_path,
      call. = FALSE
    )
  }

  target_path <- file.path(target_dir, basename(file_path))

  log_debug(
    "Copying {file_description} from {file_path} to {target_path}"
  )

  fs::file_copy(
    path = file_path,
    new_path = target_path,
    overwrite = TRUE
  )

  return(target_path)
}

#' Create YAML Null Handler
#'
#' @description Internal helper to handle NA to YAML null conversion.
#'     Extracted for clarity and testability.
#'
#' @return Function that converts NA to verbatim "null" for YAML
#' @keywords internal
create_yaml_null_handler <- function() {
  function(x) {
    if (!is.na(x)) {
      return(x)
    }
    res <- "null"
    class(res) <- "verbatim"
    return(res)
  }
}

#' @title Change Parameters (Convenience Function)
#'
#' @description Updates TIMA workflow parameters for quick setup with a simplified
#'     interface. This function modifies the prepare_params YAML configuration file
#'     by copying provided input files to the appropriate directories and updating
#'     parameter values. Implements SOLID principles with clear separation of concerns.
#'
#' @include create_dir.R
#' @include get_default_paths.R
#' @include go_to_cache.R
#' @include load_yaml_files.R
#'
#' @param fil_pat Character. Job identifier/pattern for output files (optional)
#' @param fil_fea_raw Character. Path to features file (e.g., from MZmine/SIRIUS)
#' @param fil_met_raw Character. Path to metadata file (optional if single taxon)
#' @param fil_sir_raw Character. Path to SIRIUS annotations directory/zip
#' @param fil_spe_raw Character. Path to spectra file (MGF format with MS1/MS2)
#' @param ms_pol Character. MS polarity: "pos" or "neg"
#' @param org_tax Character. Scientific name for single-taxon experiments
#' @param hig_con Logical. Filter for high confidence candidates only
#' @param summarize Logical. Summarize all candidates per feature to single row
#' @param cache_dir Character. Cache directory path (for testing; uses go_to_cache() if NULL)
#'
#' @details
#' This function:
#' \itemize{
#'   \item Validates all input files exist before copying
#'   \item Copies files to standardized cache locations
#'   \item Updates the prepare_params YAML configuration
#'   \item Handles NA values properly for YAML null representation
#' }
#'
#' @return Invisible NULL. Modifies prepare_params YAML as side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Setup complete workflow parameters
#' copy_backbone()
#' change_params_small(
#'   fil_pat = "gentiana_experiment",
#'   fil_fea_raw = "data/raw/features.csv",
#'   fil_met_raw = "data/raw/metadata.tsv",
#'   fil_sir_raw = "data/raw/sirius_output.zip",
#'   fil_spe_raw = "data/raw/spectra.mgf",
#'   ms_pol = "pos",
#'   org_tax = "Gentiana lutea",
#'   hig_con = TRUE,
#'   summarize = FALSE
#' )
#' }
change_params_small <- function(
  fil_pat = NULL,
  fil_fea_raw = NULL,
  fil_met_raw = NULL,
  fil_sir_raw = NULL,
  fil_spe_raw = NULL,
  ms_pol = NULL,
  org_tax = NULL,
  hig_con = NULL,
  summarize = NULL,
  cache_dir = NULL
) {
  # Input Validation ----
  validate_params_small_inputs(ms_pol = ms_pol)

  log_info("Updating workflow parameters")

  # Setup Environment ----
  # Use provided cache_dir or go to cache directory
  if (is.null(cache_dir)) {
    go_to_cache()
    cache_dir <- getwd()
  } else {
    # Set working directory to cache_dir for the duration of this function
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(cache_dir)
  }

  paths <- get_default_paths()
  paths_data_source <- paths$data$source$path
  paths_data_interim_annotations <- paths$data$interim$annotations$path

  create_dir(paths_data_source)
  create_dir(paths_data_interim_annotations)

  # Load Current Configuration ----
  yaml_data <- load_yaml_files()
  yaml_small <- yaml_data$yamls_params$prepare_params

  # Update File Pattern ----
  if (!is.null(fil_pat)) {
    log_debug("Setting file pattern to: {fil_pat}")
    yaml_small$files$pattern <- fil_pat
  }

  # Process and Copy Input Files ----
  if (!is.null(fil_fea_raw)) {
    yaml_small$files$features$raw <- copy_file_to_target(
      file_path = fil_fea_raw,
      target_dir = paths_data_source,
      file_description = "Features"
    )
  }

  if (!is.null(fil_met_raw)) {
    yaml_small$files$metadata$raw <- copy_file_to_target(
      file_path = fil_met_raw,
      target_dir = paths_data_source,
      file_description = "Metadata"
    )
  }

  if (!is.null(fil_sir_raw)) {
    yaml_small$files$annotations$raw$sirius <- copy_file_to_target(
      file_path = fil_sir_raw,
      target_dir = paths_data_interim_annotations,
      file_description = "SIRIUS annotations"
    )
  } else {
    # Explicitly set to NA if not provided
    yaml_small$files$annotations$raw$sirius <- NA
  }

  if (!is.null(fil_spe_raw)) {
    yaml_small$files$spectral$raw <- copy_file_to_target(
      file_path = fil_spe_raw,
      target_dir = paths_data_source,
      file_description = "Spectra"
    )
  }

  # Update Configuration Parameters ----
  if (!is.null(ms_pol)) {
    log_debug("Setting MS polarity to: {ms_pol}")
    yaml_small$ms$polarity <- ms_pol
  }

  if (!is.null(org_tax)) {
    log_debug("Setting organism taxonomy to: {org_tax}")
    yaml_small$organisms$taxon <- org_tax
  } else {
    yaml_small$organisms$taxon <- NA
  }

  if (!is.null(hig_con)) {
    log_debug("Setting high confidence filter: {hig_con}")
    yaml_small$options$high_confidence <- hig_con
  }

  if (!is.null(summarize)) {
    log_debug("Setting summarize option: {summarize}")
    yaml_small$options$summarize <- summarize
  }

  # Write Updated Configuration ----
  output_path <- paths$params$prepare_params
  log_info("Writing updated parameters to: {output_path}")

  yaml::write_yaml(
    x = yaml_small,
    file = output_path,
    handlers = list(logical = create_yaml_null_handler())
  )

  log_success("Parameters successfully updated")

  invisible(NULL)
}
