#' @title Change Params Small
#'
#' @description This function provides a convenient way to modify key parameters
#'     for a TIMA analysis workflow. It updates the parameter YAML file with
#'     user-specified file paths, polarity, organism taxonomy, and output options.
#'     Files are automatically copied to the appropriate cache directories.
#'
#' @include create_dir.R
#' @include get_default_paths.R
#' @include go_to_cache.R
#' @include load_yaml_files.R
#'
#' @param fil_pat Character string pattern identifying your analysis job (optional)
#' @param fil_fea_raw Character string path to features file (MZmine, SLAW, or SIRIUS format)
#' @param fil_met_raw Character string path to metadata file (optional if using org_tax)
#' @param fil_sir_raw Character string path to SIRIUS annotation directory/zip
#' @param fil_spe_raw Character string path to spectra file (MGF format)
#' @param ms_pol Character string MS polarity: "pos" or "neg"
#' @param org_tax Character string organism scientific name (e.g., "Gentiana lutea")
#'     for single-organism experiments (optional)
#' @param hig_con Logical whether to filter for high confidence candidates only
#' @param summarize Logical whether to summarize candidates to one row per feature
#'
#' @return NULL (invisibly). Updates parameter YAML file as a side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' change_params_small(
#'   fil_pat = "myExamplePattern",
#'   fil_fea_raw = "data/features.csv",
#'   fil_met_raw = "data/metadata.tsv",
#'   fil_sir_raw = "data/sirius_project.zip",
#'   fil_spe_raw = "data/spectra.mgf",
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
  summarize = NULL
) {
  # Validate polarity if provided
  if (!is.null(ms_pol)) {
    if (!is.character(ms_pol) || length(ms_pol) != 1L) {
      stop("ms_pol must be a single character string")
    }
    if (!ms_pol %in% c("pos", "neg")) {
      stop("ms_pol must be either 'pos' or 'neg', got: ", ms_pol)
    }
  }

  # Validate logical parameters
  if (!is.null(hig_con) && !is.logical(hig_con)) {
    stop("hig_con must be logical (TRUE/FALSE)")
  }

  if (!is.null(summarize) && !is.logical(summarize)) {
    stop("summarize must be logical (TRUE/FALSE)")
  }

  # Validate file paths if provided
  file_params <- list(
    features = fil_fea_raw,
    metadata = fil_met_raw,
    sirius = fil_sir_raw,
    spectra = fil_spe_raw
  )

  for (param_name in names(file_params)) {
    param_value <- file_params[[param_name]]
    if (!is.null(param_value)) {
      if (!is.character(param_value) || length(param_value) != 1L) {
        stop(param_name, " file path must be a single character string")
      }
      if (!file.exists(param_value)) {
        stop(param_name, " file not found: ", param_value)
      }
    }
  }

  logger::log_info("Updating TIMA parameters")

  go_to_cache()
  paths_data_source <- get_default_paths()$data$source$path
  paths_data_interim_annotations <- get_default_paths()$data$interim$annotations$path
  create_dir(paths_data_source)

  list <- load_yaml_files()
  yamls_params <- list$yamls_params

  yaml_small <- yamls_params[["params/prepare_params"]]

  # Update pattern if provided
  if (!is.null(fil_pat)) {
    logger::log_debug("Setting pattern: ", fil_pat)
    yaml_small$files$pattern <- fil_pat
  }

  # Update features file if provided
  if (!is.null(fil_fea_raw)) {
    logger::log_debug("Copying features file to cache")
    fil_fea_raw_rdy <- paths_data_source |>
      file.path(basename(fil_fea_raw))
    fs::file_copy(
      path = fil_fea_raw,
      new_path = fil_fea_raw_rdy,
      overwrite = TRUE
    )
    yaml_small$files$features$raw <- fil_fea_raw_rdy
    logger::log_trace("Features: ", fil_fea_raw_rdy)
  }

  # Update metadata file if provided
  if (!is.null(fil_met_raw)) {
    logger::log_debug("Copying metadata file to cache")
    fil_met_raw_rdy <- paths_data_source |>
      file.path(basename(fil_met_raw))
    fs::file_copy(
      path = fil_met_raw,
      new_path = fil_met_raw_rdy,
      overwrite = TRUE
    )
    yaml_small$files$metadata$raw <- fil_met_raw_rdy
    logger::log_trace("Metadata: ", fil_met_raw_rdy)
  }

  # Update SIRIUS directory if provided
  if (!is.null(fil_sir_raw)) {
    logger::log_debug("Copying SIRIUS annotations to cache")
    fil_sir_raw_rdy <- paths_data_interim_annotations |>
      file.path(basename(fil_sir_raw))
    fs::file_copy(
      path = fil_sir_raw,
      new_path = fil_sir_raw_rdy,
      overwrite = TRUE
    )
    yaml_small$files$annotations$raw$sirius <- fil_sir_raw_rdy
    logger::log_trace("SIRIUS: ", fil_sir_raw_rdy)
  } else {
    yaml_small$files$annotations$raw$sirius <- NA
  }

  # Update spectra file if provided
  if (!is.null(fil_spe_raw)) {
    logger::log_debug("Copying spectra file to cache")
    fil_spe_raw_rdy <- paths_data_source |>
      file.path(basename(fil_spe_raw))
    fs::file_copy(
      path = fil_spe_raw,
      new_path = fil_spe_raw_rdy,
      overwrite = TRUE
    )
    yaml_small$files$spectral$raw <- fil_spe_raw_rdy
    logger::log_trace("Spectra: ", fil_spe_raw_rdy)
  }

  # Update polarity if provided
  if (!is.null(ms_pol)) {
    logger::log_debug("Setting polarity: ", ms_pol)
    yaml_small$ms$polarity <- ms_pol
  }

  # Update organism if provided
  if (!is.null(org_tax)) {
    logger::log_debug("Setting organism: ", org_tax)
    yaml_small$organisms$taxon <- org_tax
  } else {
    yaml_small$organisms$taxon <- NA
  }

  # Update options if provided
  if (!is.null(hig_con)) {
    logger::log_debug("Setting high_confidence: ", hig_con)
    yaml_small$options$high_confidence <- hig_con
  }
  if (!is.null(summarize)) {
    logger::log_debug("Setting summarize: ", summarize)
    yaml_small$options$summarize <- summarize
  }

  # Write updated YAML (with special handling for NA/null values)
  null_handler <- function(x) {
    if (!is.na(x)) {
      return(x)
    }
    res <- "null"
    class(res) <- "verbatim"
    return(res)
  }

  logger::log_info(
    "Writing updated parameters to: ",
    get_default_paths()$params$prepare_params
  )
  yaml::write_yaml(
    x = yaml_small,
    file = get_default_paths()$params$prepare_params,
    handlers = list(logical = null_handler)
  )

  logger::log_info("Parameters updated successfully")
  invisible(NULL)
}
