#' Sanitize All Input Data
#'
#' @description Comprehensive validation of all input data before starting
#'     expensive processing. Reports issues immediately to save time.
#'
#' @param features_file [character] Character path to features CSV/TSV
#'     (optional)
#' @param mgf_file [character] Character path to MGF file (optional)
#' @param metadata_file [character] Character path to metadata file (optional)
#' @param sirius_dir [character] Character path to SIRIUS output directory or
#'     ZIP (optional)
#' @param filename_col [character] Character name of filename column (default:
#'     "filename")
#' @param organism_col [character] Character name of organism column (default:
#'     "organism")
#' @param feature_col [character] Character name of feature ID column (default:
#'     "feature_id")
#'
#' @return Invisible TRUE if all validations pass, stops with error otherwise
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Validate all inputs before starting pipeline
#' sanitize_all_inputs(
#'   features_file = "data/features.csv",
#'   mgf_file = "data/spectra.mgf",
#'   metadata_file = "data/metadata.tsv",
#'   sirius_dir = "data/sirius_output"
#' )
#' }
sanitize_all_inputs <- function(
  features_file = NULL,
  mgf_file = NULL,
  metadata_file = NULL,
  sirius_dir = NULL,
  filename_col = "filename",
  organism_col = "organism",
  feature_col = "feature_id"
) {
  log_info("=" |> rep(60) |> paste(collapse = ""))
  log_info("Data Sanitizing: Pre-flight Checks")
  log_info("=" |> rep(60) |> paste(collapse = ""))

  all_valid <- TRUE
  issues_found <- list()

  # Check features file
  if (!is.null(features_file)) {
    log_info("Checking features file...")
    result <- sanitize_csv(
      file = features_file,
      file_type = "features table",
      required_cols = "feature_id",
      feature_col = feature_col
    )

    if (result$valid) {
      log_success(
        "[OK] Features file: %d rows, %d columns",
        result$n_rows,
        result$n_cols
      )
      log_debug("  Columns: %s", paste(result$columns, collapse = ", "))
    } else {
      log_error("[X] Features file has issues:")
      invisible(vapply(
        X = result$issues,
        FUN = function(issue) {
          log_error("  - %s", issue)
          TRUE
        },
        FUN.VALUE = logical(1L)
      ))
      issues_found$features <- result$issues
      all_valid <- FALSE
    }
  }

  # Check MGF file
  if (!is.null(mgf_file)) {
    log_info("Checking MGF file...")
    result <- sanitize_mgf(file = mgf_file, file_type = "MGF spectra")

    if (result$valid) {
      log_success("[OK] MGF file: %d MS2 spectra found", result$n_spectra)
    } else {
      log_error("[X] MGF file has issues:")
      invisible(vapply(
        X = result$issues,
        FUN = function(issue) {
          log_error("  - %s", issue)
          TRUE
        },
        FUN.VALUE = logical(1L)
      ))
      issues_found$mgf <- result$issues
      all_valid <- FALSE
    }
  }

  # Check metadata and file consistency
  if (!is.null(metadata_file)) {
    log_info("Checking metadata file...")
    result <- sanitize_metadata(
      metadata_file = metadata_file,
      features_file = features_file,
      filename_col = filename_col,
      organism_col = organism_col
    )

    if (result$valid) {
      log_success(
        "[OK] Metadata file: %d samples, %d with organism info",
        result$n_samples,
        result$n_with_organism
      )
      if (!is.na(result$filenames_match) && result$filenames_match) {
        log_debug(
          "  Filenames match between features and metadata (%d files)",
          result$n_matched
        )
      } else if (!is.na(result$filenames_match)) {
        log_debug(
          "  Filenames: %d matched, %d unmatched",
          result$n_matched,
          result$n_unmatched
        )
      }
    } else {
      log_error("[X] Metadata validation has issues:")
      invisible(vapply(
        X = result$issues,
        FUN = function(issue) {
          log_error("  - %s", issue)
          TRUE
        },
        FUN.VALUE = logical(1L)
      ))
      if (length(result$unmatched_files) > 0L) {
        log_error("  Unmatched files:")
        invisible(vapply(
          X = utils::head(result$unmatched_files, 5),
          FUN = function(f) {
            log_error("    - %s", f)
            TRUE
          },
          FUN.VALUE = logical(1L)
        ))
        if (length(result$unmatched_files) > 5L) {
          log_error("    ... and %d more", length(result$unmatched_files) - 5L)
        }
      }
      issues_found$metadata <- result$issues
      all_valid <- FALSE
    }
  }

  # Check SIRIUS output
  if (!is.null(sirius_dir)) {
    log_info("Checking SIRIUS output...")
    result <- sanitize_sirius(sirius_dir = sirius_dir)

    if (result$valid) {
      format_type <- if (result$is_zip) " (ZIP)" else " (dir)"
      version_info <- if (result$sirius_version != "unknown") {
        paste0(" [SIRIUS v", result$sirius_version, "]")
      } else {
        ""
      }
      log_success(
        "[OK] SIRIUS output%s%s: %d annotations, all required files present",
        format_type,
        version_info,
        result$n_features
      )
      log_debug(
        "  - Formula identifications: %s",
        if (result$has_formula) "[OK]" else "[X]"
      )
      log_debug(
        "  - CANOPUS summary: %s",
        if (result$has_canopus) "[OK]" else "[X]"
      )
      log_debug(
        "  - Structure identifications: %s",
        if (result$has_structure) "[OK]" else "[X]"
      )
    } else {
      log_error("[X] SIRIUS output has issues:")
      invisible(vapply(
        X = result$issues,
        FUN = function(issue) {
          log_error("  - %s", issue)
          TRUE
        },
        FUN.VALUE = logical(1L)
      ))
      issues_found$sirius <- result$issues
      all_valid <- FALSE
    }
  }

  # Summary
  log_info("=" |> rep(60) |> paste(collapse = ""))

  if (all_valid) {
    log_success("[OK] All pre-flight checks passed!")
    log_info("Data validation complete. Ready to proceed.")
    log_info("=" |> rep(60) |> paste(collapse = ""))
    invisible(TRUE)
  } else {
    log_error("[X] Pre-flight checks failed!")
    log_error(
      "Found issues in: %s",
      paste(names(issues_found), collapse = ", ")
    )
    log_info("=" |> rep(60) |> paste(collapse = ""))

    tima_abort(
      problem = "Input data validation failed",
      received = sprintf("%d issue(s) found", length(issues_found)),
      context = paste(
        "Please fix the issues above before running expensive operations",
        "like library downloads or processing",
        sep = "\n"
      ),
      fix = paste(
        "1. Review the error messages above",
        "2. Fix the identified issues in your input files",
        "3. Re-run the validation",
        sep = "\n"
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }
}
