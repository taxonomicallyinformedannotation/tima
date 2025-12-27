#' Data Sanitizing and Validation
#'
#' @description Pre-flight checks for input data to catch issues early
#'     before expensive operations like library downloads. Validates file
#'     formats, counts records, checks metadata consistency, and verifies
#'     SIRIUS output completeness.
#'
#' @name sanitize_data
#' @keywords internal
NULL

#' Sanitize and Validate MGF File
#'
#' @description Validates MGF (Mascot Generic Format) file structure and
#'     reports the number of spectra found.
#'
#' @param file Character path to MGF file
#' @param file_type Character description for error messages
#'
#' @return List with validation results:
#'   - valid: Logical, TRUE if file is valid
#'   - n_spectra: Integer, number of spectra found
#'   - issues: Character vector of issues found (if any)
#'
#' @keywords internal
sanitize_mgf <- function(file, file_type = "MGF file") {
  issues <- character(0)
  n_spectra <- 0L

  # Basic file validation
  if (!file.exists(file)) {
    return(list(
      valid = FALSE,
      n_spectra = 0L,
      issues = paste0(file_type, " not found: ", file)
    ))
  }

  # Read and parse MGF
  tryCatch(
    {
      lines <- readLines(file, warn = FALSE)

      # Count all spectra (BEGIN IONS markers)
      begin_ions <- grep("^BEGIN IONS", lines, ignore.case = TRUE)
      end_ions <- grep("^END IONS", lines, ignore.case = TRUE)
      total_spectra <- length(begin_ions)

      # Count only MS2 spectra (exclude MS1)
      # MS2 spectra either have MSLEVEL=2 or have fragment peaks
      n_ms2 <- 0L
      if (total_spectra > 0L) {
        for (i in seq_along(begin_ions)) {
          spec_start <- begin_ions[i]
          spec_end <- if (i <= length(end_ions)) end_ions[i] else length(lines)
          spectrum <- lines[spec_start:spec_end]

          # Check MSLEVEL - if present and =2, it's MS2
          mslevel_line <- grep(
            "^MSLEVEL=",
            spectrum,
            ignore.case = TRUE,
            value = TRUE
          )
          if (length(mslevel_line) > 0) {
            level <- gsub("^MSLEVEL=", "", mslevel_line[1], ignore.case = TRUE)
            if (trimws(level) == "2") {
              n_ms2 <- n_ms2 + 1L
            }
          } else {
            # No MSLEVEL specified - assume MS2 if it has fragment peaks
            # (MS1 spectra typically don't have MSLEVEL in MGF, but MS2 do have peaks)
            has_peaks <- any(grepl("^[0-9]", spectrum))
            if (has_peaks) {
              n_ms2 <- n_ms2 + 1L
            }
          }
        }
      }

      n_spectra <- n_ms2

      # Check for basic MGF structure issues
      if (total_spectra == 0L) {
        issues <- c(issues, "No spectra found (no BEGIN IONS markers)")
      } else if (n_ms2 == 0L) {
        issues <- c(
          issues,
          sprintf(
            "No MS2 spectra found (%d total spectra, but all are MS1 or empty)",
            total_spectra
          )
        )
      }

      if (length(begin_ions) != length(end_ions)) {
        issues <- c(
          issues,
          sprintf(
            "Mismatched BEGIN IONS (%d) and END IONS (%d)",
            length(begin_ions),
            length(end_ions)
          )
        )
      }

      # Check for required fields in at least one MS2 spectrum
      if (n_ms2 > 0L) {
        # Find first MS2 spectrum
        first_ms2_idx <- NULL
        for (i in seq_along(begin_ions)) {
          spec_start <- begin_ions[i]
          spec_end <- if (i <= length(end_ions)) end_ions[i] else length(lines)
          spectrum <- lines[spec_start:spec_end]

          mslevel_line <- grep(
            "^MSLEVEL=",
            spectrum,
            ignore.case = TRUE,
            value = TRUE
          )
          is_ms2 <- if (length(mslevel_line) > 0) {
            level <- gsub("^MSLEVEL=", "", mslevel_line[1], ignore.case = TRUE)
            trimws(level) == "2"
          } else {
            any(grepl("^[0-9]", spectrum))
          }

          if (is_ms2) {
            first_ms2_idx <- i
            break
          }
        }

        if (!is.null(first_ms2_idx)) {
          first_spectrum_start <- begin_ions[first_ms2_idx]
          first_spectrum_end <- if (first_ms2_idx <= length(end_ions)) {
            end_ions[first_ms2_idx]
          } else {
            length(lines)
          }
          first_spectrum <- lines[first_spectrum_start:first_spectrum_end]

          has_mz <- any(grepl("^PEPMASS=", first_spectrum, ignore.case = TRUE))
          has_peaks <- any(grepl("^[0-9]", first_spectrum))

          if (!has_mz) {
            issues <- c(issues, "First MS2 spectrum missing PEPMASS field")
          }
          if (!has_peaks) {
            issues <- c(issues, "First MS2 spectrum missing peak data")
          }
        }
      }
    },
    error = function(e) {
      issues <- c(issues, paste0("Failed to parse MGF: ", conditionMessage(e)))
    }
  )

  list(
    valid = length(issues) == 0L,
    n_spectra = n_spectra,
    issues = issues
  )
}

#' Sanitize and Validate CSV/TSV File
#'
#' @description Validates CSV/TSV file structure and reports record count.
#'
#' @param file Character path to CSV/TSV file
#' @param file_type Character description for error messages
#' @param required_cols Character vector of required column names (optional)
#' @param feature_col Character name of feature ID column (default: "feature_id")
#'
#' @return List with validation results:
#'   - valid: Logical, TRUE if file is valid
#'   - n_rows: Integer, number of rows (excluding header)
#'   - n_cols: Integer, number of columns
#'   - columns: Character vector of column names
#'   - issues: Character vector of issues found (if any)
#'
#' @keywords internal
sanitize_csv <- function(
  file,
  file_type = "CSV file",
  required_cols = NULL,
  feature_col = "feature_id"
) {
  issues <- character(0)
  n_rows <- 0L
  n_cols <- 0L
  columns <- character(0)

  # Basic file validation
  if (!file.exists(file)) {
    return(list(
      valid = FALSE,
      n_rows = 0L,
      n_cols = 0L,
      columns = character(0),
      issues = paste0(file_type, " not found: ", file)
    ))
  }

  # Try to read and validate
  df <- tryCatch(
    {
      safe_fread(
        file = file,
        file_type = file_type,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    },
    error = function(e) {
      issues <<- c(issues, paste0("Failed to read file: ", conditionMessage(e)))
      return(tidytable::tidytable()) # Return empty on error
    }
  )

  n_rows <- nrow(df)
  n_cols <- ncol(df)
  columns <- names(df)

  # Check for empty file
  if (n_rows == 0L && length(issues) == 0L) {
    issues <- c(issues, paste0(file_type, " is empty (0 rows)"))
  }

  if (n_cols == 0L && length(issues) == 0L) {
    issues <- c(issues, paste0(file_type, " has no columns)"))
  }

  # Check for required columns
  if (!is.null(required_cols) && n_cols > 0L) {
    # Replace generic "feature_id" with actual feature_col if present
    required_cols_actual <- gsub("^feature_id$", feature_col, required_cols)
    missing_cols <- setdiff(required_cols_actual, columns)
    if (length(missing_cols) > 0L) {
      issues <- c(
        issues,
        sprintf(
          "Missing required columns: %s",
          paste(missing_cols, collapse = ", ")
        )
      )
    }
  }

  list(
    valid = length(issues) == 0L,
    n_rows = n_rows,
    n_cols = n_cols,
    columns = columns,
    issues = issues
  )
}

#' Sanitize and Validate Metadata Consistency
#'
#' @description Checks that filenames in feature table match those in metadata,
#'     and validates that metadata contains organism information.
#'
#' @param metadata_file Character path to metadata file
#' @param features_file Character path to features file (optional)
#' @param filename_col Character name of column containing filenames in both files
#' @param organism_col Character name of organism column in metadata (default: "organism")
#'
#' @return List with validation results:
#'   - valid: Logical, TRUE if metadata is consistent
#'   - n_samples: Integer, number of samples in metadata
#'   - n_with_organism: Integer, number of samples with organism info
#'   - filenames_match: Logical, TRUE if features/metadata filenames match
#'   - n_matched: Integer, number of matched filenames (if features provided)
#'   - n_unmatched: Integer, number of unmatched filenames (if features provided)
#'   - unmatched_files: Character vector of unmatched filenames
#'   - issues: Character vector of issues found (if any)
#'
#' @keywords internal
sanitize_metadata <- function(
  metadata_file,
  features_file = NULL,
  filename_col = "filename",
  organism_col = "organism"
) {
  issues <- character(0)
  n_samples <- 0L
  n_with_organism <- 0L
  filenames_match <- NA
  n_matched <- 0L
  n_unmatched <- 0L
  unmatched_files <- character(0)

  # Validate metadata file exists
  if (!file.exists(metadata_file)) {
    return(list(
      valid = FALSE,
      n_samples = 0L,
      n_with_organism = 0L,
      filenames_match = FALSE,
      n_matched = 0L,
      n_unmatched = 0L,
      unmatched_files = character(0),
      issues = paste0("Metadata file not found: ", metadata_file)
    ))
  }

  # Read metadata
  metadata <- tryCatch(
    {
      safe_fread(
        file = metadata_file,
        file_type = "metadata",
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    },
    error = function(e) {
      issues <<- c(
        issues,
        paste0("Failed to read metadata: ", conditionMessage(e))
      )
      return(tidytable::tidytable()) # Return empty on error
    }
  )

  n_samples <- nrow(metadata)

  # If we couldn't read the file, return early
  if (n_samples == 0L && length(issues) > 0L) {
    return(list(
      valid = FALSE,
      n_samples = 0L,
      n_with_organism = 0L,
      filenames_match = FALSE,
      n_matched = 0L,
      n_unmatched = 0L,
      unmatched_files = character(0),
      issues = issues
    ))
  }

  meta_cols <- names(metadata)

  # Check for filename column (try common variants)
  filename_col_actual <- NULL
  possible_filename_cols <- c(
    filename_col,
    "file",
    "sample",
    "sample_id",
    "sampleID"
  )
  for (col in possible_filename_cols) {
    if (col %in% meta_cols) {
      filename_col_actual <- col
      break
    }
  }

  if (is.null(filename_col_actual)) {
    issues <- c(
      issues,
      sprintf(
        "Filename column not found (tried: %s)",
        paste(possible_filename_cols, collapse = ", ")
      )
    )
  }

  # Check for organism column (try common variants, starting with the provided one)
  organism_col_actual <- NULL
  possible_organism_cols <- unique(c(
    organism_col,
    "ATTRIBUTE_species",
    "organism",
    "organism_name",
    "species",
    "taxon",
    "taxonomy"
  ))
  for (col in possible_organism_cols) {
    if (col %in% meta_cols) {
      organism_col_actual <- col
      break
    }
  }

  if (is.null(organism_col_actual)) {
    # Organism column not required, just note it
    log_debug(
      "Note: No organism column found in metadata (tried: %s)",
      paste(possible_organism_cols, collapse = ", ")
    )
  } else {
    # Count samples with organism info
    organisms <- metadata[[organism_col_actual]]
    n_with_organism <- sum(
      !is.na(organisms) & nchar(trimws(as.character(organisms))) > 0
    )

    if (n_with_organism == 0L) {
      issues <- c(
        issues,
        sprintf(
          "No organism information in column '%s' (all values empty or NA)",
          organism_col_actual
        )
      )
    } else if (n_with_organism < n_samples) {
      log_debug(
        "Only %d/%d samples have organism information in column '%s'",
        n_with_organism,
        n_samples,
        organism_col_actual
      )
    }
  }

  # If features file provided, check filename consistency
  if (
    !is.null(features_file) &&
      file.exists(features_file) &&
      !is.null(filename_col_actual)
  ) {
    tryCatch(
      {
        features <- safe_fread(
          file = features_file,
          file_type = "features table"
        )

        feature_cols <- names(features)

        # Get metadata filenames
        metadata_filenames <- unique(metadata[[filename_col_actual]])
        metadata_filenames <- metadata_filenames[!is.na(metadata_filenames)]
        metadata_filenames <- as.character(metadata_filenames)

        # Remove file extensions from metadata filenames for matching
        metadata_basenames <- gsub("\\.[^.]*$", "", metadata_filenames)

        # Check if metadata filenames (without extension) are PART OF feature column names
        matched_files <- character(0)
        missing_files <- character(0)

        for (basename in metadata_basenames) {
          # Check if this basename appears in any feature column name
          found <- any(grepl(basename, feature_cols, fixed = TRUE))
          if (found) {
            matched_files <- c(matched_files, basename)
          } else {
            missing_files <- c(missing_files, basename)
          }
        }

        n_matched <- length(matched_files)
        n_unmatched <- length(missing_files)
        unmatched_files <- missing_files

        if (n_matched == 0L && length(metadata_basenames) > 0L) {
          # ERROR: None matched
          issues <- c(
            issues,
            sprintf(
              "No metadata filenames (without extension) found in features columns (checked %d filenames)",
              length(metadata_basenames)
            )
          )
          filenames_match <- FALSE
        } else if (length(missing_files) > 0L) {
          # WARNING: Some matched, some didn't
          log_warn(
            "Metadata filenames not found in features columns: %d/%d missing",
            length(missing_files),
            length(metadata_basenames)
          )
          if (length(missing_files) <= 5L) {
            log_warn("  Missing: %s", paste(missing_files, collapse = ", "))
          } else {
            log_warn(
              "  Missing: %s, ... and %d more",
              paste(head(missing_files, 5), collapse = ", "),
              length(missing_files) - 5L
            )
          }
          filenames_match <- TRUE # Still valid, just a warning
        } else if (length(metadata_basenames) > 0L) {
          filenames_match <- TRUE
          log_debug(
            "All %d metadata filenames (without extension) found in features columns",
            n_matched
          )
        }
      },
      error = function(e) {
        issues <<- c(
          issues,
          paste0("Failed to read features file: ", conditionMessage(e))
        )
      }
    )
  }

  list(
    valid = length(issues) == 0L,
    n_samples = n_samples,
    n_with_organism = n_with_organism,
    filenames_match = filenames_match,
    n_matched = n_matched,
    n_unmatched = n_unmatched,
    unmatched_files = unmatched_files,
    issues = issues
  )
}

#' Sanitize and Validate SIRIUS Output
#'
#' @description Checks that SIRIUS output directory or ZIP file contains all
#'     necessary files for TIMA processing. Handles both extracted directories
#'     and ZIP archives. Supports SIRIUS v5 and v6 formats.
#'
#' @param sirius_dir Character path to SIRIUS output directory or ZIP file
#'
#' @return List with validation results:
#'   - valid: Logical, TRUE if all required files present
#'   - has_formula: Logical, formula identifications file found
#'   - has_canopus: Logical, canopus summary file found
#'   - has_structure: Logical, structure identifications file found
#'   - n_features: Integer, number of UNIQUE ANNOTATIONS (rows in structure file)
#'   - is_zip: Logical, TRUE if input was a ZIP file
#'   - sirius_version: Character, detected version ("5", "6", or "unknown")
#'   - issues: Character vector of issues found (if any)
#'
#' @keywords internal
sanitize_sirius <- function(sirius_dir) {
  issues <- character(0)
  has_formula <- FALSE
  has_canopus <- FALSE
  has_structure <- FALSE
  n_features <- 0L
  is_zip <- FALSE
  cleanup_temp <- FALSE
  temp_dir <- NULL

  # Check if input exists (file or directory)
  if (!file.exists(sirius_dir)) {
    return(list(
      valid = FALSE,
      has_formula = FALSE,
      has_canopus = FALSE,
      has_structure = FALSE,
      n_features = 0L,
      is_zip = FALSE,
      issues = paste0("SIRIUS path not found: ", sirius_dir)
    ))
  }

  # Handle ZIP files
  if (file.exists(sirius_dir) && !dir.exists(sirius_dir)) {
    # It's a file, check if it's a ZIP
    if (grepl("\\.zip$", sirius_dir, ignore.case = TRUE)) {
      is_zip <- TRUE
      log_debug("SIRIUS input is a ZIP file, will check contents...")

      # Create temp directory for checking
      temp_dir <- tempfile(pattern = "sirius_check_")
      dir.create(temp_dir, recursive = TRUE)
      cleanup_temp <- TRUE

      # Extract ZIP to temp directory
      tryCatch(
        {
          utils::unzip(sirius_dir, exdir = temp_dir)

          # Find the actual SIRIUS directory (might be nested)
          extracted_items <- list.files(temp_dir, full.names = TRUE)
          if (length(extracted_items) == 1 && dir.exists(extracted_items[1])) {
            # Single directory extracted, use it
            sirius_dir <- extracted_items[1]
          } else {
            # Multiple items or files, use temp_dir
            sirius_dir <- temp_dir
          }
        },
        error = function(e) {
          issues <<- c(
            issues,
            paste0("Failed to extract ZIP: ", conditionMessage(e))
          )
          if (cleanup_temp && dir.exists(temp_dir)) {
            unlink(temp_dir, recursive = TRUE)
          }
        }
      )
    } else {
      return(list(
        valid = FALSE,
        has_formula = FALSE,
        has_canopus = FALSE,
        has_structure = FALSE,
        n_features = 0L,
        is_zip = FALSE,
        issues = paste0(
          "Path exists but is not a directory or ZIP file: ",
          sirius_dir
        )
      ))
    }
  }

  # Now check directory (either original or extracted from ZIP)
  if (!dir.exists(sirius_dir)) {
    if (cleanup_temp && !is.null(temp_dir) && dir.exists(temp_dir)) {
      unlink(temp_dir, recursive = TRUE)
    }
    return(list(
      valid = FALSE,
      has_formula = FALSE,
      has_canopus = FALSE,
      has_structure = FALSE,
      n_features = 0L,
      is_zip = is_zip,
      issues = c(issues, "Could not access SIRIUS directory")
    ))
  }

  # Check for required summary files (try both SIRIUS v5 and v6 names)
  # SIRIUS v5 names
  formula_file_v5 <- file.path(sirius_dir, "formula_identifications.tsv")
  canopus_file_v5 <- file.path(sirius_dir, "canopus_summary.tsv")
  structure_file_v5 <- file.path(sirius_dir, "compound_identifications.tsv")

  # SIRIUS v6 names (with _all suffix)
  formula_file_v6 <- file.path(sirius_dir, "formula_identifications_all.tsv")
  canopus_file_v6 <- file.path(sirius_dir, "canopus_formula_summary_all.tsv")
  structure_file_v6 <- file.path(
    sirius_dir,
    "structure_identifications_all.tsv"
  )

  # Check which files exist
  has_formula <- file.exists(formula_file_v5) || file.exists(formula_file_v6)
  has_canopus <- file.exists(canopus_file_v5) || file.exists(canopus_file_v6)
  has_structure <- file.exists(structure_file_v5) ||
    file.exists(structure_file_v6)

  # Detect SIRIUS version
  sirius_version <- if (
    file.exists(formula_file_v6) || file.exists(canopus_file_v6)
  ) {
    "6"
  } else if (file.exists(formula_file_v5) || file.exists(canopus_file_v5)) {
    "5"
  } else {
    "unknown"
  }

  if (!has_formula) {
    issues <- c(
      issues,
      "Missing formula identifications file (tried v5 and v6 names)"
    )
  }

  if (!has_canopus) {
    issues <- c(issues, "Missing CANOPUS summary file (tried v5 and v6 names)")
  }

  if (!has_structure) {
    issues <- c(
      issues,
      "Missing structure identifications file (tried v5 and v6 names)"
    )
  }

  # Count UNIQUE ANNOTATIONS from structure identifications file
  n_features <- 0L
  tryCatch(
    {
      # Determine which structure file exists
      structure_file_to_read <- NULL
      if (file.exists(structure_file_v6)) {
        structure_file_to_read <- structure_file_v6
      } else if (file.exists(structure_file_v5)) {
        structure_file_to_read <- structure_file_v5
      }

      if (!is.null(structure_file_to_read)) {
        # Read structure identifications and count UNIQUE annotations
        structure_data <- safe_fread(
          file = structure_file_to_read,
          file_type = "SIRIUS structure identifications",
          na.strings = c("", "NA"),
          colClasses = "character"
        )

        # Count unique structure annotations (unique InChIKeys or structure IDs)
        # Each row is an annotation, count total rows for unique annotations
        n_features <- nrow(structure_data)

        log_debug(
          "Counted %d unique annotations from %s",
          n_features,
          basename(structure_file_to_read)
        )
      }
    },
    error = function(e) {
      log_debug("Could not count SIRIUS annotations: %s", conditionMessage(e))
    }
  )

  # Cleanup temp directory if we created one
  if (cleanup_temp && !is.null(temp_dir) && dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }

  list(
    valid = length(issues) == 0L,
    has_formula = has_formula,
    has_canopus = has_canopus,
    has_structure = has_structure,
    n_features = n_features,
    is_zip = is_zip,
    sirius_version = sirius_version,
    issues = issues
  )
}

#' Sanitize All Input Data
#'
#' @description Comprehensive validation of all input data before starting
#'     expensive processing. Reports issues immediately to save time.
#'
#' @param features_file Character path to features CSV/TSV (optional)
#' @param mgf_file Character path to MGF file (optional)
#' @param metadata_file Character path to metadata file (optional)
#' @param sirius_dir Character path to SIRIUS output directory or ZIP (optional)
#' @param filename_col Character name of filename column (default: "filename")
#' @param organism_col Character name of organism column (default: "organism")
#' @param feature_col Character name of feature ID column (default: "feature_id")
#'
#' @return Invisible TRUE if all validations pass, stops with error otherwise
#'
#' @export
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
      required_cols = c("feature_id"),
      feature_col = feature_col
    )

    if (result$valid) {
      log_success(
        "✓ Features file: %d rows, %d columns",
        result$n_rows,
        result$n_cols
      )
      log_debug("  Columns: %s", paste(result$columns, collapse = ", "))
    } else {
      log_error("✗ Features file has issues:")
      for (issue in result$issues) {
        log_error("  - %s", issue)
      }
      issues_found$features <- result$issues
      all_valid <- FALSE
    }
  }

  # Check MGF file
  if (!is.null(mgf_file)) {
    log_info("Checking MGF file...")
    result <- sanitize_mgf(file = mgf_file, file_type = "MGF spectra")

    if (result$valid) {
      log_success("✓ MGF file: %d MS2 spectra found", result$n_spectra)
    } else {
      log_error("✗ MGF file has issues:")
      for (issue in result$issues) {
        log_error("  - %s", issue)
      }
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
        "✓ Metadata file: %d samples, %d with organism info",
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
      log_error("✗ Metadata validation has issues:")
      for (issue in result$issues) {
        log_error("  - %s", issue)
      }
      if (length(result$unmatched_files) > 0L) {
        log_error("  Unmatched files:")
        for (f in head(result$unmatched_files, 5)) {
          log_error("    - %s", f)
        }
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
        "✓ SIRIUS output%s%s: %d annotations, all required files present",
        format_type,
        version_info,
        result$n_features
      )
      log_debug(
        "  - Formula identifications: %s",
        if (result$has_formula) "✓" else "✗"
      )
      log_debug("  - CANOPUS summary: %s", if (result$has_canopus) "✓" else "✗")
      log_debug(
        "  - Structure identifications: %s",
        if (result$has_structure) "✓" else "✗"
      )
    } else {
      log_error("✗ SIRIUS output has issues:")
      for (issue in result$issues) {
        log_error("  - %s", issue)
      }
      issues_found$sirius <- result$issues
      all_valid <- FALSE
    }
  }

  # Summary
  log_info("=" |> rep(60) |> paste(collapse = ""))

  if (all_valid) {
    log_success("✓ All pre-flight checks passed!")
    log_info("Data validation complete. Ready to proceed.")
    log_info("=" |> rep(60) |> paste(collapse = ""))
    return(invisible(TRUE))
  } else {
    log_error("✗ Pre-flight checks failed!")
    log_error(
      "Found issues in: %s",
      paste(names(issues_found), collapse = ", ")
    )
    log_info("=" |> rep(60) |> paste(collapse = ""))

    stop(
      format_error(
        problem = "Input data validation failed",
        received = sprintf("%d issue(s) found", length(issues_found)),
        context = paste(
          "Please fix the issues above before running expensive operations",
          "like library downloads or processing.",
          sep = "\n"
        ),
        fix = paste(
          "1. Review the error messages above",
          "2. Fix the identified issues in your input files",
          "3. Re-run the validation",
          sep = "\n"
        )
      ),
      call. = FALSE
    )
  }
}
