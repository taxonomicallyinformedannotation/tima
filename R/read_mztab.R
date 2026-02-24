#' @title Read mzTab-M File and Convert to TIMA Format
#'
#' @description Reads a mzTab-M (Metabolomics) format file and extracts data
#'     into TIMA-compatible formats: feature table (CSV), MS/MS spectra (MGF),
#'     and metadata table (CSV). The function handles missing sections gracefully,
#'     with features (SML table) being required and spectra/metadata optional.
#'
#'     This implementation uses custom parsing of the mzTab-M format following
#'     the PSI standard (https://github.com/HUPO-PSI/mzTab-M). It does not
#'     require the RmzTabM package as that package's API is still under development.
#'
#' @include mztab_utils.R
#' @include export_output.R
#' @include logs_utils.R
#' @include validations_utils.R
#'
#' @param input Character string path to input mzTab-M file (.mztab)
#' @param output_features Character string path for output feature table (CSV/TSV)
#' @param output_spectra Character string path for output spectra file (MGF).
#'     Set to NULL to skip spectra export.
#' @param output_metadata Character string path for output metadata file (CSV/TSV).
#'     Set to NULL to skip metadata export.
#' @param name_features Character string for feature ID column name (default: "feature_id")
#' @param name_rt Character string for retention time column name (default: "rt")
#' @param name_mz Character string for m/z column name (default: "mz")
#' @param name_adduct Character string for adduct column name (default: "adduct")
#'
#' @return List with paths to created files:
#'   \item{features}{Path to feature table}
#'   \item{spectra}{Path to spectra file (NULL if not created)}
#'   \item{metadata}{Path to metadata file (NULL if not created)}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read mzTab-M file and convert to TIMA format
#' paths <- read_mztab(
#'   input = "data/source/experiment.mztab",
#'   output_features = "data/interim/features/features.csv",
#'   output_spectra = "data/source/spectra.mgf",
#'   output_metadata = "data/source/metadata.csv"
#' )
#'
#' # Access the created files
#' features <- read.csv(paths$features)
#' }
read_mztab <- function(
  input,
  output_features,
  output_spectra = NULL,
  output_metadata = NULL,
  name_features = "feature_id",
  name_rt = "rt",
  name_mz = "mz",
  name_adduct = "adduct"
) {
  # Start logging ----
  ctx <- log_operation(
    "read_mztab",
    input = input,
    has_spectra = !is.null(output_spectra),
    has_metadata = !is.null(output_metadata)
  )

  # Input Validation ----
  log_metadata(ctx, phase = "validation")

  validate_character(input, param_name = "input")
  validate_character(output_features, param_name = "output_features")

  if (!is.null(output_spectra)) {
    validate_character(output_spectra, param_name = "output_spectra")
  }

  if (!is.null(output_metadata)) {
    validate_character(output_metadata, param_name = "output_metadata")
  }

  # Check input file exists
  if (!file.exists(input)) {
    stop(
      "✗ mzTab file not found: ",
      input,
      "\n\n",
      "Fix: Verify the file path is correct",
      call. = FALSE
    )
  }

  # Load mzTab-M File ----
  log_metadata(ctx, phase = "loading")
  log_info("Reading mzTab-M file: %s", basename(input))

  mztab_obj <- tryCatch(
    {
      # Read mzTab file line by line and parse sections
      # mzTab format has different section prefixes:
      # MTD = metadata, SMH/SML = small molecule, SME = small molecule evidence

      lines <- readLines(input)

      # Parse sections
      mtd_lines <- grep("^MTD", lines, value = TRUE)
      smh_line <- grep("^SMH", lines, value = TRUE)
      sml_lines <- grep("^SML", lines, value = TRUE)
      sme_lines <- grep("^SME", lines, value = TRUE)

      # Parse metadata
      if (length(mtd_lines) > 0) {
        mtd_data <- purrr::map(mtd_lines, function(line) {
          parts <- stringi::stri_split_fixed(line, "\t")[[1]]
          if (length(parts) >= 3) {
            data.frame(
              key = parts[2],
              value = parts[3],
              stringsAsFactors = FALSE
            )
          } else {
            NULL
          }
        }) |>
          purrr::compact() |>
          tidytable::bind_rows()
      } else {
        mtd_data <- data.frame(key = character(), value = character())
      }

      # Parse small molecule table
      if (length(smh_line) > 0 && length(sml_lines) > 0) {
        # Get column names from header
        header_parts <- stringi::stri_split_fixed(smh_line[1], "\t")[[1]]
        col_names <- header_parts[-1] # Remove "SMH" prefix

        # Parse data rows
        sml_data <- purrr::map(sml_lines, function(line) {
          parts <- stringi::stri_split_fixed(line, "\t")[[1]]
          parts <- parts[-1] # Remove "SML" prefix

          # Create row as named list
          row_list <- as.list(parts)
          names(row_list) <- col_names[seq_along(row_list)]

          as.data.frame(row_list, stringsAsFactors = FALSE)
        }) |>
          tidytable::bind_rows()
      } else {
        sml_data <- NULL
      }

      # Parse small molecule evidence (MS/MS spectra info)
      if (length(sme_lines) > 0) {
        sme_header <- grep("^SME", lines, value = TRUE)[1]
        if (!is.na(sme_header)) {
          sme_col_names <- stringi::stri_split_fixed(sme_header, "\t")[[1]][-1]

          sme_data <- purrr::map(sme_lines[-1], function(line) {
            parts <- stringi::stri_split_fixed(line, "\t")[[1]][-1]
            row_list <- as.list(parts)
            names(row_list) <- sme_col_names[seq_along(row_list)]
            as.data.frame(row_list, stringsAsFactors = FALSE)
          }) |>
            tidytable::bind_rows()
        } else {
          sme_data <- NULL
        }
      } else {
        sme_data <- NULL
      }

      # Return structured object similar to what RmzTabM would return
      list(
        Metadata = mtd_data,
        Small_Molecule = sml_data,
        Small_Molecule_Evidence = sme_data
      )
    },
    error = function(e) {
      stop(
        "✗ Failed to parse mzTab file\n\n",
        "Error: ",
        conditionMessage(e),
        "\n\n",
        "Fix: Ensure file follows mzTab-M 2.0 specification:\n",
        "  https://github.com/HUPO-PSI/mzTab-M/",
        call. = FALSE
      )
    }
  )

  # Validate mzTab structure
  .validate_mztab_structure(mztab_obj)

  # Extract and Convert Features ----
  log_metadata(ctx, phase = "extracting_features")

  sml_data <- mztab_obj$Small_Molecule
  log_debug("Found %d small molecules in SML table", nrow(sml_data))

  # Map columns to TIMA schema
  features <- .map_mztab_sml_to_features(
    sml_data,
    name_features = name_features,
    name_rt = name_rt,
    name_mz = name_mz,
    name_adduct = name_adduct
  )

  # Get abundance column mapping
  metadata <- .extract_mztab_metadata(mztab_obj$Metadata)
  abundance_mapping <- .map_abundance_to_samples(sml_data, metadata)

  # Rename abundance columns to sample names
  if (length(abundance_mapping) > 0) {
    for (old_col in names(abundance_mapping)) {
      new_col <- abundance_mapping[old_col]
      if (old_col %in% names(features)) {
        names(features)[names(features) == old_col] <- new_col
      }
    }
    log_debug(
      "Mapped %d abundance columns to sample names",
      length(abundance_mapping)
    )
  }

  # Export features table
  log_metadata(ctx, phase = "exporting_features")
  export_output(x = features, file = output_features)
  log_info("Exported feature table: %s", basename(output_features))

  # Extract and Convert Spectra (MS2 if available, MS1 fallback) ----
  spectra_path <- NULL

  if (!is.null(output_spectra)) {
    log_metadata(ctx, phase = "extracting_spectra")

    # Check for SME (Small Molecule Evidence) table with MS/MS data
    has_sme <- !is.null(mztab_obj$Small_Molecule_Evidence) &&
      nrow(mztab_obj$Small_Molecule_Evidence) > 0

    if (has_sme) {
      log_debug(
        "Found Small Molecule Evidence table with %d entries",
        nrow(mztab_obj$Small_Molecule_Evidence)
      )

      # Convert SME table to MGF format
      spectra_path <- .convert_sme_to_mgf(
        sme_data = mztab_obj$Small_Molecule_Evidence,
        sml_data = sml_data,
        output_file = output_spectra
      )

      if (!is.null(spectra_path)) {
        log_info("Exported MS/MS spectra file: %s", basename(spectra_path))
      }
    }

    # If no MS2 spectra available, create MS1-only MGF from features
    if (is.null(spectra_path) || !file.exists(spectra_path)) {
      log_info(
        "No MS/MS spectra found - creating MS1-only MGF from feature table"
      )

      spectra_path <- .create_ms1_mgf_from_features(
        features = features,
        output_file = output_spectra
      )

      if (!is.null(spectra_path)) {
        log_info("Exported MS1 feature MGF: %s", basename(spectra_path))
      }
    }
  }

  # Extract and Convert Metadata (if requested) ----
  metadata_path <- NULL

  if (!is.null(output_metadata)) {
    log_metadata(ctx, phase = "extracting_metadata")

    if (nrow(metadata$samples) > 0 || nrow(metadata$assays) > 0) {
      # Create metadata table
      metadata_table <- .create_metadata_table(metadata, abundance_mapping)

      if (nrow(metadata_table) > 0) {
        export_output(x = metadata_table, file = output_metadata)
        metadata_path <- output_metadata
        log_info("Exported metadata table: %s", basename(metadata_path))
      } else {
        log_warn("No sample metadata found in mzTab file")
      }
    } else {
      log_warn(
        "No metadata found in mzTab file - consider providing organism info manually"
      )
    }
  }

  # Complete ----
  log_complete(
    ctx,
    n_features = nrow(features),
    n_samples = length(abundance_mapping),
    has_spectra = !is.null(spectra_path),
    has_metadata = !is.null(metadata_path)
  )

  # Return paths
  result <- list(
    features = output_features,
    spectra = spectra_path,
    metadata = metadata_path
  )

  invisible(result)
}

#' Convert Small Molecule Evidence table to MGF format
#'
#' @description Internal function to convert mzTab-M SME table (MS/MS spectra)
#'     to MGF format for TIMA processing.
#'
#' @param sme_data Data frame with Small Molecule Evidence table
#' @param sml_data Data frame with Small Molecule table (for metadata)
#' @param output_file Character string path for output MGF file
#'
#' @return Character string path to created MGF file, or NULL if no spectra
#'
#' @keywords internal
.convert_sme_to_mgf <- function(sme_data, sml_data, output_file) {
  # mzTab-M Small Molecule Evidence (SME) table contains:
  # - SME_ID: evidence identifier
  # - evidence_input_id: links back to Small Molecule Feature (SMF)
  # - SML_ID_REFS / sml_id_refs: links back to small molecule
  # - ms_run_ref / ms_run[1]: which MS run
  # - spectra_ref: reference to spectrum (e.g., "ms_run[1]:scan=123")
  # - exp_mass_to_charge: experimental precursor m/z
  # - charge: precursor charge
  # - retention_time_in_seconds: RT in seconds
  # - opt_*: optional columns that may contain peak lists

  if (is.null(sme_data) || nrow(sme_data) == 0) {
    return(NULL)
  }

  # Identify which column holds the precursor m/z
  mz_col <- NULL
  for (candidate in c(
    "exp_mass_to_charge",
    "precursor_mz",
    "calc_mass_to_charge"
  )) {
    if (candidate %in% names(sme_data)) {
      mz_col <- candidate
      break
    }
  }

  if (is.null(mz_col)) {
    log_warn("SME table has no precursor m/z column; cannot generate MGF")
    return(NULL)
  }

  # Identify peak list columns (mzTab-M may store peaks in opt_* columns)
  peak_col <- NULL
  for (candidate in c(
    "opt_peak_mz_intensity",
    "opt_spectra_peaks",
    "opt_ms2_peaks",
    "opt_peak_list"
  )) {
    if (candidate %in% names(sme_data)) {
      peak_col <- candidate
      break
    }
  }

  # Identify the SML reference column
  sml_ref_col <- NULL
  for (candidate in c("SML_ID_REFS", "sml_id_refs", "SML_ID")) {
    if (candidate %in% names(sme_data)) {
      sml_ref_col <- candidate
      break
    }
  }

  # Identify the evidence ID column
  id_col <- NULL
  for (candidate in c("SME_ID", "evidence_input_id")) {
    if (candidate %in% names(sme_data)) {
      id_col <- candidate
      break
    }
  }

  # RT column
  rt_col <- NULL
  for (candidate in c("retention_time_in_seconds", "retention_time")) {
    if (candidate %in% names(sme_data)) {
      rt_col <- candidate
      break
    }
  }

  # Charge column
  charge_col <- NULL
  for (candidate in c("charge", "precursor_charge")) {
    if (candidate %in% names(sme_data)) {
      charge_col <- candidate
      break
    }
  }

  # Build MGF blocks
  create_dir(export = output_file)

  mgf_blocks <- vapply(
    seq_len(nrow(sme_data)),
    function(i) {
      row <- sme_data[i, ]

      # Feature ID: use evidence ID, or SML ref, or row index
      feat_id <- if (!is.null(id_col) && !is.na(row[[id_col]])) {
        as.character(row[[id_col]])
      } else if (!is.null(sml_ref_col) && !is.na(row[[sml_ref_col]])) {
        as.character(row[[sml_ref_col]])
      } else {
        as.character(i)
      }

      # Precursor m/z
      pepmass <- as.character(row[[mz_col]])
      if (is.na(pepmass) || pepmass == "null") {
        return(NA_character_)
      }

      # Retention time
      rt_val <- if (!is.null(rt_col) && !is.na(row[[rt_col]]) &&
        row[[rt_col]] != "null") {
        as.character(row[[rt_col]])
      } else {
        "0"
      }

      # Charge
      charge_val <- if (!is.null(charge_col) && !is.na(row[[charge_col]]) &&
        row[[charge_col]] != "null") {
        ch <- as.character(row[[charge_col]])
        # Ensure format like "1+" or "1-"
        if (!grepl("[+-]$", ch)) paste0(ch, "+") else ch
      } else {
        "1+"
      }

      # Peak list
      peaks_str <- if (!is.null(peak_col) && !is.na(row[[peak_col]]) &&
        row[[peak_col]] != "null" && nzchar(row[[peak_col]])) {
        # Parse peak list: typically "mz1:int1|mz2:int2|..." or
        # "mz1 int1\nmz2 int2\n..."
        raw <- as.character(row[[peak_col]])
        # Normalize separators
        raw <- gsub("\\|", "\n", raw)
        raw <- gsub(":", "\t", raw)
        raw <- gsub(",", "\t", raw)
        # Split into lines and filter empty
        peak_lines <- strsplit(raw, "\n")[[1]]
        peak_lines <- peak_lines[nzchar(trimws(peak_lines))]
        if (length(peak_lines) > 0) {
          paste(peak_lines, collapse = "\n")
        } else {
          # Fallback: single peak at precursor m/z
          paste(pepmass, "1000.0", sep = "\t")
        }
      } else {
        # No peak data — create MS1-level entry with single peak at precursor
        paste(pepmass, "1000.0", sep = "\t")
      }

      # Build MGF block
      paste0(
        "BEGIN IONS\n",
        "TITLE=", feat_id, "\n",
        "PEPMASS=", pepmass, "\n",
        "RTINSECONDS=", rt_val, "\n",
        "CHARGE=", charge_val, "\n",
        "SCANS=", feat_id, "\n",
        "MSLEVEL=2\n",
        peaks_str, "\n",
        "END IONS\n"
      )
    },
    character(1)
  )

  # Remove NAs (entries with no valid m/z)
  mgf_blocks <- mgf_blocks[!is.na(mgf_blocks)]

  if (length(mgf_blocks) == 0) {
    log_warn("No valid spectra could be extracted from SME table")
    return(NULL)
  }

  # Write all blocks at once (vectorized)
  writeLines(mgf_blocks, output_file)

  log_debug(
    "Converted %d SME entries to MGF format",
    length(mgf_blocks)
  )
  return(output_file)
}

#' Create MS1-only MGF from feature table
#'
#' @description Internal function to create a minimal MGF file containing
#'     MS1-level information (precursor m/z) for features when no MS/MS
#'     spectra are available. This allows the workflow to continue with
#'     mass-based matching.
#'
#' @param features Data frame with feature table (must have feature_id, mz)
#' @param output_file Character string path for output MGF file
#'
#' @return Character string path to created MGF file, or NULL on failure
#'
#' @keywords internal
.create_ms1_mgf_from_features <- function(features, output_file) {
  # Validate required columns
  required_cols <- c("feature_id", "mz")
  if (!all(required_cols %in% names(features))) {
    log_warn(
      "Cannot create MS1 MGF - missing required columns: %s",
      paste(setdiff(required_cols, names(features)), collapse = ", ")
    )
    return(NULL)
  }

  # Filter features with valid m/z
  valid_features <- features[!is.na(features$mz) & features$mz > 0, ]

  if (nrow(valid_features) == 0) {
    log_warn("No valid features with m/z values for MGF creation")
    return(NULL)
  }

  # Create output directory
  create_dir(export = output_file)

  tryCatch(
    {
      # Build all MGF blocks vectorized
      has_rt <- "rt" %in% names(valid_features)
      has_adduct <- "adduct" %in% names(valid_features)

      rt_seconds <- if (has_rt) {
        ifelse(
          !is.na(valid_features$rt),
          valid_features$rt * 60,
          0
        )
      } else {
        rep(0, nrow(valid_features))
      }

      charges <- if (has_adduct) {
        vapply(
          valid_features$adduct,
          function(a) {
            if (is.na(a)) "1+" else .extract_charge_from_adduct(a)
          },
          character(1)
        )
      } else {
        rep("1+", nrow(valid_features))
      }

      mgf_blocks <- paste0(
        "BEGIN IONS\n",
        "TITLE=", valid_features$feature_id, "\n",
        "PEPMASS=", valid_features$mz, "\n",
        "RTINSECONDS=", rt_seconds, "\n",
        "CHARGE=", charges, "\n",
        "SCANS=", valid_features$feature_id, "\n",
        "MSLEVEL=1\n",
        valid_features$mz, "\t1000.0\n",
        "END IONS\n"
      )

      writeLines(mgf_blocks, output_file)

      log_debug("Created MS1-only MGF with %d features", nrow(valid_features))
      return(output_file)
    },
    error = function(e) {
      log_error("Failed to create MS1 MGF: %s", conditionMessage(e))
      return(NULL)
    }
  )
}

#' Extract charge from adduct notation
#'
#' @description Helper to extract charge state from adduct string
#'
#' @param adduct Character string with adduct notation (e.g., "\[M+H\]+")
#'
#' @return Character string with charge (e.g., "1+", "2-")
#'
#' @keywords internal
.extract_charge_from_adduct <- function(adduct) {
  if (is.na(adduct) || nchar(adduct) == 0) {
    return("1+")
  }

  # Extract charge from adduct notation
  # Patterns: [M+H]+, [M+2H]2+, [M-H]-, etc.
  charge <- stringi::stri_extract_first_regex(adduct, "\\d*[+-]$")

  if (is.na(charge)) {
    return("1+")
  }

  # If no number before +/-, assume 1
  if (nchar(charge) == 1) {
    charge <- paste0("1", charge)
  }

  return(charge)
}

#' Create metadata table from mzTab-M metadata
#'
#' @description Internal function to create TIMA-compatible metadata table
#'     from parsed mzTab-M metadata section.
#'
#' @param metadata List with parsed metadata (from .extract_mztab_metadata)
#' @param abundance_mapping Named vector mapping abundance columns to sample names
#'
#' @return Data frame with metadata in TIMA format
#'
#' @keywords internal
.create_metadata_table <- function(metadata, abundance_mapping) {
  # Create basic metadata table with sample names
  if (length(abundance_mapping) == 0) {
    return(data.frame())
  }

  # Use sample names from abundance mapping
  sample_names <- as.character(abundance_mapping)

  metadata_table <- data.frame(
    filename = sample_names,
    sample_id = names(abundance_mapping),
    stringsAsFactors = FALSE
  )

  # Add placeholder for organism (user will need to fill this manually)
  metadata_table$ATTRIBUTE_species <- NA_character_

  # Add note about organism information
  log_info("Note: Please add organism/taxonomy information to metadata file")
  log_info("      Column 'ATTRIBUTE_species' is currently empty")

  return(metadata_table)
}
