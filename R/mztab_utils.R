#' @title mzTab-M Utility Functions
#'
#' @description Internal utility functions for parsing and handling mzTab-M files.
#'     These functions provide column mapping, metadata extraction, and validation
#'     helpers for mzTab-M format support.
#'
#' @keywords internal
#' @name mztab_utils
NULL

#' Map mzTab-M SML columns to TIMA feature table schema
#'
#' @description Maps mzTab-M Small Molecule (SML) table columns to TIMA's
#'     internal feature table format.
#'
#' @param sml_data Data frame containing mzTab-M SML table
#' @param name_features Character string for feature ID column name (default: "feature_id")
#' @param name_rt Character string for retention time column name (default: "rt")
#' @param name_mz Character string for m/z column name (default: "mz")
#' @param name_adduct Character string for adduct column name (default: "adduct")
#'
#' @return Data frame with mapped column names following TIMA schema
#'
#' @keywords internal
.map_mztab_sml_to_features <- function(
  sml_data,
  name_features = "feature_id",
  name_rt = "rt",
  name_mz = "mz",
  name_adduct = "adduct"
) {
  # mzTab-M SML table standard columns:
  # SML_ID, chemical_formula, smiles, inchi, chemical_name, uri, theoretical_neutral_mass,
  # adduct_ions, reliability, retention_time_in_seconds, exp_mass_to_charge
  # abundance_assay[n] for quantification

  result <- sml_data

  # Map feature ID - use SML_ID (unique identifier for each small molecule)
  if ("SML_ID" %in% names(sml_data)) {
    result[[name_features]] <- as.character(sml_data$SML_ID)
  }

  # Map retention time - convert seconds to minutes
  if ("retention_time_in_seconds" %in% names(sml_data)) {
    rt_seconds <- sml_data$retention_time_in_seconds
    # Handle NULL/NA values gracefully
    result[[name_rt]] <- ifelse(
      is.na(rt_seconds) | rt_seconds == "null",
      NA_real_,
      as.numeric(rt_seconds) / 60.0
    )
  } else if ("retention_time" %in% names(sml_data)) {
    # Some files might use retention_time directly
    result[[name_rt]] <- as.numeric(sml_data$retention_time)
  }

  # Map m/z - use exp_mass_to_charge (experimental m/z)
  if ("exp_mass_to_charge" %in% names(sml_data)) {
    result[[name_mz]] <- as.numeric(sml_data$exp_mass_to_charge)
  }

  # Map adduct - mzTab uses adduct_ions field
  if ("adduct_ions" %in% names(sml_data)) {
    # Extract adduct notation from mzTab format
    # mzTab format example: "[M+H]1+"
    result[[name_adduct]] <- .parse_mztab_adduct(sml_data$adduct_ions)
  }

  # Keep abundance columns (abundance_assay[n]) - using functional approach
  abundance_cols <- grep("^abundance_assay\\[", names(sml_data), value = TRUE)
  if (length(abundance_cols) > 0) {
    # Copy all abundance columns at once
    result[abundance_cols] <- sml_data[abundance_cols]
  }

  return(result)
}

#' Parse mzTab-M adduct notation to TIMA format
#'
#' @description Converts mzTab-M adduct notation to TIMA's harmonized format.
#'     mzTab format: "\[M+H\]1+" → TIMA format: "\[M+H\]+"
#'
#' @param adduct_string Character vector of mzTab adduct notations
#'
#' @return Character vector with TIMA-compatible adduct notations
#'
#' @keywords internal
.parse_mztab_adduct <- function(adduct_string) {
  # Handle NULL, NA, or "null" values
  if (is.null(adduct_string)) {
    return(NA_character_)
  }

  result <- vapply(
    adduct_string,
    function(x) {
      if (is.na(x) || x == "null" || nchar(x) == 0) {
        return(NA_character_)
      }

      # mzTab format: "[M+H]1+" → extract "[M+H]+"
      # Remove the charge number (keep just the charge sign)
      x <- stringi::stri_replace_all_regex(
        x,
        pattern = "(\\])(\\d+)([+-])",
        replacement = "$1$3"
      )

      return(as.character(x))
    },
    character(1),
    USE.NAMES = FALSE
  )

  return(result)
}

#' Extract metadata from mzTab-M MTD section
#'
#' @description Parses the metadata section of mzTab-M file to extract
#'     sample information, assay-to-sample mappings, and organism data.
#'
#' @param mtd_data Data frame containing mzTab-M metadata (MTD lines)
#'
#' @return List with components:
#'   \item{samples}{Data frame with sample metadata}
#'   \item{assays}{Data frame with assay-to-sample mappings}
#'   \item{ms_runs}{Data frame with MS run information}
#'   \item{study_variables}{Data frame with study variable definitions}
#'
#' @keywords internal
.extract_mztab_metadata <- function(mtd_data) {
  # Initialize result lists
  samples <- list()
  assays <- list()
  ms_runs <- list()
  study_vars <- list()

  # Parse MTD lines
  # Format: MTD\tkey\tvalue
  # Example: MTD\tsample[1]\tSampleName

  if (!is.data.frame(mtd_data) || nrow(mtd_data) == 0) {
    log_warn("No metadata found in mzTab file")
    return(list(
      samples = data.frame(),
      assays = data.frame(),
      ms_runs = data.frame(),
      study_variables = data.frame()
    ))
  }

  # Extract sample information
  sample_rows <- mtd_data[grepl("^sample\\[\\d+\\]$", mtd_data[[1]]), ]
  if (nrow(sample_rows) > 0) {
    for (i in seq_len(nrow(sample_rows))) {
      key <- sample_rows[[1]][i]
      value <- sample_rows[[2]][i]
      sample_id <- stringi::stri_extract_first_regex(key, "\\d+")
      samples[[sample_id]] <- list(
        sample_id = paste0("sample[", sample_id, "]"),
        name = value
      )
    }
  }

  # Extract assay information
  assay_rows <- mtd_data[grepl("^assay\\[\\d+\\]$", mtd_data[[1]]), ]
  if (nrow(assay_rows) > 0) {
    for (i in seq_len(nrow(assay_rows))) {
      key <- assay_rows[[1]][i]
      value <- assay_rows[[2]][i]
      assay_id <- stringi::stri_extract_first_regex(key, "\\d+")
      assays[[assay_id]] <- list(
        assay_id = paste0("assay[", assay_id, "]"),
        name = value
      )
    }
  }

  # Convert lists to data frames
  samples_df <- if (length(samples) > 0) {
    do.call(rbind, lapply(samples, as.data.frame))
  } else {
    data.frame(sample_id = character(), name = character())
  }

  assays_df <- if (length(assays) > 0) {
    do.call(rbind, lapply(assays, as.data.frame))
  } else {
    data.frame(assay_id = character(), name = character())
  }

  return(list(
    samples = samples_df,
    assays = assays_df,
    ms_runs = data.frame(), # Can be extended later
    study_variables = data.frame() # Can be extended later
  ))
}

#' Map mzTab-M abundance columns to sample names
#'
#' @description Creates a mapping between mzTab abundance column names
#'     (abundance_assay\[n\]) and human-readable sample names.
#'
#' @param sml_data Data frame containing mzTab-M SML table
#' @param metadata List containing parsed metadata (from .extract_mztab_metadata)
#'
#' @return Named character vector mapping abundance columns to sample names
#'
#' @keywords internal
.map_abundance_to_samples <- function(sml_data, metadata) {
  # Extract abundance column names
  abundance_cols <- grep("^abundance_assay\\[", names(sml_data), value = TRUE)

  if (length(abundance_cols) == 0) {
    return(character(0))
  }

  # Create mapping
  mapping <- vapply(
    abundance_cols,
    function(col) {
      # Extract assay number: "abundance_assay[3]" → "3"
      assay_num <- stringi::stri_extract_first_regex(col, "(?<=\\[)\\d+(?=\\])")
      assay_id <- paste0("assay[", assay_num, "]")

      # Look up sample name from metadata
      if (!is.null(metadata$assays) && nrow(metadata$assays) > 0) {
        match_idx <- which(metadata$assays$assay_id == assay_id)
        if (length(match_idx) > 0) {
          return(as.character(metadata$assays$name[match_idx[1]]))
        }
      }

      # Fallback to assay ID if no name found
      return(assay_id)
    },
    character(1),
    USE.NAMES = TRUE
  )

  return(mapping)
}

#' Validate mzTab-M file structure
#'
#' @description Checks that a parsed mzTab object has required sections
#'     and minimum required data for TIMA processing.
#'
#' @param mztab_obj mzTab object returned by RmzTabM::readMzTab()
#'
#' @return Invisible NULL on success, stops with error on validation failure
#'
#' @keywords internal
.validate_mztab_structure <- function(mztab_obj) {
  # Check that it's a valid mzTab object
  if (is.null(mztab_obj)) {
    stop(
      "✗ Invalid mzTab file\n\n",
      "The file could not be parsed as mzTab-M format.\n\n",
      "Fix: Ensure the file follows mzTab-M 2.0 specification:\n",
      "  https://github.com/HUPO-PSI/mzTab-M/",
      call. = FALSE
    )
  }

  # Check for Small Molecule (SML) table - REQUIRED
  has_sml <- !is.null(mztab_obj$Small_Molecule) &&
    nrow(mztab_obj$Small_Molecule) > 0

  if (!has_sml) {
    stop(
      "✗ Missing required Small Molecule table\n\n",
      "mzTab-M file must contain at least one small molecule feature (SML table).\n\n",
      "Found sections: ",
      paste(names(mztab_obj), collapse = ", "),
      "\n\n",
      "Fix: Ensure your mzTab file includes feature annotations in the SML section",
      call. = FALSE
    )
  }

  # Check for required columns in SML table
  sml_data <- mztab_obj$Small_Molecule
  required_cols <- c("SML_ID") # Minimum requirement
  missing_cols <- setdiff(required_cols, names(sml_data))

  if (length(missing_cols) > 0) {
    stop(
      "✗ Missing required columns in Small Molecule table\n\n",
      "Required: ",
      paste(required_cols, collapse = ", "),
      "\n",
      "Missing: ",
      paste(missing_cols, collapse = ", "),
      "\n\n",
      "Fix: Ensure SML table has feature identifiers",
      call. = FALSE
    )
  }

  # Warn about optional but useful data
  if (!"exp_mass_to_charge" %in% names(sml_data)) {
    log_warn(
      "SML table missing 'exp_mass_to_charge' - m/z values won't be available"
    )
  }

  if (!"retention_time_in_seconds" %in% names(sml_data)) {
    log_warn(
      "SML table missing 'retention_time_in_seconds' - RT filtering unavailable"
    )
  }

  # Check for abundance data
  abundance_cols <- grep("^abundance_assay\\[", names(sml_data), value = TRUE)
  if (length(abundance_cols) == 0) {
    log_warn("No abundance data found - features will have no quantification")
  } else {
    log_debug("Found %d assay abundance columns", length(abundance_cols))
  }

  invisible(NULL)
}
