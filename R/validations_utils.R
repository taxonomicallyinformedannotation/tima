#' Validation Utilities
#'
#' @description Functions for validating inputs to TIMA functions
#'
#' @include errors_utils.R
#' @keywords internal
#' @name validation_utils
NULL

# Core Validation Functions ----

#' Choice validation with fuzzy matching
#'
#' @description Validates that a value is one of allowed choices, with
#'     intelligent suggestions for typos
#'
#' @param value [character] Value to validate
#' @param valid_values [character] Character vector of allowed values
#' @param param_name [character] Parameter name for error messages
#' @param context [character] Optional context (e.g., "Found in:
#'     params$annotate_masses$ms_mode")
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_choice("positive", c("pos", "neg"), "ms_mode")
#' # Error shows: Did you mean 'pos'?
#' }
validate_choice <- function(
  value,
  valid_values,
  param_name = "value",
  context = NULL
) {
  # Type checking
  if (!is.character(value) || length(value) != 1L) {
    msg <- format_error(
      problem = paste0("Invalid type for ", param_name),
      expected = "single character string",
      received = paste0(class(value)[1L], " of length ", length(value)),
      location = context,
      fix = "Provide a single character value"
    )
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Value checking
  if (!value %in% valid_values) {
    suggestion <- .fuzzy_match(value, valid_values)

    msg <- format_error(
      problem = paste0("Invalid ", param_name, ": '", value, "'"),
      expected = paste0(
        "one of: ",
        paste(paste0("'", valid_values, "'"), collapse = ", ")
      ),
      location = context,
      suggestion = if (!is.null(suggestion)) {
        paste0("Did you mean '", suggestion, "'?")
      } else {
        NULL
      },
      fix = paste0("Choose one of: ", paste(valid_values, collapse = ", "))
    )
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  invisible(TRUE)
}

#' File existence validation
#'
#' @description Validates file exists with helpful suggestions for similar files
#'
#' @param path [character] File path to validate
#' @param file_type [character] Type description (e.g., "MGF file", "parameter
#'     file")
#' @param param_name [character] Parameter name for error messages
#' @param required [logical] Whether file is required (vs optional)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
validate_file_exists <- function(
  path,
  file_type = "file",
  param_name = NULL,
  required = TRUE
) {
  if (is.null(path)) {
    if (required) {
      msg <- format_error(
        problem = paste0(
          stringi::stri_trans_totitle(file_type),
          " path is NULL"
        ),
        location = param_name,
        fix = "Provide a valid file path"
      )
      tima_abort_message(
        msg,
        class = c("tima_validation_error", "tima_error")
      )
    }
    return(invisible(TRUE))
  }

  if (!is.character(path) || length(path) != 1L) {
    msg <- format_error(
      problem = "Invalid file path type",
      expected = "single character string",
      received = class(path)[1L],
      location = param_name,
      fix = "Provide a character string file path"
    )
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (!file.exists(path)) {
    dir_path <- dirname(path)
    dir_exists <- dir.exists(dir_path)

    # Look for similar files
    similar <- character(0)
    available <- character(0)
    if (dir_exists) {
      available_files <- list.files(dir_path, full.names = FALSE)
      if (length(available_files) > 0) {
        available <- utils::head(available_files, 5)
        distances <- utils::adist(basename(path), available_files)
        similar_idx <- which(distances <= 3)
        if (length(similar_idx) > 0) {
          similar <- available_files[similar_idx][seq_len(min(
            3,
            length(similar_idx)
          ))]
        }
      }
    }

    suggestion_text <- NULL
    if (length(similar) > 0) {
      suggestion_text <- paste0(
        "Similar files in directory:\n  ",
        paste0("- ", similar, collapse = "\n  ")
      )
    } else if (length(available) > 0) {
      suggestion_text <- paste0(
        "Available files in directory:\n  ",
        paste0("- ", available, collapse = "\n  ")
      )
    }

    msg <- format_error(
      problem = paste0(stringi::stri_trans_totitle(file_type), " not found"),
      received = path,
      location = if (!dir_exists) {
        paste0("Directory does not exist: ", dir_path)
      } else {
        "Directory exists, but file is missing"
      },
      suggestion = suggestion_text,
      fix = paste0(
        "1. Verify the file path is correct\n",
        "2. Check file permissions\n",
        "3. Ensure the file exists at the specified location"
      )
    )
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  invisible(TRUE)
}

#' DataFrame structure validation
#'
#' @description Validates dataframe with helpful column name suggestions
#'
#' @param df [data.frame] Data frame to validate
#' @param required_cols [character] Character vector of required column names
#' @param optional_cols [character] Character vector of optional column names
#' @param df_name [character] Name of the data frame for error messages
#' @param min_rows [integer] Minimum number of rows required
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#' @keywords internal
validate_dataframe_structure <- function(
  df,
  required_cols = NULL,
  optional_cols = NULL,
  df_name = "data frame",
  min_rows = 0L
) {
  # Type check
  if (!is.data.frame(df)) {
    msg <- format_error(
      problem = paste0("Expected data frame for ", df_name),
      expected = "data.frame or tibble",
      received = paste(class(df), collapse = ", "),
      fix = "Ensure input is a valid data frame"
    )
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Row count check
  if (nrow(df) < min_rows) {
    msg <- format_error(
      problem = paste0(df_name, " has insufficient rows"),
      expected = paste0("at least ", min_rows, " row(s)"),
      received = paste0(nrow(df), " row(s)"),
      fix = "Provide a data frame with more rows or adjust min_rows parameter"
    )
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Column validation
  if (!is.null(required_cols)) {
    missing_required <- setdiff(required_cols, names(df))

    if (length(missing_required) > 0) {
      # Try to match missing columns with suggestions
      suggestions <- lapply(missing_required, function(col) {
        match <- .fuzzy_match(col, names(df))
        if (!is.null(match)) {
          list(missing = col, suggestion = match)
        } else {
          NULL
        }
      })
      suggestions <- Filter(Negate(is.null), suggestions)

      suggestion_text <- NULL
      if (length(suggestions) > 0) {
        suggestion_text <- paste0(
          "Did you mean:\n",
          paste(
            sprintf(
              "  - '%s' instead of '%s'?",
              vapply(suggestions, `[[`, character(1L), "suggestion"),
              vapply(suggestions, `[[`, character(1L), "missing")
            ),
            collapse = "\n"
          )
        )
      }

      msg <- format_error(
        problem = paste0("Missing required columns in ", df_name),
        expected = paste(paste0("'", required_cols, "'"), collapse = ", "),
        received = paste0(
          "present: ",
          paste(paste0("'", names(df), "'"), collapse = ", "),
          "\n",
          "missing: ",
          paste(paste0("'", missing_required, "'"), collapse = ", ")
        ),
        suggestion = suggestion_text,
        fix = "Ensure your data contains all required columns with correct names"
      )
      tima_abort_message(
        msg,
        class = c("tima_validation_error", "tima_error")
      )
    }
  }

  # Warn about unexpected columns
  if (!is.null(optional_cols)) {
    all_expected <- c(required_cols, optional_cols)
    unexpected <- setdiff(names(df), all_expected)

    if (length(unexpected) > 0) {
      warning(
        sprintf(
          "\u26A0 Unexpected columns in %s: %s\n  These will be ignored.",
          df_name,
          paste(paste0("'", unexpected, "'"), collapse = ", ")
        ),
        call. = FALSE,
        immediate. = TRUE
      )
    }
  }

  invisible(TRUE)
}

#' Numeric range validation
#'
#' @description Validates numeric values with contextual guidance
#'
#' @param value [numeric] Numeric value to validate
#' @param min_value [numeric] Minimum allowed value
#' @param max_value [numeric] Maximum allowed value
#' @param param_name [character] Parameter name for error messages
#' @param recommended_min [numeric] Recommended minimum (for warnings)
#' @param recommended_max [numeric] Recommended maximum (for warnings)
#' @param context [character] Why this range matters
#'
#' @return Invisible TRUE if valid, stops/warns with messages otherwise
#' @keywords internal
validate_numeric_range <- function(
  value,
  min_value = -Inf,
  max_value = Inf,
  param_name = "value",
  recommended_min = NULL,
  recommended_max = NULL,
  context = NULL
) {
  # Type check
  if (!is.numeric(value) || length(value) != 1L) {
    msg <- format_error(
      problem = paste0("Invalid type for ", param_name),
      expected = "single numeric value",
      received = if (is.numeric(value)) {
        paste0("numeric vector of length ", length(value))
      } else {
        class(value)[1L]
      },
      fix = "Provide a single numeric value"
    )
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (is.na(value)) {
    msg <- format_error(
      problem = paste0(param_name, " cannot be NA"),
      fix = "Provide a non-NA numeric value"
    )
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Hard limits
  if (value < min_value || value > max_value) {
    msg <- format_error(
      problem = paste0(param_name, " out of valid range"),
      expected = paste0("between ", min_value, " and ", max_value),
      received = as.character(value),
      context = context,
      fix = paste0("Use a value between ", min_value, " and ", max_value)
    )
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Recommended range warnings
  if (!is.null(recommended_min) && value < recommended_min) {
    warning(
      sprintf(
        "\u26A0 %s (%s) is below recommended minimum (%s)\n  %s",
        param_name,
        value,
        recommended_min,
        rlang::`%||%`(context, "")
      ),
      call. = FALSE,
      immediate. = TRUE
    )
  }

  if (!is.null(recommended_max) && value > recommended_max) {
    warning(
      sprintf(
        "\u26A0 %s (%s) exceeds recommended maximum (%s)\n  %s",
        param_name,
        value,
        recommended_max,
        rlang::`%||%`(context, "")
      ),
      call. = FALSE,
      immediate. = TRUE
    )
  }

  invisible(TRUE)
}

#' Tolerance validation with instrument context
#'
#' @description Validates mass tolerance with instrument-specific guidance
#'
#' @param tolerance_ppm [numeric] Mass tolerance in parts per million
#' @param param_name [character] Parameter name for error messages
#' @param context [character] Additional context about usage
#'
#' @return Invisible TRUE if valid, stops/warns with messages otherwise
#' @keywords internal
validate_tolerance_ppm <- function(
  tolerance_ppm,
  param_name = "tolerance_ppm",
  context = NULL,
  max_ppm = MAX_TOLERANCE_PPM
) {
  if (!is.numeric(tolerance_ppm) || length(tolerance_ppm) != 1L) {
    msg <- format_error(
      problem = paste0("Invalid ", param_name),
      expected = "single numeric value (ppm)",
      received = class(tolerance_ppm)[1L],
      fix = "Provide a numeric tolerance value in parts per million (ppm)"
    )
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (tolerance_ppm <= 0) {
    msg <- format_error(
      problem = paste0(param_name, " must be positive"),
      received = paste0(tolerance_ppm, " ppm"),
      context = "Mass tolerance defines the matching window for annotation",
      fix = "Use a positive value appropriate for your instrument"
    )
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Provide instrument-specific guidance
  if (tolerance_ppm > max_ppm) {
    instrument_guidance <- paste0(
      "Your instrument: This seems unusually large. For comparison:\n",
      "  - Orbitrap: typically 1-5 ppm\n",
      "  - Q-TOF: typically 5-15 ppm\n",
      "  - Q-Trap: typically 10-50 ppm\n",
      "  - Ion Trap: typically 100-500 ppm"
    )

    msg <- format_error(
      problem = "Mass tolerance exceeds recommended maximum",
      expected = paste0(
        "typically \u2264 ",
        max_ppm,
        " ppm for high-resolution MS"
      ),
      received = paste0(tolerance_ppm, " ppm"),
      context = paste0(
        "Large tolerances may produce excessive false positives and slow annotation.\n",
        instrument_guidance
      ),
      fix = paste0(
        "1. Check if you meant ",
        floor(tolerance_ppm / 100),
        " ppm (not ",
        tolerance_ppm,
        ")\n",
        "2. Verify your instrument's actual mass accuracy\n",
        "3. Consider recalibrating your mass spectrometer"
      )
    )
    warning(msg, call. = FALSE, immediate. = TRUE)
  }

  invisible(TRUE)
}

# TIMA-Specific Validation Functions ----

#' Validate a single file path
#' @keywords internal
#' @noRd
.validate_single_file_path <- function(
  file_path,
  file_name,
  allow_null = FALSE
) {
  # Handle NULL values
  if (is.null(file_path)) {
    if (!allow_null) {
      return(list(
        type = "missing",
        msg = paste0(file_name, ": NULL (file path is NULL)")
      ))
    }
    return(NULL)
  }

  # Validate type
  if (!is.character(file_path) || length(file_path) != 1L) {
    return(list(
      type = "invalid",
      msg = paste0(
        file_name,
        ": must be a single character string, got ",
        class(file_path)[1L]
      )
    ))
  }

  # Check existence using validation
  tryCatch(
    {
      validate_file_exists(
        path = file_path,
        file_type = file_name,
        param_name = file_name,
        required = !allow_null
      )
      NULL
    },
    error = function(e) {
      list(
        type = "missing",
        msg = paste0(file_name, ": ", file_path)
      )
    }
  )
}

#' Validate that files exist
#'
#' @description Checks that one or more file paths exist on the filesystem.
#'     Provides detailed error messages indicating which files are missing.
#'
#' @param file_list [list] Named list of file paths to validate. Names are used
#'     in error messages to identify which file is missing.
#' @param allow_null [logical] Logical, if TRUE, NULL values are allowed
#'     (default: FALSE)
#'
#' @return Invisible TRUE if all files exist, stops with error otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_file_existence(list(
#'   features = "data/features.tsv",
#'   library = "data/library.tsv"
#' ))
#' }
