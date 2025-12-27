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
#' @param value Value to validate
#' @param valid_values Character vector of allowed values
#' @param param_name Parameter name for error messages
#' @param context Optional context (e.g., "Found in: params$annotate_masses$ms_mode")
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
    stop(msg, call. = FALSE)
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
    stop(msg, call. = FALSE)
  }

  invisible(TRUE)
}

#' File existence validation
#'
#' @description Validates file exists with helpful suggestions for similar files
#'
#' @param path File path to validate
#' @param file_type Type description (e.g., "MGF file", "parameter file")
#' @param param_name Parameter name for error messages
#' @param required Whether file is required (vs optional)
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
        problem = paste0(stringr::str_to_title(file_type), " path is NULL"),
        location = param_name,
        fix = "Provide a valid file path"
      )
      stop(msg, call. = FALSE)
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
    stop(msg, call. = FALSE)
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
        available <- head(available_files, 5)
        distances <- adist(basename(path), available_files)
        similar_idx <- which(distances <= 3)
        if (length(similar_idx) > 0) {
          similar <- available_files[similar_idx][1:min(3, length(similar_idx))]
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
      problem = paste0(stringr::str_to_title(file_type), " not found"),
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
    stop(msg, call. = FALSE)
  }

  invisible(TRUE)
}

#' DataFrame structure validation
#'
#' @description Validates dataframe with helpful column name suggestions
#'
#' @param df Data frame to validate
#' @param required_cols Character vector of required column names
#' @param optional_cols Character vector of optional column names
#' @param df_name Name of the data frame for error messages
#' @param min_rows Minimum number of rows required
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
    stop(msg, call. = FALSE)
  }

  # Row count check
  if (nrow(df) < min_rows) {
    msg <- format_error(
      problem = paste0(df_name, " has insufficient rows"),
      expected = paste0("at least ", min_rows, " row(s)"),
      received = paste0(nrow(df), " row(s)"),
      fix = "Provide a data frame with more rows or adjust min_rows parameter"
    )
    stop(msg, call. = FALSE)
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
              sapply(suggestions, `[[`, "suggestion"),
              sapply(suggestions, `[[`, "missing")
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
      stop(msg, call. = FALSE)
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
#' @param value Numeric value to validate
#' @param min_value Minimum allowed value
#' @param max_value Maximum allowed value
#' @param param_name Parameter name for error messages
#' @param recommended_min Recommended minimum (for warnings)
#' @param recommended_max Recommended maximum (for warnings)
#' @param context Why this range matters
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
    stop(msg, call. = FALSE)
  }

  if (is.na(value)) {
    msg <- format_error(
      problem = paste0(param_name, " cannot be NA"),
      fix = "Provide a non-NA numeric value"
    )
    stop(msg, call. = FALSE)
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
    stop(msg, call. = FALSE)
  }

  # Recommended range warnings
  if (!is.null(recommended_min) && value < recommended_min) {
    warning(
      sprintf(
        "\u26A0 %s (%s) is below recommended minimum (%s)\n  %s",
        param_name,
        value,
        recommended_min,
        if (!is.null(context)) context else ""
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
        if (!is.null(context)) context else ""
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
#' @param tolerance_ppm Mass tolerance in parts per million
#' @param param_name Parameter name for error messages
#' @param context Additional context about usage
#'
#' @return Invisible TRUE if valid, stops/warns with messages otherwise
#' @keywords internal
validate_tolerance_ppm <- function(
  tolerance_ppm,
  param_name = "tolerance_ppm",
  context = NULL
) {
  if (!is.numeric(tolerance_ppm) || length(tolerance_ppm) != 1L) {
    msg <- format_error(
      problem = paste0("Invalid ", param_name),
      expected = "single numeric value (ppm)",
      received = class(tolerance_ppm)[1L],
      fix = "Provide a numeric tolerance value in parts per million (ppm)"
    )
    stop(msg, call. = FALSE)
  }

  if (tolerance_ppm <= 0) {
    msg <- format_error(
      problem = paste0(param_name, " must be positive"),
      received = paste0(tolerance_ppm, " ppm"),
      context = "Mass tolerance defines the matching window for annotation",
      fix = "Use a positive value appropriate for your instrument"
    )
    stop(msg, call. = FALSE)
  }

  # Provide instrument-specific guidance
  if (tolerance_ppm > MAX_TOLERANCE_PPM) {
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
        MAX_TOLERANCE_PPM,
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
#' @param file_list Named list of file paths to validate. Names are used
#'     in error messages to identify which file is missing.
#' @param allow_null Logical, if TRUE, NULL values are allowed (default: FALSE)
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
validate_file_existence <- function(file_list, allow_null = FALSE) {
  if (!is.list(file_list)) {
    stop("file_list must be a named list of file paths", call. = FALSE)
  }

  if (length(file_list) == 0L) {
    stop("file_list cannot be empty", call. = FALSE)
  }

  # Validate each file and collect results
  validation_results <- purrr::imap(
    .x = file_list,
    .f = .validate_single_file_path,
    allow_null = allow_null
  )

  # Filter and categorize results
  validation_results <- purrr::compact(validation_results)
  invalid_files <- purrr::map_chr(
    purrr::keep(validation_results, ~ .x$type == "invalid"),
    ~ .x$msg
  )
  missing_files <- purrr::map_chr(
    purrr::keep(validation_results, ~ .x$type == "missing"),
    ~ .x$msg
  )

  # Report errors
  errors <- c()
  if (length(invalid_files) > 0L) {
    errors <- c(
      errors,
      "Invalid file path types:",
      paste0("  - ", invalid_files)
    )
  }
  if (length(missing_files) > 0L) {
    errors <- c(
      errors,
      "Required file(s) not found:",
      paste0("  - ", missing_files)
    )
  }

  if (length(errors) > 0L) {
    stop(
      paste(
        c(
          errors,
          "",
          "Please verify file paths and ensure all required files are present."
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate MS ionization mode
#'
#' @description Ensures the MS mode is valid ('pos' or 'neg')
#'
#' @param ms_mode Character string indicating ionization mode
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_ms_mode("pos") # OK
#' validate_ms_mode("neg") # OK
#' validate_ms_mode("both") # Error with suggestion
#' }
validate_ms_mode <- function(ms_mode) {
  if (missing(ms_mode) || is.null(ms_mode)) {
    stop(
      format_error(
        problem = "ms_mode is required but was not provided",
        expected = "one of: 'pos', 'neg'",
        fix = "Specify ms_mode = 'pos' or ms_mode = 'neg' in your parameters"
      ),
      call. = FALSE
    )
  }

  validate_choice(
    value = ms_mode,
    valid_values = VALID_MS_MODES,
    param_name = "ms_mode",
    context = "Ionization mode for mass spectrometry analysis"
  )

  # log_trace("MS mode validated: ", ms_mode)
  invisible(TRUE)
}

#' Validate tolerance parameters
#'
#' @description Validates that tolerance values are within acceptable ranges
#'
#' @param tolerance_ppm Numeric mass tolerance in parts per million
#' @param tolerance_rt Numeric retention time tolerance in minutes
#' @param max_ppm Maximum allowed ppm tolerance (default: from constants)
#' @param max_rt Maximum allowed RT tolerance (default: from constants)
#' @param context Character string describing context (for error messages)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_tolerances(tolerance_ppm = 10, tolerance_rt = 0.05)
#' }
validate_tolerances <- function(
  tolerance_ppm = NULL,
  tolerance_rt = NULL,
  max_ppm = MAX_TOLERANCE_PPM,
  max_rt = MAX_TOLERANCE_RT_ADDUCTS,
  context = "annotation"
) {
  # Validate PPM tolerance
  if (!is.null(tolerance_ppm)) {
    validate_tolerance_ppm(
      tolerance_ppm = tolerance_ppm,
      param_name = "tolerance_ppm",
      context = context
    )
    # log_trace("Mass tolerance validated: ", tolerance_ppm, " ppm")
  }

  # Validate RT tolerance
  if (!is.null(tolerance_rt)) {
    validate_numeric_range(
      value = tolerance_rt,
      min_value = 0,
      max_value = Inf,
      param_name = "tolerance_rt",
      recommended_max = max_rt,
      context = paste0(
        "RT tolerance for ",
        context,
        ". ",
        "Large values may group unrelated features together"
      )
    )
    # log_trace("RT tolerance validated: ", tolerance_rt, " minutes")
  }

  invisible(TRUE)
}

#' Validate adduct list structure
#'
#' @description Ensures adduct list contains required mode and is properly formatted
#'
#' @param adducts_list List containing adduct definitions
#' @param ms_mode Required ionization mode ('pos' or 'neg')
#' @param list_name Name of the list (for error messages)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
validate_adduct_list <- function(
  adducts_list,
  ms_mode,
  list_name = "adducts_list"
) {
  if (!is.list(adducts_list)) {
    msg <- format_error(
      problem = paste0(list_name, " has wrong type"),
      expected = "list",
      received = class(adducts_list)[1L],
      context = "Adduct lists must be organized by MS mode (pos/neg)",
      fix = "Ensure the adduct configuration is a list with 'pos' and 'neg' entries"
    )
    stop(msg, call. = FALSE)
  }

  if (is.null(adducts_list[[ms_mode]])) {
    available_modes <- names(adducts_list)
    msg <- format_error(
      problem = paste0("Missing '", ms_mode, "' mode in ", list_name),
      expected = paste0("List entry for '", ms_mode, "' mode"),
      received = paste0(
        "Available modes: ",
        paste(available_modes, collapse = ", ")
      ),
      context = "Adduct lists must contain entries for the ionization mode being used",
      suggestion = if (ms_mode == "pos" && "neg" %in% available_modes) {
        "Did you mean to use 'neg' mode instead?"
      } else if (ms_mode == "neg" && "pos" %in% available_modes) {
        "Did you mean to use 'pos' mode instead?"
      } else {
        NULL
      },
      fix = paste0(
        "Add adduct definitions for '",
        ms_mode,
        "' mode to your configuration:\n",
        "  ",
        list_name,
        "$",
        ms_mode,
        " <- c('[M+H]+', '[M+Na]+', ...)"
      )
    )
    stop(msg, call. = FALSE)
  }

  if (length(adducts_list[[ms_mode]]) == 0L) {
    msg <- paste0(
      "⚠ ",
      list_name,
      " for '",
      ms_mode,
      "' mode is empty. ",
      "No ",
      list_name,
      " will be applied."
    )
    warning(msg, call. = FALSE)
    log_warn(msg)
  }

  invisible(TRUE)
}

#' Validate data frame structure
#'
#' @description Validates that input is a data frame and optionally checks
#'     for required columns and minimum row count
#'
#' @param df Input to validate
#' @param param_name Name of the parameter (for error messages)
#' @param required_cols Character vector of required column names
#' @param min_rows Minimum number of rows required (default: 0)
#' @param allow_empty Logical, whether empty data frames are allowed
#' @param optional_cols Optional character vector of expected optional columns
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_dataframe(df, param_name = "features", required_cols = c("feature_id", "mz"))
#' }
validate_dataframe <- function(
  df,
  param_name = "input",
  required_cols = NULL,
  min_rows = 0L,
  allow_empty = TRUE,
  optional_cols = NULL
) {
  validate_dataframe_structure(
    df = df,
    required_cols = required_cols,
    optional_cols = optional_cols,
    df_name = param_name,
    min_rows = if (allow_empty) 0L else max(1L, min_rows)
  )

  invisible(TRUE)
}

#' Validate numeric range
#'
#' @description Validates that a numeric value is within a specified range
#'
#' @param value Numeric value to validate
#' @param min_value Minimum allowed value (inclusive)
#' @param max_value Maximum allowed value (inclusive)
#' @param param_name Name of the parameter (for error messages)
#' @param allow_null Whether NULL values are allowed
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
#'

#' Validate weights
#'
#' @description Validates that a set of weights are non-negative and positive.
#'     Note: Weights do NOT need to sum to 1, as the weighting functions
#'     normalize them by dividing by their sum.
#'
#' @param weights Named list or vector of weights
#' @param param_name Name of the parameter (for error messages)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Weights can be any positive numbers - they will be normalized
#' validate_weights(list(spectral = 1, chemical = 2, biological = 3))
#' validate_weights(list(spectral = 0.5, biological = 0.5))
#' }
validate_weights <- function(weights, param_name = "weights") {
  if (!is.numeric(weights)) {
    stop(
      param_name,
      " must be numeric, got: ",
      class(weights)[1L],
      call. = FALSE
    )
  }

  # Check for NA values
  if (any(is.na(weights))) {
    stop(param_name, " cannot contain NA values", call. = FALSE)
  }

  # Check for negative weights
  if (any(weights < 0, na.rm = TRUE)) {
    negative_idx <- which(weights < 0)
    negative_names <- if (!is.null(names(weights))) {
      names(weights)[negative_idx]
    } else {
      paste0("position ", negative_idx)
    }
    stop(
      param_name,
      " must be non-negative. Negative weight(s): ",
      paste(negative_names, collapse = ", "),
      call. = FALSE
    )
  }

  # Check for all zeros (would cause division by zero)
  if (sum(weights) == 0) {
    stop(
      param_name,
      " cannot all be zero (would cause division by zero)",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate choice parameter
#'
#' @description Validates that a value is one of a set of allowed choices
#'
#' @param value Value to validate
#' @param choices Character vector of allowed choices
#' @param param_name Name of the parameter (for error messages)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_choice("OR", c("OR", "AND"), param_name = "condition")
#' }

#' Validate character parameter
#'
#' @description Generic validator for character string parameters
#'
#' @param value Character value to validate
#' @param allowed_values Optional character vector of allowed values
#' @param param_name Name of the parameter (for error messages)
#' @param allow_null Allow NULL values (default: FALSE)
#' @param allow_empty Allow empty strings (default: FALSE)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
validate_character <- function(
  value,
  allowed_values = NULL,
  param_name = "value",
  allow_null = FALSE,
  allow_empty = FALSE
) {
  if (is.null(value)) {
    if (!allow_null) {
      stop(
        format_error(
          problem = paste0(param_name, " cannot be NULL"),
          fix = "Provide a non-NULL character string"
        ),
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }

  if (!is.character(value) || length(value) != 1L) {
    stop(
      format_error(
        problem = paste0("Invalid type for ", param_name),
        expected = "single character string",
        received = class(value)[1L],
        fix = "Ensure the parameter is a length-1 character value"
      ),
      call. = FALSE
    )
  }

  if (!allow_empty && nchar(value) == 0L) {
    stop(
      format_error(
        problem = paste0(param_name, " cannot be an empty string"),
        fix = "Provide a non-empty string"
      ),
      call. = FALSE
    )
  }

  if (!is.null(allowed_values) && !value %in% allowed_values) {
    # Use the validate_choice for this case
    validate_choice(
      value = value,
      valid_values = allowed_values,
      param_name = param_name
    )
  }

  invisible(TRUE)
}

#' Validate logical parameter
#'
#' @description Generic validator for logical (boolean) parameters
#'
#' @param value Logical value to validate
#' @param param_name Name of the parameter (for error messages)
#' @param allow_null Allow NULL values (default: FALSE)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
validate_logical <- function(value, param_name = "value", allow_null = FALSE) {
  if (is.null(value)) {
    if (!allow_null) {
      stop(param_name, " cannot be NULL", call. = FALSE)
    }
    return(invisible(TRUE))
  }

  if (!is.logical(value) || length(value) != 1L) {
    stop(
      param_name,
      " must be a single logical value (TRUE/FALSE), got: ",
      class(value)[1L],
      call. = FALSE
    )
  }

  if (is.na(value)) {
    stop(param_name, " cannot be NA", call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate list or vector parameter
#'
#' @description Validates that a parameter is either a list or vector
#'
#' @param value Value to validate
#' @param min_length Minimum required length
#' @param max_length Maximum allowed length (NULL = no limit)
#' @param param_name Name of the parameter (for error messages)
#' @param allow_null Allow NULL values (default: FALSE)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
validate_list_or_vector <- function(
  value,
  min_length = 0L,
  max_length = NULL,
  param_name = "value",
  allow_null = FALSE
) {
  if (is.null(value)) {
    if (!allow_null) {
      stop(param_name, " cannot be NULL", call. = FALSE)
    }
    return(invisible(TRUE))
  }

  if (!is.list(value) && !is.vector(value)) {
    stop(
      param_name,
      " must be a list or vector, got: ",
      class(value)[1L],
      call. = FALSE
    )
  }

  value_length <- length(value)

  if (value_length < min_length) {
    stop(
      param_name,
      " must have at least ",
      min_length,
      " element(s), got: ",
      value_length,
      call. = FALSE
    )
  }

  if (!is.null(max_length) && value_length > max_length) {
    stop(
      param_name,
      " cannot have more than ",
      max_length,
      " element(s), got: ",
      value_length,
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Assert that a value is a valid logical flag
#'
#' @description Validates that a parameter is a single non-NA logical value.
#'     This is a strict version of validate_logical.
#'
#' @param x Value to check
#' @param arg_name Name of the argument (for error messages)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
assert_flag <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(
      arg_name,
      " must be a single TRUE or FALSE value, got: ",
      if (!is.logical(x)) {
        class(x)[1L]
      } else if (length(x) != 1L) {
        paste0("logical vector of length ", length(x))
      } else {
        "NA"
      },
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Assert that a value is a positive integer
#'
#' @description Validates that a parameter is a single positive integer.
#'
#' @param x Value to check
#' @param arg_name Name of the argument (for error messages)
#' @param allow_zero Whether zero is allowed (default: FALSE)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
assert_positive_integer <- function(
  x,
  arg_name = deparse(substitute(x)),
  allow_zero = FALSE
) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    stop(
      arg_name,
      " must be a single numeric value, got: ",
      class(x)[1L],
      call. = FALSE
    )
  }

  # Check if it's a whole number
  if (x != as.integer(x)) {
    stop(
      arg_name,
      " must be an integer, got: ",
      x,
      call. = FALSE
    )
  }

  min_val <- if (allow_zero) {
    0L
  } else {
    1L
  }

  if (x < min_val) {
    stop(
      arg_name,
      " must be ",
      if (allow_zero) {
        ">= 0"
      } else {
        "> 0"
      },
      ", got: ",
      x,
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# Generic assertion helpers (centralized) ----

assert_choice <- function(value, choices, param_name = "value") {
  if (length(value) != 1L || !is.character(value)) {
    stop(param_name, " must be a single character string", call. = FALSE)
  }
  if (!value %in% choices) {
    stop(
      "Invalid ",
      param_name,
      ": '",
      value,
      "'. Must be one of: ",
      paste(choices, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Assert scalar numeric value
#'
#' @description Validates that a parameter is a single numeric value within range
#'
#' @param x Value to validate
#' @param param_name Name of the parameter (for error messages)
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#' @param allow_na Whether NA is allowed
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
assert_scalar_numeric <- function(
  x,
  param_name,
  min = -Inf,
  max = Inf,
  allow_na = FALSE
) {
  if (!is.numeric(x) || length(x) != 1L) {
    stop(param_name, " must be a single numeric value", call. = FALSE)
  }
  if (is.na(x) && !allow_na) {
    stop(param_name, " cannot be NA", call. = FALSE)
  }
  if (!is.na(x) && (x < min || x > max)) {
    stop(
      param_name,
      " must be between ",
      min,
      " and ",
      max,
      ", got: ",
      x,
      call. = FALSE
    )
  }
  invisible(TRUE)
}
