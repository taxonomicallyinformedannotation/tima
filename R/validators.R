#' @title Input Validation Utilities
#'
#' @description This module provides reusable validation functions for
#'     input parameters throughout the TIMA package. Centralizing validation
#'     logic ensures consistency, reduces code duplication, and improves
#'     error messages.
#'
#' @include constants.R
#'
#' @name validators
#' @keywords internal

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
    stop("file_list must be a named list of file paths")
  }

  if (length(file_list) == 0L) {
    stop("file_list cannot be empty")
  }

  missing_files <- list()

  for (file_name in names(file_list)) {
    file_path <- file_list[[file_name]]

    # Handle NULL values
    if (is.null(file_path)) {
      if (!allow_null) {
        missing_files[[file_name]] <- "NULL (file path is NULL)"
      }
      next
    }

    # Validate type
    if (!is.character(file_path) || length(file_path) != 1L) {
      stop(
        "File path for '",
        file_name,
        "' must be a single character string, ",
        "got: ",
        class(file_path)[1L]
      )
    }

    # Check existence
    if (!file.exists(file_path)) {
      missing_files[[file_name]] <- file_path
    }
  }

  # Report missing files
  if (length(missing_files) > 0L) {
    error_msg <- paste0(
      "The following required file(s) were not found:\n",
      paste0(
        "  - ",
        names(missing_files),
        ": ",
        unlist(missing_files),
        collapse = "\n"
      ),
      "\n\nPlease verify file paths and ensure all required files are present."
    )
    stop(error_msg)
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
#' validate_ms_mode("both") # Error
#' }
validate_ms_mode <- function(ms_mode) {
  if (missing(ms_mode) || is.null(ms_mode)) {
    stop("ms_mode must be provided")
  }

  if (!is.character(ms_mode) || length(ms_mode) != 1L) {
    stop("ms_mode must be a single character string, got: ", class(ms_mode)[1L])
  }

  if (!ms_mode %in% VALID_MS_MODES) {
    stop(
      "Invalid ms_mode: '",
      ms_mode,
      "'. ",
      "Must be one of: ",
      paste(VALID_MS_MODES, collapse = ", "),
      ".\n",
      "Please check your configuration and ensure polarity is correctly specified."
    )
  }

  # logger::log_trace("MS mode validated: ", ms_mode)
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
    if (!is.numeric(tolerance_ppm) || length(tolerance_ppm) != 1L) {
      stop(
        "tolerance_ppm must be a single numeric value, got: ",
        class(tolerance_ppm)[1L]
      )
    }

    if (tolerance_ppm <= 0) {
      stop(
        "tolerance_ppm must be positive, got: ",
        tolerance_ppm,
        ".\n",
        "Recommended range: 1-",
        max_ppm,
        " ppm for ",
        context
      )
    }

    if (tolerance_ppm > max_ppm) {
      msg <- paste0(
        "tolerance_ppm (",
        tolerance_ppm,
        ") exceeds recommended maximum (",
        max_ppm,
        " ppm). This may result in excessive false positives."
      )
      warning(msg, call. = FALSE)
      logger::log_warn(msg)
    }

    # logger::log_trace("Mass tolerance validated: ", tolerance_ppm, " ppm")
  }

  # Validate RT tolerance
  if (!is.null(tolerance_rt)) {
    if (!is.numeric(tolerance_rt) || length(tolerance_rt) != 1L) {
      stop(
        "tolerance_rt must be a single numeric value, got: ",
        class(tolerance_rt)[1L]
      )
    }

    if (tolerance_rt <= 0) {
      stop(
        "tolerance_rt must be positive, got: ",
        tolerance_rt,
        " minutes.\n",
        "Recommended range: 0.01-",
        max_rt,
        " minutes for ",
        context
      )
    }

    if (tolerance_rt > max_rt) {
      msg <- paste0(
        "tolerance_rt (",
        tolerance_rt,
        " min) exceeds recommended maximum (",
        max_rt,
        " min). This may group unrelated features."
      )
      warning(msg, call. = FALSE)
      logger::log_warn(msg)
    }

    # logger::log_trace("RT tolerance validated: ", tolerance_rt, " minutes")
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
    stop(list_name, " must be a list, got: ", class(adducts_list)[1L])
  }

  if (is.null(adducts_list[[ms_mode]])) {
    stop(
      list_name,
      " must contain '",
      ms_mode,
      "' mode entries.\n",
      "Available modes: ",
      paste(names(adducts_list), collapse = ", "),
      ".\n",
      "Please ensure your configuration includes adducts for the specified polarity."
    )
  }

  if (length(adducts_list[[ms_mode]]) == 0L) {
    msg <- paste0(
      list_name,
      " for '",
      ms_mode,
      "' mode is empty. ",
      "No ",
      list_name,
      " will be applied."
    )
    warning(msg, call. = FALSE)
    logger::log_warn(msg)
  }

  # logger::log_trace(
  #  list_name,
  # " validated: ",
  # length(adducts_list[[ms_mode]]),
  # " entries for ",
  # ms_mode,
  # " mode"
  # )

  invisible(TRUE)
}

#' Validate data frame structure
#'
#' @description Validates that input is a data frame and optionally checks
#'     for required columns and minimum row count
#'
#' @param df Input to validate
#' @param name Name of the parameter (for error messages)
#' @param required_cols Character vector of required column names
#' @param min_rows Minimum number of rows required (default: 0)
#' @param allow_empty Logical, whether empty data frames are allowed
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_dataframe(df, "features", required_cols = c("feature_id", "mz"))
#' }
validate_dataframe <- function(
  df,
  name = "input",
  required_cols = NULL,
  min_rows = 0L,
  allow_empty = TRUE
) {
  # Check if data frame or tibble
  if (!is.data.frame(df) && !inherits(df, "tbl")) {
    stop(
      name,
      " must be a data frame or tibble, got: ",
      paste(class(df), collapse = ", ")
    )
  }

  # Check row count
  n_rows <- nrow(df)

  if (!allow_empty && n_rows == 0L) {
    stop(name, " cannot be empty")
  }

  if (n_rows < min_rows) {
    stop(
      name,
      " must have at least ",
      min_rows,
      " row(s), got: ",
      n_rows
    )
  }

  # Check required columns
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, colnames(df))

    if (length(missing_cols) > 0L) {
      stop(
        name,
        " is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        "\nAvailable columns: ",
        paste(colnames(df), collapse = ", ")
      )
    }
  }

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
#' @examples
#' \dontrun{
#' validate_numeric_range(0.5, min_value = 0, max_value = 1, param_name = "threshold")
#' }
validate_numeric_range <- function(
  value,
  min_value = -Inf,
  max_value = Inf,
  param_name = "value",
  allow_null = FALSE
) {
  if (is.null(value)) {
    if (!allow_null) {
      stop(param_name, " cannot be NULL")
    }
    return(invisible(TRUE))
  }

  if (!is.numeric(value) || length(value) != 1L) {
    stop(
      param_name,
      " must be a single numeric value, got: ",
      if (is.numeric(value)) {
        paste0("vector of length ", length(value))
      } else {
        class(value)[1L]
      }
    )
  }

  if (is.na(value)) {
    stop(param_name, " cannot be NA")
  }

  if (value < min_value || value > max_value) {
    stop(
      param_name,
      " must be between ",
      min_value,
      " and ",
      max_value,
      ", got: ",
      value
    )
  }

  invisible(TRUE)
}

#' Validate weights sum to 1
#'
#' @description Validates that a set of weights are non-negative and sum to 1
#'
#' @param weights Named list or vector of weights
#' @param tolerance Numeric tolerance for sum check (default: 1e-6)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_weights(list(spectral = 0.5, chemical = 0.3, biological = 0.2))
#' }
validate_weights <- function(weights, tolerance = 1e-6) {
  if (!is.numeric(weights)) {
    stop("Weights must be numeric, got: ", class(weights)[1L])
  }

  # Check for negative weights
  if (any(weights < 0, na.rm = TRUE)) {
    negative_weights <- which(weights < 0)
    stop(
      "All weights must be non-negative. Negative weight(s) at position(s): ",
      paste(negative_weights, collapse = ", ")
    )
  }

  # Check for NA values
  if (any(is.na(weights))) {
    stop("Weights cannot contain NA values")
  }

  # Check sum
  weight_sum <- sum(weights)

  if (abs(weight_sum - 1.0) > tolerance) {
    stop(
      "Weights must sum to 1.0, got: ",
      round(weight_sum, 6),
      "\nWeights: ",
      paste(names(weights), "=", round(weights, 3), collapse = ", ")
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
#' @param name Name of the parameter (for error messages)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_choice("OR", c("OR", "AND"), "condition")
#' }
validate_choice <- function(value, choices, name = "value") {
  if (!is.character(value) || length(value) != 1L) {
    stop(
      name,
      " must be a single character string, got: ",
      class(value)[1L]
    )
  }

  if (!value %in% choices) {
    stop(
      name,
      " must be one of: ",
      paste(choices, collapse = ", "),
      "\nGot: '",
      value,
      "'"
    )
  }

  invisible(TRUE)
}

#' Format a standardized error message
#'
#' @description Helper to format consistent error messages containing
#'     what went wrong, why, and how to fix.
#'
#' @param what Short description of what went wrong
#' @param why  Optional explanation of why it is a problem
#' @param how  Optional guidance on how to fix
#'
#' @keywords internal
format_error <- function(what, why = NULL, how = NULL) {
  parts <- c(what, why, how)
  paste(stats::na.omit(object = parts), collapse = "\n")
}

#' Stop with standardized error formatting
#' @keywords internal
stopf <- function(what, why = NULL, how = NULL) {
  stop(format_error(what, why, how))
}

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
      stopf(
        what = paste0(param_name, " cannot be NULL"),
        how = "Provide a non-NULL character string."
      )
    }
    return(invisible(TRUE))
  }

  if (!is.character(value) || length(value) != 1L) {
    stopf(
      what = paste0(
        param_name,
        " must be a single character string, got: ",
        class(value)[1L]
      ),
      how = "Ensure the parameter is a length-1 character value."
    )
  }

  if (!allow_empty && nchar(value) == 0L) {
    stopf(
      what = paste0(param_name, " cannot be an empty string"),
      how = "Provide a non-empty string."
    )
  }

  if (!is.null(allowed_values) && !value %in% allowed_values) {
    stopf(
      what = paste0(
        param_name,
        " (",
        value,
        ") must be one of: ",
        paste(allowed_values, collapse = ", ")
      ),
      how = paste0("Choose one of: ", paste(allowed_values, collapse = ", "))
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
      stop(param_name, " cannot be NULL")
    }
    return(invisible(TRUE))
  }

  if (!is.logical(value) || length(value) != 1L) {
    stop(
      param_name,
      " must be a single logical value (TRUE/FALSE), got: ",
      class(value)[1L]
    )
  }

  if (is.na(value)) {
    stop(param_name, " cannot be NA")
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
      stop(param_name, " cannot be NULL")
    }
    return(invisible(TRUE))
  }

  if (!is.list(value) && !is.vector(value)) {
    stop(
      param_name,
      " must be a list or vector, got: ",
      class(value)[1L]
    )
  }

  value_length <- length(value)

  if (value_length < min_length) {
    stop(
      param_name,
      " must have at least ",
      min_length,
      " element(s), got: ",
      value_length
    )
  }

  if (!is.null(max_length) && value_length > max_length) {
    stop(
      param_name,
      " cannot have more than ",
      max_length,
      " element(s), got: ",
      value_length
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

  min_val <- if (allow_zero) 0L else 1L

  if (x < min_val) {
    stop(
      arg_name,
      " must be ",
      if (allow_zero) ">= 0" else "> 0",
      ", got: ",
      x,
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate data frame parameter
#'
#' @description Validates data frame structure and required columns
#'
#' @param df Data frame to validate
#' @param required_columns Character vector of required column names
#' @param min_rows Minimum required number of rows
#' @param param_name Name of the parameter (for error messages)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
validate_data_frame <- function(
  df,
  required_columns = NULL,
  min_rows = 0L,
  param_name = "data frame"
) {
  if (!is.data.frame(df)) {
    stop(param_name, " must be a data frame, got: ", class(df)[1L])
  }

  if (nrow(df) < min_rows) {
    stop(
      param_name,
      " must have at least ",
      min_rows,
      " row(s), got: ",
      nrow(df)
    )
  }

  if (!is.null(required_columns)) {
    missing_cols <- setdiff(required_columns, names(df))
    if (length(missing_cols) > 0L) {
      stop(
        param_name,
        " is missing required column(s): ",
        paste(missing_cols, collapse = ", "),
        ".\n",
        "Available columns: ",
        paste(names(df), collapse = ", ")
      )
    }
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

assert_flag <- function(x, param_name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(param_name, " must be logical (TRUE/FALSE)", call. = FALSE)
  }
  invisible(TRUE)
}

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

assert_positive_integer <- function(x, param_name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || (x %% 1) != 0) {
    stop(param_name, " must be a positive integer (>=1)", call. = FALSE)
  }
  invisible(TRUE)
}

# Weight tolerance constant moved here ----
WEIGHT_SUM_TOLERANCE <- 0.01
