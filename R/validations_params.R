validate_file_existence <- function(file_list, allow_null = FALSE) {
  if (!is.list(file_list)) {
    tima_abort(
      problem = "file_list must be a named list of file paths",
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (length(file_list) == 0L) {
    tima_abort(
      problem = "file_list cannot be empty",
      class = c("tima_validation_error", "tima_error")
    )
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
    tima_abort_message(
      message = paste(
        c(
          errors,
          "",
          "Please verify file paths and ensure all required files are present."
        ),
        collapse = "\n"
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  invisible(TRUE)
}

#' Validate MS ionization mode
#'
#' @description Ensures the MS mode is valid ('pos' or 'neg')
#'
#' @param ms_mode [character] Character string indicating ionization mode
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
    tima_abort(
      problem = "ms_mode is required but was not provided",
      expected = "one of: 'pos', 'neg'",
      fix = "Specify ms_mode = 'pos' or ms_mode = 'neg' in your parameters",
      class = c("tima_validation_error", "tima_error")
    )
  }

  validate_choice(
    value = ms_mode,
    valid_values = VALID_MS_MODES,
    param_name = "ms_mode",
    context = "Ionization mode for mass spectrometry analysis"
  )

  invisible(TRUE)
}

#' Validate tolerance parameters
#'
#' @description Validates that tolerance values are within acceptable ranges
#'
#' @param tolerance_ppm [numeric] Numeric mass tolerance in parts per million
#' @param tolerance_rt [numeric] Numeric retention time tolerance in minutes
#' @param max_ppm [numeric] Maximum allowed ppm tolerance (default: from
#'     constants)
#' @param max_rt [numeric] Maximum allowed RT tolerance (default: from
#'     constants)
#' @param context [character] Character string describing context (for error
#'     messages)
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
      context = context,
      max_ppm = max_ppm
    )
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
  }

  invisible(TRUE)
}

#' Validate adduct list structure
#'
#' @description Ensures adduct list contains required mode and is properly
#'     formatted
#'
#' @param adducts_list [list] List containing adduct definitions
#' @param ms_mode [character] Required ionization mode ('pos' or 'neg')
#' @param list_name [character] Name of the list (for error messages)
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
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Structured adduct schema is valid too:
  #   adducts: { M, charge_carriers, charges, ... }
  # and can be consumed by build_adduct_universe().
  is_structured_adduct_schema <-
    identical(list_name, "adducts_list") &&
    any(c("M", "charge_carriers", "charges") %in% names(adducts_list))
  if (is_structured_adduct_schema) {
    carriers <- adducts_list$charge_carriers
    charges <- adducts_list$charges
    if (!is.list(carriers) || is.null(carriers[[ms_mode]])) {
      msg <- format_error(
        problem = paste0(
          "Missing '",
          ms_mode,
          "' mode in ",
          list_name,
          "$charge_carriers"
        ),
        expected = paste0("List entry for '", ms_mode, "' mode"),
        received = paste0(
          "Available modes: ",
          paste(names(carriers), collapse = ", ")
        ),
        context = "Structured adduct schema requires mode-specific charge carriers",
        fix = paste0(
          "Add charge carriers for '",
          ms_mode,
          "' mode under ",
          list_name,
          "$charge_carriers"
        )
      )
      tima_abort_message(
        msg,
        class = c("tima_validation_error", "tima_error")
      )
    }
    if (!is.list(charges) || is.null(charges[[ms_mode]])) {
      msg <- format_error(
        problem = paste0(
          "Missing '",
          ms_mode,
          "' mode in ",
          list_name,
          "$charges"
        ),
        expected = paste0("List entry for '", ms_mode, "' mode"),
        received = paste0(
          "Available modes: ",
          paste(names(charges), collapse = ", ")
        ),
        context = "Structured adduct schema requires mode-specific charges",
        fix = paste0(
          "Add charges for '",
          ms_mode,
          "' mode under ",
          list_name,
          "$charges"
        )
      )
      tima_abort_message(
        msg,
        class = c("tima_validation_error", "tima_error")
      )
    }
    if (length(carriers[[ms_mode]]) == 0L || length(charges[[ms_mode]]) == 0L) {
      msg <- paste0(
        "[WARNING] ",
        list_name,
        " structured schema has empty charge_carriers/charges for '",
        ms_mode,
        "' mode."
      )
      warning(msg, call. = FALSE)
      log_warn(msg)
    }
    return(invisible(TRUE))
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
    tima_abort_message(
      msg,
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (length(adducts_list[[ms_mode]]) == 0L) {
    msg <- paste0(
      "[WARNING] ",
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
#' @param param_name [character] Name of the parameter (for error messages)
#' @param required_cols [character] Character vector of required column names
#' @param min_rows [integer] Minimum number of rows required (default: 0)
#' @param allow_empty [logical] Logical, whether empty data frames are allowed
#' @param optional_cols [character] Optional character vector of expected
#'     optional columns
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' validate_dataframe(df, param_name = "features", required_cols =
#'     c("feature_id", "mz"))
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

#' Validate weights
#'
#' @description Validates that a set of weights are non-negative and positive.
#'     Note: Weights do NOT need to sum to 1, as the weighting functions
#'     normalize them by dividing by their sum.
#'
#' @param weights [numeric] Named list or vector of weights
#' @param param_name [character] Name of the parameter (for error messages)
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
    tima_abort(
      problem = paste0(
        param_name,
        " must be numeric, got: ",
        class(weights)[1L]
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Check for NA values
  if (anyNA(weights)) {
    tima_abort(
      problem = paste0(param_name, " cannot contain NA values"),
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Check for negative weights
  if (any(weights < 0, na.rm = TRUE)) {
    negative_idx <- which(weights < 0)
    negative_names <- if (!is.null(names(weights))) {
      names(weights)[negative_idx]
    } else {
      paste0("position ", negative_idx)
    }
    tima_abort(
      problem = paste0(
        param_name,
        " must be non-negative. Negative weight(s): ",
        paste(negative_names, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Check for all zeros (would cause division by zero)
  if (sum(weights) == 0) {
    tima_abort(
      problem = paste0(
        param_name,
        " cannot all be zero (would cause division by zero)"
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  invisible(TRUE)
}


#' Validate character parameter
#'
#' @description Generic validator for character string parameters
#'
#' @param value [character] Character value to validate
#' @param allowed_values [character] Optional character vector of allowed values
#' @param param_name [character] Name of the parameter (for error messages)
#' @param allow_null [logical] Allow NULL values (default: FALSE)
#' @param allow_empty [logical] Allow empty strings (default: FALSE)
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
      tima_abort(
        problem = paste0(param_name, " cannot be NULL"),
        fix = "Provide a non-NULL character string"
      )
    }
    return(invisible(TRUE))
  }

  if (!is.character(value) || length(value) != 1L) {
    tima_abort(
      problem = paste0("Invalid type for ", param_name),
      expected = "single character string",
      received = class(value)[1L],
      fix = "Ensure the parameter is a length-1 character value"
    )
  }

  if (!allow_empty && nchar(value) == 0L) {
    tima_abort(
      problem = paste0(param_name, " cannot be an empty string"),
      fix = "Provide a non-empty string"
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
#' @param value [logical] Logical value to validate
#' @param param_name [character] Name of the parameter (for error messages)
#' @param allow_null [logical] Allow NULL values (default: FALSE)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
validate_logical <- function(value, param_name = "value", allow_null = FALSE) {
  if (is.null(value)) {
    if (!allow_null) {
      tima_abort(
        problem = paste0(param_name, " cannot be NULL"),
        class = c("tima_validation_error", "tima_error")
      )
    }
    return(invisible(TRUE))
  }

  if (!is.logical(value) || length(value) != 1L) {
    tima_abort(
      problem = paste0(
        param_name,
        " must be a single logical value (TRUE/FALSE), got: ",
        class(value)[1L]
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (is.na(value)) {
    tima_abort(
      problem = paste0(param_name, " cannot be NA"),
      class = c("tima_validation_error", "tima_error")
    )
  }

  invisible(TRUE)
}

#' Validate list or vector parameter
#'
#' @description Validates that a parameter is either a list or vector
#'
#' @param value Value to validate
#' @param min_length [integer] Minimum required length
#' @param max_length [integer] Maximum allowed length (NULL = no limit)
#' @param param_name [character] Name of the parameter (for error messages)
#' @param allow_null [logical] Allow NULL values (default: FALSE)
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
      tima_abort(
        problem = paste0(param_name, " cannot be NULL"),
        class = c("tima_validation_error", "tima_error")
      )
    }
    return(invisible(TRUE))
  }

  if (!is.list(value) && !is.vector(value)) {
    tima_abort(
      problem = paste0(
        param_name,
        " must be a list or vector, got: ",
        class(value)[1L]
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  value_length <- length(value)

  if (value_length < min_length) {
    tima_abort(
      problem = paste0(
        param_name,
        " must have at least ",
        min_length,
        " element(s), got: ",
        value_length
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (!is.null(max_length) && value_length > max_length) {
    tima_abort(
      problem = paste0(
        param_name,
        " cannot have more than ",
        max_length,
        " element(s), got: ",
        value_length
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  invisible(TRUE)
}
