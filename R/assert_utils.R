#' Assert that a value is a valid logical flag
#'
#' @description Validates that a parameter is a single non-NA logical value.
#'     This is a strict version of validate_logical.
#'
#' @param x [logical] Value to check
#' @param arg_name [character] Name of the argument (for error messages)
#'
#' @return Invisible TRUE if valid, stops with error otherwise
#'
#' @keywords internal
assert_flag <- function(x, arg_name = deparse(substitute(x))) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    tima_abort(
      problem = paste0(
        arg_name,
        " must be a single TRUE or FALSE value, got: ",
        if (!is.logical(x)) {
          class(x)[1L]
        } else if (length(x) != 1L) {
          paste0("logical vector of length ", length(x))
        } else {
          "NA"
        }
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }
  invisible(TRUE)
}

#' Assert that a value is a positive integer
#'
#' @description Validates that a parameter is a single positive integer.
#'
#' @param x [numeric] Value to check
#' @param arg_name [character] Name of the argument (for error messages)
#' @param allow_zero [logical] Whether zero is allowed (default: FALSE)
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
    tima_abort(
      problem = paste0(
        arg_name,
        " must be a single numeric value, got: ",
        class(x)[1L]
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  # Check if it's a whole number
  if (x != as.integer(x)) {
    tima_abort(
      problem = paste0(arg_name, " must be an integer, got: ", x),
      class = c("tima_validation_error", "tima_error")
    )
  }

  min_val <- if (allow_zero) {
    0L
  } else {
    1L
  }

  if (x < min_val) {
    tima_abort(
      problem = paste0(
        arg_name,
        " must be ",
        if (allow_zero) {
          ">= 0"
        } else {
          "> 0"
        },
        ", got: ",
        x
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }

  invisible(TRUE)
}

# Generic assertion helpers (centralized) ----

assert_choice <- function(value, choices, param_name = "value") {
  if (length(value) != 1L || !is.character(value)) {
    tima_abort(
      problem = paste0(param_name, " must be a single character string"),
      class = c("tima_validation_error", "tima_error")
    )
  }
  if (!value %in% choices) {
    tima_abort(
      problem = paste0(
        "Invalid ",
        param_name,
        ": '",
        value,
        "'. Must be one of: ",
        paste(choices, collapse = ", ")
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }
  invisible(TRUE)
}

#' Assert scalar numeric value
#'
#' @description Validates that a parameter is a single numeric value within
#'     range
#'
#' @param x [numeric] Value to validate
#' @param param_name [character] Name of the parameter (for error messages)
#' @param min [numeric] Minimum allowed value
#' @param max [numeric] Maximum allowed value
#' @param allow_na [logical] Whether NA is allowed
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
    tima_abort(
      problem = paste0(param_name, " must be a single numeric value"),
      class = c("tima_validation_error", "tima_error")
    )
  }
  if (is.na(x) && !allow_na) {
    tima_abort(
      problem = paste0(param_name, " cannot be NA"),
      class = c("tima_validation_error", "tima_error")
    )
  }
  if (!is.na(x) && (x < min || x > max)) {
    tima_abort(
      problem = paste0(
        param_name,
        " must be between ",
        min,
        " and ",
        max,
        ", got: ",
        x
      ),
      class = c("tima_validation_error", "tima_error")
    )
  }
  invisible(TRUE)
}
