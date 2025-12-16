#' @title Utility predicates for validation
#'
#' @description Collection of reusable predicate functions for common
#'     validation patterns. These predicates follow SOLID principles and
#'     can be easily composed and tested.
#'
#' @name predicates_utils
#' @keywords internal
NULL

#' Check if value is a single character string
#'
#' @description Validates that a value is a character vector of length 1.
#'     Common validation pattern extracted to eliminate duplication.
#'
#' @param x Value to check
#'
#' @return Logical TRUE if x is a single non-NA character string, FALSE otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' is_single_string("hello") # TRUE
#' is_single_string(c("a", "b")) # FALSE
#' is_single_string(123) # FALSE
#' is_single_string(NA_character_) # FALSE
#' is_single_string(character(0)) # FALSE
#' }
is_single_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

#' Check if all elements in a list are single character strings
#'
#' @description Vectorized check for lists of parameters that should all be
#'     single character strings. More efficient than repeated individual checks.
#'
#' @param x_list Named list of values to check
#'
#' @return Named logical vector indicating which elements are single strings
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' params <- list(
#'   file1 = "path.txt",
#'   file2 = c("a", "b"),
#'   file3 = "another.txt"
#' )
#' all_single_strings(params)
#' # file1: TRUE, file2: FALSE, file3: TRUE
#' }
all_single_strings <- function(x_list) {
  vapply(
    X = x_list,
    FUN = is_single_string,
    FUN.VALUE = logical(1L),
    USE.NAMES = TRUE
  )
}

#' Validate that all list elements are single character strings
#'
#' @description Validates and reports which parameters failed the single string
#'     check. Stops execution with informative error if any validation fails.
#'
#' @param x_list Named list of values to validate
#' @param error_prefix Character string to prefix error message
#'
#' @return NULL (invisible) if validation passes, stops with error otherwise
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' params <- list(str_stereo = "file.txt", str_met = "meta.txt")
#' validate_all_single_strings(params, "Structure files")
#' }
validate_all_single_strings <- function(
  x_list,
  error_prefix = "Parameter(s)"
) {
  if (!is.list(x_list) || is.null(names(x_list))) {
    stop("x_list must be a named list", call. = FALSE)
  }

  checks <- all_single_strings(x_list)

  if (!all(checks)) {
    invalid_params <- names(x_list)[!checks]
    stop(
      error_prefix,
      " must be single character strings: ",
      paste(invalid_params, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Count elements meeting a condition across a list/vector
#'
#' @description Generic counter applying a predicate function to count how many
#'     elements satisfy the condition. More flexible than specific counters.
#'
#' @param x List or vector to process
#' @param predicate_fn Function taking one element and returning logical
#' @param ... Additional arguments passed to predicate_fn
#'
#' @return Integer count of elements for which predicate_fn returns TRUE
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' count_if(c(1, 2, 3, 4), function(x) x > 2)
#' # 2
#' count_if(list("a", 1, "b"), is.character)
#' # 2
#' }
count_if <- function(x, predicate_fn, ...) {
  sum(vapply(X = x, FUN = predicate_fn, FUN.VALUE = logical(1L), ...))
}
