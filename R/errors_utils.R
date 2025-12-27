#' Error Message Formatting
#'
#' @description Helper functions for creating user-friendly error messages
#'     following TIMA's error message philosophy: tell what went wrong,
#'     what was expected, where it happened, and suggest how to fix it.
#'
#' @name errors_utils
#' @keywords internal
NULL

#' Calculate Levenshtein distance for fuzzy matching
#'
#' @description Internal helper for finding typos and suggesting corrections
#'
#' @param value User-provided value to check
#' @param valid_values Vector of valid values to match against
#' @param max_distance Maximum edit distance to consider (default: 5)
#'
#' @return Character string of closest match, or NULL if no close match
#' @keywords internal
.fuzzy_match <- function(value, valid_values, max_distance = 5) {
  if (!is.character(value) || !is.character(valid_values)) {
    return(NULL)
  }

  distances <- utils::adist(value, valid_values)[1, ]
  closest_idx <- which.min(distances)

  # Be more lenient for short target strings
  # e.g., "positive" -> "pos" has distance 5, which is acceptable
  max_allowed <- max(
    max_distance,
    min(distances[closest_idx], nchar(valid_values[closest_idx]) + 2)
  )

  if (distances[closest_idx] <= max_allowed) {
    return(valid_values[closest_idx])
  }

  NULL
}

#' Format error message
#'
#' @description Creates user-friendly error messages with context and suggestions.
#'     This is the core error formatting function used throughout TIMA.
#'
#' @param problem Main problem statement (required)
#' @param expected What was expected
#' @param received What was actually received
#' @param location Where the problem occurred (e.g., parameter name, file path)
#' @param suggestion Specific suggestion to fix (e.g., "Did you mean 'pos'?")
#' @param fix Actionable fix steps
#' @param context Additional context about why this matters
#'
#' @return Formatted error message string
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' format_error(
#'   problem = "Invalid ms_mode",
#'   expected = "one of: 'pos', 'neg'",
#'   received = "'positive'",
#'   suggestion = "Did you mean 'pos'?",
#'   fix = "Use ms_mode = 'pos' or ms_mode = 'neg'"
#' )
#' }
format_error <- function(
  problem,
  expected = NULL,
  received = NULL,
  location = NULL,
  suggestion = NULL,
  fix = NULL,
  context = NULL
) {
  parts <- c(
    paste0("\u2717 ", problem), # ✗ symbol
    "",
    if (!is.null(location)) paste0("Location: ", location),
    if (!is.null(expected)) paste0("Expected: ", expected),
    if (!is.null(received)) paste0("Received: ", received),
    if (!is.null(context)) paste0("\nReason: ", context),
    if (!is.null(suggestion)) paste0("\n", suggestion),
    if (!is.null(fix)) paste0("\nFix: ", fix)
  )

  paste(parts[nzchar(parts)], collapse = "\n")
}
