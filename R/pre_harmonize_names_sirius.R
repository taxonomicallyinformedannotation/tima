#' @title Pre harmonize names sirius
#'
#' @description This function pre-processes SIRIUS column names by removing
#'     suffixes that appear after a forward slash. SIRIUS sometimes appends
#'     additional information after a slash (e.g., "columnName/suffix"), and
#'     this function removes everything from the slash onwards.
#'
#' @param x Character string containing a SIRIUS column name to pre-harmonize
#'
#' @return Character string with everything from and including the first
#'     forward slash (/) removed, leaving only the base column name
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' pre_harmonize_names_sirius("column_name/suffix") # Returns "column_name"
#' pre_harmonize_names_sirius("simple_name") # Returns "simple_name"
#' pre_harmonize_names_sirius("name/extra/info") # Returns "name"
#' }
pre_harmonize_names_sirius <- function(x) {
  # Validate input
  if (missing(x) || is.null(x)) {
    stop("Input 'x' must be provided")
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  # Remove everything from the first forward slash onwards
  # This strips SIRIUS suffix information from column names
  cleaned_name <- stringi::stri_replace_all_regex(
    str = x,
    pattern = "/.*",
    replacement = "",
    vectorize_all = FALSE
  )

  return(cleaned_name)
}
