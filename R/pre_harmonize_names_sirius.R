#' @title Pre harmonize names sirius
#'
#' @description This function pre-processes SIRIUS column names by removing
#'     suffixes that appear after a forward slash. SIRIUS sometimes appends
#'     additional information after a slash (e.g., "columnName/suffix"), and
#'     this function removes everything from the slash onwards.
#'
#' @param x [character] A SIRIUS column name to
#'     pre-harmonize
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
  .strip_sirius_name_part(x, "/.*", "input 'x' must be provided")
}
