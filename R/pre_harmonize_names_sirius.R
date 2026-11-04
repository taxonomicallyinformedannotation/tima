#' @title Pre harmonize names sirius
#'
#' @description This function pre-processes SIRIUS column names by removing
#'     suffixes that appear after a forward slash. SIRIUS sometimes appends
#'     additional information after a slash (e.g., "columnName/suffix"), and
#'     this function removes everything from the slash onwards.
#'
#' @param x [character] Character string containing a SIRIUS column name to
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
  # Validate input
  if (missing(x) || is.null(x)) {
    cli::cli_abort(
      "input 'x' must be provided",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  # Remove everything from the first forward slash onwards
  # This strips SIRIUS suffix information from column names
  n <- length(x)
  if (n > 256L) {
    ux <- unique(x)
    if (length(ux) * 2L < n) {
      u_clean <- sub(pattern = "/.*", replacement = "", x = ux, perl = TRUE)
      return(u_clean[fastmatch::fmatch(x, ux)])
    }
  }
  sub(
    pattern = "/.*",
    replacement = "",
    x = x,
    perl = TRUE
  )
}
