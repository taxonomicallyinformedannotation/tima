#' @title Harmonize names sirius
#'
#' @description This function harmonizes SIRIUS output column names by removing
#'     numeric prefixes and underscores. SIRIUS often prefixes column names with
#'     numbers and underscores (e.g., "1_compound_name"), and this function
#'     extracts the meaningful part (e.g., "name").
#'
#' @param x Character string containing a SIRIUS column name to harmonize
#'
#' @return Character string with everything before and including the last
#'     underscore removed, leaving only the meaningful column name portion
#'
#' @examples
#' \dontrun{
#' harmonize_names_sirius("1_compound_name") # Returns "name"
#' harmonize_names_sirius("2_feature_id") # Returns "id"
#' harmonize_names_sirius("score") # Returns "score" (no underscore)
#' }
harmonize_names_sirius <- function(x) {
  # Validate input
  if (missing(x) || is.null(x)) {
    stop("Input 'x' must be provided")
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  # Remove everything up to and including the last underscore
  # This extracts the meaningful column name from SIRIUS prefixed names
  harmonized_name <- gsub(
    pattern = ".*_",
    replacement = "",
    x = x,
    perl = TRUE
  )

  return(harmonized_name)
}
