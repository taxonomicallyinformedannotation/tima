#' @title Harmonize names sirius
#'
#' @description This function harmonizes the names of Sirius outputs to make them compatible
#'
#' @param x Character string containing a name
#'
#' @return Character string with the name modified according to the rules specified in the function
#'
#' @export
#'
#' @examples harmonized_name <- harmonize_names_sirius("My_name")
harmonize_names_sirius <- function(x) {
  ## Remove everything up to and including the last underscore from the name
  harmonized_name <- gsub(
    pattern = ".*_",
    replacement = "",
    x = x
  )

  return(harmonized_name)
}
