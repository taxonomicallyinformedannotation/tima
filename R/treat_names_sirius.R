#' @title Treat names sirius
#'
#' @description This function cleans the names of Sirius outputs to make them compatible
#'
#' @param x Character string containing a name
#'
#' @return Character string with the name modified according to the rules specified in the function
#'
#' @export
#'
#' @examples treated_name <- treat_names_sirius("My_name")
treat_names_sirius <- function(x) {
  # Remove everything up to and including the last underscore from the name
  treated_name <- gsub(
    pattern = ".*_",
    replacement = "",
    x = x
  )

  return(treated_name)
}
