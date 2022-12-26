#' @title Treat names sirius
#'
#' @param x a character string containing a name
#'
#' @return a character string with the name modified according to the rules specified in the function
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
