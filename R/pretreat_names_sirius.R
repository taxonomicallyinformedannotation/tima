#' @title Pretreat names sirius
#'
#' @description This function pretreats Sirius names to make them compatible
#'
#' @param x Character string containing a name
#'
#' @return Character string with the name modified according to the rules specified in the function
#'
#' @export
#'
#' @importFrom stringr fixed str_remove
#'
#' @examples pretreated_name <- pretreat_names_sirius("My name/suffix")
pretreat_names_sirius <- function(x) {
  # Remove any characters after and including the '/' character from the name
  y <- x |>
    str_remove(pattern = stringr::fixed(pattern = "/.*"))

  return(y)
}
