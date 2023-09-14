#' @title Label mandatory
#'
#' @description This function adds an asterisk to mandatory inputs
#'
#' @param label Label
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
label_mandatory <- function(label) {
  shiny::tagList(
    label,
    shiny::span("*", class = "mandatory_star")
  )
}
