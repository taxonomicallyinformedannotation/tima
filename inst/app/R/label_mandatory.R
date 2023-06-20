# add an asterisk to an input label
label_mandatory <- function(label) {
  shiny::tagList(
    label,
    shiny::span("*", class = "mandatory_star")
  )
}
