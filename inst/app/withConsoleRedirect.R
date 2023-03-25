# redirect console output to Shiny
withConsoleRedirect <- function(containerId, expr) {
  txt <- capture.output(results <- expr) |>
    stringr::str_replace(pattern = "[^[:graph:]]\\[0m[^[:graph:]]\\[22m[^[:graph:]]\\[23m[^[:graph:]]\\[24m[^[:graph:]]\\[27m[^[:graph:]]\\[28m[^[:graph:]]\\[29m[^[:graph:]]\\[49m[^[:graph:]]\\[39m", replacement = "\n") |>
    stringr::str_replace(pattern = "[^[:graph:]]\\[0m[^[:graph:]]\\[22m[^[:graph:]]\\[23m[^[:graph:]]\\[24m[^[:graph:]]\\[27m[^[:graph:]]\\[28m[^[:graph:]]\\[29m[^[:graph:]]\\[49m", replacement = "\n") |>
    stringr::str_replace(pattern = "[^[:graph:]]\\[0m[^[:graph:]]\\[22m[^[:graph:]]\\[23m[^[:graph:]]\\[24m[^[:graph:]]\\[27m[^[:graph:]]\\[28m[^[:graph:]]\\[29m[^[:graph:]]\\[39m", replacement = "\n") |>
    stringr::str_replace(pattern = "[^[:graph:]]\\[0m[^[:graph:]]\\[22m[^[:graph:]]\\[23m[^[:graph:]]\\[24m[^[:graph:]]\\[27m[^[:graph:]]\\[28m[^[:graph:]]\\[29m[^[:graph:]]\\[39m", replacement = "\n") |>
    stringr::str_replace(pattern = "\\[32m✔[^[:graph:]]\\[39m ", replacement = "\n") |>
    stringr::str_replace(pattern = "\\[32m•[^[:graph:]]\\[39m ", replacement = "\n") |>
    stringr::str_replace(pattern = "\\[34m•[^[:graph:]]\\[39m ", replacement = "\n") |>
    stringr::str_replace(pattern = "\\[39m", replacement = "\n") |>
    stringr::str_replace(pattern = "\\[90m", replacement = "\n")

  if (length(txt) > 0) {
    insertUI(
      selector = paste0("#", containerId),
      where = "beforeEnd",
      ui = paste0(txt, "\n", collapse = ""),
      immediate = TRUE
    )
  }
  results
}
