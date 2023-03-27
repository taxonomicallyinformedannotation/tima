## Simple install helper
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github(
  repo = "taxonomicallyinformedannotation/tima-r",
  upgrade = "always",
  build_vignettes = FALSE
)
