## Simple install helper
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_local(
  repo = ".",
  upgrade = "always",
  build_vignettes = FALSE
)
