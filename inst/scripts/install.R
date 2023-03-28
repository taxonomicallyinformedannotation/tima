## Simple install helper
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak")
}
# if (!requireNamespace("renv", quietly = TRUE)) {
#   install.packages("renv")
# }
# renv::restore()
pak::local_install(upgrade = TRUE,
                   dependencies = TRUE)
