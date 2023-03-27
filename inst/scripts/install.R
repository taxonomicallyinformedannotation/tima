## Simple install helper
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
# if (!requireNamespace("renv", quietly = TRUE)) {
#   install.packages("renv")
# }
# renv::restore()
devtools::install()
