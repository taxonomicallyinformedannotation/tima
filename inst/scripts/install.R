## Simple install helper
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
# if (!requireNamespace("renv", quietly = TRUE)) {
#   install.packages("renv")
# }
# renv::restore()
devtools::install()
