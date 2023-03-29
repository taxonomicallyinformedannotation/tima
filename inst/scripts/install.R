## Simple install helper
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
if (Sys.info()[["sysname"]] != "Linux") {
  renv::equip()
}
renv::restore()
renv::install(packages = ".")
