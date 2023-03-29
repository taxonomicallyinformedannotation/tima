## Simple install helper
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
if (Sys.info()[["sysname"]] == "Windows") {
  if (!requireNamespace("installr", quietly = TRUE)) {
    install.packages("installr")
  }
  installr::install.rtools()
}
if (Sys.info()[["sysname"]] != "Linux") {
  renv::equip()
}
renv::restore()
renv::install(packages = ".")
