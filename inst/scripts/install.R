## Simple install helper
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (Sys.info()[["sysname"]] == "Windows") {
  if (!requireNamespace("installr", quietly = TRUE)) {
    install.packages("installr")
  }
  installr::install.rtools()
}
if (Sys.info()[["sysname"]] != "Linux") {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
  }
  renv::equip()
}
if (!requireNamespace("pak", quietly = TRUE)) {
  stream <- "devel"
  install.packages(
    "pak",
    repos = sprintf(
      "https://r-lib.github.io/p/pak/%s/%s/%s/%s",
      stream,
      .Platform$pkgType,
      R.Version()$os,
      R.Version()$arch
    )
  )
}
pak::pak()
pak::pkg_install(pkg = desc::desc_get_urls()[[1]])
