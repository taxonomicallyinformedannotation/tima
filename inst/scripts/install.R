## Simple install helper
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
if (Sys.info()[["sysname"]] == "Windows") {
  if (!requireNamespace("installr", quietly = TRUE)) {
    install.packages("installr")
  }
  installr::install.rtools()
}
needs <-
  sprintf("Config/Needs/%s", strsplit("website", "[[:space:],]+")[[1]])
deps <- strsplit("deps::., any::sessioninfo", "[[:space:],]+")[[1]]
extra_deps <- strsplit("any::pkgdown", "[[:space:],]+")[[1]]
pak::lockfile_create(c(deps, extra_deps),
                     dependencies = c(needs, ("all")))
pak::lockfile_install()
pak::local_install()
