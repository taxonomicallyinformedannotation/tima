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
pak::pkg_install(pkg = desc::desc_get_urls()[[1]])
