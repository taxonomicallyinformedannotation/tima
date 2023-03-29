## Simple install helper
options(repos = c(CRAN = "https://cloud.r-project.org"))
###
## Probably not necessary anymore, letting for safety for now
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
###
if (!requireNamespace("pak", quietly = TRUE)) {
  lib <- Sys.getenv("R_LIBS_SITE")
  if (lib == "") {
    lib <- file.path(dirname(.Library), "site-library")
    message("Setting R_LIBS_SITE to ", lib)
  } else {
    message("R_LIBS_SITE is already set to ", lib)
    cat(
      sprintf(
        "R_LIB_FOR_PAK=%s\n",
        strsplit(lib, .Platform$path.sep)[[1]][[1]]
      ),
      file = Sys.getenv("GITHUB_ENV"),
      append = TRUE
    )
  }
  if (Sys.info()[["sysname"]] == "Windows") {
    lib <- Sys.getenv("R_LIB_FOR_PAK")
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
    install.packages("pak",
      repos = "https://r-lib.github.io/p/pak/stable",
      lib = lib
    )
  } else {
    install.packages("pak",
      repos = "https://r-lib.github.io/p/pak/stable"
    )
  }
  # stream <- "devel"
  # install.packages(
  #   "pak",
  #   repos = sprintf(
  #     "https://r-lib.github.io/p/pak/%s/%s/%s/%s",
  #     stream,
  #     .Platform$pkgType,
  #     R.Version()$os,
  #     R.Version()$arch
  #   )
  # )
}
Sys.setenv("PKGCACHE_HTTP_VERSION" = "2")
pak::lockfile_create()
pak::lockfile_install()
unlink(x = "pkg.lock")
pak::pak()
pak::pkg_install(pkg = desc::desc_get_urls()[[1]])
