## Simple install helper
options(repos = c(CRAN = "https://cloud.r-project.org"))
###
## Probably not necessary anymore, letting for safety for now
if (Sys.info()[["sysname"]] == "Windows") {
  if (!requireNamespace("installr", quietly = TRUE)) {
    install.packages("installr")
  }
  installr::install.rtools(GUI = FALSE)
}
if (Sys.info()[["sysname"]] != "Linux") {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv")
  }
  renv::equip()
}
###
###
## Trial to isolate from pre-existing libraries
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
renv::activate()
###
if (!requireNamespace("pak", quietly = TRUE)) {
  lib <- Sys.getenv("R_LIBS_SITE")
  if (lib == "") {
    lib <- file.path(dirname(.Library), "site-library")
    cat(sprintf("R_LIBS_SITE=%s\n", lib), append = TRUE)
    cat(sprintf("R_LIB_FOR_PAK=%s\n", lib), append = TRUE)

    message("Setting R_LIBS_SITE to ", lib)
  } else {
    message("R_LIBS_SITE is already set to ", lib)
    cat(
      sprintf(
        "R_LIB_FOR_PAK=%s\n",
        strsplit(lib, .Platform$path.sep)[[1]][[1]]
      ),
      append = TRUE
    )
  }
  install.packages("pak",
    repos = "https://r-lib.github.io/p/pak/stable"
  )
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
pak::pak_update()
pak::lockfile_create()
pak::lockfile_install()
unlink(x = "pkg.lock")
pak::pak(ask = FALSE)
pak::pkg_install(pkg = desc::desc_get_urls()[[1]], ask = FALSE)
