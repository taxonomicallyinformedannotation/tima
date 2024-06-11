#' @title Install latest version
#'
#' @description This function installs the latest version
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
install_latest_version <- function() {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if (Sys.info()[["sysname"]] == "Windows") {
    if (!requireNamespace("installr", quietly = TRUE)) {
      install.packages("installr")
    }
    installr::install.rtools(check_r_update = FALSE, GUI = FALSE)
  }
  if (!requireNamespace("pak", quietly = TRUE)) {
    lib <- Sys.getenv("R_LIBS_SITE")
    if (lib == "") {
      lib <- file.path(dirname(.Library), "site-library")
      cat(sprintf("R_LIBS_SITE=%s\n", lib), append = TRUE)
      cat(sprintf("R_LIB_FOR_PAK=%s\n", lib), append = TRUE)

      message("Setting R_LIBS_SITE to ", lib)
    } else {
      message("R_LIBS_SITE is already set to ", lib)
      cat(sprintf(
        "R_LIB_FOR_PAK=%s\n",
        strsplit(lib, .Platform$path.sep)[[1]][[1]]
      ), append = TRUE)
    }
    install.packages(
      "pak",
      repos = sprintf(
        "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
        .Platform$pkgType,
        R.Version()$os,
        R.Version()$arch
      )
    )
  }
  if (!requireNamespace(c("gh"), quietly = TRUE)) {
    install.packages(c("gh"))
  }
  local_sha <- pak::pkg_status("timaR")$remotesha
  if (is.na(local_sha)) {
    message("Installing for the first time...")
    local_sha <- "myFirstInstallTrickToWork"
  }
  remote_sha <- gh::gh("GET /repos/taxonomicallyinformedannotation/tima-r/commits")[[1]]$sha
  if (local_sha != remote_sha) {
    pak::pak_update()
    pak::pak(ask = FALSE, upgrade = TRUE)
    tryCatch(
      expr = {
        message("Installing local version")
        pak::pkg_install(
          pkg = ".",
          ask = FALSE,
          upgrade = FALSE
        )
      },
      error = function(e) {
        tryCatch(
          expr = {
            message("Installing local version 2")
            pak::pkg_install(
              pkg = desc::desc_get_urls()[[1]],
              ask = FALSE,
              upgrade = FALSE
            )
          },
          error = function(e) {
            tryCatch(
              expr = {
                message("Installing remote version")
                pak::pkg_install(
                  pkg = "taxonomicallyinformedannotation/tima-r",
                  ask = FALSE,
                  upgrade = FALSE
                )
              },
              error = function(e) {
                message("Install failed")
              }
            )
          }
        )
      }
    )
    cache <- fs::path_home(".tima")
    message("Creating cache at ", cache)
    fs::dir_create(path = cache)
    message("Copying default architecture ...")
    tryCatch(
      expr = {
        fs::dir_copy(
          path = "./inst",
          new_path = file.path(cache, "inst"),
          overwrite = TRUE
        )
      },
      error = function(e) {
        if (file.exists("./../../app.R")) {
          message("I'm in test dir")
          fs::dir_copy(
            path = "./../../",
            new_path = file.path(cache, "inst"),
            overwrite = TRUE
          )
        }
      }
    )
    tryCatch(
      expr = {
        message("Installing local targets")
        fs::file_copy(
          path = "./_targets.yaml",
          new_path = file.path(cache, "_targets.yaml"),
          overwrite = TRUE
        )
      },
      error = function(e) {
        message("Installing remote targets")
        timaR::get_file(
          url = "https://raw.githubusercontent.com/taxonomicallyinformedannotation/tima-r/main/_targets.yaml",
          export = file.path(cache, "_targets.yaml")
        )
      }
    )
    tryCatch(
      expr = {
        message("Getting local DESCRIPTION")
        fs::file_copy(
          path = "./DESCRIPTION",
          new_path = file.path(cache, "DESCRIPTION"),
          overwrite = TRUE
        )
      },
      error = function(e) {
        message("Getting remote DESCRIPTION")
        timaR::get_file(
          url = "https://raw.githubusercontent.com/taxonomicallyinformedannotation/tima-r/main/DESCRIPTION",
          export = file.path(cache, "DESCRIPTION")
        )
      }
    )
  } else {
    message("You already have the latest version, skipping")
  }
}
