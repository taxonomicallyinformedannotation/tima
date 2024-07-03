#' @title Install latest version
#'
#' @description This function installs the latest version
#'
#' @param test Flag for test
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
install_latest_version <- function(test = FALSE) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if (Sys.info()[["sysname"]] == "Windows") {
    message("You should install RTools if not already done")
  }
  if (Sys.info()[["sysname"]] == "Linux") {
    system(command = "sudo apt install libcurl4-openssl-dev")
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
  ref <- ifelse(
    test = Sys.getenv("BRANCH_NAME") != "",
    yes = Sys.getenv("BRANCH_NAME"),
    no = "main"
  )
  message("ref is ", ref)
  pak::pak_update()
  # Fails sometimes
  tryCatch(
    expr = {
      pak::pak(
        pkg = paste0("taxonomicallyinformedannotation/tima-r@", ref),
        ask = FALSE
      )
    },
    error = function(e) {
      if (test == FALSE) {
        if (Sys.getenv("RSTUDIO") == 1) {
          if (!requireNamespace("rstudioapi", quietly = TRUE)) {
            install.packages("rstudioapi")
          }
          rstudioapi::restartSession()
        } else {
          if (!requireNamespace("startup", quietly = TRUE)) {
            install.packages("startup")
          }
          startup::restart()
        }
      }
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
        url = paste0(
          "https://raw.githubusercontent.com/taxonomicallyinformedannotation/tima-r/",
          ref,
          "/_targets.yaml"
        ),
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
        url = paste0(
          "https://raw.githubusercontent.com/taxonomicallyinformedannotation/tima-r/",
          ref,
          "/DESCRIPTION"
        ),
        export = file.path(cache, "DESCRIPTION")
      )
    }
  )
}
