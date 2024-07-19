#' @title Install latest version
#'
#' @description This function installs the latest version
#'
#' @importFrom fs dir_copy dir_create file_copy path_home
#' @importFrom pak pak_cleanup pak_update pak pkg_install pkg_status
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
install_latest_version <- function() {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if (Sys.info()[["sysname"]] == "Windows") {
    message("You should install RTools if not already done")
  }
  if (Sys.info()[["sysname"]] == "Linux") {
    system(command = "sudo apt install libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev")
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
  if (!"timaR" %in% installed.packages()) {
    message("Installing for the first time...")
    local_version <- "myFirstInstallTrickToWork"
  } else {
    status <- pkg_status("timaR")
    local_version <- status$version[1]
    local_sha <- status$remotesha[1]
  }
  remote_version <- readLines(
    paste0(
      "https://raw.githubusercontent.com/taxonomicallyinformedannotation/tima-r/",
      ref,
      "/DESCRIPTION"
    )
  )[[3]] |>
    gsub(
      pattern = "Version: ",
      replacement = "",
      fixed = TRUE
    )
  if (local_version == remote_version) {
    message(
      "You already have the latest version (",
      local_version,
      ") skipping"
    )
  } else {
    pak_cleanup(force = TRUE)
    pak_update()
    pak(ask = FALSE, upgrade = TRUE)
    # Try installing the local version first
    success <- tryCatch(
      {
        message("Installing local version")
        pkg_install(
          pkg = ".",
          ask = FALSE,
          upgrade = FALSE
        )
        TRUE
      },
      error = function(e) {
        FALSE
      }
    )
    # If local version installation fails, try the URL from DESCRIPTION file
    if (!success) {
      success <- tryCatch(
        {
          message("Installing remote version")
          pkg_install(
            pkg = paste0("github::", "taxonomicallyinformedannotation/tima-r@", ref, "?source&reinstall&nocache"),
            ask = FALSE,
            upgrade = FALSE
          )
          TRUE
        },
        error = function(e) {
          FALSE
        }
      )
    }
    # If URL installation fails, try installing the remote version from GitHub
    if (!success) {
      success <- tryCatch(
        {
          message("Retrying remote version")
          pkg_install(
            pkg = paste0("github::", "taxonomicallyinformedannotation/tima-r@", ref, "?source&reinstall&nocache"),
            ask = FALSE,
            upgrade = FALSE
          )
          TRUE
        },
        error = function(e) {
          message("Install failed")
          FALSE
        }
      )
    }
    # Final message if all attempts fail
    if (!success) {
      message("All installation attempts failed")
    }
  }
  cache <- path_home(".tima")
  message("Creating cache at ", cache)
  dir_create(path = cache)
  message("Copying default architecture ...")
  tryCatch(
    expr = {
      dir_copy(
        path = "./inst",
        new_path = file.path(cache, "inst"),
        overwrite = TRUE
      )
    },
    error = function(e) {
      if (file.exists("./../../app.R")) {
        message("I'm in test dir")
        dir_copy(
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
      file_copy(
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
      file_copy(
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
