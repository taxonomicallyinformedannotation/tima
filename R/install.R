#' @title Install
#'
#' @description This function runs some required install
#'
#' @param test Flag for tests
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
install <- function(test = FALSE) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if (Sys.info()[["sysname"]] == "Windows") {
    message("You should install RTools if not already done")
  }
  if (Sys.info()[["sysname"]] == "Linux") {
    system(command = "sudo apt install libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev")
  }
  if (!"pak" %in% utils::installed.packages() || isTRUE(test)) {
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
    utils::install.packages(
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
  if (!"tima" %in% utils::installed.packages() || isTRUE(test)) {
    message("Installing for the first time...")
    local_version <- "myFirstInstallTrickToWork"
  } else {
    status <- pak::pkg_status("tima")
    local_version <- status$version[1]
    local_sha <- status$remotesha[1]
  }
  remote_version <- readLines(
    paste0(
      "https://raw.githubusercontent.com/taxonomicallyinformedannotation/tima/",
      ref,
      "/DESCRIPTION"
    ),
    warn = FALSE
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
    pak::pak_cleanup(force = TRUE)
    pak::pak_update()
    pak::pak(ask = FALSE, upgrade = TRUE)
    # Try installing the local version first
    success <- tryCatch(
      {
        message("Installing local version")
        pak::pkg_install(
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
    if (!success || isTRUE(test)) {
      success <- tryCatch(
        {
          message("Installing remote version")
          pak::pkg_install(
            pkg = paste0(
              "github::",
              "taxonomicallyinformedannotation/tima@",
              ref,
              "?source&reinstall&nocache"
            ),
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
    if (!success || isTRUE(test)) {
      success <- tryCatch(
        {
          message("Retrying remote version")
          pak::pkg_install(
            pkg = paste0(
              "github::",
              "taxonomicallyinformedannotation/tima@",
              ref,
              "?source&reinstall&nocache"
            ),
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
    if (!success || isTRUE(test)) {
      message("All installation attempts failed")
    }
  }
  cache <- fs::path_home(".tima")
  message("Creating cache at ", cache)
  fs::dir_create(path = cache)
}
