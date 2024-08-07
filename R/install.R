import::from(fs, dir_copy, .into = environment())
import::from(fs, dir_create, .into = environment())
import::from(fs, path_home, .into = environment())
import::from(utils, install.packages, .into = environment())
import::from(utils, installed.packages, .into = environment())

#' @title Install
#'
#' @description This function runs some required install
#'
#' @importFrom fs dir_copy
#' @importFrom fs dir_create
#' @importFrom fs path_home
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
#'
#' @param test Flag for tests
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
install <- function(test = FALSE) {
  if (Sys.info()[["sysname"]] == "Windows") {
    message("You should install RTools if not already done")
  }
  if (Sys.info()[["sysname"]] == "Linux") {
    system(command = "sudo apt install libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev")
  }
  ref <- ifelse(
    test = Sys.getenv("BRANCH_NAME") != "",
    yes = Sys.getenv("BRANCH_NAME"),
    no = "main"
  )
  installed_packages <- installed.packages() |>
    data.frame()
  if (!"tima" %in% installed_packages$Package || isTRUE(test)) {
    message("Installing for the first time...")
    local_version <- "myFirstInstallTrickToWork"
  } else {
    # TODO change to SHA
    local_version <- installed_packages$Version[installed_packages$Package ==
      "tima"]
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
    success <- tryCatch(
      {
        message("Installing latest version")
        install.packages(
          "tima",
          repos = c(
            "https://taxonomicallyinformedannotation.r-universe.dev",
            "https://bioc.r-universe.dev",
            "https://cloud.r-project.org"
          )
        )
        TRUE
      },
      error = function(e) {
        FALSE
      }
    )

    # If installation fails, try installing from source
    if (!success || isTRUE(test)) {
      success <- tryCatch(
        {
          message("Retrying install from source")
          install.packages(
            "tima",
            repos = c(
              "https://taxonomicallyinformedannotation.r-universe.dev",
              "https://bioc.r-universe.dev",
              "https://cloud.r-project.org"
            ),
            type = "source"
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
  message("Copying default architecture ...")
  fs::dir_copy(
    path = system.file(package = "tima"),
    new_path = file.path(cache, "inst"),
    overwrite = TRUE
  )
}
