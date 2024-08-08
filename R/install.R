import::from(fs, dir_copy, .into = environment())
import::from(fs, dir_create, .into = environment())
import::from(fs, path_home, .into = environment())
import::from(jsonlite, fromJSON, .into = environment())
import::from(utils, install.packages, .into = environment())
import::from(utils, installed.packages, .into = environment())

#' @title Install
#'
#' @description This function runs some required install
#'
#' @importFrom fs dir_copy
#' @importFrom fs dir_create
#' @importFrom fs path_home
#' @importFrom jsonlite fromJSON
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
#'
#' @param git_repo Git repo
#' @param r_universe_repo r-universe repo
#' @param test Flag for tests
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
install <- function(git_repo = "taxonomicallyinformedannotation/tima",
                    r_universe_repo = "taxonomicallyinformedannotation",
                    test = FALSE) {
  if (Sys.info()[["sysname"]] == "Windows") {
    message("You should install RTools if not already done")
  }
  if (Sys.info()[["sysname"]] == "Linux") {
    system(command = "sudo apt install libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev")
  }
  github_info <- fromJSON(paste0("https://api.github.com/repos/", git_repo, "/commits"))
  github_info$sha[1]
  r_universe_url <- paste0("https://", r_universe_repo, ".r-universe.dev")
  r_universe_info <- fromJSON(paste0(r_universe_url, "/api/packages/"))
  if (github_info$sha[1] == r_universe_info$RemoteSha[r_universe_info$Package == "tima"]) {
    message("You already have the latest version, skipping")
  } else {
    success <- tryCatch({
      message("Installing latest version")
      install.packages(
        "tima",
        repos = c(r_universe_url, "https://bioc.r-universe.dev", "https://cloud.r-project.org"),
        INSTALL_opts = c("--no-lock", "--no-test-load")
      )
      TRUE
    })
    if (!success || isTRUE(test)) {
      success <- tryCatch({
        message("Retrying install from source")
        install.packages(
          "tima",
          repos = c(r_universe_url, "https://bioc.r-universe.dev", "https://cloud.r-project.org"),
          INSTALL_opts = c("--no-lock", "--no-test-load"),
          type = "source"
        )
        TRUE
      })
    }
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
