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
    message("You should install RTools if not already done")
  }
  if (Sys.info()[["sysname"]] == "Linux") {
    system(command = "sudo apt install libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev")
  }
  if (!requireNamespace("devtools")) {
    install.packages("devtools")
  }
  ref <- ifelse(
    test = Sys.getenv("BRANCH_NAME") != "",
    yes = Sys.getenv("BRANCH_NAME"),
    no = "main"
  )
  devtools::install_github(repo = paste0("taxonomicallyinformedannotation/tima-r@", ref))
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
