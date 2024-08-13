import::from(utils, install.packages, .into = environment())

#' @title Install
#'
#' @description This function runs some required install
#'
#' @importFrom utils install.packages
#'
#' @include copy_backbone.R
#'
#' @param package Package
#' @param repos Repos
#' @param test Flag for tests
#'
#' @return NULL
#'
#' @export
#'
#' @examples install()
install <- function(package = "tima",
                    repos = c(
                      "https://taxonomicallyinformedannotation.r-universe.dev",
                      "https://bioc.r-universe.dev",
                      "https://cloud.r-project.org"
                    ),
                    test = FALSE) {
  if (Sys.info()[["sysname"]] == "Windows") {
    message("You should install RTools if not already done")
  }
  if (Sys.info()[["sysname"]] == "Linux") {
    message("You should install some required dependencies using")
    message("`sudo apt install libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev`")
  }
  success <- tryCatch(
    {
      message("Installing latest version")
      install.packages(
        package,
        repos = repos,
        INSTALL_opts = c("--no-lock", "--no-test-load")
      )
      TRUE
    },
    error = function(e) {
      message("Install failed")
      message(e)
      FALSE
    }
  )
  if (!success || isTRUE(test)) {
    success <- tryCatch(
      {
        message("Retrying install from source")
        install.packages(
          package,
          repos = repos,
          INSTALL_opts = c("--no-lock", "--no-test-load"),
          type = "source"
        )
        TRUE
      },
      error = function(e) {
        message("Install failed")
        message(e)
        FALSE
      }
    )
  }
  if (!success || isTRUE(test)) {
    message("All installation attempts failed")
  }
  copy_backbone()
}
