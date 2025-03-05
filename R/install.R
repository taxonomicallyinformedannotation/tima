#' @title Install
#'
#' @description This function runs some required install
#'
#' @include copy_backbone.R
#'
#' @param package Package
#' @param repos Repos
#' @param dependencies Flag for dependencies
#' @param test Flag for tests
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
install <- function(package = "tima",
                    repos = c(
                      "https://taxonomicallyinformedannotation.r-universe.dev",
                      "https://bioc.r-universe.dev",
                      "https://cloud.r-project.org"
                    ),
                    dependencies = TRUE,
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
      utils::install.packages(
        package,
        repos = repos,
        dependencies = dependencies,
        INSTALL_opts = c("--no-lock", "--no-test-load")
      )
      reticulate::install_python()
      reticulate::py_install("rdkit")
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
        utils::install.packages(
          package,
          repos = repos,
          dependencies = dependencies,
          INSTALL_opts = c("--no-lock", "--no-test-load"),
          type = "source"
        )
        reticulate::install_python()
        reticulate::py_install("rdkit")
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
