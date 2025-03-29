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
install <- function(
  package = "tima",
  repos = c(
    "https://taxonomicallyinformedannotation.r-universe.dev",
    "https://bioc.r-universe.dev",
    "https://cloud.r-project.org"
  ),
  dependencies = TRUE,
  test = FALSE
) {
  if (Sys.info()[["sysname"]] == "Windows") {
    logger::log_info("You should install RTools if not already done")
  }
  if (Sys.info()[["sysname"]] == "Linux") {
    logger::log_info("You should install some required dependencies using")
    logger::log_info(
      "`sudo apt install libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev`"
    )
  }
  success <- tryCatch(
    {
      logger::log_trace("Installing latest version")
      utils::install.packages(
        package,
        repos = repos,
        dependencies = dependencies,
        INSTALL_opts = c("--no-lock", "--no-test-load")
      )
      logger::log_trace("Installing Python environment")
      reticulate::install_python()
      logger::log_trace("Installing RDKit for Python")
      reticulate::py_install("rdkit")
      TRUE
    },
    error = function(e) {
      logger::log_error("Install failed")
      logger::log_error(e)
      FALSE
    }
  )
  if (!success || isTRUE(test)) {
    success <- tryCatch(
      {
        logger::log_warn("Retrying install from source")
        utils::install.packages(
          package,
          repos = repos,
          dependencies = dependencies,
          INSTALL_opts = c("--no-lock", "--no-test-load"),
          type = "source"
        )
        logger::log_trace("Installing Python environment")
        reticulate::install_python()
        logger::log_trace("Installing RDKit for Python")
        reticulate::py_install("rdkit")
        TRUE
      },
      error = function(e) {
        logger::log_error("Install failed")
        logger::log_error(e)
        FALSE
      }
    )
  }
  if (!success || isTRUE(test)) {
    logger::log_fatal("All installation attempts failed")
  }
  copy_backbone()
}
