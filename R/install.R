#' @title Install
#'
#' @description This function runs some required install
#'
#' @include copy_backbone.R
#'
#' @param package Package name
#' @param repos Repositories for install.packages
#' @param dependencies Whether to install dependencies
#' @param test Whether to retry with source or test install
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
    logger::log_info("You may need system dependencies:")
    logger::log_info(
      "sudo apt install libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev"
    )
  }

  check_or_install_python <- function() {
    python <- Sys.which("python3")
    if (python != "") {
      logger::log_info("System Python found at: {python}")
      return(python)
    }

    logger::log_warn(
      "System Python not found. Installing Miniconda as fallback."
    )

    if (!reticulate::miniconda_path() |> file.exists()) {
      reticulate::install_miniconda()
    }

    return(reticulate::miniconda_path())
  }

  setup_virtualenv <- function(envname = "tima-env") {
    python <- check_or_install_python()

    if (!reticulate::virtualenv_exists(envname)) {
      logger::log_info("Creating Python virtualenv: {envname}")
      reticulate::virtualenv_create(envname = envname, python = python)
    } else {
      logger::log_info("Using existing Python virtualenv: {envname}")
    }

    logger::log_info("Installing RDKit in virtualenv: {envname}")
    reticulate::virtualenv_install(
      envname,
      packages = "rdkit",
      ignore_installed = TRUE
    )
  }

  try_install <- function(from_source = FALSE) {
    tryCatch(
      {
        logger::log_trace("Installing R package: {package}")
        utils::install.packages(
          package,
          repos = repos,
          dependencies = dependencies,
          INSTALL_opts = c("--no-lock", "--no-test-load"),
          type = if (from_source) "source" else getOption("pkgType")
        )

        setup_virtualenv()
        TRUE
      },
      error = function(e) {
        logger::log_error("Installation failed")
        logger::log_error(e)
        FALSE
      }
    )
  }

  success <- try_install()

  if (!success || isTRUE(test)) {
    logger::log_warn("Retrying install from source")
    success <- try_install(from_source = TRUE)
  }

  if (!success) {
    logger::log_fatal("All installation attempts failed")
    stop("Installation failed.")
  }

  copy_backbone()
  targets::tar_destroy(ask = FALSE)
}
