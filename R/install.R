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
  system <- Sys.info()[["sysname"]]
  if (system == "Windows" || isTRUE(test)) {
    logger::log_info("You should install RTools if not already done")
  }
  if (system == "Linux") {
    logger::log_info(
      "You may need system dependencies: 'sudo apt install libarchive-dev libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev'"
    )
  }

  check_or_install_python <- function() {
    python <- Sys.which("python3")
    if (python |> nzchar() && isFALSE(test)) {
      logger::log_info("System Python found at: {python}")
      return(python)
    }

    logger::log_warn(
      "System Python not found. Installing Miniconda as fallback."
    )

    minipath <- reticulate::miniconda_path()
    if (!file.exists(minipath)) {
      reticulate::install_miniconda()
      minipath <- reticulate::miniconda_path()
    }
    python_path <- if (system == "Windows") {
      file.path(minipath, "python.exe")
    } else {
      file.path(minipath, "bin", "python")
    }

    logger::log_info("Using Miniconda Python at: {python_path}")
    return(python_path)
  }

  setup_virtualenv <- function(envname = "tima-env") {
    python <- check_or_install_python()

    if (!reticulate::virtualenv_exists(envname)) {
      logger::log_info("Creating Python virtualenv: {envname}")
      ## TODO improve this
      rescue_python_version <- "3.13"
      tryCatch(
        expr = {
          reticulate::virtualenv_create(envname = envname, python = python)
        },
        error = function(e) {
          logger::log_error("Creating Python virtualenv failed")
          logger::log_info("Retrying with a clean python install")
          python <- reticulate::install_python(version = rescue_python_version)
          reticulate::virtualenv_create(
            envname = envname,
            python = python
          )
        }
      )
    } else {
      logger::log_info("Using existing Python virtualenv: {envname}")
    }

    logger::log_info("Installing RDKit in virtualenv: {envname}")
    reticulate::virtualenv_install(
      envanme = envname,
      python = python,
      packages = "rdkit",
      ignore_installed = TRUE
    )
  }

  try_install <- function(from_source = FALSE) {
    tryCatch(
      expr = {
        logger::log_trace("Installing R package: {package}")
        utils::install.packages(
          package,
          repos = repos,
          dependencies = dependencies,
          INSTALL_opts = c("--no-lock", "--no-test-load"),
          type = if (from_source) {
            "source"
          } else {
            getOption("pkgType")
          }
        )
        logger::log_trace("Installed succesfully R package: {package}")

        setup_virtualenv()
        return(TRUE)
      },
      error = function(e) {
        logger::log_error("Installation failed")
        logger::log_error(e)
        return(FALSE)
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
    stop()
  }

  copy_backbone()
  targets::tar_destroy(ask = FALSE)
}
