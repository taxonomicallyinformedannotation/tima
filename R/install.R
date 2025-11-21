#' @title Install
#'
#' @description This function installs the TIMA package and its dependencies,
#'     including setting up a Python virtual environment with RDKit for
#'     chemical structure processing. It handles different operating systems
#'     and provides fallback installation methods if needed.
#'
#' @include copy_backbone.R
#'
#' @param package Character string name of the package to install (default: "tima")
#' @param repos Character vector of repository URLs for install.packages
#' @param dependencies Logical whether to install package dependencies (default: TRUE)
#' @param test Logical whether to use fallback/test installation mode (default: FALSE)
#'
#' @return NULL (invisibly). Installs packages and sets up Python environment as
#'     side effects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install()
#' }
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
  # Validate inputs
  if (!is.character(package) || length(package) != 1L) {
    stop("package must be a single character string")
  }

  if (!is.character(repos) || length(repos) == 0L) {
    stop("repos must be a non-empty character vector")
  }

  if (!is.logical(dependencies) || length(dependencies) != 1L) {
    stop("dependencies must be a single logical value")
  }

  if (!is.logical(test) || length(test) != 1L) {
    stop("test must be a single logical value")
  }

  system <- Sys.info()[["sysname"]]
  logger::log_info("Detected operating system: {system}")

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

    if (!reticulate::virtualenv_exists(envname = envname)) {
      logger::log_info("Creating Python virtualenv: {envname}")
      # Rescue version for fallback if default Python doesn't work
      rescue_python_version <- "3.13"
      tryCatch(
        expr = {
          reticulate::virtualenv_create(
            envname = envname,
            python = python
          )
        },
        error = function(e) {
          logger::log_error(
            "Creating Python virtualenv failed: ",
            conditionMessage(e)
          )
          logger::log_info(
            "Retrying with a clean python install (version ",
            rescue_python_version,
            ")"
          )
          python <- reticulate::install_python(
            version = rescue_python_version
          )
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
      envname = envname,
      python = python,
      packages = "rdkit",
      ignore_installed = TRUE
    )
  }

  try_install <- function(from_source = FALSE) {
    tryCatch(
      expr = {
        logger::log_info(
          "Installing R package: ",
          package,
          " (from ",
          if (from_source) "source" else "binary",
          ")"
        )
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
        logger::log_info("Successfully installed R package: {package}")

        setup_virtualenv()
        return(TRUE)
      },
      error = function(e) {
        logger::log_error("Installation failed: {conditionMessage(e)}")
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
