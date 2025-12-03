#' Validate Install Function Inputs
#'
#' @description Internal helper to validate all input parameters for install().
#'     Implements Single Responsibility Principle.
#'
#' @param package Character package name
#' @param repos Character vector of repositories
#' @param dependencies Logical dependencies flag
#' @param test Logical test mode flag
#'
#' @return NULL (stops on validation error)
#' @keywords internal
validate_install_inputs <- function(package, repos, dependencies, test) {
  if (!is.character(package) || length(package) != 1L || nchar(package) == 0L) {
    stop(
      "package must be a single non-empty character string, got: ",
      if (is.null(package)) "NULL" else class(package)[1],
      call. = FALSE
    )
  }

  if (!is.character(repos) || length(repos) == 0L) {
    stop(
      "repos must be a non-empty character vector",
      call. = FALSE
    )
  }

  if (any(nchar(repos) == 0L)) {
    stop(
      "All repository URLs must be non-empty strings",
      call. = FALSE
    )
  }

  if (!is.logical(dependencies) || length(dependencies) != 1L) {
    stop(
      "dependencies must be a single logical value (TRUE or FALSE)",
      call. = FALSE
    )
  }

  if (!is.logical(test) || length(test) != 1L) {
    stop(
      "test must be a single logical value (TRUE or FALSE)",
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Display System-Specific Installation Messages
#'
#' @description Internal helper to show OS-specific installation instructions.
#'     Implements Single Responsibility Principle.
#'
#' @param system Character OS name from Sys.info()
#' @param test Logical test mode flag
#'
#' @return NULL (side effect: logging)
#' @keywords internal
show_system_messages <- function(system, test) {
  log_info("Detected operating system: %s", system)

  if (system == "Windows" || isTRUE(test)) {
    log_info("You should install RTools if not already done")
    log_info(
      "Download from: https://cran.r-project.org/bin/windows/Rtools/"
    )
  }

  if (system == "Linux") {
    log_info("You may need system dependencies:")
    log_info(
      "  sudo apt install libarchive-dev libcurl4-openssl-dev libharfbuzz-dev libfribidi-dev"
    )
  }

  if (system == "Darwin") {
    log_info(
      "macOS detected. Ensure Xcode Command Line Tools are installed:"
    )
    log_info("  xcode-select --install")
  }

  invisible(NULL)
}

#' Check or Install Python
#'
#' @description Internal helper to ensure Python is available, installing
#'     Miniconda as fallback if needed. Implements Single Responsibility Principle.
#'
#' @param test Logical test mode flag
#'
#' @return Character path to Python executable
#' @keywords internal
check_or_install_python <- function(test = FALSE) {
  system <- Sys.info()[["sysname"]]
  python <- Sys.which("python3")

  if (nzchar(python) && isFALSE(test)) {
    log_info("System Python found at: %s", python)
    return(python)
  }

  log_warn("System Python not found. Installing Miniconda as fallback.")

  minipath <- reticulate::miniconda_path()

  if (!file.exists(minipath)) {
    log_info("Installing Miniconda...")
    tryCatch(
      {
        reticulate::install_miniconda()
        minipath <- reticulate::miniconda_path()
        log_success("Miniconda installed successfully")
      },
      error = function(e) {
        log_error("Failed to install Miniconda: %s", e$message)
        stop(
          "Failed to install Miniconda: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  python_path <- if (system == "Windows") {
    file.path(minipath, "python.exe")
  } else {
    file.path(minipath, "bin", "python")
  }

  if (!file.exists(python_path)) {
    stop(
      "Python executable not found at expected location: ",
      python_path,
      call. = FALSE
    )
  }

  log_info("Using Miniconda Python at: %s", python_path)
  return(python_path)
}

#' Setup Python Virtual Environment
#'
#' @description Internal helper to create and configure Python virtualenv with RDKit.
#'     Implements Single Responsibility Principle.
#'
#' @param envname Character name of virtual environment
#' @param python Character path to Python executable
#' @param rescue_python_version Character Python version for fallback
#'
#' @return NULL (side effect: creates virtualenv and installs packages)
#' @keywords internal
setup_virtualenv <- function(
  envname = "tima-env",
  python = NULL,
  rescue_python_version = "3.13"
) {
  if (is.null(python)) {
    python <- check_or_install_python()
  }

  if (!reticulate::virtualenv_exists(envname = envname)) {
    log_info("Creating Python virtualenv: %s", envname)

    tryCatch(
      expr = {
        reticulate::virtualenv_create(
          envname = envname,
          python = python
        )
        log_success("Virtualenv created successfully")
      },
      error = function(e) {
        log_error("Creating Python virtualenv failed: %s", e$message)
        log_info(
          "Retrying with clean Python install (version %s)",
          rescue_python_version
        )

        tryCatch(
          {
            python <- reticulate::install_python(
              version = rescue_python_version
            )
            reticulate::virtualenv_create(
              envname = envname,
              python = python
            )
            log_success("Virtualenv created with rescue Python")
          },
          error = function(e2) {
            stop(
              "Failed to create virtualenv even with rescue Python: ",
              conditionMessage(e2),
              call. = FALSE
            )
          }
        )
      }
    )
  } else {
    log_info("Using existing Python virtualenv: %s", envname)
  }

  log_info("Installing RDKit in virtualenv: %s", envname)

  tryCatch(
    {
      reticulate::virtualenv_install(
        envname = envname,
        python = python,
        packages = "rdkit",
        ignore_installed = TRUE
      )
      log_success("RDKit installed successfully")
    },
    error = function(e) {
      log_error("Failed to install RDKit: %s", e$message)
      stop(
        "Failed to install RDKit in virtualenv: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  invisible(NULL)
}

#' Verify Package Installation is Complete
#'
#' @description Checks that package is not only installed but can be loaded.
#'     This catches incomplete or corrupted installations that may occur on Windows.
#'
#' @param package Character package name
#'
#' @return Logical TRUE if valid, FALSE otherwise
#' @keywords internal
verify_package_installation <- function(package) {
  # Check namespace exists
  if (!requireNamespace(package, quietly = TRUE)) {
    log_error("Package '%s' namespace not found", package)
    return(FALSE)
  }

  # Check DESCRIPTION file exists and is readable
  pkg_path <- tryCatch(
    find.package(package, quiet = TRUE),
    error = function(e) character(0)
  )

  if (length(pkg_path) == 0) {
    log_error("Cannot find package path for '%s'", package)
    return(FALSE)
  }

  desc_file <- file.path(pkg_path, "DESCRIPTION")
  if (!file.exists(desc_file)) {
    log_error(
      "DESCRIPTION file missing for package '%s' at %s",
      package,
      pkg_path
    )
    return(FALSE)
  }

  # Try to read DESCRIPTION
  desc_ok <- tryCatch(
    {
      desc <- read.dcf(desc_file)
      !is.null(desc) && nrow(desc) > 0
    },
    error = function(e) {
      log_error("Cannot read DESCRIPTION for '%s': %s", package, e$message)
      FALSE
    },
    warning = function(w) {
      log_error("Warning reading DESCRIPTION for '%s': %s", package, w$message)
      FALSE
    }
  )

  if (!desc_ok) {
    return(FALSE)
  }

  # Try to actually load the namespace
  load_ok <- tryCatch(
    {
      loadNamespace(package)
      TRUE
    },
    error = function(e) {
      log_error("Cannot load namespace for '%s': %s", package, e$message)
      FALSE
    }
  )

  if (!load_ok) {
    return(FALSE)
  }

  log_success("Package '%s' installation verified successfully", package)
  return(TRUE)
}

#' Attempt Package Installation
#'
#' @description Internal helper to attempt R package installation with error handling.
#'     Implements Single Responsibility Principle.
#'
#' @param package Character package name
#' @param repos Character vector of repositories
#' @param dependencies Logical dependencies flag
#' @param from_source Logical whether to install from source
#'
#' @return Logical TRUE if successful, FALSE otherwise
#' @keywords internal
try_install_package <- function(
  package,
  repos,
  dependencies,
  from_source = FALSE
) {
  # Check if package was already installed before attempt
  was_installed <- requireNamespace(package, quietly = TRUE)

  tryCatch(
    expr = {
      install_type <- if (from_source) "source" else "binary"
      log_info(
        "Installing R package: %s (from %s)",
        package,
        install_type
      )

      # Capture warnings as well as errors
      result <- tryCatch(
        {
          pkg_type <- if (from_source) "source" else getOption("pkgType")
          utils::install.packages(
            package,
            repos = repos,
            dependencies = dependencies,
            INSTALL_opts = c("--no-lock", "--no-test-load"),
            type = pkg_type
          )
          TRUE
        },
        warning = function(w) {
          # Some installation failures only produce warnings
          if (
            grepl(
              "package.*not available|is not available",
              w$message,
              ignore.case = TRUE
            )
          ) {
            log_warn("Package not available: %s", w$message)
            return(FALSE)
          }
          TRUE
        }
      )

      # If warning handler already returned FALSE, bail out early
      if (isFALSE(result)) {
        return(FALSE)
      }

      # Verify package was actually installed
      is_now_installed <- requireNamespace(package, quietly = TRUE)

      if (!was_installed && !is_now_installed) {
        log_error(
          "Installation failed: package '%s' not found in repositories",
          package
        )
        return(FALSE)
      }

      log_success("Successfully installed R package: %s", package)
      return(TRUE)
    },
    error = function(e) {
      log_error("Installation failed: %s", e$message)
      return(FALSE)
    }
  )
}

#' @title Install TIMA Package and Dependencies
#'
#' @description Installs or updates the TIMA package from r-universe and sets up
#'     a Python virtual environment with RDKit for chemical structure processing.
#'     This function always installs/updates the R package to ensure you have the
#'     latest version, while the Python environment is only created if it doesn't
#'     already exist (idempotent).
#'
#' @details
#' The installation process:
#' \itemize{
#'   \item Validates all input parameters
#'   \item Detects the operating system and shows relevant instructions
#'   \item **Always installs/updates the R package** from r-universe (to get latest updates)
#'   \item Falls back to source installation if binary fails
#'   \item Verifies installation is complete and package can be loaded
#'   \item Checks for Python or installs Miniconda as fallback
#'   \item Creates a Python virtual environment (tima-env) **only if it doesn't exist**
#'   \item Installs RDKit in the virtual environment (skipped if already present)
#'   \item Copies the TIMA backbone files
#'   \item Cleans up any existing targets pipeline
#' }
#'
#' @section Idempotency:
#' - **R package**: Always reinstalled/updated (ensures latest version from r-universe)
#' - **Python environment**: Only created if missing (idempotent - safe to call multiple times)
#'
#' @include copy_backbone.R
#'
#' @param package Character string name of the package to install (default: "tima")
#' @param repos Character vector of repository URLs for install.packages.
#'     Default includes r-universe, Bioconductor, and CRAN.
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
#' # Standard installation (updates package, checks Python env)
#' install()
#'
#' # Install with custom repositories
#' install(repos = c("https://cloud.r-project.org"))
#'
#' # Install without dependencies (not recommended)
#' install(dependencies = FALSE)
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
  # Input Validation ----
  validate_install_inputs(
    package = package,
    repos = repos,
    dependencies = dependencies,
    test = test
  )

  # Test mode: shortcut - no system / python / R package side effects ----
  if (isTRUE(test)) {
    message(
      "Test mode: skipping real installation steps for package '",
      package,
      "'."
    )
    return(invisible(NULL))
  }

  log_info("Starting installation/update of '%s'", package)

  # System Detection and Messages ----
  system <- Sys.info()[["sysname"]]
  show_system_messages(system = system, test = test)

  # Python Setup ----
  log_debug("Checking Python environment")
  python <- check_or_install_python(test = test)

  # Windows-specific: Force clean removal first to avoid corruption ----
  if (system == "Windows") {
    log_debug("Windows detected: performing clean removal first")
    tryCatch(
      {
        remove.packages(package)
        # Also manually remove directory in case of corruption
        lib_path <- .libPaths()[1]
        pkg_dir <- file.path(lib_path, package)
        if (dir.exists(pkg_dir)) {
          log_debug("Removing existing package directory: %s", pkg_dir)
          unlink(pkg_dir, recursive = TRUE, force = TRUE)
          Sys.sleep(1) # Give Windows time to release file locks
        }
      },
      error = function(e) {
        log_debug(
          "Clean removal step (expected if not installed): %s",
          e$message
        )
      }
    )
  }

  # R Package Installation (Always Update) ----
  log_info("Installing/updating R package '%s' from r-universe", package)
  log_debug("This ensures you have the latest version with all updates")

  success <- try_install_package(
    package = package,
    repos = repos,
    dependencies = dependencies,
    from_source = FALSE
  )

  if (!success) {
    log_warn("Binary installation failed. Retrying from source.")
    success <- try_install_package(
      package = package,
      repos = repos,
      dependencies = dependencies,
      from_source = TRUE
    )
  }

  if (!success) {
    log_fatal("All installation attempts failed")
    stop(
      "Failed to install package '",
      package,
      "'. Please check the error messages above.",
      call. = FALSE
    )
  }

  # Verify Installation is Complete ----
  log_info("Verifying package installation integrity")

  if (!verify_package_installation(package)) {
    log_fatal("Package installed but is corrupted or incomplete")
    stop(
      "Package '",
      package,
      "' installation is incomplete or corrupted. ",
      "This may be due to file locking, path issues, or download interruption. ",
      "On Windows, try: (1) closing RStudio/R, (2) manually deleting the package ",
      "directory, (3) running the installation again.",
      call. = FALSE
    )
  }

  # Python Virtual Environment Setup (Idempotent) ----
  log_info("Configuring Python virtual environment (idempotent)")
  setup_virtualenv(envname = "tima-env", python = python)

  # Post-Installation Setup ----
  log_info("Running post-installation setup")

  tryCatch(
    {
      loadNamespace("tima")
      copy_backbone()
      log_success("Backbone files copied")
    },
    error = function(e) {
      log_warn("Failed to copy backbone files: %s", e$message)
    }
  )

  tryCatch(
    {
      targets::tar_destroy(ask = FALSE)
      log_success("Targets pipeline cleaned")
    },
    error = function(e) {
      log_warn("Failed to clean targets pipeline: %s", e$message)
    }
  )

  log_success("Installation of '%s' completed successfully!", package)

  invisible(NULL)
}
