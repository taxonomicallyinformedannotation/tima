#' Validate Install Function Inputs
#'
#' @description Internal helper to validate all input parameters for
#'     install_tima().
#'
#' @param package [character] Single non-empty package name
#' @param repos [character] Vector of repository URLs
#' @param dependencies [logical] Whether to install dependencies
#'
#' @return NULL (stops on validation error)
#' @keywords internal
validate_install_inputs <- function(package, repos, dependencies) {
  if (!is.character(package) || length(package) != 1L || nchar(package) == 0L) {
    cli::cli_abort(
      "package must be a single non-empty character string, got {.val {if (is.null(package)) 'NULL' else class(package)[1]}}",
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (!is.character(repos) || length(repos) == 0L) {
    cli::cli_abort(
      "repos must be a non-empty character vector",
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (any(nchar(repos) == 0L)) {
    cli::cli_abort(
      "all repository URLs must be non-empty strings",
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (!is.logical(dependencies) || length(dependencies) != 1L) {
    cli::cli_abort(
      "dependencies must be a single logical value (TRUE or FALSE)",
      class = c("tima_validation_error", "tima_error")
    )
  }

  invisible(NULL)
}

.require_namespace <- function(package, quietly = TRUE) {
  requireNamespace(package, quietly = quietly)
}

.find_package_paths <- function(package, quiet = TRUE) {
  find.package(package, quiet = quiet)
}

.path_exists <- function(path) {
  file.exists(path)
}

.read_description_dcf <- function(path) {
  read.dcf(path)
}

.load_namespace <- function(package) {
  loadNamespace(package)
}

.sys_which <- function(command) {
  Sys.which(command)
}

.miniconda_path <- function() {
  reticulate::miniconda_path()
}

.install_miniconda <- function() {
  reticulate::install_miniconda()
}

.virtualenv_exists <- function(envname) {
  reticulate::virtualenv_exists(envname = envname)
}

.virtualenv_create <- function(envname, python) {
  reticulate::virtualenv_create(envname = envname, python = python)
}

.virtualenv_install <- function(envname, packages) {
  reticulate::virtualenv_install(envname = envname, packages = packages)
}

.py_install <- function(packages) {
  reticulate::py_install(packages = packages)
}

.install_packages <- function(package, repos, dependencies, type, ...) {
  utils::install.packages(
    package,
    repos = repos,
    dependencies = dependencies,
    type = type,
    ...
  )
}

.tar_destroy <- function() {
  targets::tar_destroy(ask = FALSE)
}

#' Display System-Specific Installation Messages
#'
#' @description Internal helper to show OS-specific installation instructions.
#'
#' @param system [character] OS name from Sys.info()
#'
#' @return NULL (side effect: logging)
#' @keywords internal
show_system_messages <- function(system) {
  log_info("Detected operating system: %s", system)

  if (system == "Windows") {
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
#'     Miniconda as fallback if needed.
#'
#' @return Character path to Python executable
#' @keywords internal
check_or_install_python <- function() {
  python <- .sys_which("python3")

  if (nzchar(python)) {
    log_info("System Python found at: %s", python)
    return(python)
  }

  log_warn("System Python not found. Installing Miniconda...")

  minipath <- .miniconda_path()
  if (!.path_exists(minipath)) {
    .install_miniconda()
  }

  python_path <- if (Sys.info()[["sysname"]] == "Windows") {
    file.path(minipath, "python.exe")
  } else {
    file.path(minipath, "bin", "python")
  }

  if (!.path_exists(python_path)) {
    cli::cli_abort(
      "python executable not found at {.path {python_path}}",
      class = c("tima_runtime_error", "tima_error")
    )
  }

  log_info("Using Miniconda Python at: %s", python_path)
  python_path
}

#' Setup Python Virtual Environment
#'
#' @description Internal helper to create and configure Python virtualenv with dependencies.
#'
#' @param envname Character name of virtual environment
#' @param python Character path to Python executable
#'
#' @return NULL (side effect: creates virtualenv and installs packages)
#' @keywords internal
setup_virtualenv <- function(envname = "tima-env", python = NULL) {
  if (is.null(python)) {
    python <- check_or_install_python()
  }

  if (!.virtualenv_exists(envname = envname)) {
    log_info("Creating Python virtualenv: %s", envname)

    tryCatch(
      {
        .virtualenv_create(envname = envname, python = python)
        log_success("Virtualenv created successfully")
      },
      error = function(e) {
        log_error("Failed to create virtualenv: %s", e$message)
        cli::cli_abort(
          "failed to create python virtualenv",
          class = c("tima_runtime_error", "tima_error")
        )
      }
    )
  } else {
    log_info("Using existing virtualenv: %s", envname)
  }

  log_info("Installing dependencies...")

  tryCatch(
    {
      .virtualenv_install(
        envname = envname,
        packages = c("rdkit", "chembl-structure-pipeline")
      )
      ## Additional safety
      .py_install(
        packages = c("rdkit", "chembl-structure-pipeline")
      )
      log_success("Dependencies installed successfully")
    },
    error = function(e) {
      log_error("Failed to install dependencies: %s", e$message)
      cli::cli_abort(
        "failed to install dependencies in virtualenv",
        class = c("tima_runtime_error", "tima_error")
      )
    }
  )

  invisible(NULL)
}

#' Verify Package Installation is Complete
#'
#' @description Robust validation that the package is fully installed,
#'   not corrupted, and actually loadable. Avoids false positives from
#'   leftover / half-installed directories in Windows or CI paths.
#'
#' @param package Character package name
#'
#' @return Logical TRUE if valid, FALSE otherwise
#' @keywords internal
verify_package_installation <- function(package) {
  # 1. Namespace reachable?
  if (!.require_namespace(package, quietly = TRUE)) {
    log_error("Namespace for '%s' not found.", package)
    return(FALSE)
  }

  # 2. Attempt to resolve ALL possible paths
  pkg_paths <- .find_package_paths(package, quiet = TRUE)

  # find.package returns character(0) on failure
  if (length(pkg_paths) == 0) {
    log_error("find.package() could not resolve path for '%s'.", package)
    return(FALSE)
  }

  # 3. Filter out invalid paths (empty, nonexistent, no DESCRIPTION)
  # Helper to check if a path is valid for installation
  .is_valid_install_path <- function(p) {
    .path_exists(p) && .path_exists(file.path(p, "DESCRIPTION"))
  }

  valid_paths <- pkg_paths[
    vapply(
      X = pkg_paths,
      FUN = .is_valid_install_path,
      logical(1L)
    )
  ]

  if (length(valid_paths) == 0) {
    log_error(
      "No valid installation directory for '%s'. " |>
        paste(
          "Possible broken R library structure. All found paths:\n  - ",
          paste(pkg_paths, collapse = "\n  - ")
        )
    )
    return(FALSE)
  }

  # Prefer the first good match
  pkg_path <- valid_paths[[1L]]
  desc_file <- file.path(pkg_path, "DESCRIPTION")

  # 4. Try reading DESCRIPTION robustly
  desc_ok <- tryCatch(
    {
      df <- .read_description_dcf(desc_file)
      isTRUE(nrow(df) > 0)
    },
    error = function(e) {
      log_error(
        "Could not read DESCRIPTION for '%s' at '%s': %s",
        package,
        pkg_path,
        e$message
      )
      FALSE
    },
    warning = function(w) {
      log_warn(
        "Warning while reading DESCRIPTION for '%s' at '%s': %s",
        package,
        pkg_path,
        w$message
      )
      FALSE
    }
  )

  if (!desc_ok) {
    return(FALSE)
  }

  # 5. Validate namespace loading
  load_ok <- tryCatch(
    {
      .load_namespace(package)
      TRUE
    },
    error = function(e) {
      log_error(
        "Namespace load failed for '%s' at '%s': %s",
        package,
        pkg_path,
        e$message
      )
      FALSE
    }
  )

  if (!load_ok) {
    return(FALSE)
  }

  log_success(
    "Package '%s' installation verified successfully at: %s",
    package,
    pkg_path
  )
  TRUE
}


#' Attempt Package Installation
#'
#' @description Internal helper to attempt R package installation with error handling.
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
  was_installed <- .require_namespace(package, quietly = TRUE)

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
          .install_packages(
            package,
            repos = repos,
            dependencies = dependencies,
            type = pkg_type,
            INSTALL_opts = c("--no-lock", "--no-test-load")
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
      is_now_installed <- .require_namespace(package, quietly = TRUE)

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
      FALSE
    }
  )
}

#' @title Install TIMA Package and Dependencies
#'
#' @description Installs or updates the TIMA package from r-universe and sets up
#'     a Python virtual environment with dependencies.
#'
#' @param package [character] Name of the package (default: "tima")
#' @param repos [character] Vector of repository URLs
#' @param dependencies [logical] Whether to install dependencies (default: TRUE)
#'
#' @return NULL (invisibly). Installs packages and sets up Python environment as
#'     side effects.
#'
#' @family workflow
#'
#' @export
#'
#' @examples
#' \dontrun{
#' install_tima()
#' }
install_tima <- function(
  package = "tima",
  repos = c(
    "https://taxonomicallyinformedannotation.r-universe.dev",
    "https://bioc.r-universe.dev",
    "https://cloud.r-project.org"
  ),
  dependencies = TRUE
) {
  validate_install_inputs(
    package = package,
    repos = repos,
    dependencies = dependencies
  )

  log_info("Starting installation of '%s'", package)

  # System info
  system <- Sys.info()[["sysname"]]
  log_info("Operating system: %s", system)

  # Python setup
  log_info("Setting up Python environment")
  python <- check_or_install_python()

  # R Package installation
  log_info("Installing R package '%s'", package)

  tryCatch(
    {
      .install_packages(
        package,
        repos = repos,
        dependencies = dependencies,
        type = if (system == "Linux") "source" else "binary"
      )
      log_success("R package installed successfully")
    },
    error = function(e) {
      log_error("Installation failed: %s", e$message)
      cli::cli_abort(
        c(
          "failed to install package {.val {package}}",
          "x" = e$message
        ),
        class = c("tima_runtime_error", "tima_error")
      )
    }
  )

  # Verify installation
  if (!.require_namespace(package, quietly = TRUE)) {
    cli::cli_abort(
      "package {.val {package}} not found after installation",
      class = c("tima_runtime_error", "tima_error")
    )
  }

  # Python virtualenv setup
  log_info("Configuring Python virtual environment")
  setup_virtualenv(envname = "tima-env", python = python)

  # Post-installation setup
  log_info("Running post-installation setup")

  tryCatch(
    {
      .load_namespace(package = package)
      copy_backbone()
      log_success("Backbone files copied")
    },
    error = function(e) {
      log_warn("Failed to copy backbone files: %s", e$message)
    }
  )

  tryCatch(
    {
      .tar_destroy()
      log_success("Targets pipeline cleaned")
    },
    error = function(e) {
      log_debug("No targets pipeline to clean: %s", e$message)
    }
  )

  log_success("Installation of '%s' completed!", package)
  invisible(NULL)
}

#' @title Install TIMA Package and Dependencies (DEPRECATED)
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' **DEPRECATED:** Use [install_tima()] instead. `install()` will be removed
#' in a future version. The generic name `install` risks masking other packages.
#'
#' @inheritParams install_tima
#'
#' @return NULL (invisibly).
#'
#' @family workflow
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # DEPRECATED — use install_tima() instead
#' install_tima()
#' }
install <- function(
  package = "tima",
  repos = c(
    "https://taxonomicallyinformedannotation.r-universe.dev",
    "https://bioc.r-universe.dev",
    "https://cloud.r-project.org"
  ),
  dependencies = TRUE
) {
  lifecycle::deprecate_warn(
    "2.13.0",
    "install()",
    "install_tima()"
  )
  install_tima(
    package = package,
    repos = repos,
    dependencies = dependencies
  )
}
