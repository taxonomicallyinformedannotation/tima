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
#' @return Character path to Python executable
#' @keywords internal
check_or_install_python <- function() {
  python <- Sys.which("python3")

  if (nzchar(python)) {
    log_info("System Python found at: %s", python)
    return(python)
  }

  log_warn("System Python not found. Installing Miniconda...")

  minipath <- reticulate::miniconda_path()
  if (!file.exists(minipath)) {
    reticulate::install_miniconda()
  }

  python_path <- if (Sys.info()[["sysname"]] == "Windows") {
    file.path(minipath, "python.exe")
  } else {
    file.path(minipath, "bin", "python")
  }

  if (!file.exists(python_path)) {
    stop("Python executable not found at: ", python_path, call. = FALSE)
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
#'
#' @return NULL (side effect: creates virtualenv and installs packages)
#' @keywords internal
setup_virtualenv <- function(envname = "tima-env", python = NULL) {
  if (is.null(python)) {
    python <- check_or_install_python()
  }

  if (!reticulate::virtualenv_exists(envname = envname)) {
    log_info("Creating Python virtualenv: %s", envname)

    tryCatch(
      {
        reticulate::virtualenv_create(envname = envname, python = python)
        log_success("Virtualenv created successfully")
      },
      error = function(e) {
        log_error("Failed to create virtualenv: %s", e$message)
        stop("Failed to create Python virtualenv", call. = FALSE)
      }
    )
  } else {
    log_info("Using existing virtualenv: %s", envname)
  }

  log_info("Installing RDKit...")

  tryCatch(
    {
      reticulate::virtualenv_install(
        envname = envname,
        packages = "rdkit",
        ignore_installed = TRUE
      )
      log_success("RDKit installed successfully")
    },
    error = function(e) {
      log_error("Failed to install RDKit: %s", e$message)
      stop("Failed to install RDKit in virtualenv", call. = FALSE)
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
  if (!requireNamespace(package, quietly = TRUE)) {
    log_error("Namespace for '%s' not found.", package)
    return(FALSE)
  }

  # 2. Attempt to resolve ALL possible paths
  pkg_paths <- suppressWarnings(find.package(package, quiet = TRUE))

  # find.package returns character(0) on failure
  if (length(pkg_paths) == 0) {
    log_error("find.package() could not resolve path for '%s'.", package)
    return(FALSE)
  }

  # 3. Filter out invalid paths (empty, nonexistent, no DESCRIPTION)
  valid_paths <- pkg_paths[
    vapply(
      pkg_paths,
      function(p) {
        file.exists(p) && file.exists(file.path(p, "DESCRIPTION"))
      },
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
  pkg_path <- valid_paths[[1]]
  desc_file <- file.path(pkg_path, "DESCRIPTION")

  # 4. Try reading DESCRIPTION robustly
  desc_ok <- tryCatch(
    {
      df <- read.dcf(desc_file)
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
      loadNamespace(package)
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
#'     a Python virtual environment with RDKit.
#'
#' @param package Character string name of the package (default: "tima")
#' @param repos Character vector of repository URLs
#' @param dependencies Logical whether to install dependencies (default: TRUE)
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
  dependencies = TRUE
) {
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
      utils::install.packages(
        package,
        repos = repos,
        dependencies = dependencies,
        type = if (system == "Linux") "source" else "binary"
      )
      log_success("R package installed successfully")
    },
    error = function(e) {
      log_error("Installation failed: %s", e$message)
      stop("Failed to install package '", package, "'", call. = FALSE)
    }
  )

  # Verify installation
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package '", package, "' not found after installation", call. = FALSE)
  }

  # Python virtualenv setup
  log_info("Configuring Python virtual environment")
  setup_virtualenv(envname = "tima-env", python = python)

  # Post-installation setup
  log_info("Running post-installation setup")

  tryCatch(
    {
      loadNamespace(package = package)
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
      log_debug("No targets pipeline to clean")
    }
  )

  log_success("Installation of '%s' completed!", package)
  invisible(NULL)
}
