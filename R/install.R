#' Validate Install Function Inputs
#'
#' @description Internal helper to validate all input parameters for
#'     install_tima().
#'
#' @include logs_utils.R
#' @include copy_backbone.R
#' @param package [character] Single non-empty package name
#' @param repos [character] Vector of repository URLs
#' @param type [character] Type of install ("source", "binary")
#' @param dependencies [logical] Whether to install dependencies
#'
#' @return NULL (stops on validation error)
#' @keywords internal
validate_install_inputs <- function(
  package,
  repos,
  type = getOption("pkgType"),
  dependencies
) {
  if (
    !is.character(package) ||
      length(package) != 1L ||
      is.na(package) ||
      !nzchar(package)
  ) {
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

  if (anyNA(repos) || !all(nzchar(repos))) {
    cli::cli_abort(
      "all repository URLs must be non-empty strings",
      class = c("tima_validation_error", "tima_error")
    )
  }

  type_norm <- .normalize_install_type(type)
  if (is.na(type_norm)) {
    cli::cli_abort(
      "type must be a single character string, either 'source' or 'binary'",
      class = c("tima_validation_error", "tima_error")
    )
  }

  if (
    !is.logical(dependencies) ||
      length(dependencies) != 1L ||
      is.na(dependencies)
  ) {
    cli::cli_abort(
      "dependencies must be a single logical value (TRUE or FALSE)",
      class = c("tima_validation_error", "tima_error")
    )
  }

  invisible(NULL)
}

.normalize_install_type <- function(type) {
  if (is.null(type) || length(type) == 0L) {
    return(NA_character_)
  }
  t1 <- as.character(type)[[1L]]
  if (is.na(t1) || !nzchar(t1)) {
    return(NA_character_)
  }
  if (t1 %in% c("source", "binary")) {
    return(t1)
  }
  tl <- tolower(t1)
  if (grepl("source", tl)) {
    return("source")
  }
  if (grepl("binary|mac\\.binary|win\\.binary", tl)) {
    return("binary")
  }
  if (identical(tl, "both")) {
    return("source")
  }
  NA_character_
}

.require_namespace <- function(package, quietly = TRUE) {
  requireNamespace(package, quietly = quietly)
}

.path_exists <- function(path) {
  file.exists(path)
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

.install_packages <- function(
  package,
  repos,
  dependencies,
  type = getOption("pkgType"),
  ...
) {
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
#' @description Internal helper to create and configure Python virtualenv with
#'     dependencies.
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

#' @title Install TIMA Package and Dependencies
#'
#' @description Installs or updates the TIMA package from r-universe and sets up
#'     a Python virtual environment with dependencies.
#'
#' @param package [character] Name of the package (default: "tima")
#' @param repos [character] Vector of repository URLs
#' @param type [character]  Type of install ("source", "binary")
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
    "https://cran.r-universe.dev",
    "https://bioc.r-universe.dev"
  ),
  type = getOption("pkgType"),
  dependencies = TRUE
) {
  validate_install_inputs(
    package = package,
    repos = repos,
    type = type,
    dependencies = dependencies
  )
  type <- .normalize_install_type(type)

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
        type = type
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
    "https://cran.r-universe.dev",
    "https://bioc.r-universe.dev"
  ),
  type = getOption("pkgType"),
  dependencies = TRUE
) {
  lifecycle::deprecate_warn(
    "2.13.0",
    "tima::install()",
    "tima::install_tima()"
  )
  install_tima(
    package = package,
    repos = repos,
    type = type,
    dependencies = dependencies
  )
}
