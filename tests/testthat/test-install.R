# Test Suite: install (deterministic integration-focused) ----

library(testthat)

.make_repos <- function() {
  c(
    "https://taxonomicallyinformedannotation.r-universe.dev",
    "https://cloud.r-project.org",
    "https://cran.rstudio.com"
  )
}

# ---- Input validation ----

test_that("validate_install_inputs rejects invalid package values", {
  expect_error(
    validate_install_inputs(NULL, .make_repos(), dependencies = TRUE),
    class = "tima_validation_error"
  )
  expect_error(
    validate_install_inputs("", .make_repos(), dependencies = TRUE),
    class = "tima_validation_error"
  )
  expect_error(
    validate_install_inputs(NA_character_, .make_repos(), dependencies = TRUE),
    class = "tima_validation_error"
  )
  expect_error(
    validate_install_inputs(c("a", "b"), .make_repos(), dependencies = TRUE),
    class = "tima_validation_error"
  )
  expect_error(
    validate_install_inputs(123, .make_repos(), dependencies = TRUE),
    class = "tima_validation_error"
  )
})

test_that("validate_install_inputs rejects invalid repos and dependencies", {
  expect_error(
    validate_install_inputs("tima", character(0), dependencies = TRUE),
    class = "tima_validation_error"
  )
  expect_error(
    validate_install_inputs(
      "tima",
      c("https://cran.r-project.org", ""),
      dependencies = TRUE
    ),
    class = "tima_validation_error"
  )
  expect_error(
    validate_install_inputs(
      "tima",
      c("https://cran.r-project.org", NA_character_),
      dependencies = TRUE
    ),
    class = "tima_validation_error"
  )
  expect_error(
    validate_install_inputs("tima", 123, dependencies = TRUE),
    class = "tima_validation_error"
  )
  expect_error(
    validate_install_inputs("tima", .make_repos(), dependencies = "yes"),
    class = "tima_validation_error"
  )
  expect_error(
    validate_install_inputs("tima", .make_repos(), dependencies = NA),
    class = "tima_validation_error"
  )
  expect_error(
    validate_install_inputs(
      "tima",
      .make_repos(),
      dependencies = c(TRUE, FALSE)
    ),
    class = "tima_validation_error"
  )
})

test_that("validate_install_inputs accepts valid inputs", {
  expect_silent(validate_install_inputs(
    package = "tima",
    repos = .make_repos(),
    dependencies = TRUE
  ))
  expect_silent(validate_install_inputs(
    package = "tima",
    repos = .make_repos(),
    dependencies = FALSE
  ))
})

# ---- Lightweight helpers ----

test_that("check_or_install_python returns an executable path", {
  path <- check_or_install_python()
  expect_type(path, "character")
  expect_true(length(path) == 1L)
  expect_true(nzchar(path))
})

test_that("check_or_install_python returns system python immediately when available", {
  path <- with_mocked_bindings(
    .sys_which = function(command) "/usr/bin/python3",
    log_info = function(...) invisible(NULL),
    check_or_install_python()
  )

  expect_identical(path, "/usr/bin/python3")
})

test_that("check_or_install_python installs miniconda fallback when needed", {
  state <- new.env(parent = emptyenv())
  state$installed <- FALSE

  path <- with_mocked_bindings(
    .sys_which = function(command) "",
    .miniconda_path = function() "/opt/miniconda",
    .path_exists = function(path) {
      if (identical(path, "/opt/miniconda")) {
        return(state$installed)
      }
      identical(path, "/opt/miniconda/bin/python")
    },
    .install_miniconda = function() {
      state$installed <- TRUE
      invisible(NULL)
    },
    log_warn = function(...) invisible(NULL),
    log_info = function(...) invisible(NULL),
    check_or_install_python()
  )

  expect_true(state$installed)
  expect_identical(path, "/opt/miniconda/bin/python")
})

test_that("check_or_install_python errors when fallback python is still missing", {
  expect_error(
    with_mocked_bindings(
      .sys_which = function(command) "",
      .miniconda_path = function() "/missing/miniconda",
      .path_exists = function(path) FALSE,
      .install_miniconda = function() invisible(NULL),
      log_warn = function(...) invisible(NULL),
      check_or_install_python()
    ),
    class = "tima_runtime_error"
  )
})

test_that("setup_virtualenv creates env and installs packages", {
  calls <- new.env(parent = emptyenv())
  calls$create <- NULL
  calls$install <- NULL
  calls$py_install <- NULL

  expect_invisible(
    with_mocked_bindings(
      .virtualenv_exists = function(envname) FALSE,
      .virtualenv_create = function(envname, python) {
        calls$create <- list(envname = envname, python = python)
        invisible(NULL)
      },
      .virtualenv_install = function(envname, packages) {
        calls$install <- list(envname = envname, packages = packages)
        invisible(NULL)
      },
      .py_install = function(packages) {
        calls$py_install <- packages
        invisible(NULL)
      },
      log_info = function(...) invisible(NULL),
      log_success = function(...) invisible(NULL),
      setup_virtualenv(envname = "demo-env", python = "/usr/bin/python3")
    )
  )

  expect_identical(calls$create$envname, "demo-env")
  expect_identical(calls$create$python, "/usr/bin/python3")
  expect_equal(calls$install$packages, c("rdkit", "chembl-structure-pipeline"))
  expect_equal(calls$py_install, c("rdkit", "chembl-structure-pipeline"))
})

test_that("setup_virtualenv reuses existing env and still installs dependencies", {
  calls <- new.env(parent = emptyenv())
  calls$create <- 0L
  calls$install <- 0L

  expect_invisible(
    with_mocked_bindings(
      .virtualenv_exists = function(envname) TRUE,
      .virtualenv_create = function(envname, python) {
        calls$create <- calls$create + 1L
        invisible(NULL)
      },
      .virtualenv_install = function(envname, packages) {
        calls$install <- calls$install + 1L
        invisible(NULL)
      },
      .py_install = function(packages) invisible(NULL),
      log_info = function(...) invisible(NULL),
      log_success = function(...) invisible(NULL),
      setup_virtualenv(envname = "demo-env", python = "/usr/bin/python3")
    )
  )

  expect_identical(calls$create, 0L)
  expect_identical(calls$install, 1L)
})

test_that("setup_virtualenv surfaces create and install failures", {
  expect_error(
    with_mocked_bindings(
      .virtualenv_exists = function(envname) FALSE,
      .virtualenv_create = function(envname, python) stop("create failed"),
      log_info = function(...) invisible(NULL),
      log_success = function(...) invisible(NULL),
      log_error = function(...) invisible(NULL),
      setup_virtualenv(envname = "bad-env", python = "/usr/bin/python3")
    ),
    class = "tima_runtime_error"
  )

  expect_error(
    with_mocked_bindings(
      .virtualenv_exists = function(envname) TRUE,
      .virtualenv_install = function(envname, packages) stop("install failed"),
      .py_install = function(packages) invisible(NULL),
      log_info = function(...) invisible(NULL),
      log_success = function(...) invisible(NULL),
      log_error = function(...) invisible(NULL),
      setup_virtualenv(envname = "bad-env", python = "/usr/bin/python3")
    ),
    class = "tima_runtime_error"
  )
})

test_that("install_tima input validation fails fast", {
  expect_error(
    install_tima(package = "", repos = .make_repos(), dependencies = TRUE),
    class = "tima_validation_error"
  )
  expect_error(
    install_tima(package = "tima", repos = character(), dependencies = TRUE),
    class = "tima_validation_error"
  )
  expect_error(
    install_tima(
      package = "tima",
      repos = .make_repos(),
      type = "unsupported_type",
      dependencies = TRUE
    ),
    class = "tima_validation_error"
  )
  expect_error(
    install_tima(package = "tima", repos = .make_repos(), dependencies = "yes"),
    class = "tima_validation_error"
  )
})

test_that("install_tima orchestrates installation workflow", {
  calls <- new.env(parent = emptyenv())
  calls$install <- NULL
  calls$setup <- NULL
  calls$loaded <- NULL
  calls$copied <- 0L
  calls$destroyed <- 0L

  expect_invisible(
    with_mocked_bindings(
      log_info = function(...) invisible(NULL),
      log_success = function(...) invisible(NULL),
      log_warn = function(...) invisible(NULL),
      log_debug = function(...) invisible(NULL),
      check_or_install_python = function() "/usr/bin/python3",
      .install_packages = function(package, repos, dependencies, type, ...) {
        calls$install <- list(
          package = package,
          repos = repos,
          dependencies = dependencies,
          type = type
        )
        invisible(NULL)
      },
      .require_namespace = function(package, quietly = TRUE) TRUE,
      setup_virtualenv = function(envname = "tima-env", python = NULL) {
        calls$setup <- list(envname = envname, python = python)
        invisible(NULL)
      },
      .load_namespace = function(package) {
        calls$loaded <- package
        invisible(TRUE)
      },
      copy_backbone = function() {
        calls$copied <- calls$copied + 1L
        invisible(NULL)
      },
      .tar_destroy = function() {
        calls$destroyed <- calls$destroyed + 1L
        invisible(NULL)
      },
      install_tima(
        package = "tima",
        repos = .make_repos(),
        dependencies = FALSE
      )
    )
  )

  expect_identical(calls$install$package, "tima")
  expect_identical(calls$setup$envname, "tima-env")
  expect_identical(calls$setup$python, "/usr/bin/python3")
  expect_identical(calls$loaded, "tima")
  expect_identical(calls$copied, 1L)
  expect_identical(calls$destroyed, 1L)
})

test_that("install_tima surfaces installation failures and missing namespace", {
  expect_error(
    with_mocked_bindings(
      log_info = function(...) invisible(NULL),
      log_success = function(...) invisible(NULL),
      log_error = function(...) invisible(NULL),
      check_or_install_python = function() "/usr/bin/python3",
      .install_packages = function(...) stop("cannot install"),
      install_tima(
        package = "tima",
        repos = .make_repos(),
        dependencies = FALSE
      )
    ),
    class = "tima_runtime_error"
  )

  expect_error(
    with_mocked_bindings(
      log_info = function(...) invisible(NULL),
      log_success = function(...) invisible(NULL),
      check_or_install_python = function() "/usr/bin/python3",
      .install_packages = function(...) invisible(NULL),
      .require_namespace = function(package, quietly = TRUE) FALSE,
      install_tima(
        package = "tima",
        repos = .make_repos(),
        dependencies = FALSE
      )
    ),
    class = "tima_runtime_error"
  )
})

test_that("validate_install_inputs normalizes platform pkgType variants and rejects invalid type", {
  expect_null(validate_install_inputs(
    package = "tima",
    repos = .make_repos(),
    type = c("mac.binary.big-sur-arm64", "source"),
    dependencies = TRUE
  ))
  expect_null(validate_install_inputs(
    package = "tima",
    repos = .make_repos(),
    type = "win.binary",
    dependencies = TRUE
  ))
  expect_null(validate_install_inputs(
    package = "tima",
    repos = .make_repos(),
    type = "source-custom",
    dependencies = TRUE
  ))
  expect_null(validate_install_inputs(
    package = "tima",
    repos = .make_repos(),
    type = "both",
    dependencies = FALSE
  ))
  expect_error(
    validate_install_inputs(
      package = "tima",
      repos = .make_repos(),
      type = "unsupported_type",
      dependencies = TRUE
    ),
    class = "tima_validation_error"
  )
  expect_error(
    validate_install_inputs(
      package = "tima",
      repos = .make_repos(),
      type = NA_character_,
      dependencies = TRUE
    ),
    class = "tima_validation_error"
  )
})

test_that("install_tima passes normalized type to install.packages", {
  rec <- new.env(parent = emptyenv())
  rec$type <- NULL

  with_mocked_bindings(
    log_info = function(...) invisible(NULL),
    log_success = function(...) invisible(NULL),
    log_warn = function(...) invisible(NULL),
    log_debug = function(...) invisible(NULL),
    check_or_install_python = function() "/usr/bin/python3",
    .install_packages = function(package, repos, dependencies, type, ...) {
      rec$type <- type
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) TRUE,
    setup_virtualenv = function(...) invisible(NULL),
    .load_namespace = function(package) invisible(TRUE),
    copy_backbone = function() invisible(NULL),
    .tar_destroy = function() invisible(NULL),
    install_tima(
      package = "tima",
      repos = .make_repos(),
      type = "mac.binary.big-sur-arm64",
      dependencies = FALSE
    )
  )

  expect_identical(rec$type, "binary")
})

test_that("setup_virtualenv resolves python via check_or_install_python when python is NULL", {
  resolved <- new.env(parent = emptyenv())
  resolved$called <- 0L
  resolved$created_with <- NULL

  with_mocked_bindings(
    check_or_install_python = function() {
      resolved$called <- resolved$called + 1L
      "/mock/python3"
    },
    .virtualenv_exists = function(envname) FALSE,
    .virtualenv_create = function(envname, python) {
      resolved$created_with <- python
      invisible(NULL)
    },
    .virtualenv_install = function(envname, packages) invisible(NULL),
    .py_install = function(packages) invisible(NULL),
    log_info = function(...) invisible(NULL),
    log_success = function(...) invisible(NULL),
    setup_virtualenv(envname = "null-py-env", python = NULL)
  )

  expect_identical(resolved$called, 1L)
  expect_identical(resolved$created_with, "/mock/python3")
})

test_that("install_tima tolerates post-install copy_backbone and tar_destroy errors", {
  rec <- new.env(parent = emptyenv())
  rec$warns <- character(0)
  rec$debugs <- character(0)

  expect_invisible(
    with_mocked_bindings(
      log_info = function(...) invisible(NULL),
      log_success = function(...) invisible(NULL),
      log_warn = function(fmt, ...) {
        rec$warns <- c(rec$warns, sprintf(fmt, ...))
        invisible(NULL)
      },
      log_debug = function(fmt, ...) {
        rec$debugs <- c(rec$debugs, sprintf(fmt, ...))
        invisible(NULL)
      },
      check_or_install_python = function() "/usr/bin/python3",
      .install_packages = function(...) invisible(NULL),
      .require_namespace = function(package, quietly = TRUE) TRUE,
      setup_virtualenv = function(...) invisible(NULL),
      .load_namespace = function(package) invisible(TRUE),
      copy_backbone = function() stop("disk full"),
      .tar_destroy = function() stop("no _targets store"),
      install_tima(
        package = "tima",
        repos = .make_repos(),
        dependencies = FALSE
      )
    )
  )
  expect_true(any(grepl("Failed to copy backbone files: disk full", rec$warns)))
  expect_true(any(grepl(
    "No targets pipeline to clean: no _targets store",
    rec$debugs
  )))
})

test_that("install emits lifecycle deprecation warning and delegates to install_tima", {
  withr::local_options(lifecycle_verbosity = "warning")
  rec <- new.env(parent = emptyenv())
  rec$args <- NULL

  with_mocked_bindings(
    install_tima = function(package, repos, type, dependencies) {
      rec$args <- list(
        package = package,
        repos = repos,
        type = type,
        dependencies = dependencies
      )
      invisible(NULL)
    },
    {
      expect_warning(
        install(
          package = "tima",
          repos = "https://cran.r-universe.dev",
          type = "source",
          dependencies = FALSE
        ),
        "install_tima"
      )
      expect_identical(
        rec$args,
        list(
          package = "tima",
          repos = "https://cran.r-universe.dev",
          type = "source",
          dependencies = FALSE
        )
      )

      # Also works when evaluated in a non-namespace package environment (e.g. covr)
      fn_pkg_env <- install
      ns_list <- as.list(asNamespace("tima"), all.names = TRUE)
      ns_list[[".__NAMESPACE__."]] <- NULL
      environment(fn_pkg_env) <- list2env(ns_list, parent = globalenv())
      caller <- new.env(parent = globalenv())
      caller$fn_pkg_env <- fn_pkg_env
      expect_warning(
        evalq(
          fn_pkg_env(
            package = "tima",
            repos = "https://cran.r-universe.dev",
            type = "source",
            dependencies = FALSE
          ),
          caller
        ),
        "install_tima"
      )
    }
  )

  # Thin wrapper coverage and Windows python.exe branch in check_or_install_python
  testthat::with_mocked_bindings(
    miniconda_path = function() "/mock/mini",
    install_miniconda = function() "installed_mini",
    virtualenv_exists = function(envname) paste0("exists:", envname),
    virtualenv_create = function(envname, python) {
      paste0("create:", envname, ":", python)
    },
    virtualenv_install = function(envname, packages) {
      paste0("inst:", envname, ":", paste(packages, collapse = ","))
    },
    py_install = function(packages) {
      paste0("py:", paste(packages, collapse = ","))
    },
    .package = "reticulate",
    {
      expect_identical(.miniconda_path(), "/mock/mini")
      expect_identical(.install_miniconda(), "installed_mini")
      expect_identical(.virtualenv_exists("e"), "exists:e")
      expect_identical(.virtualenv_create("e", "py"), "create:e:py")
      expect_identical(.virtualenv_install("e", "p"), "inst:e:p")
      expect_identical(.py_install("p"), "py:p")
    }
  )
  testthat::with_mocked_bindings(
    tar_destroy = function(ask) paste0("destroyed:", ask),
    .package = "targets",
    {
      expect_identical(.tar_destroy(), "destroyed:FALSE")
    }
  )
  win_py <- testthat::with_mocked_bindings(
    .sys_which = function(cmd) "",
    .miniconda_path = function() "C:/miniconda",
    .path_exists = function(p) TRUE,
    log_warn = function(...) invisible(NULL),
    log_info = function(...) invisible(NULL),
    {
      testthat::with_mocked_bindings(
        Sys.info = function() c(sysname = "Windows"),
        .package = "base",
        check_or_install_python()
      )
    }
  )
  expect_identical(win_py, file.path("C:/miniconda", "python.exe"))
})
