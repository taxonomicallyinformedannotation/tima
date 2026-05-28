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
  expect_error(validate_install_inputs(NULL, .make_repos(), TRUE))
  expect_error(validate_install_inputs("", .make_repos(), TRUE))
  expect_error(validate_install_inputs(c("a", "b"), .make_repos(), TRUE))
  expect_error(validate_install_inputs(123, .make_repos(), TRUE))
})

test_that("validate_install_inputs rejects invalid repos and dependencies", {
  expect_error(validate_install_inputs("tima", character(0), TRUE))
  expect_error(validate_install_inputs(
    "tima",
    c("https://cran.r-project.org", ""),
    TRUE
  ))
  expect_error(validate_install_inputs("tima", 123, TRUE))
  expect_error(validate_install_inputs("tima", .make_repos(), "yes"))
  expect_error(validate_install_inputs("tima", .make_repos(), c(TRUE, FALSE)))
})

test_that("validate_install_inputs accepts valid inputs", {
  expect_silent(validate_install_inputs("tima", .make_repos(), TRUE))
  expect_silent(validate_install_inputs("tima", .make_repos(), FALSE))
})

# ---- Lightweight helpers ----

test_that("show_system_messages handles known and unknown OS names", {
  expect_no_error(show_system_messages("Windows"))
  expect_no_error(show_system_messages("Linux"))
  expect_no_error(show_system_messages("Darwin"))
  expect_no_error(show_system_messages("Unknown"))
})

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
  installed <- FALSE

  path <- with_mocked_bindings(
    .sys_which = function(command) "",
    .miniconda_path = function() "/opt/miniconda",
    .path_exists = function(path) {
      if (identical(path, "/opt/miniconda")) {
        return(installed)
      }
      identical(path, "/opt/miniconda/bin/python")
    },
    .install_miniconda = function() {
      installed <<- TRUE
      invisible(NULL)
    },
    log_warn = function(...) invisible(NULL),
    log_info = function(...) invisible(NULL),
    check_or_install_python()
  )

  expect_true(installed)
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
  calls <- list(create = NULL, install = NULL, py_install = NULL)

  expect_invisible(
    with_mocked_bindings(
      .virtualenv_exists = function(envname) FALSE,
      .virtualenv_create = function(envname, python) {
        calls$create <<- list(envname = envname, python = python)
        invisible(NULL)
      },
      .virtualenv_install = function(envname, packages) {
        calls$install <<- list(envname = envname, packages = packages)
        invisible(NULL)
      },
      .py_install = function(packages) {
        calls$py_install <<- packages
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
  calls <- list(create = 0L, install = 0L)

  expect_invisible(
    with_mocked_bindings(
      .virtualenv_exists = function(envname) TRUE,
      .virtualenv_create = function(envname, python) {
        calls$create <<- calls$create + 1L
        invisible(NULL)
      },
      .virtualenv_install = function(envname, packages) {
        calls$install <<- calls$install + 1L
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

test_that("verify_package_installation succeeds for base package and fails for missing", {
  expect_true(verify_package_installation("stats"))
  expect_false(verify_package_installation("definitelynotapackage12345"))
})

test_that("verify_package_installation handles broken install states", {
  expect_false(
    with_mocked_bindings(
      .require_namespace = function(package, quietly = TRUE) FALSE,
      log_error = function(...) invisible(NULL),
      verify_package_installation("brokenpkg")
    )
  )

  expect_false(
    with_mocked_bindings(
      .require_namespace = function(package, quietly = TRUE) TRUE,
      .find_package_paths = function(package, quiet = TRUE) character(),
      log_error = function(...) invisible(NULL),
      verify_package_installation("brokenpkg")
    )
  )

  expect_false(
    with_mocked_bindings(
      .require_namespace = function(package, quietly = TRUE) TRUE,
      .find_package_paths = function(package, quiet = TRUE) {
        c("/bad/a", "/bad/b")
      },
      .path_exists = function(path) FALSE,
      log_error = function(...) invisible(NULL),
      verify_package_installation("brokenpkg")
    )
  )
})

test_that("verify_package_installation handles DESCRIPTION and namespace failures", {
  expect_false(
    with_mocked_bindings(
      .require_namespace = function(package, quietly = TRUE) TRUE,
      .find_package_paths = function(package, quiet = TRUE) "/good/pkg",
      .path_exists = function(path) TRUE,
      .read_description_dcf = function(path) stop("cannot read"),
      log_error = function(...) invisible(NULL),
      verify_package_installation("brokenpkg")
    )
  )

  expect_false(
    with_mocked_bindings(
      .require_namespace = function(package, quietly = TRUE) TRUE,
      .find_package_paths = function(package, quiet = TRUE) "/good/pkg",
      .path_exists = function(path) TRUE,
      .read_description_dcf = function(path) {
        structure(matrix("x", nrow = 1), class = "matrix")
      },
      .load_namespace = function(package) stop("namespace load failed"),
      log_error = function(...) invisible(NULL),
      verify_package_installation("brokenpkg")
    )
  )
})

# ---- Network/install wrappers (stable failure mode) ----

test_that("try_install_package returns FALSE on unreachable repository", {
  result <- try_install_package(
    package = "somepkgthatdoesnotexist",
    repos = "https://invalid.invalid",
    dependencies = FALSE,
    from_source = FALSE
  )
  expect_false(result)
})

test_that("try_install_package handles successful, unavailable and error install paths", {
  expect_true(
    with_mocked_bindings(
      .require_namespace = local({
        calls <- 0L
        function(package, quietly = TRUE) {
          calls <<- calls + 1L
          calls >= 2L
        }
      }),
      .install_packages = function(...) invisible(NULL),
      log_info = function(...) invisible(NULL),
      log_success = function(...) invisible(NULL),
      log_error = function(...) invisible(NULL),
      try_install_package("pkg", repos = .make_repos(), dependencies = FALSE)
    )
  )

  expect_false(
    with_mocked_bindings(
      .require_namespace = function(package, quietly = TRUE) FALSE,
      .install_packages = function(...) warning("package is not available"),
      log_info = function(...) invisible(NULL),
      log_warn = function(...) invisible(NULL),
      log_error = function(...) invisible(NULL),
      try_install_package("pkg", repos = .make_repos(), dependencies = FALSE)
    )
  )

  expect_false(
    with_mocked_bindings(
      .require_namespace = function(package, quietly = TRUE) FALSE,
      .install_packages = function(...) stop("boom"),
      log_info = function(...) invisible(NULL),
      log_warn = function(...) invisible(NULL),
      log_error = function(...) invisible(NULL),
      try_install_package("pkg", repos = .make_repos(), dependencies = FALSE)
    )
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
