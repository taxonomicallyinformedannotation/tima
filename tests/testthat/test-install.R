# Test Suite: install (mother-function only) ----

library(testthat)

.make_repos <- function() {
  c(
    "https://taxonomicallyinformedannotation.r-universe.dev",
    "https://cloud.r-project.org",
    "https://cran.rstudio.com"
  )
}

# ---- Success paths ----

test_that("install_tima() runs without error", {
  calls <- new.env(parent = emptyenv())
  calls$install <- list()
  calls$setup_virtualenv <- list()
  calls$copy_backbone <- 0L
  calls$tar_destroy <- 0L

  local_mocked_bindings(
    check_or_install_python = function() "/opt/mock/bin/python3",
    .install_packages = function(package, repos, dependencies, type, ...) {
      calls$install <- list(
        package = package,
        repos = repos,
        dependencies = dependencies,
        type = type,
        dots = list(...)
      )
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) {
      force(package)
      force(quietly)
      TRUE
    },
    setup_virtualenv = function(envname, python) {
      calls$setup_virtualenv <- list(envname = envname, python = python)
      invisible(NULL)
    },
    .load_namespace = function(package) {
      force(package)
      invisible(TRUE)
    },
    copy_backbone = function() {
      calls$copy_backbone <- calls$copy_backbone + 1L
      invisible(NULL)
    },
    .tar_destroy = function() {
      calls$tar_destroy <- calls$tar_destroy + 1L
      invisible(NULL)
    }
  )

  expect_no_error(
    install_tima(package = "tima", dependencies = TRUE)
  )

  expect_identical(calls$install$package, "tima")
  expect_true(isTRUE(calls$install$dependencies))
  expect_identical(calls$setup_virtualenv$envname, "tima-env")
  expect_identical(calls$setup_virtualenv$python, "/opt/mock/bin/python3")
  expect_equal(calls$copy_backbone, 1L)
  expect_equal(calls$tar_destroy, 1L)
})

test_that("install_tima() with dependencies=TRUE runs without error", {
  local_mocked_bindings(
    check_or_install_python = function() "/opt/mock/bin/python3",
    .install_packages = function(package, repos, dependencies, type, ...) {
      expect_true(isTRUE(dependencies))
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) TRUE,
    setup_virtualenv = function(envname, python) invisible(NULL),
    .load_namespace = function(package) invisible(TRUE),
    copy_backbone = function() invisible(NULL),
    .tar_destroy = function() invisible(NULL)
  )

  expect_no_error(install_tima(package = "tima", dependencies = TRUE))
})

test_that("install_tima() accepts multiple repos", {
  repos_seen <- NULL

  local_mocked_bindings(
    check_or_install_python = function() "/opt/mock/bin/python3",
    .install_packages = function(package, repos, dependencies, type, ...) {
      repos_seen <<- repos
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) TRUE,
    setup_virtualenv = function(envname, python) invisible(NULL),
    .load_namespace = function(package) invisible(TRUE),
    copy_backbone = function() invisible(NULL),
    .tar_destroy = function() invisible(NULL)
  )

  expect_no_error(install_tima(package = "tima", repos = .make_repos()))
  expect_identical(repos_seen, .make_repos())
})

test_that("install_tima() can run with dependencies=FALSE", {
  dependencies_seen <- NULL

  local_mocked_bindings(
    check_or_install_python = function() "/opt/mock/bin/python3",
    .install_packages = function(package, repos, dependencies, type, ...) {
      dependencies_seen <<- dependencies
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) TRUE,
    setup_virtualenv = function(envname, python) invisible(NULL),
    .load_namespace = function(package) invisible(TRUE),
    copy_backbone = function() invisible(NULL),
    .tar_destroy = function() invisible(NULL)
  )

  expect_no_error(install_tima(package = "tima", dependencies = FALSE))
  expect_false(dependencies_seen)
})

test_that("install() emits deprecation warning", {
  local_mocked_bindings(
    install_tima = function(package, repos, dependencies) {
      force(package)
      force(repos)
      force(dependencies)
      invisible(NULL)
    }
  )

  lifecycle::expect_deprecated(install(package = "tima"))
})

# ---- Input validation tests ----

test_that("validate_install_inputs rejects NULL package", {
  expect_error(
    validate_install_inputs(NULL, .make_repos(), TRUE),
    "package must be a single non-empty"
  )
})

test_that("validate_install_inputs rejects empty string package", {
  expect_error(
    validate_install_inputs("", .make_repos(), TRUE),
    "package must be a single non-empty"
  )
})

test_that("validate_install_inputs rejects multi-value package", {
  expect_error(
    validate_install_inputs(c("a", "b"), .make_repos(), TRUE),
    "package must be a single non-empty"
  )
})

test_that("validate_install_inputs rejects non-character package", {
  expect_error(
    validate_install_inputs(123, .make_repos(), TRUE),
    "package must be a single non-empty"
  )
})

test_that("validate_install_inputs rejects empty repos vector", {
  expect_error(
    validate_install_inputs("tima", character(0), TRUE),
    "repos must be a non-empty character vector"
  )
})

test_that("validate_install_inputs rejects repos with empty strings", {
  expect_error(
    validate_install_inputs(
      "tima",
      c("https://cran.r-project.org", ""),
      TRUE
    ),
    "All repository URLs must be non-empty"
  )
})

test_that("validate_install_inputs rejects non-character repos", {
  expect_error(
    validate_install_inputs("tima", 123, TRUE),
    "repos must be a non-empty character vector"
  )
})

test_that("validate_install_inputs rejects non-logical dependencies", {
  expect_error(
    validate_install_inputs("tima", .make_repos(), "yes"),
    "dependencies must be a single logical value"
  )
})

test_that("validate_install_inputs rejects multi-value dependencies", {
  expect_error(
    validate_install_inputs("tima", .make_repos(), c(TRUE, FALSE)),
    "dependencies must be a single logical value"
  )
})

test_that("validate_install_inputs accepts valid inputs", {
  expect_silent(
    validate_install_inputs("tima", .make_repos(), TRUE)
  )
  expect_silent(
    validate_install_inputs("tima", .make_repos(), FALSE)
  )
})

# ---- show_system_messages tests ----

test_that("show_system_messages handles Windows", {
  expect_no_error(show_system_messages("Windows"))
})

test_that("show_system_messages handles Linux", {
  expect_no_error(show_system_messages("Linux"))
})

test_that("show_system_messages handles Darwin", {
  expect_no_error(show_system_messages("Darwin"))
})

test_that("show_system_messages handles unknown OS", {
  expect_no_error(show_system_messages("Unknown"))
})

# ---- installation verification tests ----

test_that("verify_package_installation returns FALSE when namespace is missing", {
  local_mocked_bindings(
    .require_namespace = function(package, quietly = TRUE) {
      force(package)
      force(quietly)
      FALSE
    }
  )

  expect_false(verify_package_installation("missingpkg"))
})

test_that("verify_package_installation returns FALSE when no valid install path exists", {
  local_mocked_bindings(
    .require_namespace = function(package, quietly = TRUE) {
      force(package)
      force(quietly)
      TRUE
    },
    .find_package_paths = function(package, quiet = TRUE) {
      force(package)
      force(quiet)
      c("/tmp/broken-one", "/tmp/broken-two")
    }
  )

  expect_false(verify_package_installation("brokenpkg"))
})

test_that("verify_package_installation returns FALSE when DESCRIPTION cannot be read", {
  local_mocked_bindings(
    .require_namespace = function(package, quietly = TRUE) {
      force(package)
      force(quietly)
      TRUE
    },
    .find_package_paths = function(package, quiet = TRUE) {
      force(package)
      force(quiet)
      "/tmp/installedpkg"
    },
    .path_exists = function(path) {
      path %in% c("/tmp/installedpkg", "/tmp/installedpkg/DESCRIPTION")
    },
    .read_description_dcf = function(path) {
      force(path)
      stop("bad DESCRIPTION")
    },
    .load_namespace = function(package) {
      force(package)
      TRUE
    }
  )

  expect_false(verify_package_installation("brokenpkg"))
})

test_that("verify_package_installation returns FALSE when namespace load fails", {
  local_mocked_bindings(
    .require_namespace = function(package, quietly = TRUE) {
      force(package)
      force(quietly)
      TRUE
    },
    .find_package_paths = function(package, quiet = TRUE) {
      force(package)
      force(quiet)
      "/tmp/installedpkg"
    },
    .path_exists = function(path) {
      path %in% c("/tmp/installedpkg", "/tmp/installedpkg/DESCRIPTION")
    },
    .read_description_dcf = function(path) {
      force(path)
      matrix("ok", nrow = 1)
    },
    .load_namespace = function(package) {
      force(package)
      stop("namespace failed")
    }
  )

  expect_false(verify_package_installation("brokenpkg"))
})

test_that("verify_package_installation returns TRUE for a valid package", {
  local_mocked_bindings(
    .require_namespace = function(package, quietly = TRUE) {
      force(package)
      force(quietly)
      TRUE
    },
    .find_package_paths = function(package, quiet = TRUE) {
      force(package)
      force(quiet)
      c("/tmp/installedpkg", "/tmp/other")
    },
    .path_exists = function(path) {
      path %in% c("/tmp/installedpkg", "/tmp/installedpkg/DESCRIPTION")
    },
    .read_description_dcf = function(path) {
      force(path)
      matrix("ok", nrow = 1)
    },
    .load_namespace = function(package) {
      force(package)
      TRUE
    }
  )

  expect_true(verify_package_installation("goodpkg"))
})

test_that("install_tima fails when installed package remains unavailable", {
  local_mocked_bindings(
    check_or_install_python = function() "/opt/mock/bin/python3",
    .install_packages = function(package, repos, dependencies, type, ...) invisible(NULL),
    .require_namespace = function(package, quietly = TRUE) FALSE
  )

  expect_error(
    install_tima(package = "tima"),
    "not found after installation"
  )
})

# ---- Python / virtualenv tests ----

test_that("check_or_install_python returns path when python3 exists", {
  local_mocked_bindings(
    .sys_which = function(command) {
      expect_identical(command, "python3")
      "/opt/mock/bin/python3"
    },
    .path_exists = function(path) {
      expect_identical(path, "/opt/mock/bin/python3")
      TRUE
    }
  )

  result <- check_or_install_python()
  expect_identical(result, "/opt/mock/bin/python3")
})

test_that("setup_virtualenv handles test scenarios", {
  calls <- new.env(parent = emptyenv())
  calls$created <- 0L
  calls$installed <- 0L
  calls$py_install <- 0L

  local_mocked_bindings(
    check_or_install_python = function() "/opt/mock/bin/python3",
    .virtualenv_exists = function(envname) FALSE,
    .virtualenv_create = function(envname, python) {
      calls$created <- calls$created + 1L
      expect_identical(envname, "tima-env")
      expect_identical(python, "/opt/mock/bin/python3")
      invisible(NULL)
    },
    .virtualenv_install = function(envname, packages) {
      calls$installed <- calls$installed + 1L
      expect_identical(envname, "tima-env")
      expect_true(all(c("rdkit", "chembl-structure-pipeline") %in% packages))
      invisible(NULL)
    },
    .py_install = function(packages) {
      calls$py_install <- calls$py_install + 1L
      expect_true(all(c("rdkit", "chembl-structure-pipeline") %in% packages))
      invisible(NULL)
    }
  )

  expect_no_error(setup_virtualenv())
  expect_equal(calls$created, 1L)
  expect_equal(calls$installed, 1L)
  expect_equal(calls$py_install, 1L)
})

test_that("try_install_package handles errors gracefully", {
  local_mocked_bindings(
    .require_namespace = function(package, quietly = TRUE) FALSE,
    .install_packages = function(package, repos, dependencies, type, ...) {
      warning("package 'thisPackageDefinitelyDoesNotExist12345' is not available")
    }
  )

  result <- try_install_package(
    package = "thisPackageDefinitelyDoesNotExist12345",
    repos = .make_repos(),
    dependencies = FALSE,
    from_source = FALSE
  )
  expect_true(is.logical(result))
  expect_false(result)
})
