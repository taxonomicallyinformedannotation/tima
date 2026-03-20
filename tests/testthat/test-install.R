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
      captured_args <- list(
        package = package,
        repos = repos,
        type = type,
        dots = list(...)
      )
      expect_true(isTRUE(dependencies))
      invisible(captured_args)
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) {
      captured_args <- list(package = package, quietly = quietly)
      invisible(captured_args)
      TRUE
    },
    setup_virtualenv = function(envname, python) {
      captured_args <- list(envname = envname, python = python)
      invisible(captured_args)
      invisible(NULL)
    },
    .load_namespace = function(package) {
      invisible(package)
      invisible(TRUE)
    },
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
      captured_args <- list(
        package = package,
        dependencies = dependencies,
        type = type,
        dots = list(...)
      )
      repos_seen <<- repos
      invisible(captured_args)
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) {
      captured_args <- list(package = package, quietly = quietly)
      invisible(captured_args)
      TRUE
    },
    setup_virtualenv = function(envname, python) {
      captured_args <- list(envname = envname, python = python)
      invisible(captured_args)
      invisible(NULL)
    },
    .load_namespace = function(package) {
      invisible(package)
      invisible(TRUE)
    },
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
      captured_args <- list(
        package = package,
        repos = repos,
        type = type,
        dots = list(...)
      )
      dependencies_seen <<- dependencies
      invisible(captured_args)
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) {
      captured_args <- list(package = package, quietly = quietly)
      invisible(captured_args)
      TRUE
    },
    setup_virtualenv = function(envname, python) {
      captured_args <- list(envname = envname, python = python)
      invisible(captured_args)
      invisible(NULL)
    },
    .load_namespace = function(package) {
      invisible(package)
      invisible(TRUE)
    },
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
    "all repository URLs must be non-empty"
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
    .install_packages = function(package, repos, dependencies, type, ...) {
      captured_args <- list(
        package = package,
        repos = repos,
        dependencies = dependencies,
        type = type,
        dots = list(...)
      )
      invisible(captured_args)
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) {
      captured_args <- list(package = package, quietly = quietly)
      invisible(captured_args)
      FALSE
    }
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
    .virtualenv_exists = function(envname) {
      invisible(envname)
      FALSE
    },
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
    .require_namespace = function(package, quietly = TRUE) {
      captured_args <- list(package = package, quietly = quietly)
      invisible(captured_args)
      FALSE
    },
    .install_packages = function(package, repos, dependencies, type, ...) {
      captured_args <- list(
        package = package,
        repos = repos,
        dependencies = dependencies,
        type = type,
        dots = list(...)
      )
      invisible(captured_args)
      warning(
        "package 'thisPackageDefinitelyDoesNotExist12345' is not available"
      )
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

test_that("check_or_install_python installs miniconda when system python is missing", {
  calls <- new.env(parent = emptyenv())
  calls$installed <- FALSE

  minipath <- file.path(tempdir(), "mock_miniconda")
  expected_python <- if (Sys.info()[["sysname"]] == "Windows") {
    file.path(minipath, "python.exe")
  } else {
    file.path(minipath, "bin", "python")
  }

  local_mocked_bindings(
    .sys_which = function(command) {
      force(command)
      ""
    },
    .miniconda_path = function() minipath,
    .path_exists = function(path) {
      if (identical(path, minipath)) {
        return(calls$installed)
      }
      if (identical(path, expected_python)) {
        return(calls$installed)
      }
      FALSE
    },
    .install_miniconda = function() {
      calls$installed <- TRUE
      invisible(NULL)
    }
  )

  result <- check_or_install_python()
  expect_identical(result, expected_python)
  expect_true(calls$installed)
})

test_that("check_or_install_python errors when miniconda python executable is missing", {
  minipath <- file.path(tempdir(), "mock_miniconda_missing_py")

  local_mocked_bindings(
    .sys_which = function(command) {
      force(command)
      ""
    },
    .miniconda_path = function() minipath,
    .path_exists = function(path) {
      identical(path, minipath)
    },
    .install_miniconda = function() invisible(NULL)
  )

  expect_error(
    check_or_install_python(),
    "python executable not found",
    class = "tima_runtime_error"
  )
})

test_that("check_or_install_python uses existing miniconda without reinstalling", {
  calls <- new.env(parent = emptyenv())
  calls$installed <- 0L

  minipath <- file.path(tempdir(), "existing_miniconda")
  expected_python <- if (Sys.info()[["sysname"]] == "Windows") {
    file.path(minipath, "python.exe")
  } else {
    file.path(minipath, "bin", "python")
  }

  local_mocked_bindings(
    .sys_which = function(command) {
      invisible(command)
      ""
    },
    .miniconda_path = function() minipath,
    .path_exists = function(path) {
      path %in% c(minipath, expected_python)
    },
    .install_miniconda = function() {
      calls$installed <- calls$installed + 1L
      invisible(NULL)
    }
  )

  expect_identical(check_or_install_python(), expected_python)
  expect_equal(calls$installed, 0L)
})

test_that("setup_virtualenv uses existing env without creating it", {
  calls <- new.env(parent = emptyenv())
  calls$created <- 0L
  calls$venv_install <- 0L
  calls$py_install <- 0L

  local_mocked_bindings(
    .virtualenv_exists = function(envname) {
      force(envname)
      TRUE
    },
    .virtualenv_create = function(envname, python) {
      force(envname)
      force(python)
      calls$created <- calls$created + 1L
      invisible(NULL)
    },
    .virtualenv_install = function(envname, packages) {
      force(envname)
      force(packages)
      calls$venv_install <- calls$venv_install + 1L
      invisible(NULL)
    },
    .py_install = function(packages) {
      force(packages)
      calls$py_install <- calls$py_install + 1L
      invisible(NULL)
    }
  )

  expect_no_error(setup_virtualenv(
    envname = "tima-env",
    python = "/tmp/python"
  ))
  expect_equal(calls$created, 0L)
  expect_equal(calls$venv_install, 1L)
  expect_equal(calls$py_install, 1L)
})

test_that("setup_virtualenv aborts when virtualenv creation fails", {
  local_mocked_bindings(
    .virtualenv_exists = function(envname) {
      force(envname)
      FALSE
    },
    .virtualenv_create = function(envname, python) {
      force(envname)
      force(python)
      stop("boom")
    }
  )

  expect_error(
    setup_virtualenv(envname = "tima-env", python = "/tmp/python"),
    "failed to create python virtualenv",
    class = "tima_runtime_error"
  )
})

test_that("setup_virtualenv aborts when dependency install fails", {
  local_mocked_bindings(
    .virtualenv_exists = function(envname) {
      force(envname)
      TRUE
    },
    .virtualenv_install = function(envname, packages) {
      force(envname)
      force(packages)
      stop("dep fail")
    },
    .py_install = function(packages) {
      force(packages)
      invisible(NULL)
    }
  )

  expect_error(
    setup_virtualenv(envname = "tima-env", python = "/tmp/python"),
    "failed to install dependencies in virtualenv",
    class = "tima_runtime_error"
  )
})

test_that("install_tima continues when copy_backbone fails", {
  calls <- new.env(parent = emptyenv())
  calls$tar_destroy <- 0L

  local_mocked_bindings(
    check_or_install_python = function() "/opt/mock/bin/python3",
    .install_packages = function(package, repos, dependencies, type, ...) {
      force(package)
      force(repos)
      force(dependencies)
      force(type)
      invisible(list(...))
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) {
      force(package)
      force(quietly)
      TRUE
    },
    setup_virtualenv = function(envname, python) {
      force(envname)
      force(python)
      invisible(NULL)
    },
    .load_namespace = function(package) {
      force(package)
      TRUE
    },
    copy_backbone = function() {
      stop("copy failed")
    },
    .tar_destroy = function() {
      calls$tar_destroy <- calls$tar_destroy + 1L
      invisible(NULL)
    }
  )

  expect_no_error(install_tima(package = "tima", dependencies = FALSE))
  expect_equal(calls$tar_destroy, 1L)
})

test_that("install_tima uses source installation on Linux", {
  install_type_seen <- NULL

  local_mocked_bindings(
    Sys.info = function() c(sysname = "Linux"),
    .package = "base"
  )

  local_mocked_bindings(
    check_or_install_python = function() "/opt/mock/bin/python3",
    .install_packages = function(package, repos, dependencies, type, ...) {
      invisible(list(
        package = package,
        repos = repos,
        dependencies = dependencies,
        dots = list(...)
      ))
      install_type_seen <<- type
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) {
      invisible(list(package = package, quietly = quietly))
      TRUE
    },
    setup_virtualenv = function(envname, python) {
      invisible(list(envname = envname, python = python))
      invisible(NULL)
    },
    .load_namespace = function(package) {
      invisible(package)
      TRUE
    },
    copy_backbone = function() invisible(NULL),
    .tar_destroy = function() invisible(NULL)
  )

  expect_no_error(install_tima(package = "tima", dependencies = FALSE))
  expect_identical(install_type_seen, "source")
})

test_that("install_tima tolerates tar_destroy failure", {
  local_mocked_bindings(
    check_or_install_python = function() "/opt/mock/bin/python3",
    .install_packages = function(package, repos, dependencies, type, ...) {
      invisible(list(
        package = package,
        repos = repos,
        dependencies = dependencies,
        type = type,
        dots = list(...)
      ))
      invisible(NULL)
    },
    .require_namespace = function(package, quietly = TRUE) {
      invisible(list(package = package, quietly = quietly))
      TRUE
    },
    setup_virtualenv = function(envname, python) {
      invisible(list(envname = envname, python = python))
      invisible(NULL)
    },
    .load_namespace = function(package) {
      invisible(package)
      TRUE
    },
    copy_backbone = function() invisible(NULL),
    .tar_destroy = function() stop("no targets")
  )

  expect_no_error(install_tima(package = "tima", dependencies = FALSE))
})

test_that("try_install_package returns TRUE when installation succeeds", {
  state <- new.env(parent = emptyenv())
  state$installed <- FALSE

  local_mocked_bindings(
    .require_namespace = function(package, quietly = TRUE) {
      force(package)
      force(quietly)
      state$installed
    },
    .install_packages = function(package, repos, dependencies, type, ...) {
      force(package)
      force(repos)
      force(dependencies)
      force(type)
      invisible(list(...))
      state$installed <- TRUE
      invisible(NULL)
    }
  )

  expect_true(
    try_install_package(
      package = "tima",
      repos = .make_repos(),
      dependencies = TRUE,
      from_source = FALSE
    )
  )
})

test_that("try_install_package returns FALSE when install throws error", {
  local_mocked_bindings(
    .require_namespace = function(package, quietly = TRUE) {
      force(package)
      force(quietly)
      FALSE
    },
    .install_packages = function(package, repos, dependencies, type, ...) {
      force(package)
      force(repos)
      force(dependencies)
      force(type)
      invisible(list(...))
      stop("install explosion")
    }
  )

  expect_false(
    try_install_package(
      package = "tima",
      repos = .make_repos(),
      dependencies = FALSE,
      from_source = TRUE
    )
  )
})

test_that("verify_package_installation returns FALSE on DESCRIPTION warning", {
  local_mocked_bindings(
    .require_namespace = function(package, quietly = TRUE) {
      force(package)
      force(quietly)
      TRUE
    },
    .find_package_paths = function(package, quiet = TRUE) {
      force(package)
      force(quiet)
      "/tmp/warnpkg"
    },
    .path_exists = function(path) {
      path %in% c("/tmp/warnpkg", "/tmp/warnpkg/DESCRIPTION")
    },
    .read_description_dcf = function(path) {
      force(path)
      warning("warning in DESCRIPTION")
    },
    .load_namespace = function(package) {
      force(package)
      TRUE
    }
  )

  expect_false(verify_package_installation("warnpkg"))
})

test_that("install_tima aborts when package installation itself fails", {
  local_mocked_bindings(
    check_or_install_python = function() "/opt/mock/bin/python3",
    .install_packages = function(package, repos, dependencies, type, ...) {
      force(package)
      force(repos)
      force(dependencies)
      force(type)
      invisible(list(...))
      stop("repo unavailable")
    }
  )

  expect_error(
    install_tima(package = "tima", dependencies = FALSE),
    "failed to install package",
    class = "tima_runtime_error"
  )
})
