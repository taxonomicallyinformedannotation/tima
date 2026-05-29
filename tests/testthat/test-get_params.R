# Test Suite: get_params ----

library(testthat)

test_that("get_params validates required step argument", {
  expect_error(
    do.call(get_params, list()),
    "step name must be provided and non-empty",
    class = "tima_validation_error"
  )

  expect_error(
    get_params(step = NULL),
    "step name must be provided and non-empty",
    class = "tima_validation_error"
  )

  expect_error(
    get_params(step = NA_character_),
    "step name must be provided and non-empty",
    class = "tima_validation_error"
  )

  expect_error(
    get_params(step = 123),
    "step must be a single character string",
    class = "tima_validation_error"
  )

  expect_error(
    get_params(step = c("prepare_params", "annotate_masses")),
    "step must be a single character string",
    class = "tima_validation_error"
  )

  expect_error(
    get_params(step = ""),
    "step name must be provided and non-empty",
    class = "tima_validation_error"
  )
})

test_that("get_params errors for unknown step", {
  expect_error(
    get_params(step = "does_not_exist"),
    "step does not exist",
    class = "tima_validation_error"
  )
})

test_that("get_params loads default parameters for prepare steps", {
  params_default <- get_params(step = "prepare_params")
  expect_type(params_default, "list")
  expect_true(length(params_default) > 0L)

  params_advanced <- get_params(step = "prepare_params_advanced")
  expect_type(params_advanced, "list")
  expect_true(length(params_advanced) > 0L)
})

make_mock_paths <- function() {
  list(
    params = list(
      prepare_params = "params/prepare_params.yaml",
      prepare_params_advanced = "params/prepare_params_advanced.yaml",
      default = list(path = "params/default"),
      user = list(path = "params/user")
    ),
    version = "test-version"
  )
}

make_mock_pkg_root <- function(steps = "prepare_params") {
  root <- tempfile(pattern = "tima-get-params-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  withr::defer(
    unlink(root, recursive = TRUE, force = TRUE),
    envir = parent.frame()
  )
  docopt_dir <- file.path(root, "scripts", "docopt")
  dir.create(docopt_dir, recursive = TRUE)
  for (step in steps) {
    writeLines(
      "Usage:\n  cmd",
      con = file.path(docopt_dir, paste0(step, ".txt"))
    )
  }
  root
}

write_mock_file <- function(path, text) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(text, con = path)
}

test_that("get_params reports missing default parameter files", {
  mocked_paths <- make_mock_paths()
  mocked_root <- make_mock_pkg_root(steps = "prepare_params")

  with_mocked_bindings(
    get_default_paths = function() mocked_paths,
    get_path = function(path) path,
    pkg_system_file = function(..., package = NULL) {
      force(package)
      pieces <- c(...)
      if (length(pieces) == 0L) {
        return(mocked_root)
      }
      file.path(mocked_root, pieces)
    },
    {
      expect_error(
        get_params(step = "prepare_params"),
        "default parameter file not found",
        class = "tima_runtime_error"
      )
    }
  )
})

test_that("get_params reports missing docopt files", {
  mocked_paths <- make_mock_paths()
  mocked_root <- tempfile(pattern = "tima-get-params-")
  dir.create(mocked_root, recursive = TRUE, showWarnings = FALSE)
  withr::defer(
    unlink(mocked_root, recursive = TRUE, force = TRUE),
    envir = parent.frame()
  )

  write_mock_file(
    file.path(mocked_root, "params", "prepare_params.yaml"),
    "x: 1"
  )
  dir.create(file.path(mocked_root, "scripts", "docopt"), recursive = TRUE)
  file.symlink(
    from = file.path(mocked_root, "scripts", "docopt", "missing.txt"),
    to = file.path(mocked_root, "scripts", "docopt", "prepare_params.txt")
  )

  with_mocked_bindings(
    get_default_paths = function() mocked_paths,
    get_path = function(path) path,
    pkg_system_file = function(..., package = NULL) {
      force(package)
      pieces <- c(...)
      if (length(pieces) == 0L) {
        return(mocked_root)
      }
      file.path(mocked_root, pieces)
    },
    {
      expect_error(
        get_params(step = "prepare_params"),
        "docopt documentation not found",
        class = "tima_runtime_error"
      )
    }
  )
})

test_that("get_params wraps docopt file read failures", {
  mocked_paths <- make_mock_paths()
  mocked_root <- make_mock_pkg_root(steps = "prepare_params")

  write_mock_file(
    file.path(mocked_root, "params", "prepare_params.yaml"),
    "x: 1"
  )
  unlink(file.path(mocked_root, "scripts", "docopt", "prepare_params.txt"))
  dir.create(file.path(mocked_root, "scripts", "docopt", "prepare_params.txt"))

  with_mocked_bindings(
    get_default_paths = function() mocked_paths,
    get_path = function(path) path,
    pkg_system_file = function(..., package = NULL) {
      force(package)
      pieces <- c(...)
      if (length(pieces) == 0L) {
        return(mocked_root)
      }
      file.path(mocked_root, pieces)
    },
    {
      expect_error(
        suppressWarnings(get_params(step = "prepare_params")),
        "failed to read docopt file",
        class = "tima_runtime_error"
      )
    }
  )
})

test_that("get_params errors when parsed parameters are empty", {
  mocked_paths <- make_mock_paths()
  mocked_root <- make_mock_pkg_root(steps = "prepare_params")

  write_mock_file(
    file.path(mocked_root, "params", "prepare_params.yaml"),
    "x: 1"
  )

  with_mocked_bindings(
    get_default_paths = function() mocked_paths,
    get_path = function(path) path,
    pkg_system_file = function(..., package = NULL) {
      force(package)
      pieces <- c(...)
      if (length(pieces) == 0L) {
        return(mocked_root)
      }
      file.path(mocked_root, pieces)
    },
    parse_yaml_params = function(def, usr) {
      force(def)
      force(usr)
      list()
    },
    {
      expect_error(
        get_params(step = "prepare_params"),
        "failed to load parameters for step",
        class = "tima_runtime_error"
      )
    }
  )
})

test_that("get_params uses user yaml and falls back when CLI parsing fails", {
  mocked_paths <- make_mock_paths()
  mocked_root <- make_mock_pkg_root(steps = "annotate_masses")
  mocked_paths$params$user$path <- file.path(mocked_root, "params", "user")

  write_mock_file(
    file.path(mocked_root, "params", "default", "annotate_masses.yaml"),
    "x: 1"
  )
  write_mock_file(
    file.path(mocked_root, "params", "user", "annotate_masses.yaml"),
    "x: 2"
  )
  write_mock_file(
    file.path(mocked_root, "scripts", "docopt", "annotate_masses.txt"),
    "not a docopt spec"
  )

  seen <- new.env(parent = emptyenv())
  seen$usr <- NA_character_

  out <- with_mocked_bindings(
    get_default_paths = function() mocked_paths,
    get_path = function(path) path,
    pkg_system_file = function(..., package = NULL) {
      force(package)
      pieces <- c(...)
      if (length(pieces) == 0L) {
        return(mocked_root)
      }
      file.path(mocked_root, pieces)
    },
    parse_yaml_params = function(def, usr) {
      force(def)
      seen$usr <- usr
      list(alpha = 1L)
    },
    parse_cli_params = function(arguments, parameters) {
      expect_identical(arguments, list())
      expect_identical(parameters, list(alpha = 1L))
      c(parameters, list(from_cli = TRUE))
    },
    log_debug = function(...) {
      force(list(...))
      invisible(NULL)
    },
    {
      get_params(step = "annotate_masses")
    }
  )

  expect_identical(
    seen$usr,
    file.path(mocked_root, "params", "user", "annotate_masses.yaml")
  )
  expect_true(isTRUE(out$from_cli))
})
