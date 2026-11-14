# Tests for deprecated tima_full function

test_that("tima_full exists and is exported", {
  # Verify the deprecated function exists
  expect_true(exists("tima_full"))
  expect_true("tima_full" %in% getNamespaceExports("tima"))
})

test_that("tima_full issues deprecation warning", {
  withr::local_options(list(
    tima.tima_full_runner = function(...) {
      invisible(list(...))
      invisible(NULL)
    }
  ))

  expect_warning(
    tima_full(
      target_pattern = "^test$",
      log_file = tempfile(fileext = ".log"),
      clean_old_logs = FALSE,
      log_level = "warn"
    ),
    regexp = "deprecated.*run_tima",
    ignore.case = TRUE
  )
})

test_that("tima_full has same parameters as run_tima", {
  # Get formals (parameters) of both functions
  tima_full_params <- names(formals(tima_full))
  run_tima_params <- names(formals(run_tima))

  # Should have identical parameters
  expect_equal(tima_full_params, run_tima_params)

  # Should have same defaults
  expect_equal(
    formals(tima_full)$target_pattern,
    formals(run_tima)$target_pattern
  )
  expect_equal(
    formals(tima_full)$log_file,
    formals(run_tima)$log_file
  )
  expect_equal(
    formals(tima_full)$clean_old_logs,
    formals(run_tima)$clean_old_logs
  )
  expect_equal(
    formals(tima_full)$log_level,
    formals(run_tima)$log_level
  )
})

test_that("tima_full deprecation message is clear", {
  withr::local_options(list(
    tima.tima_full_runner = function(...) {
      invisible(list(...))
      invisible(NULL)
    }
  ))

  expect_warning(
    tima_full(),
    regexp = "run_tima"
  )
})

test_that("tima_full documentation mentions deprecation", {
  skip_on_cran()

  rd_file <- system.file("help", "tima_full.Rd", package = "tima")
  if (!nzchar(rd_file) || !file.exists(rd_file)) {
    rd_file <- normalizePath(
      testthat::test_path("..", "..", "man", "tima_full.Rd"),
      mustWork = FALSE
    )
  }

  skip_if_not(file.exists(rd_file), "Help file not available in test context")
  help_content <- readLines(rd_file, warn = FALSE)
  help_string <- paste(help_content, collapse = " ")
  expect_true(grepl("DEPRECATED|deprecated", help_string, ignore.case = TRUE))
})

test_that("tima_full calls run_tima with correct arguments", {
  state <- new.env(parent = emptyenv())
  state$called_args <- NULL

  withr::local_options(list(
    tima.tima_full_runner = function(...) {
      state$called_args <- list(...)
      invisible(NULL)
    }
  ))

  suppressWarnings({
    tima_full(
      target_pattern = "^custom$",
      log_file = "custom.log",
      clean_old_logs = FALSE,
      log_level = "debug"
    )
  })

  # Verify arguments were passed through correctly
  expect_equal(state$called_args$target_pattern, "^custom$")
  expect_equal(state$called_args$log_file, "custom.log")
  expect_equal(state$called_args$clean_old_logs, FALSE)
  expect_equal(state$called_args$log_level, "debug")
})
