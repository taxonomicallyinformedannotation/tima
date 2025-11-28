# Tests for deprecated tima_full function

test_that("tima_full exists and is exported", {
  # Verify the deprecated function exists
  expect_true(exists("tima_full"))
  expect_true("tima_full" %in% getNamespaceExports("tima"))
})

test_that("tima_full issues deprecation warning", {
  # The function should issue a deprecation warning
  # Mock run_tima to avoid actually running the workflow
  with_mocked_bindings(
    {
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
    },
    run_tima = function(...) invisible(NULL),
    .package = "tima"
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
  # Capture the deprecation warning
  expect_warning(
    {
      with_mocked_bindings(
        {
          tima_full()
        },
        run_tima = function(...) invisible(NULL),
        .package = "tima"
      )
    },
    regexp = "run_tima"
  )
})

test_that("tima_full documentation mentions deprecation", {
  # Try to get the help text, but skip if not available in test context
  skip_on_cran()
  skip_on_ci()

  # Simpler approach - just verify the Rd file exists and contains DEPRECATED
  rd_file <- system.file("help", "tima_full.Rd", package = "tima")

  if (rd_file == "" || !file.exists(rd_file)) {
    # Try alternative location
    rd_file <- file.path("man", "tima_full.Rd")
  }

  if (file.exists(rd_file)) {
    help_content <- readLines(rd_file, warn = FALSE)
    help_string <- paste(help_content, collapse = " ")
    expect_true(grepl("DEPRECATED|deprecated", help_string, ignore.case = TRUE))
  } else {
    skip("Help file not available in test context")
  }
})

test_that("tima_full calls run_tima with correct arguments", {
  # Mock run_tima to capture arguments
  called_args <- NULL

  with_mocked_bindings(
    {
      suppressWarnings({
        tima_full(
          target_pattern = "^custom$",
          log_file = "custom.log",
          clean_old_logs = FALSE,
          log_level = "debug"
        )
      })
    },
    run_tima = function(...) {
      called_args <<- list(...)
      invisible(NULL)
    },
    .package = "tima"
  )

  # Verify arguments were passed through correctly
  expect_equal(called_args$target_pattern, "^custom$")
  expect_equal(called_args$log_file, "custom.log")
  expect_equal(called_args$clean_old_logs, FALSE)
  expect_equal(called_args$log_level, "debug")
})
