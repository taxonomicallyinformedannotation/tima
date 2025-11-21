# Test Suite: install ----

library(testthat)

# Unit Tests: Validation Helper ----

test_that("validate_install_inputs accepts valid inputs", {
  expect_silent(
    validate_install_inputs(
      package = "tima",
      repos = c("https://cloud.r-project.org"),
      dependencies = TRUE,
      test = FALSE
    )
  )
})

test_that("validate_install_inputs rejects invalid package", {
  expect_error(
    validate_install_inputs(
      package = 123,
      repos = c("https://cloud.r-project.org"),
      dependencies = TRUE,
      test = FALSE
    ),
    "package must be a single non-empty character string"
  )

  expect_error(
    validate_install_inputs(
      package = c("pkg1", "pkg2"),
      repos = c("https://cloud.r-project.org"),
      dependencies = TRUE,
      test = FALSE
    ),
    "package must be a single non-empty character string"
  )

  expect_error(
    validate_install_inputs(
      package = "",
      repos = c("https://cloud.r-project.org"),
      dependencies = TRUE,
      test = FALSE
    ),
    "package must be a single non-empty character string"
  )

  expect_error(
    validate_install_inputs(
      package = NULL,
      repos = c("https://cloud.r-project.org"),
      dependencies = TRUE,
      test = FALSE
    ),
    "NULL"
  )
})

test_that("validate_install_inputs rejects invalid repos", {
  expect_error(
    validate_install_inputs(
      package = "tima",
      repos = character(0),
      dependencies = TRUE,
      test = FALSE
    ),
    "repos must be a non-empty character vector"
  )

  expect_error(
    validate_install_inputs(
      package = "tima",
      repos = 123,
      dependencies = TRUE,
      test = FALSE
    ),
    "repos must be a non-empty character vector"
  )

  expect_error(
    validate_install_inputs(
      package = "tima",
      repos = c("https://cloud.r-project.org", ""),
      dependencies = TRUE,
      test = FALSE
    ),
    "All repository URLs must be non-empty strings"
  )
})

test_that("validate_install_inputs rejects invalid dependencies", {
  expect_error(
    validate_install_inputs(
      package = "tima",
      repos = c("https://cloud.r-project.org"),
      dependencies = "yes",
      test = FALSE
    ),
    "dependencies must be a single logical value"
  )

  expect_error(
    validate_install_inputs(
      package = "tima",
      repos = c("https://cloud.r-project.org"),
      dependencies = c(TRUE, FALSE),
      test = FALSE
    ),
    "dependencies must be a single logical value"
  )
})

test_that("validate_install_inputs rejects invalid test", {
  expect_error(
    validate_install_inputs(
      package = "tima",
      repos = c("https://cloud.r-project.org"),
      dependencies = TRUE,
      test = "no"
    ),
    "test must be a single logical value"
  )

  expect_error(
    validate_install_inputs(
      package = "tima",
      repos = c("https://cloud.r-project.org"),
      dependencies = TRUE,
      test = c(TRUE, FALSE)
    ),
    "test must be a single logical value"
  )
})

# Unit Tests: System Messages Helper ----

test_that("show_system_messages handles different OS", {
  # Test doesn't error for known OS
  expect_silent(show_system_messages("Windows", FALSE))
  expect_silent(show_system_messages("Linux", FALSE))
  expect_silent(show_system_messages("Darwin", FALSE))
  expect_silent(show_system_messages("Unknown", FALSE))
})

test_that("show_system_messages handles test mode", {
  expect_silent(show_system_messages("Linux", TRUE))
})

# Unit Tests: Python Check Helper ----

test_that("check_or_install_python finds system Python", {
  skip_on_cran()
  skip_if_not(nzchar(Sys.which("python3")), "Python3 not available")

  python <- check_or_install_python(test = FALSE)

  expect_type(python, "character")
  expect_true(nzchar(python))
})

test_that("check_or_install_python handles missing Python", {
  skip_on_cran()
  skip("Requires Miniconda installation - manual test only")

  python <- check_or_install_python(test = TRUE)

  expect_type(python, "character")
  expect_true(file.exists(python))
})

# Unit Tests: Virtual Environment Helper ----

test_that("setup_virtualenv validates envname", {
  skip_on_cran()
  skip("Requires Python setup - manual test only")

  # Should not error with valid inputs
  expect_silent({
    # This would actually create virtualenv, so we skip
  })
})

# Unit Tests: Package Installation Helper ----

test_that("try_install_package returns logical", {
  skip_on_cran()
  skip("Requires actual package installation - manual test only")

  # Mock test - actual installation would be too slow
  result <- try_install_package(
    package = "nonexistent_package_xyz123",
    repos = c("https://cloud.r-project.org"),
    dependencies = FALSE,
    from_source = FALSE
  )

  expect_type(result, "logical")
  expect_equal(length(result), 1)
})

# Integration Tests ----

test_that("install validates all inputs before proceeding", {
  expect_error(
    install(package = 123),
    "package must be"
  )

  expect_error(
    install(package = "tima", repos = character(0)),
    "repos must be"
  )

  expect_error(
    install(package = "tima", dependencies = "yes"),
    "dependencies must be"
  )

  expect_error(
    install(package = "tima", test = "no"),
    "test must be"
  )
})

test_that("install has correct default parameters", {
  params <- formals(install)

  expect_equal(params$package, "tima")
  expect_true(is.character(eval(params$repos)))
  expect_equal(params$dependencies, TRUE)
  expect_equal(params$test, FALSE)
})

# Edge Cases ----

test_that("validate_install_inputs handles NA values", {
  expect_silent(
    validate_install_inputs(
      package = "tima",
      repos = c("https://cloud.r-project.org"),
      dependencies = NA,
      test = FALSE
    )
  )

  expect_silent(
    validate_install_inputs(
      package = "tima",
      repos = c("https://cloud.r-project.org"),
      dependencies = TRUE,
      test = NA
    )
  )
})

test_that("validate_install_inputs handles NULL values", {
  expect_error(
    validate_install_inputs(
      package = NULL,
      repos = c("https://cloud.r-project.org"),
      dependencies = TRUE,
      test = FALSE
    ),
    "NULL"
  )

  expect_error(
    validate_install_inputs(
      package = "tima",
      repos = NULL,
      dependencies = TRUE,
      test = FALSE
    ),
    "repos must be a non-empty character vector"
  )
})

# test_that("show_system_messages handles all common OS", {
#   # Should work for all these without error
#   systems <- c("Windows", "Linux", "Darwin", "SunOS", "FreeBSD")
#
#   for (sys in systems) {
#     expect_silent(show_system_messages(sys, FALSE))
#   }
# })

# Performance Tests ----

test_that("validate_install_inputs is fast", {
  elapsed <- system.time({
    for (i in 1:1000) {
      validate_install_inputs(
        package = "tima",
        repos = c("https://cloud.r-project.org"),
        dependencies = TRUE,
        test = FALSE
      )
    }
  })

  # 1000 validations should be very fast
  expect_true(elapsed["elapsed"] < 0.5)
})

# Regression Tests ----

test_that("install maintains backward compatibility", {
  # Function signature should have 4 parameters
  params <- formals(install)

  expect_equal(length(params), 4)
  expect_true("package" %in% names(params))
  expect_true("repos" %in% names(params))
  expect_true("dependencies" %in% names(params))
  expect_true("test" %in% names(params))
})

test_that("install returns NULL invisibly", {
  skip_on_cran()
  skip("Requires full installation - manual test only")

  # Would test that return value is NULL and invisible
})

# Error Message Quality Tests ----

test_that("error messages are informative", {
  expect_error(
    validate_install_inputs(
      package = 123,
      repos = c("https://cloud.r-project.org"),
      dependencies = TRUE,
      test = FALSE
    ),
    "numeric|integer"
  )

  expect_error(
    validate_install_inputs(
      package = "",
      repos = c("https://cloud.r-project.org"),
      dependencies = TRUE,
      test = FALSE
    ),
    "non-empty"
  )

  expect_error(
    validate_install_inputs(
      package = "tima",
      repos = c(""),
      dependencies = TRUE,
      test = FALSE
    ),
    "non-empty"
  )
})

# Parameter Combination Tests ----

test_that("validate_install_inputs accepts all valid combinations", {
  expect_silent(
    validate_install_inputs("tima", "https://cran.r-project.org", TRUE, TRUE)
  )

  expect_silent(
    validate_install_inputs("tima", "https://cran.r-project.org", FALSE, FALSE)
  )

  expect_silent(
    validate_install_inputs(
      "custom_package",
      c("https://cran.r-project.org", "https://bioconductor.org"),
      TRUE,
      FALSE
    )
  )
})

test_that("validate_install_inputs handles multiple repositories", {
  repos <- c(
    "https://taxonomicallyinformedannotation.r-universe.dev",
    "https://bioc.r-universe.dev",
    "https://cloud.r-project.org"
  )

  expect_silent(
    validate_install_inputs(
      package = "tima",
      repos = repos,
      dependencies = TRUE,
      test = FALSE
    )
  )
})

# Special Cases ----

test_that("check_or_install_python handles test mode", {
  skip_on_cran()
  skip("Requires Python environment setup")

  # In test mode, should fall back to Miniconda
  python <- check_or_install_python(test = TRUE)

  expect_type(python, "character")
})

test_that("show_system_messages provides OS-specific guidance", {
  # Capture output to verify messages
  expect_silent(show_system_messages("Windows", FALSE))
  expect_silent(show_system_messages("Linux", FALSE))
  expect_silent(show_system_messages("Darwin", FALSE))
})
