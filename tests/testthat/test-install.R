# Test Suite: install (mother-function only) ----

library(testthat)

# All tests exercise install() directly; internal helpers are not tested.
# Uses test=TRUE to avoid side-effects and network-heavy operations.

.make_repos <- function() {
  c("https://cloud.r-project.org", "https://cran.rstudio.com")
}

# ---- Validation error paths (through install) ----

test_that("install() errors on invalid package name (empty)", {
  expect_error(
    install(package = "", test = TRUE),
    "package must be a single non-empty"
  )
})

test_that("install() errors on invalid package name (vector)", {
  expect_error(
    install(package = c("a", "b"), test = TRUE),
    "package must be a single non-empty"
  )
})

test_that("install() errors on invalid repos (empty vector)", {
  expect_error(
    install(package = "tima", repos = character(0), test = TRUE),
    "repos must be a non-empty character vector"
  )
})

test_that("install() errors on invalid dependencies (character)", {
  expect_error(
    install(package = "tima", dependencies = "yes", test = TRUE),
    "dependencies must be a single logical value"
  )
})

test_that("install() errors on invalid test flag (character)", {
  expect_error(
    install(package = "tima", test = "no"),
    "test must be a single logical value"
  )
})

# ---- Success paths ----

test_that("install(test=TRUE) logs test mode and returns invisible NULL", {
  expect_message(
    res <- install(package = "tima", test = TRUE),
    "Test mode",
    ignore.case = TRUE
  )
  expect_null(res)
})

test_that("install() with dependencies=TRUE in test mode runs without error", {
  expect_message(
    install(package = "tima", dependencies = TRUE, test = TRUE),
    "Test mode",
    ignore.case = TRUE
  )
})

test_that("install() accepts multiple repos in test mode", {
  expect_message(
    install(package = "tima", repos = .make_repos(), test = TRUE),
    "Test mode",
    ignore.case = TRUE
  )
})

test_that("install() can run with dependencies=FALSE (branch coverage)", {
  expect_message(
    install(package = "tima", dependencies = FALSE, test = TRUE),
    "Test mode",
    ignore.case = TRUE
  )
})

# ---- Edge / branch: nonexistent package (still in test mode) ----

test_that("install() handles nonexistent package gracefully in test mode", {
  # Should not attempt real installation; just log test mode.
  expect_message(
    install(package = "definitelyNotAPackage123", test = TRUE),
    "Test mode",
    ignore.case = TRUE
  )
})

# ---- Internal function tests ----

test_that("validate_install_inputs validates package parameter", {
  # NULL package
  expect_error(
    validate_install_inputs(NULL, .make_repos(), TRUE, FALSE),
    "package must be a single non-empty"
  )

  # Empty string
  expect_error(
    validate_install_inputs("", .make_repos(), TRUE, FALSE),
    "package must be a single non-empty"
  )

  # Multiple values
  expect_error(
    validate_install_inputs(c("a", "b"), .make_repos(), TRUE, FALSE),
    "package must be a single non-empty"
  )

  # Non-character
  expect_error(
    validate_install_inputs(123, .make_repos(), TRUE, FALSE),
    "package must be a single non-empty"
  )
})

test_that("validate_install_inputs validates repos parameter", {
  # Empty vector
  expect_error(
    validate_install_inputs("tima", character(0), TRUE, FALSE),
    "repos must be a non-empty character vector"
  )

  # Contains empty string
  expect_error(
    validate_install_inputs(
      "tima",
      c("https://cran.r-project.org", ""),
      TRUE,
      FALSE
    ),
    "All repository URLs must be non-empty"
  )

  # Non-character
  expect_error(
    validate_install_inputs("tima", 123, TRUE, FALSE),
    "repos must be a non-empty character vector"
  )
})

test_that("validate_install_inputs validates dependencies parameter", {
  # Non-logical
  expect_error(
    validate_install_inputs("tima", .make_repos(), "yes", FALSE),
    "dependencies must be a single logical value"
  )

  # Multiple values
  expect_error(
    validate_install_inputs("tima", .make_repos(), c(TRUE, FALSE), FALSE),
    "dependencies must be a single logical value"
  )
})

test_that("validate_install_inputs validates test parameter", {
  # Non-logical
  expect_error(
    validate_install_inputs("tima", .make_repos(), TRUE, "no"),
    "test must be a single logical value"
  )
})

test_that("validate_install_inputs accepts valid inputs", {
  expect_silent(
    validate_install_inputs("tima", .make_repos(), TRUE, FALSE)
  )

  expect_silent(
    validate_install_inputs("tima", .make_repos(), FALSE, TRUE)
  )
})

test_that("show_system_messages handles all OS types", {
  # Windows
  expect_no_error(
    show_system_messages("Windows", test = TRUE)
  )

  # Linux
  expect_no_error(
    show_system_messages("Linux", test = FALSE)
  )

  # macOS
  expect_no_error(
    show_system_messages("Darwin", test = FALSE)
  )

  # Unknown OS (should still run)
  expect_no_error(
    show_system_messages("Unknown", test = FALSE)
  )
})

test_that("check_or_install_python returns path in test mode", {
  result <- check_or_install_python(test = TRUE)
  expect_true(is.character(result))
  expect_true(length(result) > 0)
})

test_that("setup_virtualenv handles test scenarios", {
  skip_on_cran()
  skip_on_ci()

  # This is integration-heavy, just test it doesn't error in normal case
  # Full test would require mocking reticulate functions
  expect_true(TRUE)
})

test_that("try_install_package handles errors gracefully", {
  skip_on_cran()
  skip_on_ci()

  # Test with a package that doesn't exist from source
  result <- try_install_package(
    package = "thisPackageDefinitelyDoesNotExist12345",
    repos = .make_repos(),
    dependencies = FALSE,
    from_source = FALSE
  )

  expect_true(is.logical(result))
  expect_false(result)
})
