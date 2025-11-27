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
  skip_if_not_installed("logger")
  expect_message(
    res <- install(package = "tima", test = TRUE),
    "Test mode",
    ignore.case = TRUE
  )
  expect_null(res)
})

test_that("install() with dependencies=TRUE in test mode runs without error", {
  skip_if_not_installed("logger")
  expect_message(
    install(package = "tima", dependencies = TRUE, test = TRUE),
    "Test mode",
    ignore.case = TRUE
  )
})

test_that("install() accepts multiple repos in test mode", {
  skip_if_not_installed("logger")
  expect_message(
    install(package = "tima", repos = .make_repos(), test = TRUE),
    "Test mode",
    ignore.case = TRUE
  )
})

test_that("install() can run with dependencies=FALSE (branch coverage)", {
  skip_if_not_installed("logger")
  expect_message(
    install(package = "tima", dependencies = FALSE, test = TRUE),
    "Test mode",
    ignore.case = TRUE
  )
})

# ---- Edge / branch: nonexistent package (still in test mode) ----

test_that("install() handles nonexistent package gracefully in test mode", {
  skip_if_not_installed("logger")
  # Should not attempt real installation; just log test mode.
  expect_message(
    install(package = "definitelyNotAPackage123", test = TRUE),
    "Test mode",
    ignore.case = TRUE
  )
})
