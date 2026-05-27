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
  expect_error(validate_install_inputs("tima", c("https://cran.r-project.org", ""), TRUE))
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

test_that("verify_package_installation succeeds for base package and fails for missing", {
  expect_true(verify_package_installation("stats"))
  expect_false(verify_package_installation("definitelynotapackage12345"))
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
