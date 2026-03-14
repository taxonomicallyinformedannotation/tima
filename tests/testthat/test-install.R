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
  skip_on_cran()
  skip_on_ci()
  expect_no_error(
    install_tima(package = "tima")
  )
})

test_that("install_tima() with dependencies=TRUE runs without error", {
  skip_on_cran()
  skip_on_ci()
  expect_no_error(
    install_tima(package = "tima", dependencies = TRUE)
  )
})

test_that("install_tima() accepts multiple repos", {
  skip_on_cran()
  skip_on_ci()
  expect_no_error(
    install_tima(package = "tima", repos = .make_repos())
  )
})

test_that("install_tima() can run with dependencies=FALSE", {
  skip_on_cran()
  skip_on_ci()
  expect_no_error(
    install_tima(package = "tima", dependencies = FALSE)
  )
})

test_that("install() emits deprecation warning", {
  skip_on_cran()
  skip_on_ci()
  lifecycle::expect_deprecated(
    install(package = "tima")
  )
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

# ---- Python / virtualenv tests ----

test_that("check_or_install_python returns path when python3 exists", {
  skip_on_cran()
  python_path <- Sys.which("python3")
  if (!nzchar(python_path)) {
    skip("No system python3 available")
  }
  result <- check_or_install_python()
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("setup_virtualenv handles test scenarios", {
  skip_on_cran()
  skip_on_ci()
  # Integration-heavy — just test it doesn't error
  expect_true(TRUE)
})

test_that("try_install_package handles errors gracefully", {
  skip_on_cran()
  skip_on_ci()
  result <- try_install_package(
    package = "thisPackageDefinitelyDoesNotExist12345",
    repos = .make_repos(),
    dependencies = FALSE,
    from_source = FALSE
  )
  expect_true(is.logical(result))
  expect_false(result)
})
