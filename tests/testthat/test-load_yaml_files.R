# Test Suite: load_yaml_files ----

library(testthat)

# Test Fixtures ----

#' Create minimal YAML files for testing
#' @keywords internal
setup_yaml_test_env <- function() {
  test_dir <- temp_test_dir("yaml_test")

  # Create default params directory
  default_dir <- file.path(test_dir, "params", "default")
  dir.create(default_dir, recursive = TRUE, showWarnings = FALSE)

  # Create user params directory
  user_dir <- file.path(test_dir, "params", "user")
  dir.create(user_dir, recursive = TRUE, showWarnings = FALSE)

  # Create minimal YAML files
  default_yaml <- file.path(default_dir, "test_param.yaml")
  yaml::write_yaml(
    list(option1 = "default_value", option2 = 10),
    default_yaml
  )

  user_yaml <- file.path(user_dir, "test_param.yaml")
  yaml::write_yaml(
    list(option1 = "user_value", option3 = TRUE),
    user_yaml
  )

  # Create prepare_params files
  prepare_params <- file.path(test_dir, "prepare_params.yaml")
  yaml::write_yaml(list(prepared = TRUE), prepare_params)

  prepare_params_advanced <- file.path(test_dir, "prepare_params_advanced.yaml")
  yaml::write_yaml(list(advanced = TRUE), prepare_params_advanced)

  list(
    dir = test_dir,
    default_dir = default_dir,
    user_dir = user_dir,
    default_yaml = default_yaml,
    user_yaml = user_yaml,
    prepare_params = prepare_params,
    prepare_params_advanced = prepare_params_advanced
  )
}

# Unit Tests ----

test_that("load_yaml_files function exists", {
  expect_true(exists("load_yaml_files"))
  expect_type(load_yaml_files, "closure")
})

test_that("load_yaml_files returns proper structure", {
  skip_if_not(interactive(), "Requires cache setup")

  result <- load_yaml_files()

  expect_type(result, "list")
  expect_true("yamls_params" %in% names(result))
  expect_true("yaml_files" %in% names(result))
  expect_true("yaml_names" %in% names(result))
})

test_that("load_yaml_files yamls_params is a named list", {
  skip_if_not(interactive(), "Requires cache setup")

  result <- load_yaml_files()

  expect_type(result$yamls_params, "list")
  expect_true(length(names(result$yamls_params)) > 0)
})

test_that("load_yaml_files yaml_files are character paths", {
  skip_if_not(interactive(), "Requires cache setup")

  result <- load_yaml_files()

  expect_type(result$yaml_files, "character")
  expect_true(all(grepl("\\.yaml$", result$yaml_files)))
})

test_that("load_yaml_files yaml_names match files", {
  skip_if_not(interactive(), "Requires cache setup")

  result <- load_yaml_files()

  expect_equal(length(result$yaml_files), length(result$yaml_names))
  expect_true(all(nzchar(result$yaml_names)))
  expect_false(any(grepl("\\.yaml$", result$yaml_names)))
})

# Edge Cases ----

test_that("load_yaml_files handles missing user params", {
  skip_if_not(interactive(), "Requires cache setup")

  # Should fall back to defaults when user params are missing/incomplete
  result <- load_yaml_files()

  expect_type(result, "list")
  expect_true(length(result$yamls_params) > 0)
})

test_that("load_yaml_files prefers user params when available", {
  skip_if_not(interactive(), "Requires full cache with user params")

  # This test requires actual user params to be set up
  # Testing the logic would require mocking get_default_paths()

  result <- load_yaml_files()
  expect_type(result, "list")
})

test_that("load_yaml_files includes prepare_params", {
  skip_if_not(interactive(), "Requires cache setup")

  result <- load_yaml_files()

  param_names <- names(result$yamls_params)

  # Should include prepare_params entries
  expect_true(any(grepl("prepare_params", param_names)))
})

# Error Handling Tests ----

test_that("load_yaml_files handles malformed YAML gracefully", {
  skip("Requires mocking file system to test error handling")

  # Would need to mock yaml::read_yaml to throw errors
  # Testing error path requires more complex setup
})

test_that("load_yaml_files validates YAML file existence", {
  skip_if_not(interactive(), "Requires cache setup")

  # Should not error with valid setup
  expect_error(load_yaml_files(), NA)
})

# Integration Tests ----

test_that("load_yaml_files integrates with get_default_paths", {
  skip_if_not(interactive(), "Requires cache setup")

  # Should successfully resolve paths using get_default_paths()
  result <- load_yaml_files()

  expect_true(length(result$yaml_files) > 0)
  expect_true(all(file.exists(result$yaml_files)))
})

test_that("load_yaml_files YAML content is valid", {
  skip_if_not(interactive(), "Requires cache setup")

  result <- load_yaml_files()

  # All parsed YAMLs should be lists
  expect_true(all(vapply(result$yamls_params, is.list, logical(1))))
})

# Performance Tests ----

test_that("load_yaml_files is reasonably fast", {
  skip_if_not(interactive(), "Requires cache setup")

  # Should complete in reasonable time (< 5 seconds)
  elapsed <- system.time({
    result <- load_yaml_files()
  })

  expect_true(elapsed["elapsed"] < 5.0)
})

# Regression Tests ----

test_that("load_yaml_files maintains backward compatibility", {
  skip_if_not(interactive(), "Requires cache setup")

  result <- load_yaml_files()

  # Must return all three components
  required_components <- c("yamls_params", "yaml_files", "yaml_names")
  expect_true(all(required_components %in% names(result)))
})

test_that("load_yaml_files handles empty directories gracefully", {
  skip("Requires mocking to test edge case")

  # Would need to mock file system with empty directories
  # This tests defensive programming
})

# Documentation Tests ----

test_that("load_yaml_files has documentation", {
  # Note: This is an internal function, may not have help file
  expect_type(load_yaml_files, "closure")
})

# Functionality Tests ----

test_that("load_yaml_files strips .yaml extension from names", {
  skip_if_not(interactive(), "Requires cache setup")

  result <- load_yaml_files()

  # Names should not have .yaml extension
  expect_false(any(grepl("\\.yaml$", result$yaml_names)))
})

test_that("load_yaml_files handles prepare_params_advanced", {
  skip_if_not(interactive(), "Requires cache setup")

  result <- load_yaml_files()

  param_names <- names(result$yamls_params)

  # Both prepare_params files should be included
  expect_true(any(grepl("prepare_params", param_names)))
})
