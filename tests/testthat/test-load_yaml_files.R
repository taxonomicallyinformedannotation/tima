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
  yaml::write_yaml(x = list(prepared = TRUE), file = prepare_params)

  prepare_params_advanced <- file.path(test_dir, "prepare_params_advanced.yaml")
  yaml::write_yaml(x = list(advanced = TRUE), file = prepare_params_advanced)

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
