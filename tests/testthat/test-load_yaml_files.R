# Test Suite: load_yaml_files ----

library(testthat)

test_that("load_yaml_files function exists and is callable", {
  expect_true(exists("load_yaml_files"))
  expect_type(load_yaml_files, "closure")
})

# test_that("load_yaml_files returns expected structure", {
#   result <- load_yaml_files()
#
#   expect_type(result, "list")
#   expect_named(result, c("yamls_params", "yaml_files", "yaml_names"))
#   expect_type(result$yamls_params, "list")
#   expect_type(result$yaml_files, "character")
#   expect_type(result$yaml_names, "character")
# })

# test_that("load_yaml_files loads default parameters", {
#   result <- load_yaml_files()
#
#   # Should have some YAML files loaded
#   expect_true(length(result$yaml_files) > 0)
#   expect_true(length(result$yamls_params) > 0)
#   expect_equal(length(result$yaml_files), length(result$yaml_names))
# })

# test_that("load_yaml_files names match between files and params", {
#   result <- load_yaml_files()
#
#   # Names should correspond to file paths
#   expect_equal(length(result$yaml_names), length(result$yamls_params))
# })

# test_that("load_yaml_files returns valid YAML content", {
#   result <- load_yaml_files()
#
#   # Each element in yamls_params should be a list (parsed YAML)
#   expect_true(all(vapply(result$yamls_params, is.list, logical(1))))
# })
