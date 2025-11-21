# Test Suite: load_yaml_files ----

# library(testthat)
#
# test_that("load_yaml_files returns list with expected structure", {
#   local_test_project(copy = TRUE)
#
#   result <- load_yaml_files()
#
#   expect_type(result, "list")
#   expect_named(result, c("yamls_params", "yaml_files", "yaml_names"))
# })
#
# test_that("load_yaml_files yamls_params is a list", {
#   local_test_project(copy = TRUE)
#
#   result <- load_yaml_files()
#
#   expect_type(result$yamls_params, "list")
#   expect_true(length(result$yamls_params) > 0)
# })
#
# test_that("load_yaml_files yaml_files contains file paths", {
#   local_test_project(copy = TRUE)
#
#   result <- load_yaml_files()
#
#   expect_type(result$yaml_files, "character")
#   expect_true(all(grepl("\\.yaml$", result$yaml_files)))
# })
#
# test_that("load_yaml_files yaml_names matches number of files", {
#   local_test_project(copy = TRUE)
#
#   result <- load_yaml_files()
#
#   expect_equal(length(result$yaml_names), length(result$yaml_files))
#   expect_equal(length(result$yaml_names), length(result$yamls_params))
# })
#
# test_that("load_yaml_files includes prepare params entries", {
#   local_test_project(copy = TRUE)
#
#   loaded <- load_yaml_files()
#
#   expect_true(any(grepl("prepare_params", loaded$yaml_names)))
#   expect_true(any(grepl("prepare_params_advanced", loaded$yaml_names)))
# })
#
# test_that("load_yaml_files removes .yaml extension from names", {
#   local_test_project(copy = TRUE)
#
#   loaded <- load_yaml_files()
#
#   # Names should not have .yaml extension
#   expect_false(any(grepl("\\.yaml$", loaded$yaml_names)))
# })
#
# test_that("load_yaml_files parses YAML content correctly", {
#   local_test_project(copy = TRUE)
#
#   loaded <- load_yaml_files()
#
#   # Each entry should be a parsed list
#   expect_true(all(vapply(loaded$yamls_params, is.list, logical(1))))
# })
#
# test_that("load_yaml_files loads both default and prepare params", {
#   local_test_project(copy = TRUE)
#
#   loaded <- load_yaml_files()
#
#   # Should have multiple parameter files
#   expect_true(length(loaded$yamls_params) >= 2)
# })
#
# test_that("load_yaml_files handles user params when available", {
#   local_test_project(copy = TRUE)
#
#   # This test documents behavior - user params take precedence if sufficient
#   loaded <- load_yaml_files()
#
#   # Should successfully load regardless of which set is used
#   expect_type(loaded$yamls_params, "list")
#   expect_true(length(loaded$yamls_params) > 0)
# })
#
# test_that("load_yaml_files prepares named list correctly", {
#   local_test_project(copy = TRUE)
#
#   loaded <- load_yaml_files()
#
#   # Names should be extracted from filenames
#   expect_true(!is.null(names(loaded$yamls_params)))
#   expect_equal(names(loaded$yamls_params), loaded$yaml_names)
# })
