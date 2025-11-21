# Test Suite: parse_yaml_params ----

library(testthat)

test_that("parse_yaml_params errors if default file missing", {
  expect_error(
    parse_yaml_params(def = temp_test_path("missing.yaml"), usr = NULL),
    "not found"
  )
})

test_that("parse_yaml_params loads default YAML", {
  def_file <- temp_test_path("def.yaml")
  writeLines("a: 1", def_file)
  params <- parse_yaml_params(def = def_file, usr = NULL)
  expect_type(params, "list")
  expect_equal(params$a, 1)
})

test_that("parse_yaml_params merges user YAML overriding defaults", {
  def_file <- temp_test_path("def.yaml")
  usr_file <- temp_test_path("usr.yaml")
  writeLines("a: 1\nb: 2", def_file)
  writeLines("b: 3\nc: 4", usr_file)
  params <- parse_yaml_params(def = def_file, usr = usr_file)
  expect_equal(params$b, 3)
  expect_equal(params$a, 1)
  expect_equal(params$c, 4)
})
