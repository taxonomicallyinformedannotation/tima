# Test Suite: parse_yaml_params ----

library(testthat)

test_that("parse_yaml_params errors if default file missing", {
  expect_error(parse_yaml_params(def = "missing.yaml", usr = NULL), "not found")
})

test_that("parse_yaml_params loads default YAML", {
  writeLines("a: 1", "def.yaml")
  params <- parse_yaml_params(def = "def.yaml", usr = NULL)
  expect_type(params, "list")
  expect_equal(params$a, 1)
})

test_that("parse_yaml_params merges user YAML overriding defaults", {
  writeLines("a: 1\nb: 2", "def.yaml")
  writeLines("b: 3\nc: 4", "usr.yaml")
  params <- parse_yaml_params(def = "def.yaml", usr = "usr.yaml")
  expect_equal(params$b, 3)
  expect_equal(params$a, 1)
  expect_equal(params$c, 4)
})
