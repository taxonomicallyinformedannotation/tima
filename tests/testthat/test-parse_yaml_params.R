# Test Suite: parse_yaml_params ----

library(testthat)

test_that("parse_yaml_params errors if default file missing", {
  expect_error(
    parse_yaml_params(def = temp_test_path("missing.yaml"), usr = NULL),
    "not found",
    class = "tima_validation_error"
  )
})

test_that("parse_yaml_params errors on invalid default YAML", {
  def_file <- temp_test_path("broken_default.yaml")
  writeLines("a: [", def_file)

  expect_error(
    parse_yaml_params(def = def_file, usr = NULL),
    "failed to parse default YAML file",
    class = "tima_runtime_error"
  )
})

test_that("parse_yaml_params loads default YAML", {
  def_file <- temp_test_path("def.yaml")
  writeLines("a: 1", def_file)
  params <- parse_yaml_params(def = def_file, usr = NULL)
  expect_type(params, "list")
  expect_equal(params$a, 1)
})

test_that("parse_yaml_params errors when default YAML is empty", {
  def_file <- temp_test_path("empty_default.yaml")
  writeLines("{}", def_file)

  expect_error(
    parse_yaml_params(def = def_file, usr = NULL),
    "empty or invalid",
    class = "tima_validation_error"
  )
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

test_that("parse_yaml_params errors on invalid user YAML", {
  def_file <- temp_test_path("def_valid.yaml")
  usr_file <- temp_test_path("usr_broken.yaml")
  writeLines("a: 1", def_file)
  writeLines("a: [", usr_file)

  expect_error(
    parse_yaml_params(def = def_file, usr = usr_file),
    "failed to parse user YAML file",
    class = "tima_runtime_error"
  )
})

test_that("parse_yaml_params keeps defaults when user YAML is not a list", {
  def_file <- temp_test_path("def_list.yaml")
  usr_file <- temp_test_path("usr_scalar.yaml")
  writeLines("a: 1\nb: 2", def_file)
  writeLines("5", usr_file)

  params <- parse_yaml_params(def = def_file, usr = usr_file)

  expect_equal(params$a, 1)
  expect_equal(params$b, 2)
})

test_that("merge_lists_recursive handles non-list user and nested overrides", {
  expect_equal(merge_lists_recursive(list(a = 1), 2), 2)

  merged <- merge_lists_recursive(
    default = list(a = list(x = 1, y = 2), b = 3),
    user = list(a = list(y = 9, z = 10))
  )

  expect_equal(merged$a$x, 1)
  expect_equal(merged$a$y, 9)
  expect_equal(merged$a$z, 10)
  expect_equal(merged$b, 3)
})
