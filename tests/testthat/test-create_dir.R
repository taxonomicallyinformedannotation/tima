# Test Suite: create_dir ----

library(testthat)

test_that("create_dir creates directory for file path", {
  target <- temp_test_path("sub/dir/out.tsv")
  create_dir(export = target)
  expected_dir <- dirname(target)
  expect_true(dir.exists(expected_dir))
})

# Updated validation error expectations (now using validate_character)
test_that("create_dir validates input", {
  expect_error(create_dir(export = NULL), "export cannot be NULL")
  expect_error(create_dir(export = ""), "export cannot be an empty string")
  # Vector input should error about single character string
  expect_error(create_dir(export = c("a", "b")), "single character string")
})

test_that("create_dir creates directory when export path is a directory", {
  dir_path <- temp_test_path("subdir_only")
  create_dir(export = dir_path)
  expect_true(dir.exists(dir_path))
})

test_that("create_dir is idempotent", {
  dir_path <- temp_test_path("idempotent_dir")
  create_dir(export = dir_path)
  expect_true(dir.exists(dir_path))
  # Call again should not error
  expect_silent(create_dir(export = dir_path))
})

test_that("create_dir works for deeply nested path", {
  base_dir <- temp_test_path("a")
  nested <- file.path(base_dir, "b/c/d/e/f/output.txt")
  create_dir(export = nested)
  expect_true(dir.exists(file.path(base_dir, "b/c/d/e/f")))
})

# test_that("create_dir warns on very long path (length heuristic)", {
#   long_component <- paste(rep("x", MAX_PATH_LENGTH + 5), collapse = "")
#   long_path <- file.path(long_component, "file.txt")
#   expect_warning(
#     create_dir(export = long_path),
#     "exceeds typical OS maximum",
#     ignore.case = TRUE
#   )
# })
