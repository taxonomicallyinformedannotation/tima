# Test Suite: create_dir ----

library(testthat)

test_that("create_dir creates directory for file path", {
  target <- file.path("sub/dir/out.tsv")
  create_dir(export = target)
  expect_true(dir.exists(file.path("sub/dir")))
})

# Updated validation error expectations (now using validate_character)
test_that("create_dir validates input", {
  expect_error(create_dir(export = NULL), "export cannot be NULL")
  expect_error(create_dir(export = ""), "export cannot be an empty string")
  # Vector input should error about single character string
  expect_error(create_dir(export = c("a", "b")), "single character string")
})

test_that("create_dir handles pure directory path", {
  dir_path <- file.path("subdir_only")
  create_dir(export = dir_path)
  expect_true(dir.exists(dir_path))
})

test_that("create_dir is idempotent", {
  dir_path <- file.path("idempotent_dir")
  create_dir(export = dir_path)
  expect_true(dir.exists(dir_path))
  # Call again should not error
  expect_silent(create_dir(export = dir_path))
})

test_that("create_dir works for deeply nested path", {
  nested <- file.path("a/b/c/d/e/f/output.txt")
  create_dir(export = nested)
  expect_true(dir.exists(file.path("a/b/c/d/e/f")))
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
