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

test_that("extract_directory_path, ensure_directory_exists, verify_directory_writable, and long-path check handle all branches", {
  # 1. extract_directory_path handles bare filename, nested file, and trailing-slash dir with dot
  expect_identical(extract_directory_path("bare_file.tsv"), ".")
  expect_identical(
    extract_directory_path("parent/child/file.tsv"),
    "parent/child"
  )
  expect_identical(extract_directory_path("dir.with.dot/"), "dir.with.dot/")
  expect_identical(extract_directory_path("plain_dir"), "plain_dir")

  # 2. ensure_directory_exists aborts with tima_runtime_error when dir.create errors
  tmp_dir <- withr::local_tempdir()
  existing <- file.path(tmp_dir, "already_there")
  dir.create(existing)
  expect_true(ensure_directory_exists(existing))

  err <- tryCatch(
    with_mocked_bindings(
      ensure_directory_exists(file.path(tmp_dir, "cannot_create")),
      dir.create = function(...) stop("simulated permission denied"),
      .package = "base"
    ),
    error = identity
  )
  expect_s3_class(err, "tima_runtime_error")
  expect_match(conditionMessage(err), "could not create directory")

  # 3. verify_directory_writable returns FALSE for missing dir and when writeLines fails
  expect_false(verify_directory_writable(file.path(tmp_dir, "missing_dir")))
  expect_true(verify_directory_writable(existing))

  writable_fail <- with_mocked_bindings(
    verify_directory_writable(existing),
    writeLines = function(...) stop("simulated read-only filesystem"),
    .package = "base"
  )
  expect_false(writable_fail)
})

test_that("create_dir handles paths exceeding MAX_PATH_LENGTH", {
  tmp_dir <- withr::local_tempdir()
  withr::local_dir(tmp_dir)
  # Create a path string > MAX_PATH_LENGTH (260 chars) using short nested segments
  segs <- rep("abcdefghij", 26L)
  long_rel <- do.call(file.path, as.list(c(segs, "out.tsv")))
  expect_gt(nchar(long_rel), MAX_PATH_LENGTH)
  expect_no_error(create_dir(export = long_rel))
  expect_true(dir.exists(dirname(long_rel)))
})
