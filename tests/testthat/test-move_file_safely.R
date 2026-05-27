library(testthat)

test_that("move_file_safely returns FALSE for non-existent source", {
  result <- move_file_safely(from = tempfile("nonexistent_"), to = tempfile())
  expect_false(result)
})

test_that("move_file_safely moves a file successfully", {
  src <- tempfile(pattern = "tima-mfs-src-")
  writeLines("hello", src)
  dst <- tempfile(pattern = "tima-mfs-dst-")
  on.exit(unlink(c(src, dst)), add = TRUE)

  result <- move_file_safely(from = src, to = dst)

  expect_true(result)
  expect_false(file.exists(src))
  expect_true(file.exists(dst))
  expect_equal(readLines(dst), "hello")
})

test_that("move_file_safely creates destination directory if needed", {
  src <- tempfile(pattern = "tima-mfs-src-")
  writeLines("content", src)
  dst_dir <- file.path(tempdir(), paste0("tima-mfs-dir-", Sys.getpid()))
  dst <- file.path(dst_dir, "moved.txt")
  on.exit(unlink(c(src, dst_dir), recursive = TRUE), add = TRUE)

  result <- move_file_safely(from = src, to = dst)

  expect_true(result)
  expect_true(file.exists(dst))
})

test_that("move_file_safely overwrites existing destination", {
  src <- tempfile(pattern = "tima-mfs-src-")
  dst <- tempfile(pattern = "tima-mfs-dst-")
  writeLines("new content", src)
  writeLines("old content", dst)
  on.exit(unlink(c(src, dst)), add = TRUE)

  result <- move_file_safely(from = src, to = dst)

  expect_true(result)
  expect_false(file.exists(src))
  expect_equal(readLines(dst), "new content")
})
