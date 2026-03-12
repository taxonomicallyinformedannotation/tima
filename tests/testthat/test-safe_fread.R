# Test Suite: safe_fread ----
# Tests the error-wrapping fread wrapper.

library(testthat)

write_tsv <- function(df) {
  tf <- tempfile(fileext = ".tsv")
  tidytable::fwrite(x = df, file = tf, sep = "\t")
  tf
}

test_that("safe_fread reads a valid TSV file", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  tf <- write_tsv(df)
  on.exit(unlink(tf))

  result <- tima:::safe_fread(file = tf)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3L)
  expect_equal(names(result), c("a", "b"))
})

test_that("safe_fread errors with actionable message on missing file", {
  expect_error(
    tima:::safe_fread(
      file = tempfile(fileext = ".tsv"),
      file_type = "features table"
    ),
    "Features Table"
  )
})

test_that("safe_fread validates required_cols after reading", {
  df <- data.frame(feature_id = 1:3, mz = c(100, 200, 300))
  tf <- write_tsv(df)
  on.exit(unlink(tf))

  # Should pass when all required columns present
  expect_no_error(tima:::safe_fread(tf, required_cols = c("feature_id", "mz")))

  # Should error when required column missing
  expect_error(
    tima:::safe_fread(tf, required_cols = c("feature_id", "rt")),
    "rt"
  )
})

test_that("safe_fread passes extra arguments to fread", {
  # Write a semicolon-delimited file
  tf <- tempfile(fileext = ".csv")
  writeLines(c("a;b", "1;x", "2;y"), tf)
  on.exit(unlink(tf))

  result <- tima:::safe_fread(file = tf, sep = ";")
  expect_equal(nrow(result), 2L)
  expect_equal(names(result), c("a", "b"))
})
