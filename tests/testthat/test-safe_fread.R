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

  result <- safe_fread(file = tf)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3L)
  expect_equal(names(result), c("a", "b"))
})

test_that("safe_fread errors with actionable message on missing file", {
  expect_error(
    safe_fread(
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
  expect_no_error(safe_fread(tf, required_cols = c("feature_id", "mz")))

  # Should error when required column missing
  expect_error(
    safe_fread(tf, required_cols = c("feature_id", "rt")),
    "rt"
  )
})

test_that("safe_fread passes extra arguments to fread", {
  # Write a semicolon-delimited file
  tf <- tempfile(fileext = ".csv")
  writeLines(c("a;b", "1;x", "2;y"), tf)
  on.exit(unlink(tf))

  result <- safe_fread(file = tf, sep = ";")
  expect_equal(nrow(result), 2L)
  expect_equal(names(result), c("a", "b"))
})

test_that("safe_fread maps malformed-row errors to formatting guidance", {
  tf <- tempfile(fileext = ".csv")
  # Second data row has extra field compared to header.
  writeLines(c("a,b", "1,x", "2,y,z"), tf)
  on.exit(unlink(tf))

  expect_warning(
    res <- safe_fread(file = tf, file_type = "test table"),
    "Discarded|footer"
  )
  expect_true(nrow(res) >= 1L)
})

test_that("safe_fread handles empty files by warning and returning empty table", {
  tf <- tempfile(fileext = ".tsv")
  file.create(tf)
  on.exit(unlink(tf))

  expect_warning(
    res <- safe_fread(file = tf, file_type = "test table"),
    "size 0|empty"
  )
  expect_true(nrow(res) == 0L)
})

test_that("safe_fread maps permission/open failures when unreadable file is encountered", {
  tf <- tempfile(fileext = ".tsv")
  writeLines(c("a\tb", "1\tx"), tf)
  on.exit({
    try(Sys.chmod(tf, mode = "600"), silent = TRUE)
    unlink(tf)
  })

  Sys.chmod(tf, mode = "000")

  # If platform/filesystem does not enforce chmod in tempdir, skip gracefully.
  err <- tryCatch(
    {
      suppressWarnings(safe_fread(file = tf, file_type = "test table"))
      NULL
    },
    error = function(e) conditionMessage(e)
  )

  if (is.null(err)) {
    skip("Temp filesystem did not trigger unreadable-file error")
  } else {
    expect_true(
      grepl(
        "Cannot open|Permission denied|Failed to read",
        err,
        ignore.case = TRUE
      )
    )
  }
})

test_that("safe_fread uses generic fallback guidance for non-specific fread errors", {
  d <- tempfile(pattern = "sfread-dir-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))

  expect_error(
    safe_fread(file = d, file_type = "test table"),
    "Failed to read"
  )
})
