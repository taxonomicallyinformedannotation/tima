# Test Suite: read_from_sirius_zip ----

library(testthat)

test_that("read_from_sirius_zip validates sirius_zip parameter", {
  expect_error(
    read_from_sirius_zip(file = "test.tsv"),
    "sirius_zip must be a single character string"
  )

  expect_error(
    read_from_sirius_zip(sirius_zip = NULL, file = "test.tsv"),
    "sirius_zip must be a single character string"
  )

  expect_error(
    read_from_sirius_zip(sirius_zip = 123, file = "test.tsv"),
    "sirius_zip must be a single character string"
  )

  expect_error(
    read_from_sirius_zip(
      sirius_zip = c("zip1.zip", "zip2.zip"),
      file = "test.tsv"
    ),
    "sirius_zip must be a single character string"
  )
})

test_that("read_from_sirius_zip validates file exists", {
  expect_error(
    read_from_sirius_zip(
      sirius_zip = "nonexistent.zip",
      file = "test.tsv"
    ),
    "SIRIUS zip file not found"
  )
})

test_that("read_from_sirius_zip validates file pattern parameter", {
  temp_zip <- tempfile(fileext = ".zip")
  writeLines("test", temp_zip)

  expect_error(
    read_from_sirius_zip(sirius_zip = temp_zip),
    "file pattern must be a single character string"
  )

  expect_error(
    read_from_sirius_zip(sirius_zip = temp_zip, file = NULL),
    "file pattern must be a single character string"
  )

  expect_error(
    read_from_sirius_zip(sirius_zip = temp_zip, file = 123),
    "file pattern must be a single character string"
  )
})

test_that("read_from_sirius_zip reads file from zip archive", {
  # Create a zip with a tsv file inside
  temp_dir <- tempfile("ziptest")
  dir.create(temp_dir, recursive = TRUE)

  test_file <- file.path(temp_dir, "canopus_summary.tsv")
  writeLines("col1\tcol2\nval1\tval2", test_file)

  temp_zip <- tempfile(fileext = ".zip")
  utils::zip(
    zipfile = temp_zip,
    files = basename(test_file),
    root = temp_dir,
    mode = "cherry-pick"
  )

  result <- read_from_sirius_zip(
    sirius_zip = temp_zip,
    file = "canopus_summary"
  )

  expect_s3_class(result, "data.frame")
  expect_true("col1" %in% names(result))
  expect_true("col2" %in% names(result))
  expect_equal(nrow(result), 1)
})

test_that("read_from_sirius_zip filters out underscore-prefixed files", {
  temp_dir <- tempfile("ziptest2")
  dir.create(temp_dir, recursive = TRUE)

  # Create file with underscore prefix (should be skipped)
  skip_file <- file.path(temp_dir, "_canopus_summary.tsv")
  writeLines("skip\nskip", skip_file)

  # Create valid file
  valid_file <- file.path(temp_dir, "canopus_summary.tsv")
  writeLines("col1\tcol2\nval1\tval2", valid_file)

  temp_zip <- tempfile(fileext = ".zip")
  utils::zip(
    zipfile = temp_zip,
    files = c(basename(skip_file), basename(valid_file)),
    root = temp_dir,
    mode = "cherry-pick"
  )

  result <- read_from_sirius_zip(
    sirius_zip = temp_zip,
    file = "canopus_summary"
  )

  # Should read the valid file, not the underscore one
  expect_s3_class(result, "data.frame")
  expect_true("col1" %in% names(result))
})

test_that("read_from_sirius_zip handles no matching files", {
  temp_dir <- tempfile("ziptest3")
  dir.create(temp_dir, recursive = TRUE)

  test_file <- file.path(temp_dir, "other_file.tsv")
  writeLines("col1\tcol2\nval1\tval2", test_file)

  temp_zip <- tempfile(fileext = ".zip")
  utils::zip(
    zipfile = temp_zip,
    files = basename(test_file),
    root = temp_dir,
    mode = "cherry-pick"
  )

  expect_error(
    read_from_sirius_zip(
      sirius_zip = temp_zip,
      file = "nonexistent_pattern"
    ),
    "No matching file found for pattern"
  )
})

test_that("read_from_sirius_zip takes first match when multiple files match", {
  temp_dir <- tempfile("ziptest4")
  dir.create(temp_dir, recursive = TRUE)

  # Create multiple matching files
  file1 <- file.path(temp_dir, "a_formula_identifications.tsv")
  file2 <- file.path(temp_dir, "b_formula_identifications.tsv")

  writeLines("col1\nfirst", file1)
  writeLines("col1\nsecond", file2)

  temp_zip <- tempfile(fileext = ".zip")
  utils::zip(
    zipfile = temp_zip,
    files = c(basename(file1), basename(file2)),
    root = temp_dir,
    mode = "cherry-pick"
  )

  result <- read_from_sirius_zip(
    sirius_zip = temp_zip,
    file = "formula_identifications"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("read_from_sirius_zip parses tab-delimited data correctly", {
  temp_dir <- tempfile("ziptest5")
  dir.create(temp_dir, recursive = TRUE)

  test_file <- file.path(temp_dir, "test.tsv")
  writeLines("name\tvalue\tcount\nitem1\t123\t5\nitem2\t456\t10", test_file)

  temp_zip <- tempfile(fileext = ".zip")
  utils::zip(
    zipfile = temp_zip,
    files = basename(test_file),
    root = temp_dir,
    mode = "cherry-pick"
  )

  result <- read_from_sirius_zip(
    sirius_zip = temp_zip,
    file = "test"
  )

  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 2)
  expect_true(all(c("name", "value", "count") %in% names(result)))
})

test_that("read_from_sirius_zip handles NA strings correctly", {
  temp_dir <- tempfile("ziptest6")
  dir.create(temp_dir, recursive = TRUE)

  test_file <- file.path(temp_dir, "test.tsv")
  writeLines("col1\tcol2\nval1\tNA\n\tval2", test_file)

  temp_zip <- tempfile(fileext = ".zip")
  utils::zip(
    zipfile = temp_zip,
    files = basename(test_file),
    root = temp_dir,
    mode = "cherry-pick"
  )

  result <- read_from_sirius_zip(
    sirius_zip = temp_zip,
    file = "test"
  )

  # NA strings should be parsed as NA
  expect_true(is.na(result$col2[1]))
  expect_true(is.na(result$col1[2]))
})
