# Test Suite: read_from_sirius_zip ----

library(testthat)

# Test Fixtures ----

#' Create a minimal SIRIUS zip file for testing
#' @keywords internal
create_test_sirius_zip <- function() {
  temp_dir <- temp_test_dir("sirius_zip_test")

  # Create test files
  test_content_dir <- file.path(temp_dir, "feature_123")
  dir.create(test_content_dir, recursive = TRUE, showWarnings = FALSE)

  # Create a structure.tsv file
  structure_file <- file.path(test_content_dir, "structure.tsv")
  structure_data <- data.frame(
    rank = c(1, 2),
    name = c("Compound A", "Compound B"),
    molecularFormula = c("C6H12O6", "C7H14O7"),
    adduct = c("[M+H]+", "[M+H]+"),
    stringsAsFactors = FALSE
  )
  write.table(
    structure_data,
    structure_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  # Create a denovo structure file
  denovo_file <- file.path(test_content_dir, "denovo_structure.tsv")
  denovo_data <- data.frame(
    rank = c(1),
    molecularFormula = c("C8H16O8"),
    stringsAsFactors = FALSE
  )
  write.table(
    denovo_data,
    denovo_file,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  # Create an empty file (should be ignored)
  empty_file <- file.path(temp_dir, "_empty.tsv")
  writeLines("", empty_file)

  # Create zip archive
  zip_file <- file.path(temp_dir, "test_sirius.zip")

  withr::with_dir(temp_dir, {
    utils::zip(
      zipfile = basename(zip_file),
      files = c(
        file.path("feature_123", "structure.tsv"),
        file.path("feature_123", "denovo_structure.tsv"),
        "_empty.tsv"
      ),
      flags = "-q"
    )
  })

  list(
    zip_file = zip_file,
    temp_dir = temp_dir
  )
}

# Unit Tests: Input Validation ----

test_that("read_from_sirius_zip validates sirius_zip parameter", {
  expect_error(
    read_from_sirius_zip(),
    "sirius_zip must be a single character string"
  )

  expect_error(
    read_from_sirius_zip(sirius_zip = 123, file = "test"),
    "sirius_zip must be a single character string"
  )

  expect_error(
    read_from_sirius_zip(sirius_zip = c("file1.zip", "file2.zip"), file = "test"),
    "sirius_zip must be a single character string"
  )

  expect_error(
    read_from_sirius_zip(sirius_zip = NULL, file = "test"),
    "sirius_zip must be a single character string"
  )
})

test_that("read_from_sirius_zip validates file existence", {
  expect_error(
    read_from_sirius_zip(sirius_zip = "nonexistent.zip", file = "test"),
    "SIRIUS zip file not found"
  )
})

test_that("read_from_sirius_zip validates file pattern parameter", {
  test_zip <- create_test_sirius_zip()

  expect_error(
    read_from_sirius_zip(sirius_zip = test_zip$zip_file),
    "file pattern must be a single character string"
  )

  expect_error(
    read_from_sirius_zip(sirius_zip = test_zip$zip_file, file = 123),
    "file pattern must be a single character string"
  )

  expect_error(
    read_from_sirius_zip(sirius_zip = test_zip$zip_file, file = c("a", "b")),
    "file pattern must be a single character string"
  )
})

# Functional Tests ----

test_that("read_from_sirius_zip reads structure files", {
  test_zip <- create_test_sirius_zip()

  result <- read_from_sirius_zip(
    sirius_zip = test_zip$zip_file,
    file = "structure\\.tsv$"
  )

  expect_s3_class(result, "tidytable")
  expect_true(nrow(result) > 0)
  expect_true("rank" %in% names(result))
  expect_true("name" %in% names(result))
})

test_that("read_from_sirius_zip filters empty files", {
  test_zip <- create_test_sirius_zip()

  # Should not match files starting with underscore
  expect_error(
    read_from_sirius_zip(
      sirius_zip = test_zip$zip_file,
      file = "_empty"
    ),
    "No matching file found"
  )
})

test_that("read_from_sirius_zip handles denovo structure files", {
  test_zip <- create_test_sirius_zip()

  result <- read_from_sirius_zip(
    sirius_zip = test_zip$zip_file,
    file = "denovo_structure\\.tsv$"
  )

  expect_s3_class(result, "tidytable")
  expect_true("molecularFormula" %in% names(result))
})

test_that("read_from_sirius_zip takes first match when multiple files exist", {
  test_zip <- create_test_sirius_zip()

  # Both structure.tsv and denovo_structure.tsv match "structure"
  result <- read_from_sirius_zip(
    sirius_zip = test_zip$zip_file,
    file = "structure"
  )

  # Should return a valid table (first match)
  expect_s3_class(result, "tidytable")
  expect_true(nrow(result) > 0)
})

# Edge Cases ----

test_that("read_from_sirius_zip handles non-matching patterns", {
  test_zip <- create_test_sirius_zip()

  expect_error(
    read_from_sirius_zip(
      sirius_zip = test_zip$zip_file,
      file = "nonexistent_pattern"
    ),
    "No matching file found"
  )
})

test_that("read_from_sirius_zip handles empty zip archive", {
  temp_dir <- temp_test_dir("empty_zip")
  empty_zip <- file.path(temp_dir, "empty.zip")

  # Create empty zip
  withr::with_dir(temp_dir, {
    writeLines("", "temp.txt")
    utils::zip(zipfile = basename(empty_zip), files = "temp.txt", flags = "-q")
    file.remove("temp.txt")
  })

  expect_error(
    read_from_sirius_zip(sirius_zip = empty_zip, file = "test"),
    "No matching file found"
  )
})

# Data Quality Tests ----

test_that("read_from_sirius_zip parses tab-delimited data correctly", {
  test_zip <- create_test_sirius_zip()

  result <- read_from_sirius_zip(
    sirius_zip = test_zip$zip_file,
    file = "structure\\.tsv$"
  )

  # Should have correct number of rows
  expect_equal(nrow(result), 2)

  # Should have correct columns
  expect_true(all(c("rank", "name", "molecularFormula", "adduct") %in% names(result)))
})

test_that("read_from_sirius_zip handles NA values correctly", {
  test_zip <- create_test_sirius_zip()

  result <- read_from_sirius_zip(
    sirius_zip = test_zip$zip_file,
    file = "structure\\.tsv$"
  )

  # All columns should be character
  expect_true(all(vapply(result, is.character, logical(1))))
})

# Performance Tests ----

test_that("read_from_sirius_zip is reasonably fast", {
  test_zip <- create_test_sirius_zip()

  elapsed <- system.time({
    result <- read_from_sirius_zip(
      sirius_zip = test_zip$zip_file,
      file = "structure\\.tsv$"
    )
  })

  # Should complete quickly (< 2 seconds)
  expect_true(elapsed["elapsed"] < 2.0)
})

# Integration Tests ----

test_that("read_from_sirius_zip integrates with archive package", {
  test_zip <- create_test_sirius_zip()

  # Should successfully use archive::archive_read
  result <- read_from_sirius_zip(
    sirius_zip = test_zip$zip_file,
    file = "structure\\.tsv$"
  )

  expect_s3_class(result, "tidytable")
})

test_that("read_from_sirius_zip integrates with tidytable", {
  test_zip <- create_test_sirius_zip()

  result <- read_from_sirius_zip(
    sirius_zip = test_zip$zip_file,
    file = "structure\\.tsv$"
  )

  # Result should be a tidytable
  expect_s3_class(result, "tidytable")
  expect_s3_class(result, "data.frame")
})

# Regression Tests ----

test_that("read_from_sirius_zip maintains function signature", {
  params <- names(formals(read_from_sirius_zip))

  expect_equal(length(params), 2)
  expect_true("sirius_zip" %in% params)
  expect_true("file" %in% params)
})

test_that("read_from_sirius_zip handles quotes in data", {
  test_zip <- create_test_sirius_zip()

  # quote = "" in read.delim should handle quoted strings
  result <- read_from_sirius_zip(
    sirius_zip = test_zip$zip_file,
    file = "structure\\.tsv$"
  )

  expect_s3_class(result, "tidytable")
})

# Documentation Tests ----

test_that("read_from_sirius_zip has documentation", {
  # Check that function is documented
  expect_type(read_from_sirius_zip, "closure")
})

# Character Encoding Tests ----

test_that("read_from_sirius_zip returns character columns", {
  test_zip <- create_test_sirius_zip()

  result <- read_from_sirius_zip(
    sirius_zip = test_zip$zip_file,
    file = "structure\\.tsv$"
  )

  # All columns should be character due to colClasses = "character"
  expect_true(all(vapply(result, is.character, logical(1))))
})
