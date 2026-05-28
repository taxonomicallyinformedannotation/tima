# Test Suite: fake_ecmdb ----

library(testthat)

test_that("fake_ecmdb creates zip file with JSON content", {
  skip_on_os("windows") # zip command may not be available

  temp_file <- withr::local_tempfile(fileext = ".json.zip")

  result <- fake_ecmdb(export = temp_file)

  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))

  # Verify it's a valid zip file
  zip_list <- utils::unzip(zipfile = temp_file, list = TRUE)
  expect_s3_class(zip_list, "data.frame")
  expect_true(nrow(zip_list) > 0)

  # Extract and verify JSON content
  temp_dir <- tempdir()
  utils::unzip(zipfile = temp_file, exdir = temp_dir)

  json_files <- list.files(temp_dir, pattern = "\\.json$", full.names = TRUE)
  expect_true(length(json_files) > 0)

  json_content <- readLines(json_files[1])
  expect_true(any(grepl("moldb_inchikey", json_content)))
  expect_true(any(grepl("moldb_smiles", json_content)))
})

test_that("fake_ecmdb validates input parameters", {
  # Missing export parameter
  expect_error(fake_ecmdb(), "export path must be a single character string")

  # NULL export
  expect_error(
    fake_ecmdb(export = NULL),
    "export path must be a single character string"
  )

  # Non-character export
  expect_error(
    fake_ecmdb(export = 123),
    "export path must be a single character string"
  )

  # Multiple paths
  expect_error(
    fake_ecmdb(export = c("path1", "path2")),
    "export path must be a single character string"
  )
})

test_that("fake_ecmdb handles zip creation failures gracefully", {
  skip_on_os("windows")

  temp_file <- withr::local_tempfile(fileext = ".json.zip")

  expect_no_error(fake_ecmdb(export = temp_file))
  expect_true(file.exists(temp_file))
})

test_that("fake_ecmdb calls fallback zip when system zip command fails", {
  withr::local_dir(tempdir())
  temp_file <- withr::local_tempfile(fileext = ".json.zip")
  fallback_called <- FALSE

  with_mocked_bindings(
    .fake_zip_cmd = function(export, fake_export) 1L,
    .fake_zip_fallback = function(export, fake_export) {
      fallback_called <<- TRUE
      file.create(basename(export))
      invisible(NULL)
    },
    .fake_archive_exists = function(export) TRUE,
    move_file_safely = function(from, to) {
      file.create(to)
      TRUE
    },
    log_warn = function(...) invisible(NULL),
    log_debug = function(...) invisible(NULL),
    {
      expect_equal(fake_ecmdb(export = temp_file), temp_file)
    }
  )

  expect_true(fallback_called)
})

test_that("fake_ecmdb errors when temporary archive cannot be moved", {
  withr::local_dir(tempdir())
  temp_file <- withr::local_tempfile(fileext = ".json.zip")

  expect_error(
    with_mocked_bindings(
      .fake_zip_cmd = function(export, fake_export) 0L,
      .fake_archive_exists = function(export) TRUE,
      move_file_safely = function(from, to) FALSE,
      log_warn = function(...) invisible(NULL),
      fake_ecmdb(export = temp_file)
    ),
    class = "tima_runtime_error"
  )
})
