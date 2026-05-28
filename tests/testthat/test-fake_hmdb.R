# Test Suite: fake_hmdb ----

library(testthat)

test_that("fake_hmdb creates zip file with SDF content", {
  skip_on_os("windows") # zip command may not be available

  temp_file <- withr::local_tempfile(fileext = ".sdf.zip")

  result <- fake_hmdb(export = temp_file)

  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))

  # Verify it's a valid zip file
  zip_list <- utils::unzip(zipfile = temp_file, list = TRUE)
  expect_s3_class(zip_list, "data.frame")
  expect_true(nrow(zip_list) > 0)

  # Extract and verify SDF content
  temp_dir <- tempdir()
  utils::unzip(zipfile = temp_file, exdir = temp_dir)

  sdf_files <- list.files(temp_dir, pattern = "\\.sdf$", full.names = TRUE)
  expect_true(length(sdf_files) > 0)

  sdf_content <- readLines(sdf_files[1])
  expect_true(any(grepl("DATABASE_ID", sdf_content)))
  expect_true(any(grepl("SMILES", sdf_content)))
  expect_true(any(grepl("INCHI_KEY", sdf_content)))
})

test_that("fake_hmdb validates input parameters", {
  # Missing export parameter
  expect_error(
    fake_hmdb(),
    "export path must be a single character string",
    class = "tima_validation_error"
  )

  # NULL export
  expect_error(
    fake_hmdb(export = NULL),
    "export path must be a single character string",
    class = "tima_validation_error"
  )

  # Non-character export
  expect_error(
    fake_hmdb(export = 123),
    "export path must be a single character string",
    class = "tima_validation_error"
  )

  # Multiple paths
  expect_error(
    fake_hmdb(export = c("path1", "path2")),
    "export path must be a single character string",
    class = "tima_validation_error"
  )
})

test_that("fake_hmdb handles zip creation failures gracefully", {
  skip_on_os("windows")

  temp_file <- withr::local_tempfile(fileext = ".sdf.zip")

  expect_no_error(fake_hmdb(export = temp_file))
  expect_true(file.exists(temp_file))
})

test_that("fake_hmdb calls fallback zip when system zip command fails", {
  withr::local_dir(tempdir())
  temp_file <- withr::local_tempfile(fileext = ".sdf.zip")
  fallback_called <- FALSE

  with_mocked_bindings(
    .fake_hmdb_zip_cmd = function(export, fake_export) 1L,
    .fake_hmdb_zip_fallback = function(export, fake_export) {
      fallback_called <<- TRUE
      file.create(basename(export))
      invisible(NULL)
    },
    .fake_hmdb_archive_exists = function(export) TRUE,
    move_file_safely = function(from, to) {
      file.create(to)
      TRUE
    },
    log_warn = function(...) invisible(NULL),
    log_debug = function(...) invisible(NULL),
    {
      expect_equal(fake_hmdb(export = temp_file), temp_file)
    }
  )

  expect_true(fallback_called)
})

test_that("fake_hmdb errors when temporary archive cannot be moved", {
  withr::local_dir(tempdir())
  temp_file <- withr::local_tempfile(fileext = ".sdf.zip")

  expect_error(
    with_mocked_bindings(
      .fake_hmdb_zip_cmd = function(export, fake_export) 0L,
      .fake_hmdb_archive_exists = function(export) TRUE,
      move_file_safely = function(from, to) FALSE,
      log_warn = function(...) invisible(NULL),
      fake_hmdb(export = temp_file)
    ),
    class = "tima_runtime_error"
  )
})
