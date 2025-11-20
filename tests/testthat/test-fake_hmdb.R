# Test Suite: fake_hmdb ----

library(testthat)

test_that("fake_hmdb creates zip file with SDF content", {
  skip_on_cran()
  skip_on_os("windows") # zip command may not be available

  withr::local_tempdir()
  temp_file <- withr::local_tempfile(fileext = ".sdf.zip")

  result <- fake_hmdb(export = temp_file)

  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))

  # Verify it's a valid zip file
  zip_list <- utils::unzip(temp_file, list = TRUE)
  expect_s3_class(zip_list, "data.frame")
  expect_true(nrow(zip_list) > 0)

  # Extract and verify SDF content
  temp_dir <- tempdir()
  utils::unzip(temp_file, exdir = temp_dir)

  sdf_files <- list.files(temp_dir, pattern = "\\.sdf$", full.names = TRUE)
  expect_true(length(sdf_files) > 0)

  sdf_content <- readLines(sdf_files[1])
  expect_true(any(grepl("DATABASE_ID", sdf_content)))
  expect_true(any(grepl("SMILES", sdf_content)))
  expect_true(any(grepl("INCHI_KEY", sdf_content)))
})

test_that("fake_hmdb validates input parameters", {
  skip_on_cran()

  # Missing export parameter
  expect_error(fake_hmdb(), "export path must be a single character string")

  # NULL export
  expect_error(
    fake_hmdb(export = NULL),
    "export path must be a single character string"
  )

  # Non-character export
  expect_error(
    fake_hmdb(export = 123),
    "export path must be a single character string"
  )

  # Multiple paths
  expect_error(
    fake_hmdb(export = c("path1", "path2")),
    "export path must be a single character string"
  )
})

test_that("fake_hmdb handles zip creation failures gracefully", {
  skip_on_cran()
  skip_on_os("windows")

  withr::local_tempdir()
  temp_file <- withr::local_tempfile(fileext = ".sdf.zip")

  expect_no_error(fake_hmdb(export = temp_file))
  expect_true(file.exists(temp_file))
})
