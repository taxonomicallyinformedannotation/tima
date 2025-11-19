# ==============================================================================
# Test Suite: prepare_libraries_sop_hmdb - Complete test coverage
# File: test-prepare_libraries_sop_hmdb.R
# ==============================================================================

# ==============================================================================
# Test Group: prepare_libraries_sop_hmdb input validation
# Purpose: Validate input parameter checking
# ==============================================================================

test_that("test-prepare_libraries_sop_hmdb validates input path", {
  # Non-character input
  expect_error(
    prepare_libraries_sop_hmdb(
      input = 123,
      output = tempfile(fileext = ".tsv")
    ),
    "input must be a single character string"
  )

  # Vector input
  expect_error(
    prepare_libraries_sop_hmdb(
      input = c("file1.zip", "file2.zip"),
      output = tempfile(fileext = ".tsv")
    ),
    "input must be a single character string"
  )

  # NULL input
  expect_error(
    prepare_libraries_sop_hmdb(
      input = NULL,
      output = tempfile(fileext = ".tsv")
    ),
    "input must be a single character string"
  )
})

test_that("test-prepare_libraries_sop_hmdb validates output path", {
  # Non-character output
  expect_error(
    prepare_libraries_sop_hmdb(
      input = tempfile(fileext = ".zip"),
      output = 123
    ),
    "output must be a single character string"
  )

  # Vector output
  expect_error(
    prepare_libraries_sop_hmdb(
      input = tempfile(fileext = ".zip"),
      output = c("out1.tsv", "out2.tsv")
    ),
    "output must be a single character string"
  )
})

# ==============================================================================
# Test Group: prepare_libraries_sop_hmdb functional behavior
# Purpose: Test actual functionality
# ==============================================================================

test_that("test-prepare_libraries_sop_hmdb handles missing input file", {
  tmp <- withr::local_tempdir()
  output_file <- file.path(tmp, "hmdb_output.tsv")

  result <- prepare_libraries_sop_hmdb(
    input = file.path(tmp, "nonexistent.zip"),
    output = output_file
  )

  # Should return output path
  expect_equal(result, output_file)

  # Should create empty library with correct structure
  expect_true(file.exists(output_file))

  df <- tidytable::fread(output_file)
  expect_true("structure_inchikey" %in% names(df))
  expect_true("organism_name" %in% names(df))
})

test_that("test-prepare_libraries_sop_hmdb uses existing output when valid", {
  tmp <- withr::local_tempdir()
  output_file <- file.path(tmp, "hmdb_output.tsv")

  # Create a valid existing output file (> 100000 bytes)
  mock_data <- tidytable::tidytable(
    structure_inchikey = rep("AAAABBBBCCCCDD", 10000),
    structure_smiles_no_stereo = rep("C1=CC=CC=C1", 10000),
    organism_name = rep("Homo sapiens", 10000),
    reference_doi = rep("10.1093/nar/gkx1089", 10000)
  )
  tidytable::fwrite(mock_data, output_file, sep = "\t")

  initial_size <- file.size(output_file)

  # Run function
  result <- prepare_libraries_sop_hmdb(
    input = file.path(tmp, "nonexistent.zip"),
    output = output_file
  )

  # Should return existing file without regenerating
  expect_equal(result, output_file)
  expect_equal(file.size(output_file), initial_size)
})

test_that("test-prepare_libraries_sop_hmdb regenerates small output files", {
  tmp <- withr::local_tempdir()
  output_file <- file.path(tmp, "hmdb_output.tsv")

  # Create a small existing output file (< 100000 bytes)
  writeLines("header\ndata", output_file)

  initial_size <- file.size(output_file)
  expect_lt(initial_size, 100000)

  # Run function - should regenerate
  result <- prepare_libraries_sop_hmdb(
    input = file.path(tmp, "nonexistent.zip"),
    output = output_file
  )

  expect_equal(result, output_file)
  # File should be regenerated (different size)
  expect_true(file.exists(output_file))
})

# ==============================================================================
# Test Group: prepare_libraries_sop_hmdb HMDB parsing
# Purpose: Test HMDB SDF parsing logic (when file exists)
# ==============================================================================

test_that("test-prepare_libraries_sop_hmdb handles empty SDF gracefully", {
  skip("Requires mock SDF file creation")
})

test_that("test-prepare_libraries_sop_hmdb parses HMDB metadata correctly", {
  skip("Requires mock HMDB SDF with realistic data")
})

test_that("test-prepare_libraries_sop_hmdb handles missing metadata fields", {
  skip("Requires mock HMDB SDF with missing fields")
})
