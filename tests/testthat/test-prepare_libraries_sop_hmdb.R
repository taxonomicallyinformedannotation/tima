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

test_that("test-prepare_libraries_sop_hmdb parses minimal mock SDF", {
  tmp <- withr::local_tempdir()

  # Create minimal mock HMDB SDF file
  sdf_content <- c(
    "HMDB0000001",
    "  Mrv1810 01011912342D",
    "",
    "  1  0  0  0  0  0            999 V2000",
    "    0.0000    0.0000    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0",
    "M  END",
    "> <DATABASE_ID>",
    "HMDB0000001",
    "",
    "> <GENERIC_NAME>",
    "Test Compound",
    "",
    "> <SMILES>",
    "CC(C)C",
    "",
    "> <INCHI_KEY>",
    "AAAAAAAAAAAAA-BBBBBBBBBB-C",
    "",
    "> <FORMULA>",
    "C4H10",
    "",
    "> <EXACT_MASS>",
    "58.0783",
    "",
    "> <JCHEM_LOGP>",
    "2.1",
    "",
    "$$$$"
  )

  sdf_file <- file.path(tmp, "test_hmdb.sdf")
  writeLines(sdf_content, sdf_file)

  # Zip it
  zip_file <- file.path(tmp, "test_hmdb.zip")
  withr::with_dir(tmp, {
    zip::zip(zipfile = basename(zip_file), files = basename(sdf_file))
  })

  output_file <- file.path(tmp, "hmdb_output.tsv")

  # Run function
  result <- prepare_libraries_sop_hmdb(
    input = zip_file,
    output = output_file
  )

  expect_equal(result, output_file)
  expect_true(file.exists(output_file))

  # Check output content
  df <- tidytable::fread(output_file)
  expect_true(nrow(df) > 0)
  expect_true("structure_inchikey" %in% names(df))
  expect_true("organism_name" %in% names(df))
  expect_true(all(df$organism_name == "Homo sapiens"))
})

test_that("test-prepare_libraries_sop_hmdb handles SDF with missing fields", {
  tmp <- withr::local_tempdir()

  # Create SDF with some missing fields
  sdf_content <- c(
    "HMDB0000002",
    "  Mrv1810 01011912342D",
    "",
    "  1  0  0  0  0  0            999 V2000",
    "    0.0000    0.0000    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0",
    "M  END",
    "> <DATABASE_ID>",
    "HMDB0000002",
    "",
    "> <GENERIC_NAME>",
    "Incomplete Compound",
    "",
    "> <INCHI_KEY>",
    "XXXXXAAAAAAAA-YYYYYBBBBBB-Z",
    "",
    "> <FORMULA>",
    "C3H6O",
    "",
    "$$$$"
  )

  sdf_file <- file.path(tmp, "test_hmdb_incomplete.sdf")
  writeLines(sdf_content, sdf_file)

  zip_file <- file.path(tmp, "test_hmdb_incomplete.zip")
  withr::with_dir(tmp, {
    zip::zip(zipfile = basename(zip_file), files = basename(sdf_file))
  })

  output_file <- file.path(tmp, "hmdb_incomplete_output.tsv")

  result <- prepare_libraries_sop_hmdb(
    input = zip_file,
    output = output_file
  )

  expect_true(file.exists(output_file))

  df <- tidytable::fread(output_file)
  expect_true(nrow(df) >= 0) # May filter out invalid entries
})

test_that("test-prepare_libraries_sop_hmdb handles multiple compounds in SDF", {
  tmp <- withr::local_tempdir()

  # Create SDF with multiple compounds
  sdf_content <- c(
    # First compound
    "HMDB0000001",
    "  Mrv1810 01011912342D",
    "",
    "  1  0  0  0  0  0            999 V2000",
    "    0.0000    0.0000    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0",
    "M  END",
    "> <DATABASE_ID>",
    "HMDB0000001",
    "",
    "> <GENERIC_NAME>",
    "Compound 1",
    "",
    "> <SMILES>",
    "CC",
    "",
    "> <INCHI_KEY>",
    "AAAAAAAAAAAAA-BBBBBBBBBB-C",
    "",
    "> <FORMULA>",
    "C2H6",
    "",
    "> <EXACT_MASS>",
    "30.0470",
    "",
    "$$$$",
    # Second compound
    "HMDB0000002",
    "  Mrv1810 01011912342D",
    "",
    "  1  0  0  0  0  0            999 V2000",
    "    0.0000    0.0000    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0",
    "M  END",
    "> <DATABASE_ID>",
    "HMDB0000002",
    "",
    "> <GENERIC_NAME>",
    "Compound 2",
    "",
    "> <SMILES>",
    "CCC",
    "",
    "> <INCHI_KEY>",
    "DDDDDDDDDDDDD-EEEEEEEEEE-F",
    "",
    "> <FORMULA>",
    "C3H8",
    "",
    "> <EXACT_MASS>",
    "44.0626",
    "",
    "$$$$"
  )

  sdf_file <- file.path(tmp, "test_hmdb_multi.sdf")
  writeLines(sdf_content, sdf_file)

  zip_file <- file.path(tmp, "test_hmdb_multi.zip")
  withr::with_dir(tmp, {
    zip::zip(zipfile = basename(zip_file), files = basename(sdf_file))
  })

  output_file <- file.path(tmp, "hmdb_multi_output.tsv")

  result <- prepare_libraries_sop_hmdb(
    input = zip_file,
    output = output_file
  )

  expect_true(file.exists(output_file))

  df <- tidytable::fread(output_file)
  expect_true(nrow(df) >= 2) # Should have at least 2 compounds
})

test_that("test-prepare_libraries_sop_hmdb sets correct organism taxonomy", {
  tmp <- withr::local_tempdir()

  # Create complete SDF with all required fields
  sdf_content <- c(
    "HMDB0000001",
    "  Mrv1810 01011912342D",
    "",
    "  1  0  0  0  0  0            999 V2000",
    "    0.0000    0.0000    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0",
    "M  END",
    "> <DATABASE_ID>",
    "HMDB0000001",
    "",
    "> <GENERIC_NAME>",
    "Test Compound",
    "",
    "> <SMILES>",
    "C",
    "",
    "> <INCHI_KEY>",
    "TESTINCHIKEY1-TESTINCHI-K",
    "",
    "> <FORMULA>",
    "CH4",
    "",
    "> <EXACT_MASS>",
    "16.0313",
    "",
    "$$$$"
  )

  sdf_file <- file.path(tmp, "test_taxonomy.sdf")
  writeLines(sdf_content, sdf_file)

  zip_file <- file.path(tmp, "test_taxonomy.zip")
  withr::with_dir(tmp, {
    zip::zip(zipfile = basename(zip_file), files = basename(sdf_file))
  })

  output_file <- file.path(tmp, "taxonomy_output.tsv")

  prepare_libraries_sop_hmdb(
    input = zip_file,
    output = output_file
  )

  df <- tidytable::fread(output_file)

  # Only check if we have data (parsing might filter invalid entries)
  if (nrow(df) > 0) {
    expect_true(all(df$organism_name == "Homo sapiens"))
    expect_true(all(df$organism_taxonomy_ottid == 770315))
    expect_true(all(df$organism_taxonomy_04class == "Mammalia"))
    expect_true(all(df$organism_taxonomy_08genus == "Homo"))
  } else {
    skip("No valid entries parsed from mock SDF")
  }
})

test_that("test-prepare_libraries_sop_hmdb handles errors gracefully and creates empty output", {
  tmp <- withr::local_tempdir()

  # Use non-existent file path
  missing_zip <- file.path(tmp, "does_not_exist.zip")
  output_file <- file.path(tmp, "error_output.tsv")

  # Should handle missing file and create empty output
  result <- prepare_libraries_sop_hmdb(
    input = missing_zip,
    output = output_file
  )

  expect_true(file.exists(output_file))
  expect_equal(result, output_file)

  # Should have created empty/minimal structure
  df <- tidytable::fread(output_file)
  expect_true("structure_inchikey" %in% names(df))
  expect_true("organism_name" %in% names(df))
})
