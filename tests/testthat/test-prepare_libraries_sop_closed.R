# Test Suite: prepare_libraries_sop_closed ----

library(testthat)

test_that("prepare_libraries_sop_closed validates input parameter", {
  skip_on_cran()

  expect_error(
    prepare_libraries_sop_closed(input = NULL, output = "out.tsv"),
    "input must be a single character string"
  )

  expect_error(
    prepare_libraries_sop_closed(input = 123, output = "out.tsv"),
    "input must be a single character string"
  )

  expect_error(
    prepare_libraries_sop_closed(
      input = c("file1", "file2"),
      output = "out.tsv"
    ),
    "input must be a single character string"
  )
})

test_that("prepare_libraries_sop_closed validates output parameter", {
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv")
  writeLines("test", temp_input)

  expect_error(
    prepare_libraries_sop_closed(input = temp_input, output = NULL),
    "output must be a single character string"
  )

  expect_error(
    prepare_libraries_sop_closed(input = temp_input, output = 123),
    "output must be a single character string"
  )
})

test_that("prepare_libraries_sop_closed returns empty template when file missing", {
  skip_on_cran()

  nonexistent <- make_tmp_file("nonexistent.csv")
  output <- make_tmp_file(fileext = ".tsv")

  result <- prepare_libraries_sop_closed(
    input = nonexistent,
    output = output
  )

  expect_equal(result, output)
  expect_true(file.exists(output))

  # Should create empty template
  df <- tidytable::fread(output)
  expect_equal(nrow(df), 0)
})

test_that("prepare_libraries_sop_closed handles empty input file", {
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv")
  temp_output <- make_tmp_file(fileext = ".tsv")

  # Create empty file with header
  writeLines("structure_inchikey,organism_name", temp_input)

  result <- prepare_libraries_sop_closed(
    input = temp_input,
    output = temp_output
  )

  expect_true(file.exists(temp_output))

  df <- tidytable::fread(temp_output)
  expect_equal(nrow(df), 0)
})

test_that("prepare_libraries_sop_closed processes valid input", {
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv")
  temp_output <- make_tmp_file(fileext = ".tsv")

  # Create minimal valid input
  input_data <- tidytable::tidytable(
    structure_inchikey = "ABCDEFGHIJKLMN-OPQRSTUVWX-A",
    structure_nameTraditional = "Test Compound",
    structure_exact_mass = "123.456",
    organism_name = "Test organism",
    organism_taxonomy_ottid = "12345",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = "Angiosperms",
    organism_taxonomy_06family = "Testaceae"
  )

  tidytable::fwrite(input_data, temp_input)

  result <- prepare_libraries_sop_closed(
    input = temp_input,
    output = temp_output
  )

  expect_equal(result, temp_output)
  expect_true(file.exists(temp_output))

  df <- tidytable::fread(temp_output)
  expect_true(nrow(df) >= 1)
  expect_true("structure_inchikey_2D" %in% names(df))
  expect_true("structure_name" %in% names(df))
})

test_that("prepare_libraries_sop_closed extracts 2D InChIKey", {
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv")
  temp_output <- make_tmp_file(fileext = ".tsv")

  input_data <- tidytable::tidytable(
    structure_inchikey = "ABCDEFGHIJKLMN-OPQRSTUVWX-A",
    structure_nameTraditional = "Test",
    organism_name = "Test org",
    organism_taxonomy_ottid = "123"
  )

  tidytable::fwrite(input_data, temp_input)

  prepare_libraries_sop_closed(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output, colClasses = "character")

  # 2D InChIKey should be first 14 characters
  expect_equal(df$structure_inchikey_2D[1], "ABCDEFGHIJKLMN")
})

test_that("prepare_libraries_sop_closed renames structure_nameTraditional", {
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv")
  temp_output <- make_tmp_file(fileext = ".tsv")

  input_data <- tidytable::tidytable(
    structure_inchikey = "ABCDEFGHIJKLMN-OPQRSTUVWX-A",
    structure_nameTraditional = "Traditional Name",
    organism_name = "Test org",
    organism_taxonomy_ottid = "123"
  )

  tidytable::fwrite(input_data, temp_input)

  prepare_libraries_sop_closed(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output, colClasses = "character")

  expect_true("structure_name" %in% names(df))
  expect_false("structure_nameTraditional" %in% names(df))
})

test_that("prepare_libraries_sop_closed removes duplicates", {
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv")
  temp_output <- make_tmp_file(fileext = ".tsv")

  # Create input with duplicates
  input_data <- tidytable::tidytable(
    structure_inchikey = rep("ABCDEFGHIJKLMN-OPQRSTUVWX-A", 3),
    structure_nameTraditional = rep("Test", 3),
    organism_name = rep("Test org", 3),
    organism_taxonomy_ottid = rep("123", 3)
  )

  tidytable::fwrite(input_data, temp_input)

  prepare_libraries_sop_closed(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output)

  # Duplicates should be removed
  expect_equal(nrow(df), 1)
})
