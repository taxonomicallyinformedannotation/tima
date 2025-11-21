# Test Suite: prepare_libraries_sop_lotus ----

library(testthat)

test_that("prepare_libraries_sop_lotus returns empty template when file missing", {
  nonexistent <- file.path(tempdir(), "nonexistent_lotus.csv")
  output <- tempfile(fileext = ".tsv")

  result <- prepare_libraries_sop_lotus(
    input = nonexistent,
    output = output
  )

  expect_equal(result, output)
  expect_true(file.exists(output))

  # Should create empty template
  df <- tidytable::fread(output)
  expect_equal(nrow(df), 1)
})

test_that("prepare_libraries_sop_lotus processes fixture LOTUS data", {
  # Copy fixture to temp location
  temp_input <- file.path(tempdir(), "lotus_test.csv")
  file.copy(
    testthat::test_path("fixtures", "lotus.csv"),
    temp_input,
    overwrite = TRUE
  )
  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_libraries_sop_lotus(
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

test_that("prepare_libraries_sop_lotus extracts 2D InChIKey from fixture", {
  temp_input <- file.path(tempdir(), "lotus_inchikey.csv")
  file.copy(
    testthat::test_path("fixtures", "lotus.csv"),
    temp_input,
    overwrite = TRUE
  )
  temp_output <- tempfile(fileext = ".tsv")

  prepare_libraries_sop_lotus(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output, colClasses = "character")

  # 2D InChIKey should be first 14 characters
  expect_true(all(nchar(df$structure_inchikey_2D) == 14))
})

test_that("prepare_libraries_sop_lotus renames structure_nameTraditional", {
  temp_input <- file.path(tempdir(), "lotus_rename.csv")
  file.copy(
    testthat::test_path("fixtures", "lotus.csv"),
    temp_input,
    overwrite = TRUE
  )
  temp_output <- tempfile(fileext = ".tsv")

  prepare_libraries_sop_lotus(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output, colClasses = "character")

  expect_true("structure_name" %in% names(df))
  expect_false("structure_nameTraditional" %in% names(df))
})

test_that("prepare_libraries_sop_lotus removes duplicates from fixture", {
  # Create fixture with duplicates
  lotus_dup <- load_fixture("lotus")
  lotus_dup <- tidytable::bind_rows(lotus_dup, lotus_dup, lotus_dup)

  temp_input <- tempfile(fileext = ".csv")
  tidytable::fwrite(lotus_dup, temp_input)
  temp_output <- tempfile(fileext = ".tsv")

  prepare_libraries_sop_lotus(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output)

  # Duplicates should be removed
  original_rows <- nrow(load_fixture("lotus"))
  expect_equal(nrow(df), original_rows)
})

test_that("prepare_libraries_sop_lotus handles multiple structure-organism pairs", {
  temp_input <- tempfile(fileext = ".csv")
  temp_output <- tempfile(fileext = ".tsv")

  lotus_data <- tidytable::tidytable(
    structure_inchikey = c(
      "AAAAAAAAAAAAAA-BBBBBBBBBB-C",
      "DDDDDDDDDDDDDD-EEEEEEEEEE-F",
      "GGGGGGGGGGGGGG-HHHHHHHHHH-I"
    ),
    structure_nameTraditional = c("Compound A", "Compound B", "Compound C"),
    organism_name = c("Org 1", "Org 2", "Org 3"),
    organism_taxonomy_ottid = c("111", "222", "333")
  )

  tidytable::fwrite(lotus_data, temp_input)

  prepare_libraries_sop_lotus(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output)

  expect_equal(nrow(df), 3)
})

test_that("prepare_libraries_sop_lotus rounds numeric values", {
  temp_input <- tempfile(fileext = ".csv")
  temp_output <- tempfile(fileext = ".tsv")

  lotus_data <- tidytable::tidytable(
    structure_inchikey = "ABCDEFGHIJKLMN-OPQRSTUVWX-A",
    structure_nameTraditional = "Test",
    structure_exact_mass = "123.456789012345",
    organism_name = "Test org",
    organism_taxonomy_ottid = "123"
  )

  tidytable::fwrite(lotus_data, temp_input)

  prepare_libraries_sop_lotus(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output, colClasses = "character")

  # Numeric values should be rounded (exact precision depends on round_reals())
  if ("structure_exact_mass" %in% names(df)) {
    mass_str <- df$structure_exact_mass[1]
    # Should be shorter than the input
    expect_true(nchar(mass_str) < nchar("123.456789012345"))
  }
})

test_that("prepare_libraries_sop_lotus handles compressed input", {
  temp_input <- tempfile(fileext = ".csv.gz")
  temp_output <- tempfile(fileext = ".tsv")

  lotus_data <- tidytable::tidytable(
    structure_inchikey = "ABCDEFGHIJKLMN-OPQRSTUVWX-A",
    structure_nameTraditional = "Test Compressed",
    organism_name = "Test org",
    organism_taxonomy_ottid = "123"
  )

  # Write compressed file
  tidytable::fwrite(lotus_data, temp_input)

  result <- prepare_libraries_sop_lotus(
    input = temp_input,
    output = temp_output
  )

  expect_true(file.exists(temp_output))

  df <- tidytable::fread(temp_output)
  expect_equal(nrow(df), 1)
})
