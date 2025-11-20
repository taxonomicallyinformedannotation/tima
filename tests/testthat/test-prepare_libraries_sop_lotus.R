# Test Suite: prepare_libraries_sop_lotus ----

library(testthat)

test_that("prepare_libraries_sop_lotus returns empty template when file missing", {
  skip_on_cran()

  nonexistent <- make_tmp_file("nonexistent_lotus.csv")
  output <- make_tmp_file(fileext = ".tsv")

  result <- prepare_libraries_sop_lotus(
    input = nonexistent,
    output = output
  )

  expect_equal(result, output)
  expect_true(file.exists(output))

  # Should create empty template
  df <- tidytable::fread(output)
  expect_equal(nrow(df), 0)
})

test_that("prepare_libraries_sop_lotus processes valid LOTUS data", {
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv.gz")
  temp_output <- make_tmp_file(fileext = ".tsv")

  # Create minimal valid LOTUS data
  lotus_data <- tidytable::tidytable(
    structure_inchikey = "ABCDEFGHIJKLMN-OPQRSTUVWX-A",
    structure_nameTraditional = "Test Compound",
    structure_exact_mass = "123.456",
    organism_name = "Test organism",
    organism_taxonomy_ottid = "12345",
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = "Streptophyta",
    organism_taxonomy_06family = "Gentianaceae"
  )

  tidytable::fwrite(lotus_data, temp_input)

  result <- prepare_libraries_sop_lotus(
    input = temp_input,
    output = temp_output
  )

  expect_equal(result, temp_output)
  expect_true(file.exists(temp_output))

  df <- tidytable::fread(temp_output)
  expect_true(nrow(df) >= 1)
})

test_that("prepare_libraries_sop_lotus extracts 2D InChIKey", {
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv")
  temp_output <- make_tmp_file(fileext = ".tsv")

  lotus_data <- tidytable::tidytable(
    structure_inchikey = "XYZABCDEFGHIJK-LMNOPQRSTU-V",
    structure_nameTraditional = "Test",
    organism_name = "Test org",
    organism_taxonomy_ottid = "123"
  )

  tidytable::fwrite(lotus_data, temp_input)

  prepare_libraries_sop_lotus(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output, colClasses = "character")

  # 2D InChIKey should be first 14 characters
  expect_equal(df$structure_inchikey_2D[1], "XYZABCDEFGHIJK")
})

test_that("prepare_libraries_sop_lotus renames structure_nameTraditional", {
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv")
  temp_output <- make_tmp_file(fileext = ".tsv")

  lotus_data <- tidytable::tidytable(
    structure_inchikey = "ABCDEFGHIJKLMN-OPQRSTUVWX-A",
    structure_nameTraditional = "Traditional Name",
    organism_name = "Test org",
    organism_taxonomy_ottid = "123"
  )

  tidytable::fwrite(lotus_data, temp_input)

  prepare_libraries_sop_lotus(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output, colClasses = "character")

  expect_true("structure_name" %in% names(df))
  expect_false("structure_nameTraditional" %in% names(df))
})

test_that("prepare_libraries_sop_lotus removes duplicates", {
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv")
  temp_output <- make_tmp_file(fileext = ".tsv")

  # Create LOTUS data with duplicates
  lotus_data <- tidytable::tidytable(
    structure_inchikey = rep("ABCDEFGHIJKLMN-OPQRSTUVWX-A", 5),
    structure_nameTraditional = rep("Test", 5),
    organism_name = rep("Test org", 5),
    organism_taxonomy_ottid = rep("123", 5)
  )

  tidytable::fwrite(lotus_data, temp_input)

  prepare_libraries_sop_lotus(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output)

  # Duplicates should be removed
  expect_equal(nrow(df), 1)
})

test_that("prepare_libraries_sop_lotus handles multiple structure-organism pairs", {
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv")
  temp_output <- make_tmp_file(fileext = ".tsv")

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
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv")
  temp_output <- make_tmp_file(fileext = ".tsv")

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
  skip_on_cran()

  temp_input <- make_tmp_file(fileext = ".csv.gz")
  temp_output <- make_tmp_file(fileext = ".tsv")

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
