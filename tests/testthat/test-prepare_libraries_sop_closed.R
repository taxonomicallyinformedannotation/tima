# Test Suite: prepare_libraries_sop_closed ----

library(testthat)

test_that("prepare_libraries_sop_closed validates input parameter", {
  expect_error(
    prepare_libraries_sop_closed(
      input = NULL,
      output = temp_test_path("out.tsv")
    ),
    "input must be a single character string"
  )

  expect_error(
    prepare_libraries_sop_closed(
      input = 123,
      output = temp_test_path("out.tsv")
    ),
    "input must be a single character string"
  )

  expect_error(
    prepare_libraries_sop_closed(
      input = c("file1", "file2"),
      output = temp_test_path("out.tsv")
    ),
    "input must be a single character string"
  )
})

test_that("prepare_libraries_sop_closed validates output parameter", {
  temp_input <- tempfile(fileext = ".csv")
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
  nonexistent <- file.path(tempfile(), "nonexistent.csv")
  output <- tempfile(fileext = ".tsv")

  result <- prepare_libraries_sop_closed(
    input = nonexistent,
    output = output
  )

  expect_equal(result, output)
  expect_true(file.exists(output))

  # Should create empty template
  df <- tidytable::fread(output)
  expect_equal(nrow(df), 1)
})

test_that("prepare_libraries_sop_closed handles empty input file", {
  temp_input <- tempfile(fileext = ".csv")
  temp_output <- tempfile(fileext = ".tsv")

  # Create empty file with header
  writeLines("structure_inchikey,organism_name", temp_input)

  result <- prepare_libraries_sop_closed(
    input = temp_input,
    output = temp_output
  )

  expect_true(file.exists(temp_output))

  df <- tidytable::fread(temp_output)
  expect_equal(nrow(df), 1)
})

test_that("prepare_libraries_sop_closed processes fixture closed data", {
  temp_input <- file.path(tempfile(), "closed_test.csv")
  file.copy(
    testthat::test_path("fixtures", "closed.csv"),
    temp_input,
    overwrite = TRUE
  )
  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_libraries_sop_closed(
    input = temp_input,
    output = temp_output
  )

  expect_equal(result, temp_output)
  expect_true(file.exists(temp_output))

  df <- tidytable::fread(temp_output)
  expect_true(nrow(df) >= 1)
  expect_true("structure_inchikey_connectivity_layer" %in% names(df))
  expect_true("structure_name" %in% names(df))
})

test_that("prepare_libraries_sop_closed extracts 2D InChIKey from fixture", {
  temp_input <- file.path(tempfile(), "closed_inchikey.csv")
  file.copy(
    testthat::test_path("fixtures", "closed.csv"),
    temp_input,
    overwrite = TRUE
  )
  temp_output <- tempfile(fileext = ".tsv")

  prepare_libraries_sop_closed(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output, colClasses = "character")

  # 2D InChIKey should be first 14 characters
  expect_true(all(nchar(df$structure_inchikey_2D) == 14))
})

test_that("prepare_libraries_sop_closed renames structure_nameTraditional", {
  temp_input <- file.path(tempfile(), "closed_rename.csv")
  file.copy(
    testthat::test_path("fixtures", "closed.csv"),
    temp_input,
    overwrite = TRUE
  )
  temp_output <- tempfile(fileext = ".tsv")

  prepare_libraries_sop_closed(
    input = temp_input,
    output = temp_output
  )

  df <- tidytable::fread(temp_output, colClasses = "character")

  expect_true("structure_name" %in% names(df))
  expect_false("structure_nameTraditional" %in% names(df))
})

# test_that("prepare_libraries_sop_closed removes duplicates from fixture", {
#   # Create fixture with duplicates
#   closed_dup <- load_fixture("closed")
#   closed_dup <- tidytable::bind_rows(closed_dup, closed_dup, closed_dup)
#
#   temp_input <- tempfile(fileext = ".csv")
#   tidytable::fwrite(closed_dup, temp_input)
#   temp_output <- tempfile(fileext = ".tsv")
#
#   prepare_libraries_sop_closed(
#     input = temp_input,
#     output = temp_output
#   )
#
#   df <- tidytable::fread(temp_output)
#
#   # Duplicates should be removed
#   original_rows <- nrow(load_fixture("closed"))
#   expect_equal(nrow(df), original_rows)
# })
