# Test Suite: export_output ----

library(testthat)

## Input Validation Tests ----

test_that("export_output validates file path", {
  expect_error(
    export_output(x = data.frame(a = 1), file = NULL),
    "non-NULL"
  )
  expect_error(
    export_output(x = data.frame(a = 1), file = c("a", "b")),
    "length-1"
  )
})

test_that("export_output validates data frame", {
  file_path <- temp_test_path("test.tsv")

  expect_error(export_output(x = 123, file = file_path), "data.*frame")
  expect_error(export_output(x = NULL, file = file_path), class = "error")
  expect_error(
    export_output(x = list(a = 1), file = file_path),
    class = "error"
  )
})

## Successful Export Tests ----

test_that("export_output writes uncompressed TSV", {
  df <- tidytable::tidytable(
    id = 1:3,
    value = c("a", "b", "c"),
    number = c(1.1, 2.2, 3.3)
  )
  file_path <- temp_test_path("output.tsv")

  result <- export_output(x = df, file = file_path)

  expect_identical(result, file_path)
  expect_true(file.exists(file_path))

  loaded <- tidytable::fread(file_path)
  expect_equal(nrow(loaded), 3)
  expect_equal(ncol(loaded), 3)
})

test_that("export_output writes gzip-compressed TSV", {
  df <- tidytable::tidytable(x = 1:100, y = rnorm(100))
  file_path <- temp_test_path("output.tsv.gz")

  export_output(x = df, file = file_path)

  expect_true(file.exists(file_path))
  loaded <- tidytable::fread(file_path)
  expect_equal(nrow(loaded), 100)
})

test_that("export_output creates necessary directories", {
  df <- tidytable::tidytable(a = 1)
  file_path <- file.path(tempdir(), "deep", "nested", "path", "data.tsv")

  export_output(x = df, file = file_path)

  expect_true(file.exists(file_path))
  expect_true(dir.exists(dirname(file_path)))
})

test_that("export_output handles empty data frames", {
  df <- tidytable::tidytable(col_a = character(0), col_b = numeric(0))
  file_path <- temp_test_path("empty.tsv")

  export_output(x = df, file = file_path)

  expect_true(file.exists(file_path))
  loaded <- tidytable::fread(file_path)
  expect_equal(nrow(loaded), 0)
  expect_equal(ncol(loaded), 2)
})

test_that("export_output handles NA values correctly", {
  df <- tidytable::tidytable(
    id = 1:3,
    value = c("a", NA, "c"),
    number = c(1.1, NA, 3.3)
  )
  file_path <- temp_test_path("with_na.tsv")

  export_output(x = df, file = file_path)

  loaded <- tidytable::fread(file_path, na.strings = "")
  expect_true(is.na(loaded$value[2]))
  expect_true(is.na(loaded$number[2]))
})

## Edge Cases ----

test_that("export_output handles special characters in data", {
  df <- tidytable::tidytable(
    text = c("tab\there", "newline\nhere", "quote\"here")
  )
  file_path <- temp_test_path("special_chars.tsv")

  export_output(x = df, file = file_path)

  loaded <- tidytable::fread(file_path)
  expect_equal(nrow(loaded), 3)
})

test_that("export_output overwrites existing files", {
  df1 <- tidytable::tidytable(a = 1:3)
  df2 <- tidytable::tidytable(a = 4:6)
  file_path <- temp_test_path("overwrite.tsv")

  export_output(x = df1, file = file_path)
  export_output(x = df2, file = file_path)

  loaded <- tidytable::fread(file_path)
  expect_equal(loaded$a, 4:6)
})

## Return Value Tests ----

test_that("export_output returns path invisibly", {
  df <- tidytable::tidytable(a = 1)
  file_path <- temp_test_path("invisible.tsv")

  expect_invisible(export_output(x = df, file = file_path))
})

test_that("export_output returns correct path", {
  df <- tidytable::tidytable(a = 1)
  file_path <- temp_test_path("return_path.tsv")

  result <- export_output(x = df, file = file_path)
  expect_identical(result, file_path)
})
