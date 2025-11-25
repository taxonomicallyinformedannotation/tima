# Test Suite: export_output ----

library(testthat)

# Validation tests
test_that("export_output validates file path", {
  expect_error(
    export_output(x = data.frame(a = 1), file = NULL),
    "Provide a non-NULL character string."
  )
  expect_error(
    export_output(x = data.frame(a = 1), file = c("a", "b")),
    "Ensure the parameter is a length-1 character value."
  )
})

test_that("export_output validates data frame", {
  expect_error(
    export_output(x = 123, file = "data/interim/test_export.tsv"),
    "data frame"
  )
})

test_that("export_output writes uncompressed file", {
  file_path <- temp_test_path("out.tsv")
  returned <- export_output(x = data.frame(a = 1, b = "x"), file = file_path)
  expect_equal(returned, file_path)
  expect_true(file.exists(file_path))
})

test_that("export_output writes compressed file", {
  file_path <- temp_test_path("out.tsv.gz")
  export_output(x = data.frame(a = 1:3), file = file_path)
  expect_true(file.exists(file_path))
})
