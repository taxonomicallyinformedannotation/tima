# ==============================================================================
# Test Suite: export_output
# ==============================================================================

# Validation tests
test_that("export_output validates file path", {
  expect_error(export_output(x = data.frame(a = 1), file = NULL), "non-empty")
  expect_error(
    export_output(x = data.frame(a = 1), file = c("a", "b")),
    "non-empty"
  )
})

test_that("export_output validates data frame", {
  expect_error(
    export_output(x = 123, file = "data/interim/test_export.tsv"),
    "data frame"
  )
})

test_that("export_output writes uncompressed file", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  file_path <- file.path(tmp, "out.tsv")
  returned <- export_output(x = data.frame(a = 1, b = "x"), file = file_path)
  expect_equal(returned, file_path)
  expect_true(file.exists(file_path))
})

test_that("export_output writes compressed file", {
  tmp <- withr::local_tempdir()
  withr::local_dir(tmp)
  file_path <- file.path(tmp, "out.tsv.gz")
  export_output(x = data.frame(a = 1:3), file = file_path)
  expect_true(file.exists(file_path))
})
