test_that("extended helper fixtures build expected shapes", {
  small <- create_fixture_params_small(id = "X")
  expect_type(small, "list")
  expect_true("files" %in% names(small))
  expect_true("pattern" %in% names(small$files))

  yamls <- create_fixture_yamls_params(id = "Y")
  expect_type(yamls, "list")
  expect_true("yamls_params" %in% names(yamls))
  expect_true("prepare_params_advanced" %in% names(yamls$yamls_params))
})
