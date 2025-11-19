# Test Suite: decorate_masses ----

library(testthat)

test_that("decorate_masses handles empty input", {
  empty_df <- tidytable::tidytable(
    feature_id = character(0),
    candidate_structure_inchikey_connectivity_layer = character(0)
  )

  result <- decorate_masses(annotation_table_ms1 = empty_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("decorate_masses returns input unchanged", {
  test_df <- tidytable::tidytable(
    feature_id = c("FT001", "FT002"),
    candidate_structure_inchikey_connectivity_layer = c("AAAAA", "BBBBB")
  )

  result <- decorate_masses(annotation_table_ms1 = test_df)

  # Should return input unchanged (decorator pattern)
  expect_identical(result, test_df)
})

test_that("decorate_masses filters NA and notAnnotated", {
  test_df <- tidytable::tidytable(
    feature_id = c("FT001", "FT002", "FT003", "FT004"),
    candidate_structure_inchikey_connectivity_layer = c(
      "AAAAA",
      NA,
      "notAnnotated",
      "BBBBB"
    )
  )

  result <- decorate_masses(annotation_table_ms1 = test_df)

  # Should return input unchanged - function just logs statistics
  expect_identical(result, test_df)
})
