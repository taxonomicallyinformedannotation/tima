# Test Suite: decorate_bio ----

library(testthat)

test_that("decorate_bio handles empty input", {
  # Use fixture utilities for empty structure
  empty_df <- create_empty_table("annotation")
  empty_df$score_biological <- numeric(0)

  result <- decorate_bio(
    annot_table_wei_bio = empty_df,
    score_biological_kingdom = 0.1,
    score_biological_phylum = 0.2,
    score_biological_class = 0.3,
    score_biological_order = 0.4,
    score_biological_family = 0.5,
    score_biological_tribe = 0.6,
    score_biological_genus = 0.7,
    score_biological_species = 0.8,
    score_biological_variety = 0.9
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("decorate_bio returns input unchanged", {
  # Use fixture utilities for test data
  test_df <- create_minimal_data("annotation", n_rows = 2)
  test_df$score_biological <- c(0.8, 0.9)

  result <- decorate_bio(
    annot_table_wei_bio = test_df,
    score_biological_kingdom = 0.1,
    score_biological_phylum = 0.2,
    score_biological_class = 0.3,
    score_biological_order = 0.4,
    score_biological_family = 0.5,
    score_biological_tribe = 0.6,
    score_biological_genus = 0.7,
    score_biological_species = 0.8,
    score_biological_variety = 0.9
  )

  # Should return input unchanged (decorator pattern)
  expect_identical(result, test_df)
})

test_that("decorate_bio warns on missing columns", {
  # Create incomplete data frame
  incomplete_df <- tidytable::tidytable(
    feature_id = c("FT001")
  )

  expect_warning(
    decorate_bio(
      annot_table_wei_bio = incomplete_df,
      score_biological_kingdom = 0.1,
      score_biological_phylum = 0.2,
      score_biological_class = 0.3,
      score_biological_order = 0.4,
      score_biological_family = 0.5,
      score_biological_tribe = 0.6,
      score_biological_genus = 0.7,
      score_biological_species = 0.8,
      score_biological_variety = 0.9
    ),
    NA # Should log warning but not throw error
  )
})

## Performance ----

test_that("decoration functions use vectorized operations", {
  # Create moderately-sized test data
  large_df <- tidytable::tidytable(
    feature_id = rep(paste0("FT", 1:100), each = 10),
    score_biological = runif(1000, 0, 1),
    candidate_structure_inchikey_connectivity_layer = rep(
      paste0("INK", 1:1000),
      length.out = 1000
    )
  )

  # Should complete quickly if vectorized
  start_time <- Sys.time()
  result <- decorate_bio(
    annot_table_wei_bio = large_df,
    score_biological_kingdom = 0.1,
    score_biological_phylum = 0.2,
    score_biological_class = 0.3,
    score_biological_order = 0.4,
    score_biological_family = 0.5,
    score_biological_tribe = 0.6,
    score_biological_genus = 0.7,
    score_biological_species = 0.8,
    score_biological_variety = 0.9
  )
  end_time <- Sys.time()

  # Should be fast (<1 second for 1000 rows)
  expect_lt(as.numeric(difftime(end_time, start_time, units = "secs")), 1.0)
  expect_identical(result, large_df)
})
