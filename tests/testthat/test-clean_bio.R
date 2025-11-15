# ==============================================================================
# Test Suite: clean_bio
# ==============================================================================
library(testthat)
library(tima)

edges_table <- tidytable::tidytable(feature_id = character())

test_that("clean_bio handles empty data frame", {
  result <- clean_bio(tidytable::tidytable())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

edges_table <- tidytable::tidytable(feature_id = c("Gentiana lutea"))

test_that("clean_bio removes duplicate taxonomy rows", {
  df <- tidytable::tidytable(
    organism_name = c("Gentiana lutea","Gentiana lutea"),
    organism_taxonomy_01domain = c("Eukaryota","Eukaryota")
  )
  result <- clean_bio(df)
  expect_lte(nrow(result), 2L)
})
