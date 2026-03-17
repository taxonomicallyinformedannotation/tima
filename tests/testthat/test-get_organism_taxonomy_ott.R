# Test Suite: get_organism_taxonomy_ott ----

library(testthat)

test_that("get_organism_taxonomy_ott validates df input", {
  expect_error(
    get_organism_taxonomy_ott(df = 1),
    "must be a data frame",
    class = "tima_validation_error"
  )
})

test_that("get_organism_taxonomy_ott requires organism column", {
  expect_error(
    get_organism_taxonomy_ott(df = data.frame(x = "Homo sapiens")),
    "must contain an 'organism' column",
    class = "tima_validation_error"
  )
})

test_that("get_organism_taxonomy_ott works with valid organism", {
  fake_taxon_df <- data.frame("organism" = "Gentiana lutea")

  result <- get_organism_taxonomy_ott(df = fake_taxon_df)
  expect_s3_class(result, "data.frame")
})

test_that("get_organism_taxonomy_ott handles invalid organism", {
  wrong_taxon_df <- data.frame("organism" = "Gentiano luteo")

  expect_warning(
    expect_warning(
      result <- get_organism_taxonomy_ott(df = wrong_taxon_df),
      "gentiano are not matched",
      fixed = TRUE
    ),
    "Gentiano luteo are not matched",
    fixed = TRUE
  )
  expect_s3_class(result, "data.frame")
})

test_that("get_organism_taxonomy_ott handles API failures", {
  fake_taxon_df <- data.frame("organism" = "Gentiana lutea")

  result <- get_organism_taxonomy_ott(
    df = fake_taxon_df,
    url = "https://api.opentreeoflife.org/v3/taxonomy/fakeDown"
  )
  expect_s3_class(result$matches, "data.frame")
  expect_s3_class(result$taxonomy, "data.frame")
})
