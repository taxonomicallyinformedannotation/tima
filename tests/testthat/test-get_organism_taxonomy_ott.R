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
  skip_if_no_network("https://api.opentreeoflife.org")
  fake_taxon_df <- data.frame("organism" = "Gentiana lutea")

  result <- get_organism_taxonomy_ott(df = fake_taxon_df)
  expect_s3_class(result, "data.frame")
})

test_that("get_organism_taxonomy_ott handles invalid organism", {
  skip_if_no_network("https://api.opentreeoflife.org")
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

  expect_warning(
    result <- get_organism_taxonomy_ott(
      df = fake_taxon_df,
      url = "https://api.opentreeoflife.org/v3/taxonomy/fakeDown"
    ),
    class = "tima_ott_unavailable_warning"
  )
  # Same shape as a successful result, so callers can bind it
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_true(all(
    c(
      "organism_name",
      "organism_taxonomy_ottid",
      "organism_taxonomy_09species"
    ) %in%
      names(result)
  ))
})
