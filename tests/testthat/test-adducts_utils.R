# Test Suite: adducts_utils ----
# Tests the adduct translation and forbidden list constants.

library(testthat)

test_that("adducts_translations is a named character vector", {
  expect_type(adducts_translations, "character")
  expect_named(adducts_translations)
  expect_true(length(adducts_translations) > 0L)
})

test_that("adducts_translations contains key mzmine/GNPS mappings", {
  # Spot-check well-known translations
  expect_equal(adducts_translations[["-2H"]], "-H2")
  expect_equal(adducts_translations[["+NH4"]], "+H4N")
  expect_equal(adducts_translations[["+CH3COO"]], "+C2H3O2")
  expect_equal(adducts_translations[["+ACN"]], "+C2H3N")
})

test_that("adducts_forbidden is a non-empty character vector", {
  expect_type(adducts_forbidden, "character")
  expect_true(length(adducts_forbidden) > 0L)
  # All entries should look like adduct strings (start with '[')
  expect_true(all(startsWith(adducts_forbidden, "[")))
})

test_that("adducts_translations and adducts_forbidden share no keys", {
  # The two sets are conceptually disjoint
  overlap <- intersect(names(adducts_translations), adducts_forbidden)
  expect_length(overlap, 0L)
})
