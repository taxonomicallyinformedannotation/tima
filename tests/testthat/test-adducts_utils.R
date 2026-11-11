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

test_that("adducts_forbidden_translations is a named character vector", {
  expect_type(adducts_forbidden_translations, "character")
  expect_named(adducts_forbidden_translations)
  expect_true(length(adducts_forbidden_translations) > 0L)
})

test_that("forbidden adducts map to canonical forms", {
  expect_equal(adducts_forbidden_translations[["[M-H2O+H2O-H]-"]], "[M-H]-")
  expect_equal(adducts_forbidden_translations[["[M-H3O4P+H3O4P-H]-"]], "[M-H]-")
  expect_equal(adducts_forbidden_translations[["[M-H2O+H2O]+"]], "[M]+")
  expect_equal(adducts_forbidden_translations[["[M-H2O+H2O+H]+"]], "[M+H]+")
})

test_that("calculate_formula_mass memoizes repeated formulas and warns per invalid occurrence", {
  expect_error(
    calculate_formula_mass(123),
    "x must be a character vector"
  )
  expect_equal(calculate_formula_mass(character(0)), numeric(0))

  f_vec <- c("C6H12O6", "H2O", "C6H12O6", NA_character_, "[13C6]H12O6")
  res1 <- calculate_formula_mass(f_vec)
  res2 <- calculate_formula_mass(f_vec)
  expect_identical(res1, res2)
  expect_equal(names(res1), f_vec)
  expect_true(is.na(res1[[4L]]))
  expect_gt(res1[["[13C6]H12O6"]], res1[["C6H12O6"]])

  w_env <- new.env(parent = emptyenv())
  w_env$cnt <- 0L
  bad_res <- withCallingHandlers(
    calculate_formula_mass(c("C6H12O6", "INVALID!", "INVALID!")),
    warning = function(w) {
      w_env$cnt <- w_env$cnt + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(w_env$cnt, 2L)
  expect_true(!is.na(bad_res[[1L]]))
  expect_true(all(is.na(bad_res[2:3])))

  expect_identical(canonicalize_adducts_vectorized(character(0)), character(0))
  expect_identical(
    canonicalize_adducts_vectorized(c(NA_character_, NA_character_)),
    c(NA_character_, NA_character_)
  )
  expect_identical(canonicalize_adducts_vectorized(""), "")
  expect_true(is.na(calculate_formula_mass(NA_character_)))
  expect_warning(expect_true(is.na(calculate_formula_mass(""))))
})
