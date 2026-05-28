# Test Suite: validations_params ----
# Tests for parameter validation helpers in validations_params.R

library(testthat)

# ── validate_file_existence ───────────────────────────────────────────────────

test_that("validate_file_existence returns TRUE for existing files", {
  tmp <- tempfile()
  writeLines("test", tmp)
  on.exit(unlink(tmp))
  expect_true(validate_file_existence(list(f = tmp)))
})

test_that("validate_file_existence errors on missing files", {
  expect_error(
    validate_file_existence(list(f = "/nonexistent/path/file.tsv")),
    class = "tima_error"
  )
})

test_that("validate_file_existence errors when file_list is not a list", {
  expect_error(
    validate_file_existence("not_a_list"),
    class = "tima_error"
  )
})

test_that("validate_file_existence errors on empty list", {
  expect_error(
    validate_file_existence(list()),
    class = "tima_error"
  )
})

test_that("validate_file_existence allows NULL when allow_null = TRUE", {
  tmp <- tempfile()
  writeLines("test", tmp)
  on.exit(unlink(tmp))
  expect_invisible(validate_file_existence(
    list(a = tmp, b = NULL),
    allow_null = TRUE
  ))
})

test_that("validate_file_existence errors on NULL when allow_null = FALSE", {
  tmp <- tempfile()
  writeLines("test", tmp)
  on.exit(unlink(tmp))
  expect_error(
    validate_file_existence(list(a = tmp, b = NULL), allow_null = FALSE),
    class = "tima_error"
  )
})

# ── validate_ms_mode ─────────────────────────────────────────────────────────

test_that("validate_ms_mode accepts 'pos' and 'neg'", {
  expect_invisible(validate_ms_mode("pos"))
  expect_invisible(validate_ms_mode("neg"))
})

test_that("validate_ms_mode errors on invalid modes", {
  expect_error(validate_ms_mode("both"), class = "tima_error")
  expect_error(validate_ms_mode("positive"), class = "tima_error")
  expect_error(validate_ms_mode("POS"), class = "tima_error")
})

test_that("validate_ms_mode errors on NULL/missing", {
  expect_error(validate_ms_mode(NULL), class = "tima_error")
})

# ── validate_tolerances ───────────────────────────────────────────────────────

test_that("validate_tolerances accepts valid ppm and rt values", {
  expect_invisible(validate_tolerances(tolerance_ppm = 10, tolerance_rt = 0.05))
  expect_invisible(validate_tolerances(tolerance_ppm = 5))
  expect_invisible(validate_tolerances(tolerance_rt = 0.03))
})

test_that("validate_tolerances errors on negative ppm", {
  expect_error(
    validate_tolerances(tolerance_ppm = -1),
    class = "tima_error"
  )
})

test_that("validate_tolerances errors on negative rt", {
  expect_error(
    validate_tolerances(tolerance_rt = -0.1),
    class = "tima_error"
  )
})

# ── validate_adduct_list ──────────────────────────────────────────────────────

test_that("validate_adduct_list accepts valid polarity-keyed list", {
  adducts <- list(pos = c("[M+H]+", "[M+Na]+"), neg = c("[M-H]-"))
  expect_invisible(validate_adduct_list(adducts, "pos"))
  expect_invisible(validate_adduct_list(adducts, "neg"))
})

test_that("validate_adduct_list errors on missing mode", {
  adducts <- list(pos = c("[M+H]+"))
  expect_error(validate_adduct_list(adducts, "neg"), class = "tima_error")
})

test_that("validate_adduct_list errors on non-list input", {
  expect_error(validate_adduct_list("not_a_list", "pos"), class = "tima_error")
})

test_that("validate_adduct_list warns on empty mode entry", {
  adducts <- list(pos = character(0), neg = c("[M-H]-"))
  expect_warning(validate_adduct_list(adducts, "pos"))
})

test_that("validate_adduct_list accepts structured adduct schema", {
  structured <- list(
    M = c(1L, 2L),
    charge_carriers = list(pos = c("H", "Na"), neg = c("H")),
    charges = list(pos = c(1L), neg = c(-1L))
  )
  expect_invisible(validate_adduct_list(structured, "pos"))
  expect_invisible(validate_adduct_list(structured, "neg"))
})

# ── validate_dataframe ────────────────────────────────────────────────────────

test_that("validate_dataframe accepts a valid data frame", {
  df <- data.frame(feature_id = c("f1", "f2"), mz = c(100.0, 200.0))
  expect_invisible(validate_dataframe(df, "features"))
})

test_that("validate_dataframe checks required columns", {
  df <- data.frame(feature_id = c("f1"), mz = c(100.0))
  expect_invisible(validate_dataframe(
    df,
    "features",
    required_cols = c("feature_id", "mz")
  ))
  expect_error(
    validate_dataframe(df, "features", required_cols = c("feature_id", "rt")),
    class = "tima_error"
  )
})

test_that("validate_dataframe allows empty data frames with allow_empty = TRUE", {
  df <- data.frame()
  expect_invisible(validate_dataframe(df, "features", allow_empty = TRUE))
})

# ── validate_weights ──────────────────────────────────────────────────────────

test_that("validate_weights accepts valid non-negative weights", {
  expect_invisible(validate_weights(c(1, 2, 3)))
  expect_invisible(validate_weights(c(0, 0.5, 1.0)))
  expect_invisible(validate_weights(c(spectral = 0.5, biological = 0.5)))
})

test_that("validate_weights errors on negative weights", {
  expect_error(validate_weights(c(1, -1, 1)), class = "tima_error")
})

test_that("validate_weights errors on all-zero weights", {
  expect_error(validate_weights(c(0, 0, 0)), class = "tima_error")
})

test_that("validate_weights errors on NA values", {
  expect_error(validate_weights(c(1, NA, 1)), class = "tima_error")
})

test_that("validate_weights errors on non-numeric input", {
  expect_error(validate_weights("1"), class = "tima_error")
  expect_error(validate_weights(list(1, 2, 3)), class = "tima_error")
})

# ── validate_character ────────────────────────────────────────────────────────

test_that("validate_character accepts valid strings", {
  expect_invisible(validate_character("hello", param_name = "x"))
  expect_invisible(validate_character(
    "pos",
    allowed_values = c("pos", "neg"),
    param_name = "mode"
  ))
})

test_that("validate_character errors on non-character input", {
  expect_error(validate_character(1L, param_name = "x"), class = "tima_error")
  expect_error(validate_character(TRUE, param_name = "x"), class = "tima_error")
})

test_that("validate_character errors on empty string by default", {
  expect_error(validate_character("", param_name = "x"), class = "tima_error")
})

test_that("validate_character allows empty string when allow_empty = TRUE", {
  expect_invisible(validate_character("", param_name = "x", allow_empty = TRUE))
})

test_that("validate_character handles NULL with allow_null", {
  expect_invisible(validate_character(
    NULL,
    param_name = "x",
    allow_null = TRUE
  ))
  expect_error(
    validate_character(NULL, param_name = "x", allow_null = FALSE),
    class = "tima_error"
  )
})

test_that("validate_character errors on disallowed value", {
  expect_error(
    validate_character(
      "bad",
      allowed_values = c("pos", "neg"),
      param_name = "mode"
    ),
    class = "tima_error"
  )
})

# ── validate_logical ──────────────────────────────────────────────────────────

test_that("validate_logical accepts TRUE/FALSE", {
  expect_invisible(validate_logical(TRUE, "flag"))
  expect_invisible(validate_logical(FALSE, "flag"))
})

test_that("validate_logical errors on NA", {
  expect_error(validate_logical(NA, "flag"), class = "tima_error")
})

test_that("validate_logical errors on non-logical input", {
  expect_error(validate_logical(1L, "flag"), class = "tima_error")
  expect_error(validate_logical("TRUE", "flag"), class = "tima_error")
})

test_that("validate_logical handles NULL with allow_null", {
  expect_invisible(validate_logical(NULL, "flag", allow_null = TRUE))
  expect_error(
    validate_logical(NULL, "flag", allow_null = FALSE),
    class = "tima_error"
  )
})

# ── validate_list_or_vector ───────────────────────────────────────────────────

test_that("validate_list_or_vector accepts lists and vectors", {
  expect_invisible(validate_list_or_vector(list(1, 2, 3), param_name = "x"))
  expect_invisible(validate_list_or_vector(c(1, 2, 3), param_name = "x"))
  expect_invisible(validate_list_or_vector(character(0), param_name = "x"))
})

test_that("validate_list_or_vector enforces min_length", {
  expect_invisible(validate_list_or_vector(
    c(1, 2),
    min_length = 2L,
    param_name = "x"
  ))
  expect_error(
    validate_list_or_vector(c(1), min_length = 2L, param_name = "x"),
    class = "tima_error"
  )
})

test_that("validate_list_or_vector enforces max_length", {
  expect_invisible(validate_list_or_vector(
    c(1, 2),
    max_length = 3L,
    param_name = "x"
  ))
  expect_error(
    validate_list_or_vector(c(1, 2, 3, 4), max_length = 3L, param_name = "x"),
    class = "tima_error"
  )
})

test_that("validate_list_or_vector errors on non-list/vector", {
  # data.frame is not a plain vector, but is a list - OK to pass
  # NULL without allow_null = TRUE should fail
  expect_error(
    validate_list_or_vector(NULL, param_name = "x", allow_null = FALSE),
    class = "tima_error"
  )
})

test_that("validate_list_or_vector handles NULL with allow_null", {
  expect_invisible(validate_list_or_vector(
    NULL,
    param_name = "x",
    allow_null = TRUE
  ))
  expect_error(
    validate_list_or_vector(NULL, param_name = "x", allow_null = FALSE),
    class = "tima_error"
  )
})
