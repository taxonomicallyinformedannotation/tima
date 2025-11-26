# Unit tests for validators.R

library(testthat)
library(tima)

ns <- asNamespace("tima")

# Helper: temporary files
with_temp_files <- function(n = 2L, expr) {
  paths <- vapply(seq_len(n), function(i) tempfile(pattern = paste0("tima-", i, "-")), character(1))
  file.create(paths)
  on.exit(unlink(paths), add = TRUE)
  force(expr(paths))
}

# validate_file_existence ----
validate_file_existence <- get("validate_file_existence", ns)
validate_ms_mode <- get("validate_ms_mode", ns)
validate_tolerances <- get("validate_tolerances", ns)
validate_dataframe <- get("validate_dataframe", ns)
validate_numeric_range <- get("validate_numeric_range", ns)
validate_weights <- get("validate_weights", ns)
validate_choice <- get("validate_choice", ns)
validate_character <- get("validate_character", ns)
validate_logical <- get("validate_logical", ns)
validate_list_or_vector <- get("validate_list_or_vector", ns)
assert_flag <- get("assert_flag", ns)
assert_positive_integer <- get("assert_positive_integer", ns)
assert_scalar_numeric <- get("assert_scalar_numeric", ns)
MAX_TOLERANCE_PPM <- get("MAX_TOLERANCE_PPM", ns)
MAX_TOLERANCE_RT_ADDUCTS <- get("MAX_TOLERANCE_RT_ADDUCTS", ns)

# validate_file_existence ----
test_that("validate_file_existence errors on non-list and empty list", {
  expect_error(validate_file_existence("not a list"), "must be a named list")
  expect_error(validate_file_existence(list()), "cannot be empty")
})

test_that("validate_file_existence detects missing and accepts existing files", {
  with_temp_files(2L, function(paths) {
    files <- list(a = paths[1], b = paths[2])
    expect_invisible(validate_file_existence(files))

    files_missing <- list(a = paths[1], b = tempfile())
    expect_error(validate_file_existence(files_missing), "not found")
  })
})

# validate_ms_mode ----
test_that("validate_ms_mode accepts valid modes and rejects invalid", {
  expect_invisible(validate_ms_mode("pos"))
  expect_invisible(validate_ms_mode("neg"))
  expect_error(validate_ms_mode("both"), "Invalid ms_mode")
  expect_error(validate_ms_mode(NULL), "must be provided")
})

# validate_tolerances ----
test_that("validate_tolerances validates ranges and warns when exceeding", {
  expect_invisible(validate_tolerances(tolerance_ppm = 10, tolerance_rt = 0.05))
  expect_error(validate_tolerances(tolerance_ppm = 0), "must be positive")
  expect_error(validate_tolerances(tolerance_rt = 0), "must be positive")
  expect_warning(validate_tolerances(tolerance_ppm = MAX_TOLERANCE_PPM + 1), "exceeds recommended maximum")
  expect_warning(validate_tolerances(tolerance_rt = MAX_TOLERANCE_RT_ADDUCTS + 1), "exceeds recommended maximum")
})

# validate_dataframe ----
test_that("validate_dataframe checks type, emptiness, rows, and columns", {
  df <- data.frame(feature_id = 1:3, mz = c(100.1, 200.2, 300.3))
  expect_invisible(validate_dataframe(df, param_name = "features", required_cols = c("feature_id", "mz")))
  expect_error(validate_dataframe(list(), param_name = "features"), "must be a data frame")
  expect_error(validate_dataframe(df[FALSE, ], allow_empty = FALSE), "cannot be empty")
  expect_error(validate_dataframe(df[1, ], min_rows = 2), "must have at least")
  expect_error(validate_dataframe(df, required_cols = c("feature_id", "rt")), "missing required column")
})

# validate_numeric_range ----
test_that("validate_numeric_range checks bounds and type", {
  expect_invisible(validate_numeric_range(0.5, 0, 1, param_name = "threshold"))
  expect_error(validate_numeric_range(NULL, 0, 1, allow_null = FALSE), "cannot be NULL")
  expect_invisible(validate_numeric_range(NULL, 0, 1, allow_null = TRUE))
  expect_error(validate_numeric_range("a", 0, 1), "single numeric")
  expect_error(validate_numeric_range(NA_real_, 0, 1), "cannot be NA")
  expect_error(validate_numeric_range(-1, 0, 1), "must be between")
})

# validate_weights ----
test_that("validate_weights validates numeric, non-negative, non-zero-sum", {
  expect_invisible(validate_weights(c(a = 1, b = 2)))
  expect_error(validate_weights(c(NA_real_, 1)), "cannot contain NA")
  expect_error(validate_weights(c(-1, 2)), "must be non-negative")
  expect_error(validate_weights(c(0, 0)), "cannot all be zero")
})

# validate_choice ----
test_that("validate_choice enforces allowed set", {
  expect_invisible(validate_choice("OR", c("OR", "AND"), param_name = "condition"))
  expect_error(validate_choice("X", c("OR", "AND"), param_name = "condition"), "must be one of")
})

# validate_character ----
test_that("validate_character checks null, empty, and allowed values", {
  expect_invisible(validate_character("value", allowed_values = c("value", "other"), param_name = "p"))
  expect_error(validate_character(NULL, param_name = "p"), "cannot be NULL")
  expect_error(validate_character("", param_name = "p"), "cannot be an empty")
  expect_error(validate_character("bad", allowed_values = c("value"), param_name = "p"), "must be one of")
})

# validate_logical ----
test_that("validate_logical checks type, length, and NA", {
  expect_invisible(validate_logical(TRUE, param_name = "flag"))
  expect_error(validate_logical(NULL, param_name = "flag"), "cannot be NULL")
  expect_error(validate_logical(c(TRUE, FALSE), param_name = "flag"), "single logical")
  expect_error(validate_logical(NA, param_name = "flag"), "cannot be NA")
})

# validate_list_or_vector ----
test_that("validate_list_or_vector enforces type and length bounds", {
  expect_invisible(validate_list_or_vector(list(1, 2), min_length = 1, max_length = 3))
  expect_invisible(validate_list_or_vector(c(1, 2, 3), min_length = 1, max_length = 3))
  expect_error(validate_list_or_vector(NULL, allow_null = FALSE), "cannot be NULL")
  expect_error(validate_list_or_vector(1, min_length = 2), "at least")
  expect_error(validate_list_or_vector(c(1, 2, 3, 4), max_length = 3), "cannot have more than")
})

# assert_flag ----
test_that("assert_flag enforces single TRUE/FALSE", {
  expect_invisible(assert_flag(TRUE))
  expect_error(assert_flag(c(TRUE, FALSE)), "single TRUE or FALSE")
  expect_error(assert_flag(NA), "single TRUE or FALSE")
})

# assert_positive_integer ----
test_that("assert_positive_integer enforces integer and bounds", {
  expect_invisible(assert_positive_integer(1))
  expect_invisible(assert_positive_integer(0, allow_zero = TRUE))
  expect_error(assert_positive_integer(1.5), "must be an integer")
  expect_error(assert_positive_integer(-1), "> 0")
})

# assert_scalar_numeric ----
test_that("assert_scalar_numeric enforces type and range", {
  expect_invisible(assert_scalar_numeric(1, "param", min = 0, max = 2))
  expect_error(assert_scalar_numeric(c(1, 2), "param"), "single numeric")
  expect_error(assert_scalar_numeric(NA_real_, "param"), "cannot be NA")
  expect_error(assert_scalar_numeric(-1, "param", min = 0), "must be between")
})
