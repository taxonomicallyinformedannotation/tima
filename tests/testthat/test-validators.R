# Test Suite: validators ---

library(testthat)

# Test helpers ----

#' Create temporary files for testing
#' @param n Number of files to create
#' @return Character vector of file paths
create_temp_files <- function(n = 1) {
  files <- replicate(n, withr::local_tempfile(), simplify = TRUE)
  invisible(lapply(files, file.create))
  files
}

## validate_file_existence ----

test_that("validate_file_existence accepts valid files", {
  # Create temporary files using helper
  temp_files <- create_temp_files(2)

  # Should pass silently
  expect_silent(
    validate_file_existence(list(
      file1 = temp_files[1],
      file2 = temp_files[2]
    ))
  )
})

test_that("validate_file_existence rejects missing files", {
  expect_error(
    validate_file_existence(list(
      missing = "/nonexistent/file.txt"
    )),
    "not found"
  )
})

test_that("validate_file_existence handles NULL with allow_null", {
  # Should error by default
  expect_error(
    validate_file_existence(list(test = NULL)),
    "NULL"
  )

  # Should pass with allow_null = TRUE
  expect_silent(
    validate_file_existence(list(test = NULL), allow_null = TRUE)
  )
})

test_that("validate_file_existence validates input types", {
  # Not a list
  expect_error(
    validate_file_existence("not_a_list"),
    "must be a named list"
  )

  # Empty list
  expect_error(
    validate_file_existence(list()),
    "cannot be empty"
  )

  # Non-character file path - create temp file using helper
  temp_file <- create_temp_files(1)[1]

  expect_error(
    validate_file_existence(list(bad = 123)),
    "must be a single character string"
  )
})

test_that("validate_file_existence provides detailed error messages", {
  expect_error(
    validate_file_existence(list(
      file1 = "/missing1.txt",
      file2 = "/missing2.txt"
    )),
    "file1.*file2"
  )
})

## validate_ms_mode ----

test_that("validate_ms_mode accepts valid modes", {
  expect_silent(validate_ms_mode("pos"))
  expect_silent(validate_ms_mode("neg"))
})

test_that("validate_ms_mode rejects invalid modes", {
  expect_error(validate_ms_mode("positive"), "Invalid ms_mode")
  expect_error(validate_ms_mode("negative"), "Invalid ms_mode")
  expect_error(validate_ms_mode("both"), "Invalid ms_mode")
  expect_error(validate_ms_mode(""), "Invalid ms_mode")
})

test_that("validate_ms_mode validates input types", {
  expect_error(validate_ms_mode(NULL), "must be provided")
  expect_error(validate_ms_mode(123), "single character string")
  expect_error(validate_ms_mode(c("pos", "neg")), "single character string")
  expect_error(validate_ms_mode(NA_character_), "Invalid ms_mode")
})

test_that("validate_ms_mode provides helpful error messages", {
  expect_error(
    validate_ms_mode("wrong"),
    "Must be one of: pos, neg"
  )
})

## validate_tolerances ----

test_that("validate_tolerances accepts valid values", {
  expect_silent(validate_tolerances(tolerance_ppm = 10, tolerance_rt = 0.05))
  expect_silent(validate_tolerances(tolerance_ppm = 5, tolerance_rt = 0.02))
  expect_silent(validate_tolerances(tolerance_ppm = 1, tolerance_rt = 0.01))

  # NULL values when not required
  expect_silent(validate_tolerances(tolerance_ppm = 10, tolerance_rt = NULL))
  expect_silent(validate_tolerances(tolerance_ppm = NULL, tolerance_rt = 0.05))
})

test_that("validate_tolerances warns about high values", {
  expect_warning(
    validate_tolerances(tolerance_ppm = 25, tolerance_rt = 0.05),
    "exceeds recommended maximum"
  )

  expect_warning(
    validate_tolerances(tolerance_ppm = 10, tolerance_rt = 0.1),
    "exceeds recommended maximum"
  )
})

test_that("validate_tolerances rejects invalid values", {
  # Negative values
  expect_error(validate_tolerances(tolerance_ppm = -5), "must be positive")
  expect_error(validate_tolerances(tolerance_rt = -0.01), "must be positive")

  # Zero values
  expect_error(validate_tolerances(tolerance_ppm = 0), "must be positive")
  expect_error(validate_tolerances(tolerance_rt = 0), "must be positive")

  # Wrong types
  expect_error(validate_tolerances(tolerance_ppm = "10"), "must be")
  expect_error(validate_tolerances(tolerance_rt = "0.05"), "must be")

  # Multiple values
  expect_error(
    validate_tolerances(tolerance_ppm = c(5, 10)),
    "single numeric value"
  )
})

test_that("validate_tolerances handles custom limits", {
  expect_warning(
    validate_tolerances(
      tolerance_ppm = 15,
      max_ppm = 10
    ),
    "exceeds recommended maximum"
  )
})

## validate_adduct_list ----

test_that("validate_adduct_list accepts valid lists", {
  adducts <- list(
    pos = c("[M+H]+", "[M+Na]+"),
    neg = c("[M-H]-", "[M+Cl]-")
  )

  expect_silent(validate_adduct_list(adducts, "pos"))
  expect_silent(validate_adduct_list(adducts, "neg"))
})

test_that("validate_adduct_list rejects invalid lists", {
  # Not a list
  expect_error(
    validate_adduct_list("not_a_list", "pos"),
    "must be a list"
  )

  # Missing mode
  adducts <- list(pos = c("[M+H]+"))
  expect_error(
    validate_adduct_list(adducts, "neg"),
    "must contain 'neg' mode"
  )
})

test_that("validate_adduct_list warns about empty lists", {
  adducts <- list(pos = character(0))

  expect_warning(
    validate_adduct_list(adducts, "pos"),
    "is empty"
  )
})

## validate_numeric_range ----

test_that("validate_numeric_range accepts values in range", {
  expect_silent(validate_numeric_range(5, min_value = 0, max_value = 10))
  expect_silent(validate_numeric_range(0, min_value = 0, max_value = 10))
  expect_silent(validate_numeric_range(10, min_value = 0, max_value = 10))
})

test_that("validate_numeric_range rejects values out of range", {
  expect_error(
    validate_numeric_range(-1, min_value = 0, max_value = 10),
    "must be between"
  )

  expect_error(
    validate_numeric_range(11, min_value = 0, max_value = 10),
    "must be between"
  )
})

test_that("validate_numeric_range handles NULL", {
  expect_error(
    validate_numeric_range(NULL),
    "cannot be NULL"
  )

  expect_silent(
    validate_numeric_range(NULL, allow_null = TRUE)
  )
})

test_that("validate_numeric_range handles NA", {
  expect_error(
    validate_numeric_range(NA_real_),
    "cannot be NA"
  )
})

test_that("validate_numeric_range validates types", {
  expect_error(
    validate_numeric_range("5"),
    "must be a single numeric value"
  )

  expect_error(
    validate_numeric_range(c(5, 10)),
    "must be a single numeric value"
  )
})

test_that("validate_numeric_range uses custom parameter names", {
  expect_error(
    validate_numeric_range(
      15,
      min_value = 0,
      max_value = 10,
      param_name = "my_value"
    ),
    "my_value.*must be between"
  )
})

## validate_character ----

test_that("validate_character accepts valid strings", {
  expect_silent(validate_character("test"))
  expect_silent(validate_character("pos", allowed_values = c("pos", "neg")))
})

test_that("validate_character rejects empty strings", {
  expect_error(validate_character(""), "value cannot be an empty string")

  expect_silent(validate_character("", allow_empty = TRUE))
})

test_that("validate_character validates against allowed values", {
  expect_error(
    validate_character("invalid", allowed_values = c("pos", "neg")),
    "must be one of"
  )
})

test_that("validate_character handles NULL", {
  expect_error(validate_character(NULL), "cannot be NULL")
  expect_silent(validate_character(NULL, allow_null = TRUE))
})

test_that("validate_character validates types", {
  expect_error(validate_character(123), "must be")
  expect_error(validate_character(c("a", "b")), "single")
})

## validate_logical ----

test_that("validate_logical accepts valid booleans", {
  expect_silent(validate_logical(TRUE))
  expect_silent(validate_logical(FALSE))
})

test_that("validate_logical rejects invalid values", {
  expect_error(validate_logical(NULL), "cannot be NULL")
  expect_error(validate_logical(1), "must be.*logical")
  expect_error(validate_logical("TRUE"), "must be.*logical")
  expect_error(validate_logical(c(TRUE, FALSE)), "single")
  expect_error(validate_logical(NA), "cannot be NA")
})

test_that("validate_logical allows NULL when specified", {
  expect_silent(validate_logical(NULL, allow_null = TRUE))
})

## validate_list_or_vector ----

test_that("validate_list_or_vector accepts lists and vectors", {
  expect_silent(validate_list_or_vector(list(a = 1, b = 2)))
  expect_silent(validate_list_or_vector(c(1, 2, 3)))
  expect_silent(validate_list_or_vector(1:10))
})

test_that("validate_list_or_vector validates length", {
  # Too short
  expect_error(
    validate_list_or_vector(list(), min_length = 1),
    "must have at least 1"
  )

  # Too long
  expect_error(
    validate_list_or_vector(1:10, max_length = 5),
    "cannot have more than 5"
  )
})

test_that("validate_list_or_vector handles NULL", {
  expect_error(validate_list_or_vector(NULL), "cannot be NULL")
  expect_silent(validate_list_or_vector(NULL, allow_null = TRUE))
})

## validate_dataframe ----

test_that("validate_dataframe accepts valid data frames", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_silent(validate_dataframe(df))
  expect_silent(validate_dataframe(df, required_cols = c("a", "b")))
  expect_silent(validate_dataframe(df, min_rows = 3))
})

test_that("validate_dataframe detects missing columns", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    validate_dataframe(df, required_cols = c("a", "c")),
    "missing required column"
  )

  # Error message should show available columns
  expect_error(
    validate_dataframe(df, required_cols = "missing"),
    "Available columns"
  )
})

test_that("validate_dataframe validates row count", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    validate_dataframe(df, min_rows = 5),
    "must have at least 5 row"
  )
})

test_that("validate_dataframe rejects non-data-frames", {
  expect_error(
    validate_dataframe(list(a = 1, b = 2)),
    "must be a data frame"
  )

  expect_error(
    validate_dataframe(matrix(1:9, nrow = 3)),
    "must be a data frame"
  )
})

test_that("validate_dataframe uses custom parameter names", {
  expect_error(
    validate_dataframe(list(), param_name = "my_data"),
    "my_data must be a data frame"
  )
})

## validate_weights ----

test_that("validate_weights accepts any positive weights (auto-normalized)", {
  # Weights that sum to 1
  expect_silent(validate_weights(c(0.5, 0.5)))

  # Weights that DON'T sum to 1 (these should work now!)
  expect_silent(validate_weights(c(1, 1)))
  expect_silent(validate_weights(c(1, 2, 3)))
  expect_silent(validate_weights(c(2, 3)))

  # Named weights
  expect_silent(validate_weights(c(spectral = 1, biological = 1)))
  expect_silent(validate_weights(c(spectral = 1, biological = 1, chemical = 1)))

  # Different magnitudes
  expect_silent(validate_weights(c(10, 20, 30)))
  expect_silent(validate_weights(c(0.1, 0.2, 0.7)))
})

test_that("validate_weights rejects negative weights", {
  expect_error(
    validate_weights(c(1, -1)),
    "must be non-negative"
  )

  expect_error(
    validate_weights(c(-0.5, 0.5)),
    "must be non-negative"
  )

  # Should show which weights are negative
  expect_error(
    validate_weights(c(spectral = 1, biological = -1)),
    "biological"
  )
})

test_that("validate_weights rejects all zeros (division by zero)", {
  expect_error(
    validate_weights(c(0, 0)),
    "cannot all be zero.*division by zero"
  )

  expect_error(
    validate_weights(c(0, 0, 0)),
    "cannot all be zero.*division by zero"
  )
})

test_that("validate_weights allows single zero with other positive weights", {
  # This is fine - only all zeros is problematic
  expect_silent(validate_weights(c(0, 1)))
  expect_silent(validate_weights(c(1, 0, 2)))
})

test_that("validate_weights rejects NA values", {
  expect_error(
    validate_weights(c(1, NA)),
    "weights cannot contain NA values"
  )

  expect_error(
    validate_weights(c(NA, NA)),
    "weights must be numeric, got: logical"
  )
})

test_that("validate_weights validates input types", {
  expect_error(
    validate_weights("not numeric"),
    "must be numeric"
  )

  expect_error(
    validate_weights(list(a = "text", b = "text")),
    "must be numeric"
  )
})

test_that("validate_weights uses custom parameter names", {
  expect_error(
    validate_weights(c(-1, 1), param_name = "my_weights"),
    "my_weights must be non-negative"
  )

  expect_error(
    validate_weights(c(0, 0), param_name = "score_weights"),
    "score_weights cannot all be zero"
  )
})

test_that("validate_weights examples from documentation work", {
  # From updated docs - these should all work!
  expect_silent(validate_weights(c(spectral = 1, biological = 1)))
  expect_silent(validate_weights(c(spectral = 2, chemical = 3, biological = 5)))
  expect_silent(validate_weights(c(spectral = 0.5, biological = 0.5)))
})

test_that("validate_weights matches weight_bio.R and weight_chemo.R usage", {
  # These are the actual use cases from the weight functions

  # weight_bio uses 2 weights
  expect_silent(validate_weights(c(
    spectral = 1,
    biological = 1
  )))

  # weight_chemo uses 3 weights
  expect_silent(validate_weights(c(
    spectral = 1,
    biological = 1,
    chemical = 1
  )))

  # Can use any positive ratios
  expect_silent(validate_weights(c(
    spectral = 2,
    biological = 3,
    chemical = 5
  )))
})

## Performance tests ----

test_that("validators are reasonably fast", {
  # Test that validators don't add excessive overhead
  skip_on_cran()

  # Create test data
  df <- data.frame(a = 1:1000, b = 1001:2000)

  # validate_dataframe should be fast
  timing <- system.time(
    for (i in 1:100) {
      validate_dataframe(df, param_name = "test_df")
    }
  )
  expect_lt(timing["elapsed"], 1.0) # Should complete in < 1 second

  # validate_weights should be fast
  weights <- c(1, 2, 3)
  timing <- system.time(
    for (i in 1:1000) {
      validate_weights(weights, param_name = "test_weights")
    }
  )
  expect_lt(timing["elapsed"], 0.5) # Should complete in < 0.5 seconds
})
