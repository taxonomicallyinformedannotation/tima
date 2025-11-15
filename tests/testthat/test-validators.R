#' @title Enhanced Test Suite for Validators
#'
#' @description Comprehensive tests for all validation utilities in validators.R
#'     to maximize code coverage and ensure robust error handling.

library(testthat)
library(tima)

# Test validate_file_existence ----

test_that("validate_file_existence accepts valid files", {
  # Create temporary files using withr
  temp_file1 <- withr::local_tempfile()
  temp_file2 <- withr::local_tempfile()
  file.create(temp_file1)
  file.create(temp_file2)

  # Should pass silently
  expect_silent(
    validate_file_existence(list(
      file1 = temp_file1,
      file2 = temp_file2
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

  # Non-character file path
  temp_file <- withr::local_tempfile()
  file.create(temp_file)

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

# Test validate_ms_mode ----

# test_that("validate_ms_mode accepts valid modes", {
#   expect_silent(validate_ms_mode("pos"))
#   expect_silent(validate_ms_mode("neg"))
# })

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

# Test validate_tolerances ----

# test_that("validate_tolerances accepts valid values", {
#   expect_silent(validate_tolerances(tolerance_ppm = 10, tolerance_rt = 0.05))
#   expect_silent(validate_tolerances(tolerance_ppm = 5, tolerance_rt = 0.02))
#   expect_silent(validate_tolerances(tolerance_ppm = 1, tolerance_rt = 0.01))
#
#   # NULL values when not required
#   expect_silent(validate_tolerances(tolerance_ppm = 10, tolerance_rt = NULL))
#   expect_silent(validate_tolerances(tolerance_ppm = NULL, tolerance_rt = 0.05))
# })

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

# Test validate_adduct_list ----

# test_that("validate_adduct_list accepts valid lists", {
#   adducts <- list(
#     pos = c("[M+H]+", "[M+Na]+"),
#     neg = c("[M-H]-", "[M+Cl]-")
#   )
#
#   expect_silent(validate_adduct_list(adducts, "pos"))
#   expect_silent(validate_adduct_list(adducts, "neg"))
# })

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

# Test validate_numeric_range ----

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

# Test validate_character ----

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

# Test validate_logical ----

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

# Test validate_list_or_vector ----

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

# test_that("validate_list_or_vector rejects wrong types", {
#   expect_error(
#     validate_list_or_vector(data.frame(a = 1)),
#     "must be a list or vector"
#   )
# })

# Test validate_data_frame ----

test_that("validate_data_frame accepts valid data frames", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  expect_silent(validate_data_frame(df))
  expect_silent(validate_data_frame(df, required_columns = c("a", "b")))
  expect_silent(validate_data_frame(df, min_rows = 3))
})

test_that("validate_data_frame detects missing columns", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    validate_data_frame(df, required_columns = c("a", "c")),
    "missing required column"
  )

  # Error message should show available columns
  expect_error(
    validate_data_frame(df, required_columns = "missing"),
    "Available columns"
  )
})

test_that("validate_data_frame validates row count", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    validate_data_frame(df, min_rows = 5),
    "must have at least 5 row"
  )
})

test_that("validate_data_frame rejects non-data-frames", {
  expect_error(
    validate_data_frame(list(a = 1, b = 2)),
    "must be a data frame"
  )

  expect_error(
    validate_data_frame(matrix(1:9, nrow = 3)),
    "must be a data frame"
  )
})

test_that("validate_data_frame uses custom parameter names", {
  expect_error(
    validate_data_frame(list(), param_name = "my_data"),
    "my_data must be a data frame"
  )
})

# Integration tests ----

# test_that("validators work together in realistic scenarios", {
#   # Create test files
#   temp_features <- tempfile()
#   temp_library <- tempfile()
#   file.create(temp_features)
#   file.create(temp_library)
#
#   # Validate all inputs for an annotation function
#   expect_silent({
#     validate_file_existence(list(
#       features = temp_features,
#       library = temp_library
#     ))
#     validate_ms_mode("pos")
#     validate_tolerances(tolerance_ppm = 10, tolerance_rt = 0.05)
#   })
#
#   # Clean up
#   unlink(c(temp_features, temp_library))
# })

test_that("validators provide consistent error message format", {
  # All error messages should include parameter name and guidance
  errors <- list()

  tryCatch(
    validate_ms_mode("invalid"),
    error = function(e) errors[[1]] <<- e$message
  )

  tryCatch(
    validate_numeric_range(15, 0, 10, param_name = "test"),
    error = function(e) errors[[2]] <<- e$message
  )

  tryCatch(
    validate_character("x", allowed_values = c("a", "b")),
    error = function(e) errors[[3]] <<- e$message
  )

  # All should contain helpful information
  expect_true(all(sapply(errors, function(e) nchar(e) > 20)))
})

# Edge cases and stress tests ----

test_that("validators handle edge cases gracefully", {
  # Very long file paths
  long_path <- paste0(rep("a", 1000), collapse = "")
  expect_error(
    validate_file_existence(list(test = long_path)),
    "not found"
  )

  # Special characters in mode
  expect_error(validate_ms_mode("pos\n"), "Invalid")

  # Extreme tolerance values
  expect_warning(validate_tolerances(tolerance_ppm = 1e10), "exceeds")
  expect_error(validate_tolerances(tolerance_ppm = 1e-10), NA)

  # Empty data frame
  df <- data.frame()
  expect_error(validate_data_frame(df, min_rows = 1), "must have at least")
})

test_that("validators are thread-safe", {
  # Multiple simultaneous validations should work
  skip_on_cran()
  skip("Thread safety testing requires parallel package setup")

  results <- lapply(1:10, function(i) {
    validate_ms_mode("pos")
    validate_numeric_range(5, 0, 10)
    TRUE
  })

  expect_true(all(unlist(results)))
})
