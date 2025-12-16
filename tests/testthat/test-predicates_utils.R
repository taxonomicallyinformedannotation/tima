test_that("is_single_string validates correctly", {
  # Valid cases
  expect_true(is_single_string("hello"))
  expect_true(is_single_string("path/to/file.txt"))
  expect_true(is_single_string(""))

  # Invalid cases - wrong type
  expect_false(is_single_string(123))
  expect_false(is_single_string(TRUE))
  expect_false(is_single_string(NULL))
  expect_false(is_single_string(list("a")))

  # Invalid cases - wrong length
  expect_false(is_single_string(c("a", "b")))
  expect_false(is_single_string(character(0)))

  # Invalid cases - NA
  expect_false(is_single_string(NA_character_))
  expect_false(is_single_string(NA))
})

test_that("all_single_strings works on lists", {
  # All valid
  valid_list <- list(
    file1 = "path1.txt",
    file2 = "path2.txt",
    file3 = "path3.txt"
  )
  result <- all_single_strings(valid_list)
  expect_true(all(result))
  expect_named(result, c("file1", "file2", "file3"))

  # Mixed valid/invalid
  mixed_list <- list(
    valid = "path.txt",
    invalid_vec = c("a", "b"),
    invalid_num = 123,
    invalid_na = NA_character_
  )
  result <- all_single_strings(mixed_list)
  expect_true(result["valid"])
  expect_false(result["invalid_vec"])
  expect_false(result["invalid_num"])
  expect_false(result["invalid_na"])

  # Empty list
  result <- all_single_strings(list())
  expect_length(result, 0)
})

test_that("validate_all_single_strings stops on invalid input", {
  # Should not error on valid input
  valid <- list(file1 = "path1.txt", file2 = "path2.txt")
  expect_silent(validate_all_single_strings(valid))
  expect_invisible(validate_all_single_strings(valid))

  # Should error on invalid input with informative message
  invalid <- list(file1 = c("a", "b"), file2 = 123)
  expect_error(
    validate_all_single_strings(invalid),
    "must be single character strings"
  )
  expect_error(
    validate_all_single_strings(invalid),
    "file1, file2"
  )

  # Should error with custom prefix
  expect_error(
    validate_all_single_strings(invalid, "Custom prefix"),
    "Custom prefix"
  )

  # Should error on non-list input
  expect_error(
    validate_all_single_strings("not a list"),
    "must be a named list"
  )

  # Should error on unnamed list
  expect_error(
    validate_all_single_strings(list("a", "b")),
    "must be a named list"
  )
})

test_that("count_if counts correctly", {
  # Numeric predicate
  expect_equal(count_if(1:10, function(x) x > 5), 5L)
  expect_equal(count_if(1:10, function(x) x %% 2 == 0), 5L)
  expect_equal(count_if(1:10, function(x) x < 0), 0L)
  expect_equal(count_if(1:10, function(x) TRUE), 10L)

  # Type predicate
  mixed <- list("a", 1, "b", 2, "c")
  expect_equal(count_if(mixed, is.character), 3L)
  expect_equal(count_if(mixed, is.numeric), 2L)

  # Custom predicate with extra args
  is_greater <- function(x, threshold) x > threshold
  expect_equal(count_if(1:10, is_greater, threshold = 7), 3L)

  # Empty input
  expect_equal(count_if(integer(0), function(x) TRUE), 0L)
  expect_equal(count_if(list(), function(x) TRUE), 0L)
})

test_that("is_single_string edge cases", {
  # Factor is not character
  expect_false(is_single_string(factor("text")))

  # List with single element is not character vector
  expect_false(is_single_string(list("text")))

  # Whitespace is still valid
  expect_true(is_single_string("   "))
  expect_true(is_single_string("\n"))
  expect_true(is_single_string("\t"))
})

test_that("validate_all_single_strings edge cases", {
  # Single element list - valid
  single <- list(only = "path.txt")
  expect_silent(validate_all_single_strings(single))

  # List with empty string - valid
  empty_str <- list(file = "")
  expect_silent(validate_all_single_strings(empty_str))

  # List with NULL - invalid
  null_val <- list(file = NULL)
  expect_error(
    validate_all_single_strings(null_val),
    "must be single character strings"
  )
})

test_that("count_if with complex predicates", {
  # Predicate using external variable
  threshold <- 5
  result <- count_if(1:10, function(x) x > threshold)
  expect_equal(result, 5L)

  # Predicate with multiple conditions (handles NA gracefully)
  is_valid_score <- function(x) {
    !is.na(x) && is.numeric(x) && x >= 0 && x <= 1
  }
  scores <- c(0.5, 1.2, -0.1, 0.8, NA, 0.0)
  expect_equal(count_if(scores, is_valid_score), 3L)

  # Using built-in predicates
  vals <- list(1, "text", NULL, TRUE, 3.14)
  expect_equal(count_if(vals, is.null), 1L)
  expect_equal(count_if(vals, is.logical), 1L)
})
