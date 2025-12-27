test_that("validate_choice provides fuzzy matching suggestions", {
  # Test with typo that should match
  expect_error(
    validate_choice("positive", c("pos", "neg"), "ms_mode"),
    "Invalid ms_mode: 'positive'",
    fixed = TRUE
  )

  expect_error(
    validate_choice("negatve", c("pos", "neg"), "ms_mode"),
    "Did you mean 'neg'\\?",
    fixed = FALSE
  )

  # Test with clear invalid value
  expect_error(
    validate_choice("both", c("pos", "neg"), "ms_mode"),
    "Invalid ms_mode: 'both'",
    fixed = TRUE
  )

  # Test valid value passes
  expect_invisible(
    validate_choice("pos", c("pos", "neg"), "ms_mode")
  )
})

test_that("validate_choice shows expected values", {
  expect_error(
    validate_choice("xyz", c("a", "b", "c"), "param"),
    "one of: 'a', 'b', 'c'",
    fixed = TRUE
  )
})

test_that("validate_file_exists suggests similar files", {
  # Create a temporary directory with test files
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "test_data.csv")
  similar_file <- file.path(temp_dir, "test_data.tsv")

  # Create the similar file
  writeLines("test", similar_file)
  on.exit(unlink(similar_file), add = TRUE)

  # Test that it suggests the similar file
  expect_error(
    validate_file_exists(test_file, "CSV file", "input_file"),
    "Similar files in directory",
    fixed = FALSE
  )

  # Test that valid file passes
  expect_invisible(
    validate_file_exists(similar_file, "TSV file")
  )
})

test_that("validate_file_exists handles missing directories", {
  fake_path <- file.path(tempdir(), "nonexistent_dir", "file.txt")

  expect_error(
    validate_file_exists(fake_path, "data file"),
    "Directory does not exist",
    fixed = FALSE
  )
})

test_that("validate_dataframe suggests column name fixes", {
  df <- data.frame(
    feature_id = 1:3,
    mzz = c(100, 200, 300), # Typo: should be 'mz'
    rt = c(1.0, 2.0, 3.0)
  )

  # Test missing column with similar name
  expect_error(
    validate_dataframe(
      df,
      required_cols = c("feature_id", "mz", "rt")
    ),
    "Did you mean.*'mzz'.*instead of.*'mz'",
    fixed = FALSE
  )

  # Test valid dataframe passes
  expect_invisible(
    validate_dataframe(
      df,
      required_cols = c("feature_id", "mzz", "rt")
    )
  )
})

test_that("validate_dataframe checks minimum rows", {
  df <- data.frame(x = 1:2)

  expect_error(
    validate_dataframe(df, min_rows = 5L, allow_empty = FALSE),
    "insufficient rows.*Expected.*at least 5",
    fixed = FALSE
  )

  expect_invisible(
    validate_dataframe(df, min_rows = 2L)
  )
})

test_that("validate_dataframe warns about unexpected columns", {
  df <- data.frame(
    required_col = 1:3,
    expected_col = 4:6,
    unexpected_col = 7:9
  )

  expect_warning(
    validate_dataframe(
      df,
      required_cols = "required_col",
      optional_cols = "expected_col"
    ),
    "Unexpected columns.*unexpected_col",
    fixed = FALSE
  )
})

test_that("validate_numeric_range provides context", {
  expect_error(
    validate_numeric_range(
      value = 150,
      min_value = 0,
      max_value = 100,
      param_name = "tolerance",
      context = "Values above 100 may cause performance issues"
    ),
    "performance issues",
    fixed = FALSE
  )

  expect_invisible(
    validate_numeric_range(50, 0, 100, "tolerance")
  )
})

test_that("validate_numeric_range warns for recommended ranges", {
  expect_warning(
    validate_numeric_range(
      value = 25,
      min_value = 0,
      max_value = 100,
      param_name = "tolerance",
      recommended_max = 20
    ),
    "exceeds recommended maximum",
    fixed = FALSE
  )

  expect_warning(
    validate_numeric_range(
      value = 2,
      min_value = 0,
      max_value = 100,
      param_name = "tolerance",
      recommended_min = 5
    ),
    "below recommended minimum",
    fixed = FALSE
  )
})

test_that("validate_tolerance_ppm provides instrument guidance", {
  expect_warning(
    validate_tolerance_ppm(
      tolerance_ppm = 500,
      param_name = "mass_tolerance"
    ),
    "Orbitrap.*Q-TOF.*Q-Trap",
    fixed = FALSE
  )

  expect_error(
    validate_tolerance_ppm(
      tolerance_ppm = -5,
      param_name = "mass_tolerance"
    ),
    "must be positive",
    fixed = FALSE
  )

  expect_invisible(
    validate_tolerance_ppm(5)
  )
})

test_that("format_error creates structured messages", {
  msg <- format_error(
    problem = "Invalid value",
    expected = "positive number",
    received = "-5",
    fix = "Use a positive number"
  )

  expect_true(grepl("Invalid value", msg, fixed = TRUE))
  expect_true(grepl("Expected:", msg, fixed = TRUE))
  expect_true(grepl("Received:", msg, fixed = TRUE))
  expect_true(grepl("Fix:", msg, fixed = TRUE))
  expect_true(grepl("✗", msg, fixed = TRUE)) # Check mark present
})

test_that("fuzzy_match finds close matches", {
  # Test internal function
  match <- tima:::.fuzzy_match("positiv", c("pos", "neg", "positive"))
  expect_equal(match, "positive")

  # Test partial match
  match <- tima:::.fuzzy_match("positive", c("pos", "neg"))
  expect_equal(match, "pos") # Should match closest

  # Test no match for very distant strings
  match <- tima:::.fuzzy_match(
    "verylongstring",
    c("abc", "def", "ghi"),
    max_distance = 2
  )
  expect_null(match)
})

test_that("validate_ms_mode uses enhanced errors", {
  expect_error(
    validate_ms_mode("positive"),
    "Invalid ms_mode: 'positive'",
    fixed = TRUE
  )

  expect_error(
    validate_ms_mode(NULL),
    "ms_mode is required",
    fixed = FALSE
  )

  expect_invisible(validate_ms_mode("pos"))
  expect_invisible(validate_ms_mode("neg"))
})

test_that("validate_tolerances uses enhanced errors", {
  expect_warning(
    validate_tolerances(tolerance_ppm = 500),
    "Orbitrap",
    fixed = FALSE
  )

  expect_error(
    validate_tolerances(tolerance_ppm = "not_numeric"),
    "Invalid tolerance_ppm",
    fixed = FALSE
  )

  expect_error(
    validate_tolerances(tolerance_ppm = -10),
    "must be positive",
    fixed = FALSE
  )

  expect_invisible(
    validate_tolerances(tolerance_ppm = 5, tolerance_rt = 0.05)
  )
})

test_that("validate_dataframe uses enhanced errors", {
  df <- data.frame(a = 1:3, b = 4:6)

  expect_error(
    validate_dataframe(df, required_cols = c("a", "c")),
    "Missing required columns",
    fixed = FALSE
  )

  expect_invisible(
    validate_dataframe(df, required_cols = c("a", "b"))
  )
})

test_that("validate_choice uses enhanced errors", {
  expect_error(
    validate_choice("invalid", c("valid1", "valid2"), "option"),
    "Invalid option",
    fixed = FALSE
  )

  expect_invisible(
    validate_choice("valid1", c("valid1", "valid2"))
  )
})

test_that("error messages include unicode symbols", {
  expect_error(
    validate_choice("bad", c("good"), "param"),
    "\u2717", # ✗ symbol
    fixed = TRUE
  )
})

test_that("type errors are clear and actionable", {
  expect_error(
    validate_choice(123, c("a", "b"), "param"),
    "Invalid type.*Expected.*single character string",
    fixed = FALSE
  )

  expect_error(
    validate_numeric_range(
      value = "text",
      min_value = 0,
      max_value = 100,
      param_name = "threshold"
    ),
    "Invalid type.*Expected.*single numeric value",
    fixed = FALSE
  )
})
