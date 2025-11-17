# Test: Prepare Features Components

# =============================================================================
# Tests for prepare_features_components() - Network Component Processing
# =============================================================================

test_that("prepare_features_components validates input parameter", {
  # Test non-character input
  expect_error(
    prepare_features_components(
      input = 123,
      output = "output.tsv"
    ),
    "must be a non-empty character vector"
  )

  # Test empty character vector
  expect_error(
    prepare_features_components(
      input = character(0),
      output = "output.tsv"
    ),
    "must be a non-empty character vector"
  )

  # Test NULL input
  expect_error(
    prepare_features_components(
      input = NULL,
      output = "output.tsv"
    ),
    "must be a non-empty character vector"
  )
})

test_that("prepare_features_components validates output parameter", {
  # Create temp file for testing
  temp_input <- tempfile(fileext = ".tsv")
  writeLines("cluster index\tcomponentindex\n1\t1", temp_input)

  # Test non-character output
  expect_error(
    prepare_features_components(
      input = temp_input,
      output = 123
    ),
    "must be a single character string"
  )

  # Test vector output
  expect_error(
    prepare_features_components(
      input = temp_input,
      output = c("out1.tsv", "out2.tsv")
    ),
    "must be a single character string"
  )

  unlink(temp_input)
})

test_that("prepare_features_components checks file existence", {
  # Test non-existent file
  expect_error(
    prepare_features_components(
      input = "nonexistent_file.tsv",
      output = "output.tsv"
    ),
    "not found"
  )

  # Test multiple files, some missing
  temp_file1 <- tempfile(fileext = ".tsv")
  writeLines("cluster index\tcomponentindex\n1\t1", temp_file1)

  expect_error(
    prepare_features_components(
      input = c(temp_file1, "missing_file.tsv"),
      output = "output.tsv"
    ),
    "not found"
  )

  unlink(temp_file1)
})

test_that("prepare_features_components handles single valid file", {
  local_test_project(copy = TRUE)

  # Create valid component file
  temp_input <- tempfile(fileext = ".tsv")
  component_data <- data.frame(
    `cluster index` = c("FT001", "FT002", "FT003"),
    componentindex = c("1", "1", "2"),
    check.names = FALSE
  )
  write.table(
    component_data,
    temp_input,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_features_components(
    input = temp_input,
    output = temp_output
  )

  expect_equal(result, temp_output)
  expect_true(file.exists(temp_output))

  # Verify output content
  output_data <- tidytable::fread(temp_output)
  expect_equal(nrow(output_data), 3)
  expect_true("feature_id" %in% colnames(output_data))
  expect_true("component_id" %in% colnames(output_data))

  unlink(temp_input)
  unlink(temp_output)
})

test_that("prepare_features_components handles multiple input files", {
  local_test_project(copy = TRUE)

  # Create multiple component files
  temp_input1 <- tempfile(fileext = ".tsv")
  temp_input2 <- tempfile(fileext = ".tsv")

  component_data1 <- data.frame(
    `cluster index` = c("FT001", "FT002"),
    componentindex = c("1", "1"),
    check.names = FALSE
  )

  component_data2 <- data.frame(
    `cluster index` = c("FT003", "FT004"),
    componentindex = c("2", "2"),
    check.names = FALSE
  )

  write.table(
    component_data1,
    temp_input1,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )
  write.table(
    component_data2,
    temp_input2,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_features_components(
    input = c(temp_input1, temp_input2),
    output = temp_output
  )

  expect_equal(result, temp_output)
  expect_true(file.exists(temp_output))

  # Verify combined output
  output_data <- tidytable::fread(temp_output)
  expect_equal(nrow(output_data), 4)

  unlink(temp_input1)
  unlink(temp_input2)
  unlink(temp_output)
})

test_that("prepare_features_components handles empty input file", {
  local_test_project(copy = TRUE)

  # Create empty component file
  temp_input <- tempfile(fileext = ".tsv")
  empty_data <- data.frame(
    `cluster index` = character(0),
    componentindex = character(0),
    check.names = FALSE
  )
  write.table(
    empty_data,
    temp_input,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_features_components(
    input = temp_input,
    output = temp_output
  )

  expect_equal(result, temp_output)
  expect_true(file.exists(temp_output))

  # Verify output has correct structure even when empty
  output_data <- tidytable::fread(temp_output)
  expect_equal(nrow(output_data), 0)
  expect_true("feature_id" %in% colnames(output_data))
  expect_true("component_id" %in% colnames(output_data))

  unlink(temp_input)
  unlink(temp_output)
})

test_that("prepare_features_components removes duplicates", {
  local_test_project(copy = TRUE)

  # Create component file with duplicates
  temp_input <- tempfile(fileext = ".tsv")
  component_data <- data.frame(
    `cluster index` = c("FT001", "FT001", "FT002", "FT002"),
    componentindex = c("1", "1", "2", "2"),
    check.names = FALSE
  )
  write.table(
    component_data,
    temp_input,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_features_components(
    input = temp_input,
    output = temp_output
  )

  # Verify duplicates were removed
  output_data <- tidytable::fread(temp_output)
  expect_equal(nrow(output_data), 2) # Should have 2 unique rows, not 4

  unlink(temp_input)
  unlink(temp_output)
})

test_that("prepare_features_components handles NA values correctly", {
  local_test_project(copy = TRUE)

  # Create component file with NA values
  temp_input <- tempfile(fileext = ".tsv")
  writeLines(
    "cluster index\tcomponentindex\nFT001\t1\nFT002\tNA\n\t3\nFT004\t",
    temp_input
  )

  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_features_components(
    input = temp_input,
    output = temp_output
  )

  expect_equal(result, temp_output)
  expect_true(file.exists(temp_output))

  unlink(temp_input)
  unlink(temp_output)
})

test_that("prepare_features_components standardizes column names", {
  local_test_project(copy = TRUE)

  # Create component file with original column names
  temp_input <- tempfile(fileext = ".tsv")
  component_data <- data.frame(
    `cluster index` = c("FT001", "FT002"),
    componentindex = c("comp1", "comp2"),
    check.names = FALSE
  )
  write.table(
    component_data,
    temp_input,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_features_components(
    input = temp_input,
    output = temp_output
  )

  # Verify standardized column names
  output_data <- tidytable::fread(temp_output)
  expect_true("feature_id" %in% colnames(output_data))
  expect_true("component_id" %in% colnames(output_data))
  expect_false("cluster index" %in% colnames(output_data))
  expect_false("componentindex" %in% colnames(output_data))

  unlink(temp_input)
  unlink(temp_output)
})
