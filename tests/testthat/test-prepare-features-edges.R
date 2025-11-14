# Test: Prepare Features Edges
library(testthat)

# =============================================================================
# Tests for prepare_features_edges() - Network Edge Processing
# =============================================================================

test_that("prepare_features_edges validates input structure", {
  # Test input without required elements
  expect_error(
    prepare_features_edges(
      input = list("only_one" = "file.tsv"),
      output = "output.tsv"
    ),
    "must contain 'ms1' and 'spectral'"
  )

  # Test input with NULL names
  expect_error(
    prepare_features_edges(
      input = list("file1.tsv", "file2.tsv"),
      output = "output.tsv"
    ),
    "must contain 'ms1' and 'spectral'"
  )

  # Test input missing 'spectral'
  expect_error(
    prepare_features_edges(
      input = list("ms1" = "file1.tsv", "other" = "file2.tsv"),
      output = "output.tsv"
    ),
    "must contain 'ms1' and 'spectral'"
  )
})

test_that("prepare_features_edges validates output parameter", {
  temp_ms1 <- tempfile(fileext = ".tsv")
  temp_spectral <- tempfile(fileext = ".tsv")

  writeLines("ID1\tID2\n1\t2", temp_ms1)
  writeLines("ID1\tID2\n1\t2", temp_spectral)

  # Test non-character output
  expect_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
      output = 123
    ),
    "must be a single character string"
  )

  # Test vector output
  expect_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
      output = c("out1.tsv", "out2.tsv")
    ),
    "must be a single character string"
  )

  unlink(temp_ms1)
  unlink(temp_spectral)
})

test_that("prepare_features_edges validates column name parameters", {
  temp_ms1 <- tempfile(fileext = ".tsv")
  temp_spectral <- tempfile(fileext = ".tsv")

  writeLines("ID1\tID2\n1\t2", temp_ms1)
  writeLines("ID1\tID2\n1\t2", temp_spectral)

  # Test non-character name_source
  expect_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
      output = "output.tsv",
      name_source = 123
    ),
    "must be a single character string"
  )

  # Test vector name_target
  expect_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
      output = "output.tsv",
      name_source = "ID1",
      name_target = c("ID2", "ID3")
    ),
    "must be a single character string"
  )

  unlink(temp_ms1)
  unlink(temp_spectral)
})

test_that("prepare_features_edges checks file existence", {
  # Test non-existent ms1 file
  expect_error(
    prepare_features_edges(
      input = list(
        "ms1" = "missing_ms1.tsv",
        "spectral" = "missing_spectral.tsv"
      ),
      output = "output.tsv"
    ),
    "not found"
  )

  # Test one existing, one missing
  temp_ms1 <- tempfile(fileext = ".tsv")
  writeLines("ID1\tID2\n1\t2", temp_ms1)

  expect_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = "missing.tsv"),
      output = "output.tsv"
    ),
    "not found"
  )

  unlink(temp_ms1)
})

test_that("prepare_features_edges combines ms1 and spectral edges", {
  copy_backbone(cache_dir = ".")

  # Create MS1 edges file
  temp_ms1 <- tempfile(fileext = ".tsv")
  ms1_data <- data.frame(
    ID1 = c("FT001", "FT002"),
    ID2 = c("FT003", "FT004"),
    EdgeType = c("MS1_annotation", "MS1_annotation"),
    stringsAsFactors = FALSE
  )
  write.table(ms1_data, temp_ms1, sep = "\t", row.names = FALSE, quote = FALSE)

  # Create spectral edges file
  temp_spectral <- tempfile(fileext = ".tsv")
  spectral_data <- data.frame(
    ID1 = c("FT001", "FT005"),
    ID2 = c("FT002", "FT006"),
    feature_spectrum_entropy = c("0.8", "0.9"),
    feature_spectrum_peaks = c("10", "15"),
    Cosine = c("0.95", "0.88"),
    stringsAsFactors = FALSE
  )
  write.table(
    spectral_data,
    temp_spectral,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_features_edges(
    input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
    output = temp_output,
    name_source = "ID1",
    name_target = "ID2"
  )

  expect_equal(result, temp_output)
  expect_true(file.exists(temp_output))

  # Verify combined output
  output_data <- tidytable::fread(temp_output)
  expect_gt(nrow(output_data), 0)
  expect_true("feature_source" %in% colnames(output_data))
  expect_true("feature_target" %in% colnames(output_data))

  unlink(temp_ms1)
  unlink(temp_spectral)
  unlink(temp_output)
  unlink("data", recursive = TRUE)
})

test_that("prepare_features_edges extracts entropy information", {
  copy_backbone(cache_dir = ".")

  # Create MS1 edges file (minimal)
  temp_ms1 <- tempfile(fileext = ".tsv")
  ms1_data <- data.frame(
    ID1 = c("FT001"),
    ID2 = c("FT002"),
    stringsAsFactors = FALSE
  )
  write.table(ms1_data, temp_ms1, sep = "\t", row.names = FALSE, quote = FALSE)

  # Create spectral edges with entropy info
  temp_spectral <- tempfile(fileext = ".tsv")
  spectral_data <- data.frame(
    ID1 = c("FT001", "FT003"),
    ID2 = c("FT002", "FT004"),
    feature_spectrum_entropy = c("0.85", "0.92"),
    feature_spectrum_peaks = c("12", "18"),
    Cosine = c("0.95", "0.88"),
    stringsAsFactors = FALSE
  )
  write.table(
    spectral_data,
    temp_spectral,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_features_edges(
    input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
    output = temp_output,
    name_source = "ID1",
    name_target = "ID2"
  )

  # Verify entropy information is preserved
  output_data <- tidytable::fread(temp_output)
  expect_true("feature_spectrum_entropy" %in% colnames(output_data))
  expect_true("feature_spectrum_peaks" %in% colnames(output_data))

  unlink(temp_ms1)
  unlink(temp_spectral)
  unlink(temp_output)
  unlink("data", recursive = TRUE)
})

test_that("prepare_features_edges standardizes column names", {
  copy_backbone(cache_dir = ".")

  temp_ms1 <- tempfile(fileext = ".tsv")
  temp_spectral <- tempfile(fileext = ".tsv")

  ms1_data <- data.frame(
    SourceID = c("FT001"),
    TargetID = c("FT002"),
    stringsAsFactors = FALSE
  )
  write.table(ms1_data, temp_ms1, sep = "\t", row.names = FALSE, quote = FALSE)

  spectral_data <- data.frame(
    SourceID = c("FT001"),
    TargetID = c("FT003"),
    feature_spectrum_entropy = c("0.9"),
    feature_spectrum_peaks = c("15"),
    stringsAsFactors = FALSE
  )
  write.table(
    spectral_data,
    temp_spectral,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_features_edges(
    input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
    output = temp_output,
    name_source = "SourceID",
    name_target = "TargetID"
  )

  # Verify standardized names
  output_data <- tidytable::fread(temp_output)
  expect_true("feature_source" %in% colnames(output_data))
  expect_true("feature_target" %in% colnames(output_data))
  expect_false("SourceID" %in% colnames(output_data))
  expect_false("TargetID" %in% colnames(output_data))

  unlink(temp_ms1)
  unlink(temp_spectral)
  unlink(temp_output)
  unlink("data", recursive = TRUE)
})

test_that("prepare_features_edges handles empty input files", {
  copy_backbone(cache_dir = ".")

  # Create empty MS1 edges
  temp_ms1 <- tempfile(fileext = ".tsv")
  ms1_empty <- data.frame(
    ID1 = character(0),
    ID2 = character(0)
  )
  write.table(ms1_empty, temp_ms1, sep = "\t", row.names = FALSE, quote = FALSE)

  # Create empty spectral edges
  temp_spectral <- tempfile(fileext = ".tsv")
  spectral_empty <- data.frame(
    ID1 = character(0),
    ID2 = character(0),
    feature_spectrum_entropy = character(0),
    feature_spectrum_peaks = character(0)
  )
  write.table(
    spectral_empty,
    temp_spectral,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_features_edges(
    input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
    output = temp_output,
    name_source = "ID1",
    name_target = "ID2"
  )

  expect_equal(result, temp_output)
  expect_true(file.exists(temp_output))

  unlink(temp_ms1)
  unlink(temp_spectral)
  unlink(temp_output)
  unlink("data", recursive = TRUE)
})

test_that("prepare_features_edges fills missing target with source", {
  copy_backbone(cache_dir = ".")

  temp_ms1 <- tempfile(fileext = ".tsv")
  temp_spectral <- tempfile(fileext = ".tsv")

  # MS1 file with some missing targets
  writeLines("ID1\tID2\nFT001\t\nFT003\tFT004", temp_ms1)

  spectral_data <- data.frame(
    ID1 = c("FT001"),
    ID2 = c("FT002"),
    feature_spectrum_entropy = c("0.9"),
    feature_spectrum_peaks = c("15"),
    stringsAsFactors = FALSE
  )
  write.table(
    spectral_data,
    temp_spectral,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  temp_output <- tempfile(fileext = ".tsv")

  result <- prepare_features_edges(
    input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
    output = temp_output,
    name_source = "ID1",
    name_target = "ID2"
  )

  # Verify missing targets were filled
  output_data <- tidytable::fread(temp_output, na.strings = c("", "NA"))
  expect_true(all(!is.na(output_data$feature_target)))

  unlink(temp_ms1)
  unlink(temp_spectral)
  unlink(temp_output)
  unlink("data", recursive = TRUE)
})
