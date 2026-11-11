# Test Suite: prepare_features_edges ----

library(testthat)

test_that("prepare_features_edges validates input structure", {
  # Test input without required elements
  expect_error(
    prepare_features_edges(
      input = list("only_one" = "file.tsv"),
      output = "output.tsv"
    ),
    "must contain 'ms1' and 'spectral'",
    class = "tima_validation_error"
  )

  # Test input with NULL names
  expect_error(
    prepare_features_edges(
      input = list("file1.tsv", "file2.tsv"),
      output = "output.tsv"
    ),
    "must contain 'ms1' and 'spectral'",
    class = "tima_validation_error"
  )

  # Test input missing 'spectral'
  expect_error(
    prepare_features_edges(
      input = list("ms1" = "file1.tsv", "other" = "file2.tsv"),
      output = "output.tsv"
    ),
    "must contain 'ms1' and 'spectral'",
    class = "tima_validation_error"
  )
})

test_that("prepare_features_edges validates output parameter", {
  temp_ms1 <- withr::local_tempfile(fileext = ".tsv")
  temp_spectral <- withr::local_tempfile(fileext = ".tsv")

  writeLines("ID1\tID2\n1\t2", temp_ms1)
  writeLines("ID1\tID2\n1\t2", temp_spectral)

  # Test non-character output
  expect_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
      output = 123
    ),
    "must be a single character string",
    class = "tima_validation_error"
  )

  # Test vector output
  expect_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
      output = c("out1.tsv", "out2.tsv")
    ),
    "must be a single character string",
    class = "tima_validation_error"
  )

  # unlink(temp_ms1)
  # unlink(temp_spectral)
})

test_that("prepare_features_edges validates column name parameters", {
  temp_ms1 <- withr::local_tempfile(fileext = ".tsv")
  temp_spectral <- withr::local_tempfile(fileext = ".tsv")

  writeLines("ID1\tID2\n1\t2", temp_ms1)
  writeLines("ID1\tID2\n1\t2", temp_spectral)

  # Test non-character name_source
  expect_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
      output = "output.tsv",
      name_source = 123
    ),
    "Fix: Ensure the parameter is a length-1 character value",
    fixed = TRUE
  )

  # Test vector name_target
  expect_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
      output = "output.tsv",
      name_source = "ID1",
      name_target = c("ID2", "ID3")
    ),
    "Fix: Ensure the parameter is a length-1 character value",
    fixed = TRUE
  )

  # unlink(temp_ms1)
  # unlink(temp_spectral)
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
    "not found",
    class = "tima_validation_error"
  )

  # Test one existing, one missing
  temp_ms1 <- withr::local_tempfile(fileext = ".tsv")
  writeLines("ID1\tID2\n1\t2", temp_ms1)

  expect_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = "missing.tsv"),
      output = "output.tsv"
    ),
    "not found",
    class = "tima_validation_error"
  )

  # unlink(temp_ms1)
})

test_that("prepare_features_edges combines ms1 and spectral edges", {
  local_test_project(copy = TRUE)

  # Create MS1 edges file
  temp_ms1 <- withr::local_tempfile(fileext = ".tsv")
  ms1_data <- data.frame(
    ID1 = c("FT001", "FT002"),
    ID2 = c("FT003", "FT004"),
    EdgeType = c("MS1_annotation", "MS1_annotation"),
    stringsAsFactors = FALSE
  )
  write.table(ms1_data, temp_ms1, sep = "\t", row.names = FALSE, quote = FALSE)

  # Create spectral edges file
  temp_spectral <- withr::local_tempfile(fileext = ".tsv")
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

  temp_output <- withr::local_tempfile(fileext = ".tsv")

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

  # unlink(temp_ms1)
  # unlink(temp_spectral)
  # unlink(temp_output)
})

test_that("prepare_features_edges extracts entropy information", {
  local_test_project(copy = TRUE)

  # Create MS1 edges file (minimal)
  temp_ms1 <- withr::local_tempfile(fileext = ".tsv")
  ms1_data <- data.frame(
    ID1 = c("FT001"),
    ID2 = c("FT002"),
    stringsAsFactors = FALSE
  )
  write.table(ms1_data, temp_ms1, sep = "\t", row.names = FALSE, quote = FALSE)

  # Create spectral edges with entropy info
  temp_spectral <- withr::local_tempfile(fileext = ".tsv")
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

  temp_output <- withr::local_tempfile(fileext = ".tsv")

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

  # unlink(temp_ms1)
  # unlink(temp_spectral)
  # unlink(temp_output)
})

test_that("prepare_features_edges standardizes column names", {
  local_test_project(copy = TRUE)

  temp_ms1 <- withr::local_tempfile(fileext = ".tsv")
  temp_spectral <- withr::local_tempfile(fileext = ".tsv")

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

  temp_output <- withr::local_tempfile(fileext = ".tsv")

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

  # unlink(temp_ms1)
  # unlink(temp_spectral)
  # unlink(temp_output)
})

test_that("prepare_features_edges standardizes legacy similarity columns", {
  local_test_project(copy = TRUE)

  temp_ms1 <- withr::local_tempfile(fileext = ".tsv")
  temp_spectral <- withr::local_tempfile(fileext = ".tsv")

  ms1_data <- data.frame(
    ID1 = c("FT001"),
    ID2 = c("FT002"),
    stringsAsFactors = FALSE
  )
  write.table(ms1_data, temp_ms1, sep = "\t", row.names = FALSE, quote = FALSE)

  spectral_data <- data.frame(
    ID1 = c("FT001"),
    ID2 = c("FT002"),
    score = c("0.91"),
    matched_peaks = c("2"),
    stringsAsFactors = FALSE
  )
  write.table(
    spectral_data,
    temp_spectral,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  temp_output <- withr::local_tempfile(fileext = ".tsv")

  result <- prepare_features_edges(
    input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
    output = temp_output,
    name_source = "ID1",
    name_target = "ID2"
  )

  output_data <- tidytable::fread(temp_output)
  expect_false("score" %in% colnames(output_data))
  expect_false("matched_peaks" %in% colnames(output_data))
  expect_true("candidate_score_similarity" %in% colnames(output_data))
  expect_true(
    "candidate_count_similarity_peaks_matched" %in% colnames(output_data)
  )
  expect_equal(as.numeric(output_data$candidate_score_similarity[[1]]), 0.91)
  expect_equal(
    as.integer(output_data$candidate_count_similarity_peaks_matched[[1]]),
    2L
  )

  expect_equal(result, temp_output)
})

test_that("prepare_features_edges handles empty input files", {
  local_test_project(copy = TRUE)

  # Create empty MS1 edges
  temp_ms1 <- withr::local_tempfile(fileext = ".tsv")
  ms1_empty <- data.frame(
    ID1 = character(0),
    ID2 = character(0)
  )
  write.table(ms1_empty, temp_ms1, sep = "\t", row.names = FALSE, quote = FALSE)

  # Create empty spectral edges
  temp_spectral <- withr::local_tempfile(fileext = ".tsv")
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

  temp_output <- withr::local_tempfile(fileext = ".tsv")

  result <- prepare_features_edges(
    input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
    output = temp_output,
    name_source = "ID1",
    name_target = "ID2"
  )

  expect_equal(result, temp_output)
  expect_true(file.exists(temp_output))

  # unlink(temp_ms1)
  # unlink(temp_spectral)
  # unlink(temp_output)
})

test_that("prepare_features_edges fills missing target with source", {
  local_test_project(copy = TRUE)

  temp_ms1 <- withr::local_tempfile(fileext = ".tsv")
  temp_spectral <- withr::local_tempfile(fileext = ".tsv")

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

  temp_output <- withr::local_tempfile(fileext = ".tsv")

  result <- prepare_features_edges(
    input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
    output = temp_output,
    name_source = "ID1",
    name_target = "ID2"
  )

  # Verify missing targets were filled
  output_data <- tidytable::fread(temp_output, na.strings = c("", "NA"))
  expect_true(!anyNA(output_data$feature_target))

  # unlink(temp_ms1)
  # unlink(temp_spectral)
  # unlink(temp_output)
})

test_that("prepare_features_edges merges shared (source, target) edges and retains unmatched entropy self-loops", {
  local_test_project(copy = TRUE)

  temp_ms1 <- withr::local_tempfile(fileext = ".tsv")
  temp_spectral <- withr::local_tempfile(fileext = ".tsv")
  temp_output <- withr::local_tempfile(fileext = ".tsv")

  ms1_data <- data.frame(
    ID1 = c("FT001", "FT002"),
    ID2 = c("FT002", "FT003"),
    EdgeType = c("adduct", "adduct"),
    stringsAsFactors = FALSE
  )
  write.table(ms1_data, temp_ms1, sep = "\t", row.names = FALSE, quote = FALSE)

  spectral_data <- data.frame(
    ID1 = c("FT001", "FT004"),
    ID2 = c("FT002", "FT005"),
    feature_spectrum_entropy = c("0.85", "0.92"),
    feature_spectrum_peaks = c("12", "18"),
    candidate_score_similarity = c("0.95", "0.88"),
    candidate_count_similarity_peaks_matched = c("6", "9"),
    stringsAsFactors = FALSE
  )
  write.table(
    spectral_data,
    temp_spectral,
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  prepare_features_edges(
    input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
    output = temp_output,
    name_source = "ID1",
    name_target = "ID2"
  )

  output_data <- tidytable::fread(temp_output, na.strings = c("", "NA"))
  shared_row <- output_data[
    output_data$feature_source == "FT001" &
      output_data$feature_target == "FT002",
  ]
  expect_equal(nrow(shared_row), 1L)
  expect_equal(shared_row$EdgeType[[1L]], "adduct")
  expect_equal(as.numeric(shared_row$candidate_score_similarity[[1L]]), 0.95)
  expect_true(any(
    output_data$feature_source == "FT004" &
      output_data$feature_target == "FT004"
  ))

  # Edge cases: read error, no entropy columns in spectral edges, and duplicate edges_ms2 rows triggering full_join fallback
  expect_error(
    testthat::with_mocked_bindings(
      safe_fread = function(...) stop("simulated edge read error"),
      prepare_features_edges(
        input = list("ms1" = temp_ms1, "spectral" = temp_spectral),
        output = temp_output,
        name_source = "ID1",
        name_target = "ID2"
      )
    ),
    regexp = "failed to read edge files",
    class = "tima_runtime_error"
  )

  # Spectral edges without ID1/entropy columns -> features_entropy is empty -> full_join fallback
  temp_spec_no_ent <- withr::local_tempfile(fileext = ".tsv")
  writeLines("OtherCol\tID2\nX1\tFT002", temp_spec_no_ent)
  out_no_ent <- withr::local_tempfile(fileext = ".tsv")
  expect_no_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = temp_spec_no_ent),
      output = out_no_ent,
      name_source = "ID1",
      name_target = "ID2"
    )
  )

  # Duplicate (ID1, ID2) with differing and identical entropy in spectral edges -> triggers full_join fallbacks
  temp_spec_dup <- withr::local_tempfile(fileext = ".tsv")
  writeLines(
    "ID1\tID2\tfeature_spectrum_entropy\tCosine\nFT001\tFT002\t0.5\t0.9\nFT001\tFT002\t0.6\t0.8\nFT001\tFT002\t0.6\t0.7",
    temp_spec_dup
  )
  out_dup <- withr::local_tempfile(fileext = ".tsv")
  expect_no_error(
    prepare_features_edges(
      input = list("ms1" = temp_ms1, "spectral" = temp_spec_dup),
      output = out_dup,
      name_source = "ID1",
      name_target = "ID2"
    )
  )
})
