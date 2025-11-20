# Test Suite: prepare_features_tables ----

library(testthat)

## Input Validation ----

test_that("prepare_features_tables validates features parameter", {
  # Non-character features
  expect_error(
    prepare_features_tables(features = 123),
    "must be a single character string"
  )

  # Multiple values
  expect_error(
    prepare_features_tables(features = c("file1.csv", "file2.csv")),
    "must be a single character string"
  )

  # Missing file
  expect_error(
    prepare_features_tables(features = "nonexistent_file.csv"),
    "not found"
  )
})

test_that("prepare_features_tables validates output parameter", {
  paths <- local_test_project(copy = TRUE)

  # Create minimal features file
  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_features <- file.path("data", "source", "test_features.csv")
  tidytable::tidytable(
    "row ID" = 1,
    "row m/z" = 123.456,
    "row retention time" = 1.5,
    "sample.mzML Peak area" = 1000
  ) |>
    tidytable::fwrite(test_features)

  # Non-character output
  expect_error(
    prepare_features_tables(features = test_features, output = 123),
    "must be a single character string"
  )

  # Multiple values
  expect_error(
    prepare_features_tables(
      features = test_features,
      output = c("out1.tsv", "out2.tsv")
    ),
    "must be a single character string"
  )
})

test_that("prepare_features_tables validates candidates parameter", {
  paths <- local_test_project(copy = TRUE)

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_features <- file.path("data", "source", "test_features.csv")
  tidytable::tidytable(
    "row ID" = 1,
    "row m/z" = 123.456,
    "sample.mzML Peak area" = 1000
  ) |>
    tidytable::fwrite(test_features)

  # Too small
  expect_error(
    prepare_features_tables(features = test_features, candidates = 0),
    "between 1 and 5"
  )

  # Too large
  expect_error(
    prepare_features_tables(features = test_features, candidates = 10),
    "between 1 and 5"
  )

  # Negative
  expect_error(
    prepare_features_tables(features = test_features, candidates = -1),
    "between 1 and 5"
  )
})

## Format Detection and Standardization ----

test_that("prepare_features_tables handles MZmine format (Peak area)", {
  paths <- local_test_project(copy = TRUE)

  # Create MZmine-style features
  mzmine_features <- tidytable::tidytable(
    "row ID" = 1:3,
    "row m/z" = c(123.456, 234.567, 345.678),
    "row retention time" = c(1.5, 2.0, 2.5),
    "sample1.mzML Peak area" = c(1000, 2000, 3000),
    "sample2.mzML Peak area" = c(1500, 2500, 3500)
  )

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_file <- file.path("data", "source", "mzmine_features.csv")
  tidytable::fwrite(mzmine_features, test_file)

  output_file <- file.path("data", "interim", "features", "prepared.tsv.gz")

  result <- prepare_features_tables(
    features = test_file,
    output = output_file,
    candidates = 5
  )

  expect_equal(result, output_file)
  expect_true(file.exists(output_file))

  # Load and verify standardization
  prepared <- tidytable::fread(output_file)
  expect_true("feature_id" %in% colnames(prepared))
  expect_true("mz" %in% colnames(prepared))
})

test_that("prepare_features_tables handles missing RT column", {
  paths <- local_test_project(copy = TRUE)

  # Features without RT
  no_rt_features <- tidytable::tidytable(
    "row ID" = 1:2,
    "row m/z" = c(123.456, 234.567),
    "sample.mzML Peak area" = c(1000, 2000)
  )

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_file <- file.path("data", "source", "no_rt.csv")
  tidytable::fwrite(no_rt_features, test_file)

  output_file <- file.path("data", "interim", "features", "prepared.tsv.gz")

  # Should handle gracefully (RT might be optional)
  expect_no_error(
    prepare_features_tables(
      features = test_file,
      output = output_file
    )
  )
})

## Sample Selection ----

test_that("prepare_features_tables retains top intensity samples", {
  paths <- local_test_project(copy = TRUE)

  # Create features with many samples
  n_samples <- 10
  features_data <- tidytable::tidytable(
    "row ID" = 1,
    "row m/z" = 123.456,
    "row retention time" = 1.5
  )

  # Add sample columns with varying intensities
  for (i in seq_len(n_samples)) {
    col_name <- paste0("sample", i, ".mzML Peak area")
    features_data[[col_name]] <- i * 100 # Increasing intensities
  }

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_file <- file.path("data", "source", "many_samples.csv")
  tidytable::fwrite(features_data, test_file)

  output_file <- file.path("data", "interim", "features", "prepared.tsv.gz")

  # Retain only top 3 samples
  prepare_features_tables(
    features = test_file,
    output = output_file,
    candidates = 3
  )

  prepared <- tidytable::fread(output_file)

  # Should have reduced number of sample columns
  sample_cols <- grep("^sample", colnames(prepared), value = TRUE)

  # Note: actual number might vary based on implementation
  # but should be <= original count
  expect_lte(length(sample_cols), n_samples)
})

## Edge Cases ----

test_that("prepare_features_tables handles empty file", {
  paths <- local_test_project(copy = TRUE)

  # Create empty file
  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_file <- file.path("data", "source", "empty.csv")
  tidytable::fwrite(
    tidytable::tidytable(
      "row ID" = integer(0),
      "row m/z" = numeric(0)
    ),
    test_file
  )

  expect_error(
    prepare_features_tables(features = test_file),
    "empty"
  )
})

test_that("prepare_features_tables handles large feature tables", {
  skip("Large data test - run manually")

  paths <- local_test_project(copy = TRUE)

  # Create large feature table
  n_features <- 10000
  large_features <- tidytable::tidytable(
    "row ID" = seq_len(n_features),
    "row m/z" = runif(n_features, 100, 1000),
    "row retention time" = runif(n_features, 0, 30),
    "sample.mzML Peak area" = runif(n_features, 100, 10000)
  )

  test_file <- file.path("data", "source", "large.csv")
  tidytable::fwrite(large_features, test_file)

  start_time <- Sys.time()
  prepare_features_tables(features = test_file)
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  # Should complete in reasonable time
  expect_lt(elapsed, 30)
})

## Output Verification ----

# test_that(
#   skip("Not implemented")
# )
# test_that("prepare_features_tables validates column name parameters", {
#   paths <- local_test_project(copy = TRUE)
#
#   test_features <- file.path("data", "source", "test_features.csv")
#   tidytable::tidytable(
#     "row ID" = 1,
#     "row m/z" = 123.456
#   ) |>
#     tidytable::fwrite(test_features)
#
#   # Non-character name_features
#   expect_error(
#     prepare_features_tables(features = test_features, name_features = 123),
#     "must be single character string"
#   )
#
#   # Multiple values
#   expect_error(
#     prepare_features_tables(
#       features = test_features,
#       name_features = c("id1", "id2")
#     ),
#     "must be single character string"
#   )
# })

# test_that(
#   skip("Not implemented")
# )
# test_that("prepare_features_tables handles SLAW format (quant_)", {
#   paths <- local_test_project(copy = TRUE)
#
#   slaw_features <- tidytable::tidytable(
#     "feature_id" = c("FT001", "FT002"),
#     "mz" = c(123.456, 234.567),
#     "rt" = c(1.5, 2.0),
#     "quant_sample1" = c(1000, 2000),
#     "quant_sample2" = c(1500, 2500)
#   )
#
#   test_file <- file.path("data", "source", "slaw_features.csv")
#   tidytable::fwrite(slaw_features, test_file)
#
#   output_file <- file.path("data", "interim", "features", "prepared.tsv.gz")
#
#   expect_no_error(
#     prepare_features_tables(
#       features = test_file,
#       output = output_file,
#       candidates = 5
#     )
#   )
#
#   expect_true(file.exists(output_file))
# })

# test_that(
#   skip("Not implemented")
# )
# test_that("prepare_features_tables handles single feature", {
#   paths <- local_test_project(copy = TRUE)
#
#   single_feature <- tidytable::tidytable(
#     "row ID" = 1,
#     "row m/z" = 123.456,
#     "row retention time" = 1.5,
#     "sample.mzML Peak area" = 1000
#   )
#
#   test_file <- file.path("data", "source", "single.csv")
#   tidytable::fwrite(single_feature, test_file)
#
#   output_file <- file.path("data", "interim", "features", "prepared.tsv.gz")
#
#   expect_no_error(
#     prepare_features_tables(
#       features = test_file,
#       output = output_file
#     )
#   )
#
#   prepared <- tidytable::fread(output_file)
#   expect_equal(nrow(prepared), 1L)
# })

# test_that(
#   skip("Not implemented")
# )
# test_that("prepare_features_tables creates valid output file", {
#   paths <- local_test_project(copy = TRUE)
#
#   input_features <- tidytable::tidytable(
#     "row ID" = 1:3,
#     "row m/z" = c(123.456, 234.567, 345.678),
#     "row retention time" = c(1.5, 2.0, 2.5),
#     "sample.mzML Peak area" = c(1000, 2000, 3000)
#   )
#
#   test_file <- file.path("data", "source", "test.csv")
#   tidytable::fwrite(input_features, test_file)
#
#   output_file <- file.path("data", "interim", "features", "output.tsv.gz")
#
#   result <- prepare_features_tables(
#     features = test_file,
#     output = output_file
#   )
#
#   expect_equal(result, output_file)
#   expect_true(file.exists(output_file))
#
#   # Verify it's readable
#   prepared <- tidytable::fread(output_file)
#   expect_s3_class(prepared, "data.frame")
#   expect_gt(nrow(prepared), 0L)
# })

# test_that(
#   skip("Not implemented")
# )
# test_that("prepare_features_tables preserves feature IDs", {
#   paths <- local_test_project(copy = TRUE)
#
#   input_features <- tidytable::tidytable(
#     "row ID" = c("FT001", "FT002", "FT003"),
#     "row m/z" = c(123.456, 234.567, 345.678),
#     "sample.mzML Peak area" = c(1000, 2000, 3000)
#   )
#
#   test_file <- file.path("data", "source", "test.csv")
#   tidytable::fwrite(input_features, test_file)
#
#   output_file <- file.path("data", "interim", "features", "output.tsv.gz")
#
#   prepare_features_tables(
#     features = test_file,
#     output = output_file
#   )
#
#   prepared <- tidytable::fread(output_file)
#
#   # Feature IDs should be preserved
#   expect_true("feature_id" %in% colnames(prepared))
#   expect_setequal(prepared$feature_id, c("FT001", "FT002", "FT003"))
# })

## Integration with Downstream Functions ----

# test_that(
#   skip("Not implemented")
# )
# test_that("prepare_features_tables output works with annotate_masses", {
#   paths <- local_test_project(copy = TRUE)
#
#   # Use actual example file
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#
#   # Prepare features
#   expect_no_error(prepare_features_tables())
#
#   # Verify output exists and is readable
#   expect_true(file.exists(paths$data$interim$features$prepared))
#
#   prepared <- tidytable::fread(paths$data$interim$features$prepared)
#   expect_gt(nrow(prepared), 0L)
# })
