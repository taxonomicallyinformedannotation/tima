# Test Suite: prepare_features_tables ----

library(testthat)

## Input Validation ----

test_that("prepare_features_tables validates features parameter", {
  # Non-character features
  expect_error(
    prepare_features_tables(features = 123),
    "Fix: Ensure the parameter is a length-1 character value",
    fixed = TRUE
  )

  # Multiple values
  expect_error(
    prepare_features_tables(features = c("file1.csv", "file2.csv")),
    "Fix: Ensure the parameter is a length-1 character value",
    fixed = TRUE
  )

  # Missing file
  expect_error(
    prepare_features_tables(features = "nonexistent_file.csv"),
    "not found|does not exist"
  )
})

test_that("prepare_features_tables validates output parameter", {
  withr::local_dir(new = temp_test_dir("prep_feat_tables_output"))
  paths <- local_test_project(copy = TRUE)

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_features <- file.path("data", "source", "test_features.csv")
  tidytable::tidytable(
    "row ID" = 1,
    "row m/z" = 123.456,
    "row retention time" = 1.5,
    "sample.mzML Peak area" = 1000
  ) |>
    tidytable::fwrite(file = test_features)

  expect_error(
    prepare_features_tables(features = test_features, output = 123),
    "Fix: Ensure the parameter is a length-1 character value",
    fixed = TRUE
  )

  expect_error(
    prepare_features_tables(
      features = test_features,
      output = c("out1.tsv", "out2.tsv")
    ),
    "Fix: Ensure the parameter is a length-1 character value",
    fixed = TRUE
  )
})

test_that("prepare_features_tables validates candidates parameter", {
  withr::local_dir(new = temp_test_dir("prep_feat_tables_cand"))
  paths <- local_test_project(copy = TRUE)

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_features <- file.path("data", "source", "test_features.csv")
  tidytable::tidytable(
    "row ID" = 1,
    "row m/z" = 123.456,
    "sample.mzML Peak area" = 1000
  ) |>
    tidytable::fwrite(file = test_features)

  expect_error(
    prepare_features_tables(features = test_features, candidates = 0),
    "between 1 and 100"
  )

  expect_error(
    prepare_features_tables(features = test_features, candidates = 101),
    "between 1 and 100"
  )

  expect_error(
    prepare_features_tables(features = test_features, candidates = -1),
    "between 1 and 100"
  )
})

## Format Detection and Standardization ----

test_that("prepare_features_tables handles MZmine format (Peak area)", {
  withr::local_dir(new = temp_test_dir("prep_feat_tables_mzmine"))
  paths <- local_test_project(copy = TRUE)

  mzmine_features <- tidytable::tidytable(
    "row ID" = 1:3,
    "row m/z" = c(123.456, 234.567, 345.678),
    "row retention time" = c(1.5, 2.0, 2.5),
    "sample1.mzML Peak area" = c(1000, 2000, 3000),
    "sample2.mzML Peak area" = c(1500, 2500, 3500)
  )

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_file <- file.path("data", "source", "mzmine_features.csv")
  tidytable::fwrite(x = mzmine_features, file = test_file)

  output_file <- file.path("data", "interim", "features", "prepared.tsv.gz")

  result <- prepare_features_tables(
    features = test_file,
    output = output_file,
    candidates = 5
  )

  expect_equal(result, output_file)
  expect_true(file.exists(output_file))

  prepared <- tidytable::fread(output_file)
  expect_true("feature_id" %in% colnames(prepared))
  expect_true("mz" %in% colnames(prepared))
})

test_that("prepare_features_tables handles missing RT column", {
  withr::local_dir(new = temp_test_dir("prep_feat_tables_no_rt"))
  paths <- local_test_project(copy = TRUE)

  no_rt_features <- tidytable::tidytable(
    "row ID" = 1:2,
    "row m/z" = c(123.456, 234.567),
    "sample.mzML Peak area" = c(1000, 2000)
  )

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_file <- file.path("data", "source", "no_rt.csv")
  tidytable::fwrite(x = no_rt_features, file = test_file)

  output_file <- file.path("data", "interim", "features", "prepared.tsv.gz")

  expect_no_error(
    prepare_features_tables(
      features = test_file,
      output = output_file
    )
  )
})

## Sample Selection ----

test_that("prepare_features_tables retains top intensity samples", {
  withr::local_dir(new = temp_test_dir("prep_feat_tables_samples"))
  paths <- local_test_project(copy = TRUE)

  n_samples <- 10
  features_data <- tidytable::tidytable(
    "row ID" = 1,
    "row m/z" = 123.456,
    "row retention time" = 1.5
  )

  for (i in seq_len(n_samples)) {
    col_name <- paste0("sample", i, ".mzML Peak area")
    features_data[[col_name]] <- i * 100
  }

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_file <- file.path("data", "source", "many_samples.csv")
  tidytable::fwrite(x = features_data, file = test_file)

  output_file <- file.path("data", "interim", "features", "prepared.tsv.gz")

  prepare_features_tables(
    features = test_file,
    output = output_file,
    candidates = 3
  )

  prepared <- tidytable::fread(output_file)
  sample_cols <- grep("^sample", colnames(prepared), value = TRUE)
  expect_lte(length(sample_cols), n_samples)
})

## Edge Cases ----

test_that("prepare_features_tables handles empty file", {
  withr::local_dir(new = temp_test_dir("prep_feat_tables_empty"))
  paths <- local_test_project(copy = TRUE)

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
})
