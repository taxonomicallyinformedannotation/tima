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
    "between 1 and 100",
    class = "tima_validation_error"
  )

  expect_error(
    prepare_features_tables(features = test_features, candidates = 101),
    "between 1 and 100",
    class = "tima_validation_error"
  )

  expect_error(
    prepare_features_tables(features = test_features, candidates = -1),
    "between 1 and 100",
    class = "tima_validation_error"
  )
})

## Format Detection and Standardization ----

test_that("prepare_features_tables handles mzmine format (Peak area)", {
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

test_that("prepare_features_tables resolves mzTab-style core column names", {
  withr::local_dir(new = temp_test_dir("prep_feat_tables_mztab"))
  paths <- local_test_project(copy = TRUE)

  mztab_features <- tidytable::tidytable(
    feature_id = c("101", "102"),
    mz = c(123.456, 234.567),
    rt = c(1.5, 2.0),
    adduct = c("[M+H]+", "[M+Na]+"),
    sampleA = c(1000, 2000),
    sampleB = c(1500, 1000)
  )

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_file <- file.path("data", "source", "mztab_features.tsv")
  tidytable::fwrite(x = mztab_features, file = test_file)

  output_file <- file.path("data", "interim", "features", "prepared.tsv.gz")

  expect_no_error(
    prepare_features_tables(
      features = test_file,
      output = output_file,
      candidates = 1
    )
  )

  prepared <- tidytable::fread(output_file)
  expect_true(all(
    c("feature_id", "mz", "rt", "adduct", "sample") %in% colnames(prepared)
  ))
  expect_equal(nrow(prepared), 2)
})

## Sample Selection ----

test_that("prepare_features_tables retains top intensity samples", {
  withr::local_dir(new = temp_test_dir("prep_feat_tables_samples"))
  paths <- local_test_project(copy = TRUE)

  n_samples <- 10
  sample_cols <- as.list(seq_len(n_samples) * 100)
  names(sample_cols) <- paste0("sample", seq_len(n_samples), ".mzML Peak area")
  features_data <- tidytable::tidytable(
    "row ID" = 1,
    "row m/z" = 123.456,
    "row retention time" = 1.5
  ) |>
    tidytable::mutate(!!!sample_cols)

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

test_that("prepare_features_tables skips rows without usable intensities", {
  withr::local_dir(new = temp_test_dir("prep_feat_tables_invalid_vals"))
  paths <- local_test_project(copy = TRUE)

  features_data <- tidytable::tidytable(
    "row ID" = 1:2,
    "row m/z" = c(123.456, 234.567),
    "row retention time" = c(1.5, 2.0),
    "sample1.mzML Peak area" = c(0, NA),
    "sample2.mzML Peak area" = c(NA, 0)
  )

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_file <- file.path("data", "source", "invalid_values.csv")
  tidytable::fwrite(x = features_data, file = test_file)

  output_file <- file.path("data", "interim", "features", "prepared.tsv.gz")
  expect_no_error(
    prepare_features_tables(
      features = test_file,
      output = output_file,
      candidates = 3
    )
  )

  prepared <- tidytable::fread(output_file)
  expect_equal(nrow(prepared), 0L)
})

## Edge Cases ----

test_that("prepare_features_tables handles both Peak area and Peak height", {
  withr::local_dir(new = temp_test_dir("prep_feat_tables_area_height"))
  paths <- local_test_project(copy = TRUE)

  both_features <- tidytable::tidytable(
    "row ID" = 1:2,
    "row m/z" = c(123.456, 234.567),
    "row retention time" = c(1.5, 2.0),
    "sample1.mzML Peak area" = c(1000, 2000),
    "sample1.mzML Peak height" = c(500, 900),
    "sample2.mzML Peak area" = c(1500, 2500),
    "sample2.mzML Peak height" = c(700, 1100)
  )

  dir.create("data/source", recursive = TRUE, showWarnings = FALSE)
  test_file <- file.path("data", "source", "area_height.csv")
  tidytable::fwrite(x = both_features, file = test_file)

  output_file <- file.path("data", "interim", "features", "prepared.tsv.gz")

  result <- prepare_features_tables(
    features = test_file,
    output = output_file,
    candidates = 5
  )

  expect_true(file.exists(result))
  prepared <- tidytable::fread(output_file)
  expect_true("feature_id" %in% colnames(prepared))
  # Should not have duplicate sample columns
  expect_equal(length(unique(colnames(prepared))), length(colnames(prepared)))
})

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
    "empty",
    class = "tima_validation_error"
  )
})

test_that(".filter_top_intensity_samples preserves tie-breaking and handles >16 candidates", {
  tbl <- data.frame(
    feature_id = c("feat_b", "feat_a"),
    rt = c(1.2, 0.8),
    mz = c(200.1, 150.1),
    s_z = c(100, 50),
    s_a = c(100, 200),
    s_m = c(0, 200),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  res_small <- .filter_top_intensity_samples(
    tbl = tbl,
    candidates = 2L,
    name_features = "feature_id",
    name_rt = "rt",
    name_mz = "mz",
    name_adduct = "adduct"
  )

  expect_equal(res_small$feature_id, c("feat_a", "feat_a", "feat_b", "feat_b"))
  # Ties broken by first column order (s_a before s_m for feat_a; s_z before s_a for feat_b)
  expect_equal(res_small$sample, c("s_a", "s_m", "s_z", "s_a"))
  expect_equal(res_small$intensity, c(200, 200, 100, 100))
  # intensity_all sorted alphabetically by sample name: s_a, s_m, s_z
  expect_equal(res_small$intensity_all[[1]], c(200, 200, 50))
  expect_equal(res_small$intensity_all[[3]], c(100, 0, 100))

  # Test > 16 candidates branch (radix sort path)
  n_samp <- 20L
  samp_cols <- sprintf("samp_%02d", seq_len(n_samp))
  wide_df <- data.frame(
    feature_id = c("10", "2"),
    rt = c(1.0, 2.0),
    mz = c(100.0, 200.0),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(n_samp)) {
    wide_df[[samp_cols[i]]] <- c(i * 10, (n_samp - i + 1L) * 5)
  }

  res_large <- .filter_top_intensity_samples(
    tbl = wide_df,
    candidates = 18L,
    name_features = "feature_id",
    name_rt = "rt",
    name_mz = "mz",
    name_adduct = "adduct"
  )
  expect_equal(nrow(res_large), 36L)
  # Numeric feature ordering puts "2" before "10"
  expect_equal(unique(res_large$feature_id), c("2", "10"))
  expect_equal(res_large$sample[1], "samp_01")
  expect_equal(res_large$sample[19], "samp_20")
})

test_that("internal validation helpers enforce exact parameter contracts and boundaries", {
  # .validate_column_name_param rejects non-char, length != 1, NA, and empty string
  for (bad_val in list(123, c("a", "b"), NA_character_, "")) {
    expect_error(
      .validate_column_name_param(bad_val, "name_mz"),
      regexp = "`name_mz` must be a single non-empty character string",
      class = "tima_validation_error"
    )
  }
  expect_no_error(.validate_column_name_param("row m/z", "name_mz"))

  # .validate_prepare_features_inputs candidates validation
  tmp_f <- withr::local_tempfile(fileext = ".csv")
  writeLines("id,mz\n1,100.0", tmp_f)

  for (bad_cand in list("five", c(1, 2), NA_real_)) {
    expect_error(
      .validate_prepare_features_inputs(
        features = tmp_f,
        output = "out.tsv",
        candidates = bad_cand,
        name_adduct = "adduct",
        name_features = "id",
        name_rt = "rt",
        name_mz = "mz"
      ),
      regexp = "candidates must be a single numeric value or NULL",
      class = "tima_validation_error"
    )
  }

  # Boundary 1 and 100 succeed; NULL succeeds
  for (ok_cand in list(NULL, 1L, 100L)) {
    expect_no_error(
      .validate_prepare_features_inputs(
        features = tmp_f,
        output = "out.tsv",
        candidates = ok_cand,
        name_adduct = "adduct",
        name_features = "id",
        name_rt = "rt",
        name_mz = "mz"
      )
    )
  }
})

test_that(".select_intensity_columns, .standardize_column_names, and .resolve_feature_column_names handle edge cases", {
  # 1. Numeric-like column detection boundary (4/5 = 0.8 >= 0.8 kept, 3/5 = 0.6 < 0.8 dropped, all-NA dropped)
  tbl_numlike <- tidytable::tidytable(
    id = as.character(1:5),
    mz = rep("100.0", 5),
    col_80 = c("10", "20", "30", "40", "bad"),
    col_60 = c("10", "20", "30", "bad1", "bad2"),
    col_na = c(NA_character_, "", NA_character_, "", NA_character_)
  )
  sel_num <- .select_intensity_columns(tbl_numlike, "id", "rt", "mz", "adduct")
  expect_identical(colnames(sel_num), c("id", "mz", "col_80"))

  # 2. No standard and no numeric-like columns -> keeps only core_present
  tbl_nonum <- tidytable::tidytable(
    id = c("1", "2"),
    mz = c("100", "200"),
    notes = c("alpha", "beta")
  )
  sel_nonum <- .select_intensity_columns(tbl_nonum, "id", "rt", "mz", "adduct")
  expect_identical(colnames(sel_nonum), c("id", "mz"))

  # 3. .standardize_column_names deduplicates colliding clean names keeping first
  tbl_dup <- tidytable::tidytable(
    id = 1:2,
    `datafile:S1:area` = c(100, 200),
    `quant_S1` = c(10, 20),
    `quant_S2` = c(30, 40)
  )
  std_dup <- .standardize_column_names(tbl_dup)
  expect_identical(colnames(std_dup), c("id", "S1", "S2"))
  expect_identical(std_dup$S1, c(100, 200))

  # 4. .resolve_feature_column_names aborts when required column and aliases are missing
  tbl_missing_req <- tidytable::tidytable(foo = 1:2, bar = 3:4)
  expect_error(
    .resolve_feature_column_names(
      tbl_missing_req,
      name_features = "row ID",
      name_rt = "row retention time",
      name_mz = "row m/z",
      name_adduct = "adduct"
    ),
    regexp = "Required column 'row ID' is missing",
    class = "tima_validation_error"
  )
})

test_that(".filter_top_intensity_samples handles zero intensity columns, >16 all-zero fast path, and duplicate feature_id fallback path", {
  # 1. Zero intensity columns (only core columns present)
  tbl_core_only <- tidytable::tidytable(
    feature_id = c("1", "2"),
    rt = c(1.0, 2.0),
    mz = c(100.0, 200.0),
    adduct = c("[M+H]+", "[M+Na]+")
  )
  res_no_int <- .filter_top_intensity_samples(
    tbl_core_only,
    candidates = 2L,
    name_features = "feature_id",
    name_rt = "rt",
    name_mz = "mz",
    name_adduct = "adduct"
  )
  expect_identical(nrow(res_no_int), 0L)
  expect_identical(
    names(res_no_int),
    c("feature_id", "sample", "rt", "mz", "adduct")
  )

  # 2. >16 candidates with all-zero/NA intensities in fast matrix path
  n_samp <- 18L
  zero_wide <- data.frame(
    feature_id = c("1", "2"),
    rt = c(1.0, 2.0),
    mz = c(100.0, 200.0),
    adduct = c("[M+H]+", "[M+Na]+"),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(n_samp)) {
    zero_wide[[sprintf("s_%02d", i)]] <- c(0, NA_real_)
  }
  res_zero_18 <- .filter_top_intensity_samples(
    zero_wide,
    candidates = 18L,
    name_features = "feature_id",
    name_rt = "rt",
    name_mz = "mz",
    name_adduct = "adduct"
  )
  expect_identical(nrow(res_zero_18), 0L)
  expect_identical(
    names(res_zero_18),
    c("feature_id", "sample", "rt", "mz", "adduct")
  )

  # 3. Fallback long-table path when feature_ids contain duplicates (numeric & non-numeric)
  tbl_dup_num <- data.frame(
    feature_id = c("10", "2", "2"),
    rt = c(1.0, 2.0, 2.1),
    mz = c(100.0, 200.0, 200.1),
    adduct = c("[M+H]+", "[M+Na]+", "[M+K]+"),
    s1 = c(50, 100, 10),
    s2 = c(80, 20, 300),
    stringsAsFactors = FALSE
  )
  res_dup_num <- .filter_top_intensity_samples(
    tbl_dup_num,
    candidates = 1L,
    name_features = "feature_id",
    name_rt = "rt",
    name_mz = "mz",
    name_adduct = "adduct"
  )
  expect_identical(res_dup_num$feature_id, c("2", "10"))
  expect_identical(res_dup_num$sample, c("s2", "s2"))
  expect_identical(res_dup_num$intensity, c(300, 80))

  tbl_dup_chr <- data.frame(
    feature_id = c("feat_b", "feat_a", "feat_a"),
    rt = c(1.0, 2.0, 2.1),
    mz = c(100.0, 200.0, 200.1),
    adduct = c("[M+H]+", "[M+Na]+", "[M+K]+"),
    s1 = c(50, 100, 10),
    s2 = c(80, 20, 300),
    stringsAsFactors = FALSE
  )
  res_dup_chr <- .filter_top_intensity_samples(
    tbl_dup_chr,
    candidates = 1L,
    name_features = "feature_id",
    name_rt = "rt",
    name_mz = "mz",
    name_adduct = "adduct"
  )
  expect_identical(res_dup_chr$feature_id, c("feat_a", "feat_b"))
  expect_identical(res_dup_chr$intensity, c(300, 80))

  # 4. Fallback long-table path with all-zero intensities
  tbl_dup_zero <- data.frame(
    feature_id = c("1", "1"),
    rt = c(1.0, 1.0),
    mz = c(100.0, 100.0),
    adduct = c("[M+H]+", "[M+H]+"),
    s1 = c(0, NA_real_),
    stringsAsFactors = FALSE
  )
  res_dup_zero <- .filter_top_intensity_samples(
    tbl_dup_zero,
    candidates = 1L,
    name_features = "feature_id",
    name_rt = "rt",
    name_mz = "mz",
    name_adduct = "adduct"
  )
  expect_identical(nrow(res_dup_zero), 0L)
  expect_identical(
    names(res_dup_zero),
    c("feature_id", "sample", "rt", "mz", "adduct")
  )
})
