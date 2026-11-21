# Test Suite: decorate_bio ----

library(testthat)

call_decorate_bio <- function(
  df,
  ...,
  include_domain = FALSE,
  include_subranks = FALSE,
  .score_all = NULL
) {
  scores <- if (!missing(.score_all)) {
    default_bio_scores(
      include_domain = include_domain,
      include_subranks = include_subranks,
      .all = .score_all
    )
  } else if (!include_domain && !include_subranks) {
    default_bio_scores(
      include_domain = FALSE,
      include_subranks = FALSE,
      score_biological_kingdom = 0.1,
      score_biological_phylum = 0.2,
      score_biological_class = 0.3,
      score_biological_order = 0.4,
      score_biological_family = 0.5,
      score_biological_tribe = 0.6,
      score_biological_genus = 0.7,
      score_biological_species = 0.8,
      score_biological_variety = 0.9
    )
  } else {
    default_bio_scores(
      include_domain = include_domain,
      include_subranks = include_subranks
    )
  }
  overrides <- list(...)
  if (length(overrides) > 0L) {
    scores[names(overrides)] <- overrides
  }
  do.call(decorate_bio, c(list(annot_table_wei_bio = df), scores))
}

test_that("decorate_bio handles empty input", {
  # Create empty annotation table
  empty_df <- tidytable::tidytable(
    feature_id = character(0),
    score_biological = numeric(0)
  )

  result <- call_decorate_bio(empty_df)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("decorate_bio returns input unchanged", {
  # Create minimal test data
  test_df <- tidytable::tidytable(
    feature_id = c("FT001", "FT002"),
    score_biological = c(0.8, 0.9)
  )

  result <- call_decorate_bio(test_df)

  # Should return input unchanged (decorator pattern)
  expect_identical(result, test_df)
})

test_that("decorate_bio warns on missing columns", {
  # Create incomplete data frame
  incomplete_df <- tidytable::tidytable(
    feature_id = c("FT001")
  )

  expect_warning(
    call_decorate_bio(incomplete_df),
    NA # Should log warning but not throw error
  )
})

test_that("decorate_bio logs both candidate and unique structure counts", {
  test_df <- tidytable::tidytable(
    feature_id = c("FT001", "FT002"),
    score_biological = c(0.9, 0.8),
    candidate_structure_inchikey_connectivity_layer = c("AAAAA", "AAAAA")
  )

  log_file <- withr::local_tempfile(fileext = ".log")
  setup_logger(filename = log_file, threshold = 400)

  call_decorate_bio(test_df, .score_all = 0.7)

  logged <- paste(readLines(log_file, warn = FALSE), collapse = "\n")

  expect_true(grepl("candidates", logged, fixed = TRUE))
  expect_true(grepl("unique", logged, fixed = TRUE))
  expect_true(
    grepl(
      "Kingdom\\s+level:\\s+2 candidates \\(1 unique\\)",
      logged
    )
  )
})

## Performance ----

test_that("decoration functions are fast", {
  # Create moderately-sized test data
  large_df <- tidytable::tidytable(
    feature_id = rep(paste0("FT", 1:100), each = 10),
    score_biological = runif(1000, 0, 1),
    candidate_structure_inchikey_connectivity_layer = rep(
      paste0("INK", 1:1000),
      length.out = 1000
    )
  )

  # Should complete quickly
  start_time <- Sys.time()
  result <- call_decorate_bio(large_df)
  end_time <- Sys.time()

  # Should be fast (<1 second for 1000 rows)
  expect_fast(difftime(end_time, start_time, units = "secs"), 1.0)
  expect_identical(result, large_df)
})

test_that("decorate_bio ignores below-minimum rows when counting unique structures per level", {
  test_df <- tidytable::tidytable(
    feature_id = c("FT1", "FT2", "FT3", "FT4"),
    score_biological = c(0.0, 0.2, 0.8, 0.8),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_ZERO",
      "IK_SHARED",
      "IK_SHARED",
      "IK_HIGH"
    )
  )

  log_file <- withr::local_tempfile(fileext = ".log")
  setup_logger(filename = log_file, threshold = 400)

  call_decorate_bio(test_df, score_biological_biota = 1.0)

  logged <- paste(readLines(log_file, warn = FALSE), collapse = "\n")
  expect_true(grepl("Kingdom\\s+level:\\s+3 candidates \\(2 unique\\)", logged))
  expect_true(grepl("Species\\s+level:\\s+2 candidates \\(2 unique\\)", logged))
  expect_true(grepl("Variety\\s+level:\\s+0 candidates \\(0 unique\\)", logged))

  # All scores below minimum threshold (length(valid_sc) == 0L)
  na_df <- tidytable::tidytable(
    feature_id = "FT1",
    score_biological = NA_real_,
    candidate_structure_inchikey_connectivity_layer = "IK_NA"
  )
  expect_identical(
    call_decorate_bio(na_df, score_biological_biota = 1.0),
    na_df
  )
})

test_that("decorate_bio formats optional domain and sub-rank biological levels", {
  rec <- new.env(parent = emptyenv())
  rec$logs <- character()
  local_mocked_bindings(
    log_info = function(msg, ...) {
      formatted <- if (...length() > 0L) sprintf(msg, ...) else msg
      rec$logs <- c(rec$logs, formatted)
      invisible(NULL)
    }
  )

  df_sub <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.10, 0.55, 0.85),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2", "IK3")
  )

  res <- call_decorate_bio(
    df_sub,
    include_domain = TRUE,
    include_subranks = TRUE,
    score_biological_domain = 0.05,
    score_biological_kingdom = 0.1,
    score_biological_phylum = 0.2,
    score_biological_class = 0.3,
    score_biological_order = 0.4,
    score_biological_infraorder = 0.5,
    score_biological_variety = 0.98,
    score_biological_biota = 1.0
  )

  expect_identical(res, df_sub)
  logged <- paste(rec$logs, collapse = "\n")
  expect_true(grepl("Domain\\s+level:\\s+3 candidates \\(3 unique\\)", logged))
  expect_true(grepl(
    "Infraorder\\s+level:\\s+2 candidates \\(2 unique\\)",
    logged
  ))
  expect_true(grepl(
    "Subgenus\\s+level:\\s+1 candidates \\(1 unique\\)",
    logged
  ))
})
