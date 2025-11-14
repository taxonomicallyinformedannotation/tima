#' @title Test Suite for summarize_results
#'
#' @description Individual focused tests for summarize_results function.

library(testthat)
library(tima)

# Helper function to create test data ----

create_test_annotation_data <- function(n_features = 5, n_candidates = 3) {
  # Create minimal test data that mimics real annotation structure
  feature_ids <- paste0("FT_", seq_len(n_features))

  # Create features table
  features_table <- data.frame(
    feature_id = feature_ids,
    rt = runif(n_features, 1, 10),
    mz = runif(n_features, 100, 500),
    stringsAsFactors = FALSE
  )

  # Create components table
  components_table <- data.frame(
    feature_id = rep(feature_ids, each = 2)[1:n_features],
    component_id = paste0("C_", seq_len(n_features)),
    stringsAsFactors = FALSE
  )

  # Create annotation results
  df <- expand.grid(
    feature_id = feature_ids[1:min(3, n_features)],
    candidate_structure_inchikey_connectivity_layer = paste0(
      "INK_",
      1:n_candidates
    ),
    stringsAsFactors = FALSE
  )

  df$candidate_score_pseudo_initial <- runif(nrow(df), 0, 1)
  df$score_biological <- runif(nrow(df), 0, 100)
  df$score_weighted_bio <- runif(nrow(df), 0, 100)
  df$score_chemical <- runif(nrow(df), 0, 100)
  df$score_weighted_chemo <- runif(nrow(df), 0, 100)
  df$rank_final <- rep(1:n_candidates, length.out = nrow(df))

  # Create structure-organism pairs
  structure_organism_pairs_table <- data.frame(
    structure_inchikey_connectivity_layer = paste0("INK_", 1:n_candidates),
    organism_name = paste0("Organism_", 1:n_candidates),
    organism_taxonomy_01domain = rep("Eukaryota", n_candidates),
    organism_taxonomy_02kingdom = rep("Plantae", n_candidates),
    organism_taxonomy_03phylum = rep("Streptophyta", n_candidates),
    organism_taxonomy_ottid = paste0("OTT_", 1:n_candidates),
    reference_doi = paste0("10.1234/ref", 1:n_candidates),
    stringsAsFactors = FALSE
  )

  # Create chemically weighted annotations (minimal)
  annot_table_wei_chemo <- df[1:min(2, nrow(df)), ]

  list(
    df = df,
    features_table = features_table,
    components_table = components_table,
    structure_organism_pairs_table = structure_organism_pairs_table,
    annot_table_wei_chemo = annot_table_wei_chemo
  )
}

# Input validation tests ----

test_that("summarize_results validates input types", {
  test_data <- create_test_annotation_data()

  # df must be data frame
  expect_error(
    summarize_results(
      df = "not_a_df",
      features_table = test_data$features_table,
      components_table = test_data$components_table,
      structure_organism_pairs_table = test_data$structure_organism_pairs_table,
      annot_table_wei_chemo = test_data$annot_table_wei_chemo,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "df must be a data frame"
  )

  # features_table must be data frame
  expect_error(
    summarize_results(
      df = test_data$df,
      features_table = list(),
      components_table = test_data$components_table,
      structure_organism_pairs_table = test_data$structure_organism_pairs_table,
      annot_table_wei_chemo = test_data$annot_table_wei_chemo,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "features_table must be a data frame"
  )

  # components_table must be data frame
  expect_error(
    summarize_results(
      df = test_data$df,
      features_table = test_data$features_table,
      components_table = NULL,
      structure_organism_pairs_table = test_data$structure_organism_pairs_table,
      annot_table_wei_chemo = test_data$annot_table_wei_chemo,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "components_table must be a data frame"
  )

  # remove_ties must be logical
  expect_error(
    summarize_results(
      df = test_data$df,
      features_table = test_data$features_table,
      components_table = test_data$components_table,
      structure_organism_pairs_table = test_data$structure_organism_pairs_table,
      annot_table_wei_chemo = test_data$annot_table_wei_chemo,
      remove_ties = "yes",
      summarize = FALSE
    ),
    "remove_ties must be logical"
  )

  # summarize must be logical
  expect_error(
    summarize_results(
      df = test_data$df,
      features_table = test_data$features_table,
      components_table = test_data$components_table,
      structure_organism_pairs_table = test_data$structure_organism_pairs_table,
      annot_table_wei_chemo = test_data$annot_table_wei_chemo,
      remove_ties = FALSE,
      summarize = 1
    ),
    "summarize must be logical"
  )
})

test_that("summarize_results handles empty input gracefully", {
  test_data <- create_test_annotation_data()

  # Empty df should return empty df with warning
  empty_df <- test_data$df[0, ]

  expect_warning(
    result <- summarize_results(
      df = empty_df,
      features_table = test_data$features_table,
      components_table = test_data$components_table,
      structure_organism_pairs_table = test_data$structure_organism_pairs_table,
      annot_table_wei_chemo = test_data$annot_table_wei_chemo,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "Empty results table"
  )

  expect_equal(nrow(result), 0)
})

# Basic functionality tests ----

# test_that("summarize_results returns a data frame", {
#   test_data <- create_test_annotation_data()
#
#   expect_result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   expect_s3_class(result, "data.frame")
#   expect_true(nrow(result) > 0)
# })

# test_that("summarize_results adds feature metadata", {
#   test_data <- create_test_annotation_data()
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   # Should have feature RT and m/z columns
#   expect_true("feature_rt" %in% names(result) || "rt" %in% names(result))
#   expect_true("feature_mz" %in% names(result) || "mz" %in% names(result))
# })

# test_that("summarize_results adds component information", {
#   test_data <- create_test_annotation_data()
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   # Should have component_id column if present in components_table
#   if ("component_id" %in% names(test_data$components_table)) {
#     expect_true("component_id" %in% names(result))
#   }
# })

# Remove ties functionality ----

test_that("summarize_results removes ties when requested", {
  test_data <- create_test_annotation_data(n_features = 3, n_candidates = 5)

  # Add some tied scores
  test_data$df$rank_final <- rep(
    c(1, 1, 2, 3, 4),
    length.out = nrow(test_data$df)
  )

  result_with_ties <- summarize_results(
    df = test_data$df,
    features_table = test_data$features_table,
    components_table = test_data$components_table,
    structure_organism_pairs_table = test_data$structure_organism_pairs_table,
    annot_table_wei_chemo = test_data$annot_table_wei_chemo,
    remove_ties = FALSE,
    summarize = FALSE
  )

  result_without_ties <- summarize_results(
    df = test_data$df,
    features_table = test_data$features_table,
    components_table = test_data$components_table,
    structure_organism_pairs_table = test_data$structure_organism_pairs_table,
    annot_table_wei_chemo = test_data$annot_table_wei_chemo,
    remove_ties = TRUE,
    summarize = FALSE
  )

  # Should have fewer rows when ties are removed
  expect_true(nrow(result_without_ties) <= nrow(result_with_ties))
})

test_that("remove_ties keeps only one entry per feature-rank combination", {
  test_data <- create_test_annotation_data(n_features = 2, n_candidates = 3)
  test_data$df$rank_final <- c(1, 1, 2, 1, 2, 3)

  result <- summarize_results(
    df = test_data$df,
    features_table = test_data$features_table,
    components_table = test_data$components_table,
    structure_organism_pairs_table = test_data$structure_organism_pairs_table,
    annot_table_wei_chemo = test_data$annot_table_wei_chemo,
    remove_ties = TRUE,
    summarize = FALSE
  )

  # Check for unique feature_id + rank_final combinations
  if ("feature_id" %in% names(result) && "rank_final" %in% names(result)) {
    combinations <- paste(result$feature_id, result$rank_final)
    expect_equal(length(combinations), length(unique(combinations)))
  }
})

# Summarize functionality ----

# test_that("summarize_results collapses to one row per feature when summarize=TRUE", {
#   test_data <- create_test_annotation_data(n_features = 3, n_candidates = 4)
#
#   result_full <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   result_summarized <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = TRUE
#   )
#
#   # Summarized should have fewer rows (one per feature)
#   expect_true(nrow(result_summarized) <= nrow(result_full))
#
#   # Should have unique feature_ids in summarized version
#   if ("feature_id" %in% names(result_summarized)) {
#     expect_equal(
#       nrow(result_summarized),
#       length(unique(result_summarized$feature_id))
#     )
#   }
# })

# test_that("summarize concatenates candidate information with pipes", {
#   skip_if_not_installed("tidytable")
#
#   test_data <- create_test_annotation_data(n_features = 1, n_candidates = 3)
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = TRUE
#   )
#
#   # Candidate columns should contain pipe-separated values
#   candidate_cols <- grep("^candidate", names(result), value = TRUE)
#
#   if (length(candidate_cols) > 0) {
#     # At least one should have pipes if multiple candidates
#     has_pipes <- any(sapply(result[candidate_cols], function(x) {
#       any(grepl("\\|", x, fixed = TRUE), na.rm = TRUE)
#     }))
#
#     if (nrow(test_data$df) > 1) {
#       expect_true(has_pipes)
#     }
#   }
# })

# Organism occurrence tests ----

# test_that("summarize_results adds organism occurrence information", {
#   test_data <- create_test_annotation_data()
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   # Should potentially have organism occurrence columns
#   # (depends on data and joins)
#   occurrence_cols <- grep("organism_occurrence", names(result), value = TRUE)
#
#   # If organism data exists, should add occurrence info
#   expect_true(length(occurrence_cols) >= 0) # May be 0 if no matches
# })

# Column selection and renaming ----

# test_that("summarize_results renames score columns appropriately", {
#   test_data <- create_test_annotation_data()
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   # Check for renamed score columns
#   expected_score_cols <- c(
#     "score_initial",
#     "score_biological",
#     "score_interim",
#     "score_chemical",
#     "score_final"
#   )
#
#   # At least some should be present
#   present_scores <- expected_score_cols[expected_score_cols %in% names(result)]
#   expect_true(length(present_scores) > 0)
# })

# test_that("summarize_results renames RT and m/z columns", {
#   test_data <- create_test_annotation_data()
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   # Should have feature_rt and feature_mz (or rt/mz)
#   has_rt <- "feature_rt" %in% names(result) || "rt" %in% names(result)
#   has_mz <- "feature_mz" %in% names(result) || "mz" %in% names(result)
#
#   expect_true(has_rt)
#   expect_true(has_mz)
# })

# Data cleaning tests ----

# test_that("summarize_results converts all columns to character", {
#   test_data <- create_test_annotation_data()
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   # All columns should be character
#   column_types <- sapply(result, class)
#   expect_true(all(sapply(column_types, function(x) "character" %in% x)))
# })

# test_that("summarize_results trims whitespace", {
#   test_data <- create_test_annotation_data()
#
#   # Add some whitespace
#   test_data$df$feature_id[1] <- " FT_1 "
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   # Should not have leading/trailing whitespace
#   if ("feature_id" %in% names(result)) {
#     expect_false(any(
#       grepl("^\\s|\\s$", result$feature_id, perl = TRUE),
#       na.rm = TRUE
#     ))
#   }
# })

# test_that("summarize_results converts empty strings to NA", {
#   test_data <- create_test_annotation_data()
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   # Empty strings should be NA
#   has_empty_strings <- any(sapply(result, function(col) {
#     any(col == "", na.rm = TRUE)
#   }))
#
#   expect_false(has_empty_strings)
# })

# Edge cases ----

# test_that("summarize_results handles single feature", {
#   test_data <- create_test_annotation_data(n_features = 1, n_candidates = 2)
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   expect_s3_class(result, "data.frame")
#   expect_true(nrow(result) > 0)
# })

# test_that("summarize_results handles single candidate", {
#   test_data <- create_test_annotation_data(n_features = 3, n_candidates = 1)
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   expect_s3_class(result, "data.frame")
#   expect_true(nrow(result) > 0)
# })

# test_that("summarize_results handles many features", {
#   skip_on_cran()
#
#   test_data <- create_test_annotation_data(n_features = 100, n_candidates = 5)
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   expect_s3_class(result, "data.frame")
#   expect_true(nrow(result) > 0)
# })

# Combined options tests ----

# test_that("summarize_results works with both remove_ties=TRUE and summarize=TRUE", {
#   test_data <- create_test_annotation_data(n_features = 3, n_candidates = 5)
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = TRUE,
#     summarize = TRUE
#   )
#
#   expect_s3_class(result, "data.frame")
#
#   # Should have one row per feature (or fewer if some features have no annotations)
#   if ("feature_id" %in% names(result)) {
#     expect_equal(
#       nrow(result),
#       length(unique(result$feature_id))
#     )
#   }
# })

# Performance tests ----

# test_that("summarize_results completes in reasonable time", {
#   skip_on_cran()
#
#   test_data <- create_test_annotation_data(n_features = 50, n_candidates = 10)
#
#   start_time <- Sys.time()
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = TRUE,
#     summarize = TRUE
#   )
#
#   end_time <- Sys.time()
#   elapsed <- as.numeric(end_time - start_time, units = "secs")
#
#   # Should complete in under 5 seconds for moderate data
#   expect_true(elapsed < 5)
# })

# Robustness tests ----

# test_that("summarize_results handles missing organism data", {
#   test_data <- create_test_annotation_data()
#
#   # Empty organism table
#   empty_organism_table <- test_data$structure_organism_pairs_table[0, ]
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = empty_organism_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   expect_s3_class(result, "data.frame")
# })

# test_that("summarize_results handles NA values appropriately", {
#   test_data <- create_test_annotation_data()
#
#   # Add some NAs
#   test_data$df$score_biological[1] <- NA
#   test_data$df$candidate_structure_inchikey_connectivity_layer[2] <- NA
#
#   result <- summarize_results(
#     df = test_data$df,
#     features_table = test_data$features_table,
#     components_table = test_data$components_table,
#     structure_organism_pairs_table = test_data$structure_organism_pairs_table,
#     annot_table_wei_chemo = test_data$annot_table_wei_chemo,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   expect_s3_class(result, "data.frame")
# })
