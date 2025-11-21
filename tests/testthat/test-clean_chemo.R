# Test Suite: clean_chemo ----

library(testthat)

test_that("clean_chemo validates inputs", {
  expect_error(clean_chemo(annot_table_wei_chemo = 123), "data frame")
  expect_error(
    clean_chemo(
      annot_table_wei_chemo = data.frame(),
      candidates_final = 0,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "candidates_final"
  )
  expect_error(
    clean_chemo(
      annot_table_wei_chemo = data.frame(),
      candidates_final = 1,
      best_percentile = 2,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "best_percentile"
  )
})

test_that("clean_chemo handles empty annotation table", {
  empty <- tidytable::tidytable()
  res <- clean_chemo(
    annot_table_wei_chemo = empty,
    components_table = tidytable::tidytable(),
    features_table = tidytable::tidytable(),
    structure_organism_pairs_table = tidytable::tidytable(),
    candidates_final = 5,
    best_percentile = 0.9,
    minimal_ms1_bio = 0.1,
    minimal_ms1_chemo = 0.1,
    minimal_ms1_condition = "OR",
    compounds_names = TRUE,
    high_confidence = FALSE,
    remove_ties = FALSE,
    summarize = FALSE
  )
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

# test_that("clean_chemo produces three-tier output with fixtures", {
#   # Use fixture data
#   ann <- load_fixture("annotations")
#   feats <- load_fixture("features")
#   comps <- load_fixture("components")
#   sop <- load_fixture("structure_organism_pairs")
#
#   # Ensure required columns exist with defaults if missing
#   if (!"score_weighted_chemo" %in% names(ann)) {
#     ann$score_weighted_chemo <- runif(nrow(ann), 0, 1)
#   }
#   if (!"score_biological" %in% names(ann)) {
#     ann$score_biological <- runif(nrow(ann), 0, 1)
#   }
#   if (!"score_chemical" %in% names(ann)) {
#     ann$score_chemical <- runif(nrow(ann), 0, 1)
#   }
#   if (!"candidate_score_pseudo_initial" %in% names(ann)) {
#     ann$candidate_score_pseudo_initial <- runif(nrow(ann), 0, 1)
#   }
#
#   res <- clean_chemo(
#     annot_table_wei_chemo = ann,
#     components_table = comps,
#     features_table = feats,
#     structure_organism_pairs_table = sop,
#     candidates_final = 3,
#     best_percentile = 0.5,
#     minimal_ms1_bio = 0.0,
#     minimal_ms1_chemo = 0.0,
#     minimal_ms1_condition = "OR",
#     compounds_names = TRUE,
#     high_confidence = FALSE,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   expect_type(res, "list")
#   expect_named(res, c("full", "filtered", "mini"))
#   expect_true(all(vapply(res, is.data.frame, logical(1))))
# })

# test_that("clean_chemo applies high confidence filter option", {
#   ann <- load_fixture("annotations")
#   feats <- load_fixture("features")
#   comps <- load_fixture("components")
#   sop <- load_fixture("structure_organism_pairs")
#
#   # Add required score columns
#   ann$score_weighted_chemo <- runif(nrow(ann), 0, 1)
#   ann$score_biological <- runif(nrow(ann), 0, 1)
#   ann$score_chemical <- runif(nrow(ann), 0, 1)
#   ann$candidate_score_pseudo_initial <- runif(nrow(ann), 0, 1)
#
#   res_hc <- clean_chemo(
#     annot_table_wei_chemo = ann,
#     components_table = comps,
#     features_table = feats,
#     structure_organism_pairs_table = sop,
#     candidates_final = 2,
#     best_percentile = 0.5,
#     minimal_ms1_bio = 0.0,
#     minimal_ms1_chemo = 0.0,
#     minimal_ms1_condition = "OR",
#     compounds_names = TRUE,
#     high_confidence = TRUE,
#     remove_ties = FALSE,
#     summarize = FALSE
#   )
#
#   expect_named(res_hc, c("full", "filtered", "mini"))
# })

# test_that("clean_chemo remove ties behavior", {
#   ann <- load_fixture("annotations")
#   feats <- load_fixture("features")
#   comps <- load_fixture("components")
#   sop <- load_fixture("structure_organism_pairs")
#
#   ann$score_weighted_chemo <- runif(nrow(ann), 0, 1)
#   ann$score_biological <- runif(nrow(ann), 0, 1)
#   ann$score_chemical <- runif(nrow(ann), 0, 1)
#   ann$candidate_score_pseudo_initial <- runif(nrow(ann), 0, 1)
#
#   res_no_ties <- clean_chemo(
#     annot_table_wei_chemo = ann,
#     components_table = comps,
#     features_table = feats,
#     structure_organism_pairs_table = sop,
#     candidates_final = 5,
#     best_percentile = 0.5,
#     minimal_ms1_bio = 0.0,
#     minimal_ms1_chemo = 0.0,
#     minimal_ms1_condition = "OR",
#     compounds_names = TRUE,
#     high_confidence = FALSE,
#     remove_ties = TRUE,
#     summarize = FALSE
#   )
#
#   expect_true(all(vapply(res_no_ties, is.data.frame, logical(1))))
# })

# test_that("clean_chemo summarize collapses to one row per feature", {
#   ann <- load_fixture("annotations")
#   feats <- load_fixture("features")
#   comps <- load_fixture("components")
#   sop <- load_fixture("structure_organism_pairs")
#
#   ann$score_weighted_chemo <- runif(nrow(ann), 0, 1)
#   ann$score_biological <- runif(nrow(ann), 0, 1)
#   ann$score_chemical <- runif(nrow(ann), 0, 1)
#   ann$candidate_score_pseudo_initial <- runif(nrow(ann), 0, 1)
#
#   res_sum <- clean_chemo(
#     annot_table_wei_chemo = ann,
#     components_table = comps,
#     features_table = feats,
#     structure_organism_pairs_table = sop,
#     candidates_final = 5,
#     best_percentile = 0.5,
#     minimal_ms1_bio = 0.0,
#     minimal_ms1_chemo = 0.0,
#     minimal_ms1_condition = "OR",
#     compounds_names = TRUE,
#     high_confidence = FALSE,
#     remove_ties = FALSE,
#     summarize = TRUE
#   )
#
#   expect_true(nrow(res_sum$filtered) >= length(unique(feats$feature_id)))
# })
