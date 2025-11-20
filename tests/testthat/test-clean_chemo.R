# Test Suite: clean_chemo ----

library(testthat)

.make_annot_table <- function(n = 5) {
  tidytable::tidytable(
    feature_id = sprintf("F%03d", rep(1:ceiling(n / 2), length.out = n)),
    candidate_structure_inchikey_connectivity_layer = sample(c("AAAAAA", "BBBBBB", NA), n, TRUE),
    score_weighted_chemo = runif(n, 0, 1),
    score_biological = runif(n, 0, 1),
    score_chemical = runif(n, 0, 1),
    candidate_score_pseudo_initial = runif(n, 0, 1),
    candidate_structure_name = sample(c("CmpdA", "CmpdB", "CmpdC"), n, TRUE),
    candidate_adduct = sample(c("[M+H]+", "[M+Na]+"), n, TRUE)
  )
}

.make_components <- function(features) {
  tidytable::tidytable(
    feature_id = features,
    component_id = sample(1:3, length(features), TRUE)
  )
}

.make_features <- function(features) {
  tidytable::tidytable(
    feature_id = features,
    rt = runif(length(features), 0, 10),
    mz = runif(length(features), 100, 500)
  )
}

.make_sop <- function(inchikeys) {
  tidytable::tidytable(
    structure_inchikey_connectivity_layer = inchikeys,
    organism_taxonomy_06family = sample(c("Gentianaceae", "Brassicaceae"), length(inchikeys), TRUE),
    reference_doi = sample(c("10.1/abc", "10.2/def", "10.3/ghi"), length(inchikeys), TRUE)
  )
}

test_that("clean_chemo validates inputs", {
  expect_error(clean_chemo(annot_table_wei_chemo = 123), "data frame")
  expect_error(
    clean_chemo(
      annot_table_wei_chemo = data.frame(), candidates_final = 0, best_percentile = 0.9,
      minimal_ms1_bio = 0.1, minimal_ms1_chemo = 0.1, minimal_ms1_condition = "OR",
      compounds_names = TRUE, high_confidence = FALSE, remove_ties = FALSE, summarize = FALSE
    ),
    "candidates_final"
  )
  expect_error(
    clean_chemo(
      annot_table_wei_chemo = data.frame(), candidates_final = 1, best_percentile = 2,
      minimal_ms1_bio = 0.1, minimal_ms1_chemo = 0.1, minimal_ms1_condition = "OR",
      compounds_names = TRUE, high_confidence = FALSE, remove_ties = FALSE, summarize = FALSE
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

# test_that("clean_chemo produces three-tier output", {
#   ann <- .make_annot_table(10)
#   feats <- .make_features(unique(ann$feature_id))
#   comps <- .make_components(unique(ann$feature_id))
#   sop <- .make_sop(na.omit(unique(ann$candidate_structure_inchikey_connectivity_layer)))
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
#   expect_type(res, "list")
#   expect_named(res, c("full", "filtered", "mini"))
#   expect_true(all(vapply(res, is.data.frame, logical(1))))
# })

# test_that("clean_chemo apply high confidence filter option", {
#   ann <- .make_annot_table(8)
#   feats <- .make_features(unique(ann$feature_id))
#   comps <- .make_components(unique(ann$feature_id))
#   sop <- .make_sop(na.omit(unique(ann$candidate_structure_inchikey_connectivity_layer)))
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
#   expect_named(res_hc, c("full", "filtered", "mini"))
# })

# test_that("clean_chemo remove ties behavior", {
#   ann <- .make_annot_table(12)
#   feats <- .make_features(unique(ann$feature_id))
#   comps <- .make_components(unique(ann$feature_id))
#   sop <- .make_sop(na.omit(unique(ann$candidate_structure_inchikey_connectivity_layer)))
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
#   expect_true(all(vapply(res_no_ties, is.data.frame, logical(1))))
# })

# test_that("clean_chemo summarize collapses to one row per feature", {
#   ann <- .make_annot_table(10)
#   feats <- .make_features(unique(ann$feature_id))
#   comps <- .make_components(unique(ann$feature_id))
#   sop <- .make_sop(na.omit(unique(ann$candidate_structure_inchikey_connectivity_layer)))
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
#   expect_true(nrow(res_sum$filtered) >= length(unique(ann$feature_id)))
# })
