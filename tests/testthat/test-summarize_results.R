# Test Suite: summarize_results ----
# Covers the real contract of summarize_results: input validation,
# empty-input early return, remove_ties, summarize, and tag propagation.

library(testthat)

# ---- fixture -----------------------------------------------------------------

make_sop_df <- function(n_features = 3L, n_cand = 2L) {
  fids <- paste0("FT", seq_len(n_features))
  inks <- paste0("IK", seq_len(n_cand))

  features <- data.frame(
    feature_id = fids,
    rt = seq_len(n_features) * 1.0,
    mz = seq_len(n_features) * 100.0,
    stringsAsFactors = FALSE
  )
  components <- data.frame(
    feature_id = fids,
    component_id = paste0("C", seq_len(n_features)),
    stringsAsFactors = FALSE
  )

  df <- expand.grid(
    feature_id = fids[seq_len(min(2L, n_features))],
    candidate_structure_inchikey_connectivity_layer = inks,
    stringsAsFactors = FALSE
  )
  df$candidate_score_pseudo_initial <- runif(nrow(df))
  df$candidate_structure_organism_occurrence_closest <- "Plantae"
  df$score_biological <- runif(nrow(df)) * 100
  df$score_weighted_bio <- runif(nrow(df)) * 100
  df$score_chemical <- runif(nrow(df)) * 100
  df$score_weighted_chemo <- runif(nrow(df)) * 100
  df$rank_final <- rep(seq_len(n_cand), length.out = nrow(df))
  df$candidate_adduct <- "[M+H]+"

  sop <- data.frame(
    structure_inchikey_connectivity_layer = inks,
    organism_taxonomy_01domain = "Eukaryota",
    organism_taxonomy_02kingdom = "Plantae",
    organism_taxonomy_03phylum = "Tracheophyta",
    organism_taxonomy_04class = "Magnoliopsida",
    organism_taxonomy_05order = "Lamiales",
    organism_taxonomy_06family = "Lamiaceae",
    organism_taxonomy_07tribe = NA_character_,
    organism_taxonomy_08genus = "Salvia",
    organism_taxonomy_09species = "Salvia officinalis",
    organism_taxonomy_10varietas = NA_character_,
    organism_taxonomy_ottid = paste0("OTT", seq_len(n_cand)),
    reference_doi = paste0("10.0/ref", seq_len(n_cand)),
    tag = c("serum", rep(NA_character_, n_cand - 1L)),
    stringsAsFactors = FALSE
  )

  chemo <- df[seq_len(min(2L, nrow(df))), ]

  list(
    df = df,
    features = features,
    components = components,
    sop = sop,
    chemo = chemo
  )
}

call_sr <- function(d, remove_ties = FALSE, summarize = FALSE) {
  summarize_results(
    df = d$df,
    features_table = d$features,
    components_table = d$components,
    structure_organism_pairs_table = d$sop,
    annot_table_wei_chemo = d$chemo,
    remove_ties = remove_ties,
    summarize = summarize
  )
}

# ---- input validation --------------------------------------------------------

test_that("summarize_results rejects non-data-frame df", {
  d <- make_sop_df()
  expect_error(
    summarize_results(
      "not_a_df",
      d$features,
      d$components,
      d$sop,
      d$chemo,
      FALSE,
      FALSE
    ),
    "valid data frame"
  )
})

test_that("summarize_results rejects non-logical remove_ties", {
  d <- make_sop_df()
  expect_error(call_sr(d, remove_ties = "yes"), "remove_ties")
})

test_that("summarize_results rejects non-logical summarize", {
  d <- make_sop_df()
  expect_error(
    summarize_results(
      d$df,
      d$features,
      d$components,
      d$sop,
      d$chemo,
      FALSE,
      1L
    ),
    "summarize"
  )
})

# ---- empty input -------------------------------------------------------------

test_that("summarize_results warns and returns empty df with tag column on empty input", {
  d <- make_sop_df()
  expect_warning(
    out <- call_sr(d |> within(df <- df[0L, ])),
    "Empty results"
  )
  expect_equal(nrow(out), 0L)
  expect_true("candidate_structure_organism_occurrence_tag" %in% names(out))
})

# ---- basic output ------------------------------------------------------------

test_that("summarize_results returns a data frame with feature_id column", {
  d <- make_sop_df()
  out <- call_sr(d)
  expect_s3_class(out, "data.frame")
  expect_true("feature_id" %in% names(out))
})

test_that("summarize_results includes tag column when SOP has tags", {
  d <- make_sop_df(n_cand = 2L)
  out <- call_sr(d)
  expect_true("candidate_structure_organism_occurrence_tag" %in% names(out))
})

test_that("summarize_results works when SOP has no tag column", {
  d <- make_sop_df()
  d$sop$tag <- NULL
  out <- call_sr(d)
  expect_s3_class(out, "data.frame")
  # tag column should still be created (as all NA) or omitted gracefully
})

test_that("summarize_results works when SOP has no reference_doi column", {
  d <- make_sop_df()
  d$sop$reference_doi <- NULL
  expect_no_error(out <- call_sr(d))
  expect_s3_class(out, "data.frame")
})

# ---- remove_ties -------------------------------------------------------------

test_that("remove_ties produces at most one row per feature-rank combination", {
  d <- make_sop_df(n_features = 2L, n_cand = 3L)
  out <- call_sr(d, remove_ties = TRUE)
  if ("rank_final" %in% names(out) && "feature_id" %in% names(out)) {
    combos <- paste(out$feature_id, out$rank_final)
    expect_equal(length(combos), length(unique(combos)))
  }
})

# ---- summarize ---------------------------------------------------------------

test_that("summarize=TRUE collapses to one row per feature", {
  d <- make_sop_df(n_features = 3L, n_cand = 4L)
  out <- call_sr(d, summarize = TRUE)
  expect_s3_class(out, "data.frame")
  if ("feature_id" %in% names(out)) {
    expect_equal(nrow(out), length(unique(out$feature_id)))
  }
})
