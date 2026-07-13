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
  df$candidate_structure_tag <- "tag"
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
  empty_input <- d
  empty_input$df <- empty_input$df[0L, ]
  expect_warning(
    out <- call_sr(empty_input),
    "Empty results"
  )
  expect_equal(nrow(out), 0L)
  expect_true("candidate_structure_tag" %in% names(out))
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
  expect_true("candidate_structure_tag" %in% names(out))
})

test_that("summarize_results joins annotation notes without rank_final", {
  d <- make_sop_df(n_cand = 2L)
  notes <- tidytable::tidytable(
    feature_id = "FT1",
    candidate_adduct = "[M+H]+",
    annotation_note = "Kept candidate matching sibling feature"
  )

  out <- summarize_results(
    df = d$df,
    features_table = d$features,
    components_table = d$components,
    structure_organism_pairs_table = d$sop,
    annot_table_wei_chemo = d$chemo,
    remove_ties = FALSE,
    summarize = FALSE,
    annotation_notes_lookup = notes
  )

  ft1 <- out[out$feature_id == "FT1", , drop = FALSE]
  expect_true(any(grepl("sibling feature", ft1$annotation_note)))
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
  out <- call_sr(d)
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

test_that("summarize=TRUE preserves the best-ranked values per feature", {
  d <- make_sop_df(n_features = 2L, n_cand = 2L)
  d$df <- d$df |>
    tidytable::mutate(
      rank_final = c(1L, 2L, 1L, 2L),
      candidate_score_pseudo_initial = c(0.2, 0.9, 0.7, 0.6),
      score_weighted_chemo = c(0.3, 0.8, 0.6, 0.4)
    )

  out <- call_sr(d, summarize = TRUE)

  ft1 <- out[out$feature_id == "FT1", , drop = FALSE]
  expect_equal(nrow(ft1), 1L)
  expect_equal(ft1$rank_final[[1L]], 1L)
  expect_equal(ft1$score_weighted_chemo[[1L]], 0.6)
})

test_that("summarize=TRUE breaks score ties with coverage", {
  d <- make_sop_df(n_features = 1L, n_cand = 2L)
  d$df <- d$df |>
    tidytable::mutate(
      rank_final = c(1L, 1L),
      candidate_score_pseudo_initial = c(0.2, 0.2),
      score_weighted_chemo = c(0.8, 0.8),
      score_weighted_chemo_coverage = c(0.5, 1.0)
    )

  out <- call_sr(d, summarize = TRUE)
  expect_equal(nrow(out), 1L)
  expect_equal(out$score_weighted_chemo_coverage[[1L]], 1.0)
})

test_that("count_annotated_features uses the full feature table as denominator", {
  results <- tidytable::tidytable(
    feature_id = c("FT1", "FT2"),
    candidate_structure_inchikey_connectivity_layer = c("IK1", NA_character_)
  )
  features_table <- tidytable::tidytable(feature_id = c("FT1", "FT2", "FT3"))

  coverage <- .count_annotated_features(
    results = results,
    features_table = features_table,
    feature_id_col = "feature_id",
    annotation_col = "candidate_structure_inchikey_connectivity_layer"
  )

  expect_equal(coverage$total_features, 3L)
  expect_equal(coverage$annotated_features, 1L)
  expect_equal(coverage$pct_annotated, 100 / 3, tolerance = 1e-8)
})

test_that("no-structure rows are not duplicated by mixed annotation notes", {
  d <- make_sop_df(n_features = 2L, n_cand = 2L)

  d$df <- d$df |>
    tidytable::filter(feature_id == "FT1") |>
    tidytable::mutate(
      candidate_structure_inchikey_connectivity_layer = NA_character_,
      rank_final = 1L
    )

  d$chemo <- data.frame(
    feature_id = c("FT1", "FT1"),
    annotation_note = c(
      "Spectral match rescued in neutral-mass space: observed adduct [M+K]+, library adduct [M+H]+",
      NA_character_
    ),
    stringsAsFactors = FALSE
  )

  out <- call_sr(d, summarize = FALSE)
  out_ft1 <- out[out$feature_id == "FT1", , drop = FALSE]

  expect_equal(nrow(out_ft1), 1L)
  if ("annotation_note" %in% names(out_ft1)) {
    expect_true(grepl("neutral-mass space", out_ft1$annotation_note[[1L]]))
  }
})
