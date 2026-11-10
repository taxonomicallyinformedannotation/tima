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

test_that("summarize=TRUE breaks score ties with initial score", {
  d <- make_sop_df(n_features = 1L, n_cand = 2L)
  d$df <- d$df |>
    tidytable::mutate(
      rank_final = c(1L, 1L),
      candidate_score_pseudo_initial = c(0.2, 0.9),
      score_weighted_chemo = c(0.8, 0.8),
    )

  out <- call_sr(d, summarize = TRUE)
  expect_equal(nrow(out), 1L)
  expect_equal(out$score_initial[[1L]], 0.9)
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

test_that("summarize_results collapses multiple organism DOIs and trims whitespace", {
  d <- make_sop_df(n_features = 2L, n_cand = 2L)
  d$sop <- rbind(
    d$sop,
    transform(
      d$sop[1L, , drop = FALSE],
      reference_doi = "10.0/ref_extra"
    ),
    transform(
      d$sop[1L, , drop = FALSE],
      reference_doi = "10.0/ref1"
    )
  )
  d$df$candidate_structure_tag <- c("  serum  ", "   ", "urine", "plasma")

  out <- call_sr(d, summarize = FALSE)
  ik1_rows <- out[
    out$candidate_structure_inchikey_connectivity_layer == "IK1",
    ,
    drop = FALSE
  ]
  expect_true(all(
    ik1_rows$candidate_structure_organism_occurrence_reference ==
      "10.0/ref1 | 10.0/ref_extra"
  ))
  expect_false(any(grepl(
    "^\\s|\\s$",
    stats::na.omit(out$candidate_structure_tag)
  )))
  expect_true(anyNA(out$candidate_structure_tag))
})

test_that("summarize_results removes ties by (feature_id, rank_final) and sorts numeric feature_id", {
  df <- tidytable::tidytable(
    feature_id = c("10", "2", "2", "1"),
    rt = c(10.0, 2.0, 2.0, 1.0),
    mz = c(200.0, 150.0, 150.0, 100.0),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_TEN",
      "IK_TWO_A",
      "IK_TWO_B",
      "IK_ONE"
    ),
    candidate_structure_organism_occurrence_closest = c(
      "Arabidopsis thaliana",
      "Arabidopsis thaliana",
      "Arabidopsis thaliana",
      "Arabidopsis thaliana"
    ),
    candidate_score_pseudo_initial = c(0.5, 0.9, 0.8, 0.7),
    score_biological = c(0.5, 0.9, 0.9, 0.7),
    score_weighted_bio = c(0.5, 0.9, 0.9, 0.7),
    score_chemical = c(0.5, 0.9, 0.9, 0.7),
    score_weighted_chemo = c(0.5, 0.9, 0.9, 0.7),
    rank_initial = c(1L, 1L, 2L, 1L),
    rank_final = c(1L, 1L, 1L, 1L)
  )
  features_table <- tidytable::tidytable(
    feature_id = c("1", "2", "10"),
    rt = c(1.0, 2.0, 10.0),
    mz = c(100.0, 150.0, 200.0)
  )
  components_table <- tidytable::tidytable(
    feature_id = c("1", "2", "10"),
    component_id = c("C1", "C2", "C10")
  )
  sop <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = c("IK_ONE", "IK_TWO_A", "IK_TEN"),
    reference_doi = c("10.1/one", "10.1/two", "10.1/ten"),
    organism_taxonomy_08species = rep("Arabidopsis thaliana", 3)
  )

  res <- summarize_results(
    df = df,
    features_table = features_table,
    components_table = components_table,
    structure_organism_pairs_table = sop,
    annot_table_wei_chemo = df,
    remove_ties = TRUE,
    summarize = FALSE
  )

  expect_equal(res$feature_id, c("1", "2", "10"))
  expect_equal(
    res$candidate_structure_inchikey_connectivity_layer,
    c("IK_ONE", "IK_TWO_A", "IK_TEN")
  )
})

test_that(".build_feature_consensus_table handles all-NA scores and note tie-breaking", {
  model <- columns_model()
  df_na <- tidytable::tidytable(
    feature_id = c("F2", "F1", "F1"),
    rt = c(2.0, 1.0, 1.0),
    mz = c(200.0, 100.0, 100.0),
    component_id = c("C2", "C1", "C1"),
    score_weighted_chemo = c(NA_real_, NA_real_, NA_real_),
    rank_final = c(NA_integer_, NA_integer_, NA_integer_)
  )
  out_na <- .build_feature_consensus_table(df_na, model)
  expect_equal(out_na$feature_id, c("F1", "F2"))
  expect_equal(out_na$component_id, c("C1", "C2"))
  expect_true(all(is.na(out_na$annotation_note)))

  df_notes <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F2"),
    rt = c(1.0, 1.0, 2.0, 2.0),
    mz = c(100.0, 100.0, 200.0, 200.0),
    component_id = c("C1_A", "C1_B", "C2_A", "C2_B"),
    score_weighted_chemo = c(0.9, 0.4, NA_real_, 0.5),
    rank_final = c(1L, 2L, 1L, 2L),
    annotation_note = c(NA_character_, "Consensus note", NA_character_, "   ")
  )
  out_notes <- .build_feature_consensus_table(df_notes, model)
  expect_equal(out_notes$feature_id, c("F1", "F2"))
  expect_equal(out_notes$component_id, c("C1_B", "C2_B"))
  expect_equal(out_notes$annotation_note, c("Consensus note", "   "))
})

test_that("summarize_results and helpers cover empty tables, notes lookup branches, factor trimming, and organism lookup edge cases", {
  model <- columns_model()
  features_table <- tidytable::tidytable(
    feature_id = c("1", "2"),
    rt = c(10.0, 20.0),
    mz = c(100.0, 200.0)
  )
  components_table <- tidytable::tidytable(
    feature_id = c("1", "2"),
    component_id = c("C1", "C2")
  )
  empty_sop <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = character(),
    reference_doi = character(),
    organism_taxonomy_08species = character()
  )

  # 1. Empty df with summarize = TRUE and empty_sop
  df_empty <- tidytable::tidytable(
    feature_id = character(),
    candidate_structure_inchikey_connectivity_layer = character(),
    rank_final = integer()
  )
  res_empty <- suppressWarnings(summarize_results(
    df = df_empty,
    features_table = features_table[0L, ],
    components_table = components_table[0L, ],
    structure_organism_pairs_table = empty_sop,
    remove_ties = FALSE,
    summarize = TRUE
  ))
  expect_equal(nrow(res_empty), 0L)

  # 2. Notes lookup: unique keys with existing annotation_note coalesce, and duplicate keys (with and without existing annotation_note)
  df_with_note <- tidytable::tidytable(
    feature_id = c("1", "2"),
    candidate_adduct = factor(c(" [M+H]+ ", "[M+Na]+")),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2"),
    rank_final = c(1L, 1L),
    score_final = c(0.9, 0.8),
    annotation_note = c(NA_character_, "existing")
  )
  notes_u <- tidytable::tidytable(
    feature_id = c("1", "2"),
    rank_final = c(1L, 1L),
    annotation_note = c("filled_note", "ignored_note"),
    extra_lookup_col = c("X", "Y")
  )
  res_nu <- summarize_results(
    df = df_with_note,
    features_table = features_table,
    components_table = components_table,
    structure_organism_pairs_table = empty_sop,
    annotation_notes_lookup = notes_u,
    remove_ties = FALSE,
    summarize = TRUE
  )
  expect_equal(res_nu$annotation_note, c("filled_note", "existing"))

  notes_dup <- tidytable::tidytable(
    feature_id = c("1", "1"),
    rank_final = c(1L, 1L),
    annotation_note = c("dup_note_1", "dup_note_2")
  )
  res_nd1 <- summarize_results(
    df = df_with_note,
    features_table = features_table,
    components_table = components_table,
    structure_organism_pairs_table = empty_sop,
    annotation_notes_lookup = notes_dup,
    remove_ties = FALSE,
    summarize = TRUE
  )
  expect_equal(nrow(res_nd1), 2L)

  df_no_note <- tidytable::tidytable(
    feature_id = c("1", "2"),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2"),
    rank_final = c(1L, 1L)
  )
  res_nd2 <- summarize_results(
    df = df_no_note,
    features_table = features_table,
    components_table = components_table,
    structure_organism_pairs_table = empty_sop,
    annotation_notes_lookup = notes_dup,
    remove_ties = FALSE,
    summarize = TRUE
  )
  expect_equal(nrow(res_nd2), 2L)

  # 3. .count_annotated_features edge cases
  cnt_empty <- .count_annotated_features(
    results = tidytable::tidytable(),
    features_table = NULL
  )
  expect_equal(cnt_empty$total_features, 0L)
  expect_equal(cnt_empty$pct_annotated, 0)

  cnt_no_ft <- .count_annotated_features(
    results = tidytable::tidytable(
      feature_id = c("1", "2"),
      candidate_structure_inchikey_connectivity_layer = c(
        NA_character_,
        NA_character_
      )
    ),
    features_table = NULL
  )
  expect_equal(cnt_no_ft$total_features, 2L)
  expect_equal(cnt_no_ft$annotated_features, 0L)

  # 4. .build_organism_lookup guard branches
  expect_equal(
    nrow(.build_organism_lookup(
      empty_sop,
      tidytable::tidytable(feature_id = "1")
    )),
    0L
  )
  expect_equal(
    nrow(.build_organism_lookup(
      tidytable::tidytable(other_col = "x"),
      tidytable::tidytable(
        feature_id = "1",
        candidate_structure_inchikey_connectivity_layer = "IK1",
        candidate_structure_organism_occurrence_closest = "Sp1"
      )
    )),
    0L
  )
  expect_equal(
    nrow(.build_organism_lookup(
      tidytable::tidytable(structure_inchikey_connectivity_layer = "IK1"),
      tidytable::tidytable(
        feature_id = "1",
        candidate_structure_inchikey_connectivity_layer = "IK1",
        candidate_structure_organism_occurrence_closest = "Sp1"
      )
    )),
    0L
  )
  expect_equal(
    nrow(.build_organism_lookup(
      tidytable::tidytable(
        structure_inchikey_connectivity_layer = "IK_OTHER",
        organism_taxonomy_08species = "Sp1"
      ),
      tidytable::tidytable(
        feature_id = "1",
        candidate_structure_inchikey_connectivity_layer = NA_character_,
        candidate_structure_organism_occurrence_closest = NA_character_
      )
    )),
    0L
  )
  expect_equal(
    nrow(.build_organism_lookup(
      tidytable::tidytable(
        structure_inchikey_connectivity_layer = "IK_OTHER",
        organism_taxonomy_08species = "Sp1"
      ),
      tidytable::tidytable(
        feature_id = "1",
        candidate_structure_inchikey_connectivity_layer = "IK1",
        candidate_structure_organism_occurrence_closest = "Sp1"
      )
    )),
    0L
  )
  lkp_no_doi <- .build_organism_lookup(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = "IK1",
      organism_taxonomy_08species = "Sp1"
    ),
    tidytable::tidytable(
      feature_id = "1",
      candidate_structure_inchikey_connectivity_layer = "IK1",
      candidate_structure_organism_occurrence_closest = "Sp1"
    )
  )
  expect_equal(nrow(lkp_no_doi), 1L)
  expect_identical(
    lkp_no_doi$candidate_structure_organism_occurrence_reference,
    ""
  )

  # .build_organism_lookup where IK1 and Sp2 exist in df separately (not as a pair)
  expect_equal(
    nrow(.build_organism_lookup(
      tidytable::tidytable(
        structure_inchikey_connectivity_layer = "IK1",
        organism_taxonomy_08species = "Sp2",
        reference_doi = "10.1000/cross"
      ),
      tidytable::tidytable(
        feature_id = c("1", "2"),
        candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2"),
        candidate_structure_organism_occurrence_closest = c("Sp1", "Sp2")
      )
    )),
    0L
  )

  # 5. .build_feature_consensus_table with 0-row input and character score/rank
  cons_0 <- .build_feature_consensus_table(df_empty, model)
  expect_equal(nrow(cons_0), 0L)
  expect_true("annotation_note" %in% names(cons_0))

  cons_chr <- .build_feature_consensus_table(
    tidytable::tidytable(
      feature_id = c("1", "1"),
      score_weighted_chemo = c("0.5", "0.9"),
      rank_final = c("2", "1")
    ),
    model
  )
  expect_equal(nrow(cons_chr), 1L)
})
