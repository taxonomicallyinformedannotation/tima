library(testthat)

validate_clean_chemo_inputs <- validate_clean_chemo_inputs
filter_ms1_annotations <- filter_ms1_annotations
rank_and_deduplicate <- rank_and_deduplicate
apply_percentile_filter <- apply_percentile_filter
count_candidates <- count_candidates
compute_classyfire_taxonomy <- compute_classyfire_taxonomy
compute_npclassifier_taxonomy <- compute_npclassifier_taxonomy
compute_candidate_M <- compute_candidate_M

base_annot <- function() {
  tidytable::tidytable(
    feature_id = c("F1", "F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2", "IK3"),
    score_biological = c("0.2", "0.8", "0.6"),
    score_chemical = c("0.3", "0.7", "0.4"),
    score_weighted_chemo = c("0.5", "0.9", "0.6"),
    candidate_score_pseudo_initial = c("0.4", "0.8", "0.2"),
    candidate_score_similarity = c(NA, 0.9, NA),
    candidate_score_sirius_csi = c(NA, NA, NA),
    candidate_score_sirius_confidence = c(NA, 0.5, 0)
  )
}

test_that("validate_clean_chemo_inputs accepts valid parameters", {
  expect_invisible(validate_clean_chemo_inputs(
    annot_table_wei_chemo = base_annot(),
    candidates_final = 1,
    best_percentile = 0.8,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "OR",
    compounds_names = TRUE,
    high_evidence = FALSE,
    remove_ties = FALSE,
    summarize = TRUE,
    max_per_score = 10,
    score_chemical_cla_kingdom = 0.1,
    score_chemical_cla_superclass = 0.1,
    score_chemical_cla_class = 0.1,
    score_chemical_cla_parent = 0.1,
    score_chemical_npc_pathway = 0.1,
    score_chemical_npc_superclass = 0.1,
    score_chemical_npc_class = 0.1
  ))
})

test_that("validate_clean_chemo_inputs errors on missing required columns", {
  bad <- tidytable::tidytable(feature_id = "F1")
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = bad,
      candidates_final = 1,
      best_percentile = 0.8,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = TRUE,
      max_per_score = 10,
      score_chemical_cla_kingdom = NULL,
      score_chemical_cla_superclass = NULL,
      score_chemical_cla_class = NULL,
      score_chemical_cla_parent = NULL,
      score_chemical_npc_pathway = NULL,
      score_chemical_npc_superclass = NULL,
      score_chemical_npc_class = NULL
    ),
    class = "tima_validation_error"
  )
})

test_that("filter_ms1_annotations honors OR and AND modes", {
  df <- base_annot()
  out_or <- filter_ms1_annotations(df, 0.7, 0.7, "OR")
  out_and <- filter_ms1_annotations(df, 0.7, 0.7, "AND")
  expect_true(nrow(out_or) >= nrow(out_and))
})

test_that("rank_and_deduplicate creates rank columns and deduplicates IK per feature", {
  df <- base_annot() |>
    tidytable::bind_rows(base_annot()[1, ])
  ranked <- rank_and_deduplicate(df)
  expect_true(all(c("rank_initial", "rank_final") %in% names(ranked)))
  combos <- paste(
    ranked$feature_id,
    ranked$candidate_structure_inchikey_connectivity_layer
  )
  expect_equal(length(unique(combos)), nrow(ranked))
})

test_that("rank_and_deduplicate breaks score ties with deterministic tie-breakers", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2"),
    score_weighted_chemo = c(0.8, 0.8),
    candidate_score_pseudo_initial = c(0.4, 0.7),
    candidate_score_similarity = c(0.5, 0.6)
  )

  ranked <- rank_and_deduplicate(df)

  # Equal scores share rank 1 (dense rank).
  expect_equal(
    ranked$rank_final[
      ranked$candidate_structure_inchikey_connectivity_layer == "IK2"
    ],
    1L
  )
  expect_equal(
    ranked$rank_final[
      ranked$candidate_structure_inchikey_connectivity_layer == "IK1"
    ],
    1L
  )
  # Both tied at rank 1
  expect_equal(ranked$rank_final, c(1L, 1L))

  # rank_initial is based on candidate_score_pseudo_initial (descending)
  # IK2 (0.7) > IK1 (0.4), so IK2 gets rank_initial 1, IK1 gets rank_initial 2
  expect_equal(
    ranked$rank_initial[
      ranked$candidate_structure_inchikey_connectivity_layer == "IK2"
    ],
    1L
  )
  expect_equal(
    ranked$rank_initial[
      ranked$candidate_structure_inchikey_connectivity_layer == "IK1"
    ],
    2L
  )
})

test_that("rank_and_deduplicate ranks by score without coverage", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2"),
    score_weighted_chemo = c(0.9, 0.89),
    candidate_score_pseudo_initial = c(0.4, 0.4),
    candidate_score_similarity = c(0.6, 0.6)
  )

  ranked <- rank_and_deduplicate(df)

  expect_equal(
    ranked$rank_final[
      ranked$candidate_structure_inchikey_connectivity_layer == "IK2"
    ],
    2L
  )
  expect_equal(
    ranked$rank_final[
      ranked$candidate_structure_inchikey_connectivity_layer == "IK1"
    ],
    1L
  )
})

test_that("rank_and_deduplicate shares ranks for identical ranking signatures", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2"),
    score_weighted_chemo = c(0.8, 0.8),
    candidate_score_pseudo_initial = c(0.4, 0.4),
    candidate_score_similarity = c(0.6, 0.6)
  )

  ranked <- rank_and_deduplicate(df)

  expect_equal(ranked$rank_final, c(1L, 1L))
  expect_equal(ranked$rank_initial, c(1L, 1L))
})

test_that("rank_and_deduplicate handles empty input", {
  df <- tidytable::tidytable(
    feature_id = character(),
    candidate_structure_inchikey_connectivity_layer = character(),
    score_weighted_chemo = numeric(),
    candidate_score_pseudo_initial = numeric()
  )

  ranked <- rank_and_deduplicate(df)

  expect_equal(nrow(ranked), 0L)
  expect_true(all(c("rank_initial", "rank_final") %in% names(ranked)))
})

test_that("apply_percentile_filter keeps top candidates per feature", {
  ranked <- rank_and_deduplicate(base_annot())
  top <- apply_percentile_filter(ranked, best_percentile = 0.9)
  expect_true(nrow(top) > 0)
  expect_true(all(top$score_weighted_chemo <= 1))
})

test_that("apply_percentile_filter keeps rank_final==1 even below percentile cutoff", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F2"),
    score_weighted_chemo = c(0.40, 0.95, 0.30, 0.90),
    rank_final = c(1L, 2L, 1L, 2L),
    cluster_consensus_promoted_from_anchor = c(TRUE, FALSE, FALSE, FALSE)
  )

  out <- apply_percentile_filter(df, best_percentile = 0.9)

  # High-scoring rows pass percentile; rank-1 rows are preserved as anchors.
  expect_true(any(out$feature_id == "F1" & out$rank_final == 1L))
  expect_true(any(out$feature_id == "F1" & out$score_weighted_chemo == 0.95))
  expect_true(any(out$feature_id == "F2" & out$rank_final == 1L))
})

test_that("count_candidates returns evaluated and best counts", {
  ranked <- rank_and_deduplicate(base_annot())
  top <- apply_percentile_filter(ranked, 0.9)
  counts <- count_candidates(ranked, top)
  expect_true(all(
    c("candidates_evaluated", "candidates_best") %in% names(counts)
  ))
  expect_true(all(counts$candidates_evaluated >= counts$candidates_best))
})

test_that("compute_classyfire_taxonomy picks best weighted level", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    feature_pred_tax_cla_01kin_val = c("KingdomA", "notClassified"),
    feature_pred_tax_cla_01kin_score = c(0.6, 0.1),
    feature_pred_tax_cla_02sup_val = c("SuperclassA", "SuperclassB"),
    feature_pred_tax_cla_02sup_score = c(0.8, 0.9),
    feature_pred_tax_cla_03cla_val = c("ClassA", "ClassB"),
    feature_pred_tax_cla_03cla_score = c(0.7, 0.2),
    feature_pred_tax_cla_04dirpar_val = c("ParentA", "ParentB"),
    feature_pred_tax_cla_04dirpar_score = c(0.5, 0.3)
  )
  w <- list(w_cla_kin = 0.1, w_cla_sup = 0.7, w_cla_cla = 0.1, w_cla_par = 0.1)
  out <- compute_classyfire_taxonomy(df, w)
  expect_true(nrow(out) >= 1)
  expect_true(all(
    c("label_classyfire_predicted", "score_classyfire") %in% names(out)
  ))
})

test_that("compute_npclassifier_taxonomy picks best weighted level", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    feature_pred_tax_npc_01pat_val = c("PathA", "PathB"),
    feature_pred_tax_npc_01pat_score = c(0.8, 0.1),
    feature_pred_tax_npc_02sup_val = c("SupA", "notClassified"),
    feature_pred_tax_npc_02sup_score = c(0.7, 0.2),
    feature_pred_tax_npc_03cla_val = c("ClassA", "ClassB"),
    feature_pred_tax_npc_03cla_score = c(0.4, 0.9)
  )
  w <- list(w_npc_pat = 0.6, w_npc_sup = 0.3, w_npc_cla = 0.1)
  out <- compute_npclassifier_taxonomy(df, w)
  expect_true(nrow(out) >= 1)
  expect_true(all(
    c("label_npclassifier_predicted", "score_npclassifier") %in% names(out)
  ))
})

test_that("compute_candidate_M handles valid and invalid adduct strings", {
  mz <- c(100, 200, 300)
  add <- c("[M+H]+", "[M+Na]+", "bad-adduct")
  out <- compute_candidate_M(mz, add)
  expect_length(out, 3)
  expect_true(is.finite(out[1]))
  expect_true(is.finite(out[2]))
  expect_true(is.na(out[3]))
})

test_that("compute_candidate_M returns numeric(0) for empty input", {
  out <- compute_candidate_M(numeric(0), character(0))
  expect_equal(out, numeric(0))
})

test_that("validate_clean_chemo_inputs rejects invalid parameter branches", {
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = base_annot(),
      candidates_final = 0,
      best_percentile = 0.8,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = TRUE,
      max_per_score = 10,
      score_chemical_cla_kingdom = NULL,
      score_chemical_cla_superclass = NULL,
      score_chemical_cla_class = NULL,
      score_chemical_cla_parent = NULL,
      score_chemical_npc_pathway = NULL,
      score_chemical_npc_superclass = NULL,
      score_chemical_npc_class = NULL
    ),
    "candidates_final must be a positive integer",
    class = "tima_validation_error"
  )

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = base_annot(),
      candidates_final = 1,
      best_percentile = 1.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = TRUE,
      max_per_score = 10,
      score_chemical_cla_kingdom = NULL,
      score_chemical_cla_superclass = NULL,
      score_chemical_cla_class = NULL,
      score_chemical_cla_parent = NULL,
      score_chemical_npc_pathway = NULL,
      score_chemical_npc_superclass = NULL,
      score_chemical_npc_class = NULL
    ),
    "best_percentile must be between 0 and 1",
    class = "tima_validation_error"
  )

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = base_annot(),
      candidates_final = 1,
      best_percentile = 0.8,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "XOR",
      compounds_names = TRUE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = TRUE,
      max_per_score = 10,
      score_chemical_cla_kingdom = NULL,
      score_chemical_cla_superclass = NULL,
      score_chemical_cla_class = NULL,
      score_chemical_cla_parent = NULL,
      score_chemical_npc_pathway = NULL,
      score_chemical_npc_superclass = NULL,
      score_chemical_npc_class = NULL
    ),
    "minimal_ms1_condition must be 'OR' or 'AND'",
    class = "tima_validation_error"
  )

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = base_annot(),
      candidates_final = 1,
      best_percentile = 0.8,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = "TRUE",
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = TRUE,
      max_per_score = 10,
      score_chemical_cla_kingdom = NULL,
      score_chemical_cla_superclass = NULL,
      score_chemical_cla_class = NULL,
      score_chemical_cla_parent = NULL,
      score_chemical_npc_pathway = NULL,
      score_chemical_npc_superclass = NULL,
      score_chemical_npc_class = NULL
    ),
    "must be logical",
    class = "tima_validation_error"
  )

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = base_annot(),
      candidates_final = 1,
      best_percentile = 0.8,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = TRUE,
      max_per_score = 0,
      score_chemical_cla_kingdom = NULL,
      score_chemical_cla_superclass = NULL,
      score_chemical_cla_class = NULL,
      score_chemical_cla_parent = NULL,
      score_chemical_npc_pathway = NULL,
      score_chemical_npc_superclass = NULL,
      score_chemical_npc_class = NULL
    ),
    "max_per_score must be a positive integer",
    class = "tima_validation_error"
  )

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = base_annot(),
      candidates_final = 1,
      best_percentile = 0.8,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = TRUE,
      max_per_score = 10,
      score_chemical_cla_kingdom = "bad",
      score_chemical_cla_superclass = NULL,
      score_chemical_cla_class = NULL,
      score_chemical_cla_parent = NULL,
      score_chemical_npc_pathway = NULL,
      score_chemical_npc_superclass = NULL,
      score_chemical_npc_class = NULL
    ),
    "taxonomy weight parameters must be numeric",
    class = "tima_validation_error"
  )

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = base_annot(),
      candidates_final = 1,
      best_percentile = 0.8,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = TRUE,
      max_per_score = 10,
      score_chemical_cla_kingdom = 1.2,
      score_chemical_cla_superclass = NULL,
      score_chemical_cla_class = NULL,
      score_chemical_cla_parent = NULL,
      score_chemical_npc_pathway = NULL,
      score_chemical_npc_superclass = NULL,
      score_chemical_npc_class = NULL
    ),
    "taxonomy weights must be within \\[0,1\\]",
    class = "tima_validation_error"
  )
})

test_that("filter_ms1_annotations works when confidence column is absent", {
  df <- base_annot() |> tidytable::select(-candidate_score_sirius_confidence)
  out <- filter_ms1_annotations(
    df,
    minimal_ms1_bio = 0.7,
    minimal_ms1_chemo = 0.7,
    minimal_ms1_condition = "AND"
  )
  expect_true(nrow(out) >= 1L)
})

test_that("count_candidates keeps evaluated counts when percentile table is empty", {
  ranked <- rank_and_deduplicate(base_annot())
  empty_best <- ranked |> tidytable::slice(0)
  counts <- count_candidates(ranked, empty_best)

  expect_true(!anyNA(counts$candidates_evaluated))
  expect_true(all(is.na(counts$candidates_best)))
})

test_that("compute_classyfire_taxonomy drops notClassified-only rows", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_cla_01kin_val = "notClassified",
    feature_pred_tax_cla_01kin_score = 0.9,
    feature_pred_tax_cla_02sup_val = "notClassified",
    feature_pred_tax_cla_02sup_score = 0.9,
    feature_pred_tax_cla_03cla_val = "notClassified",
    feature_pred_tax_cla_03cla_score = 0.9,
    feature_pred_tax_cla_04dirpar_val = "notClassified",
    feature_pred_tax_cla_04dirpar_score = 0.9
  )
  out <- compute_classyfire_taxonomy(
    df,
    list(w_cla_kin = 1, w_cla_sup = 1, w_cla_cla = 1, w_cla_par = 1)
  )
  expect_equal(nrow(out), 0L)
})

test_that("compute_npclassifier_taxonomy drops notClassified-only rows", {
  df <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_npc_01pat_val = "notClassified",
    feature_pred_tax_npc_01pat_score = 0.9,
    feature_pred_tax_npc_02sup_val = "notClassified",
    feature_pred_tax_npc_02sup_score = 0.9,
    feature_pred_tax_npc_03cla_val = "notClassified",
    feature_pred_tax_npc_03cla_score = 0.9
  )
  out <- compute_npclassifier_taxonomy(
    df,
    list(w_npc_pat = 1, w_npc_sup = 1, w_npc_cla = 1)
  )
  expect_equal(nrow(out), 0L)
})

test_that(".validate_features_dataframe validates type and feature_id column", {
  validator <- get(".validate_features_dataframe", envir = asNamespace("tima"))

  expect_error(
    validator(list(feature_id = "F1")),
    "must be data frames",
    class = "tima_validation_error"
  )

  expect_error(
    validator(data.frame(x = 1)),
    "must contain feature_id",
    class = "tima_validation_error"
  )

  expect_no_error(validator(data.frame(feature_id = "F1")))
})

test_that("compute_candidate_M returns NA vector when inputs are unusable", {
  out <- compute_candidate_M(
    mz = c(NA, -10, 0),
    adduct_string = c("[M+H]+", "[M+Na]+", "")
  )
  expect_length(out, 3L)
  expect_true(all(is.na(out)))
})

test_that("compute_classyfire_taxonomy and compute_npclassifier_taxonomy break ties toward most specific level", {
  df_cla <- tidytable::tidytable(
    feature_id = c("F_par", "F_cla", "F_sup"),
    feature_pred_tax_cla_01kin_val = c("Kin", "Kin", "Kin"),
    feature_pred_tax_cla_01kin_score = c(0.8, 0.8, 0.8),
    feature_pred_tax_cla_02sup_val = c("Sup", "Sup", "Sup"),
    feature_pred_tax_cla_02sup_score = c(0.8, 0.8, 0.8),
    feature_pred_tax_cla_03cla_val = c("Cla", "Cla", "notClassified"),
    feature_pred_tax_cla_03cla_score = c(0.8, 0.8, 0.9),
    feature_pred_tax_cla_04dirpar_val = c(
      "Par",
      "notClassified",
      NA_character_
    ),
    feature_pred_tax_cla_04dirpar_score = c(0.8, 0.9, 0.9)
  )
  w_cla <- list(w_cla_kin = 1, w_cla_sup = 1, w_cla_cla = 1, w_cla_par = 1)
  out_cla <- compute_classyfire_taxonomy(df_cla, w_cla)
  expect_identical(out_cla$label_classyfire_predicted, c("Par", "Cla", "Sup"))
  expect_equal(out_cla$score_classyfire, c(0.8, 0.8, 0.8))

  df_npc <- tidytable::tidytable(
    feature_id = c("F_cla", "F_sup"),
    feature_pred_tax_npc_01pat_val = c("Pat", "Pat"),
    feature_pred_tax_npc_01pat_score = c(0.7, 0.7),
    feature_pred_tax_npc_02sup_val = c("Sup", "Sup"),
    feature_pred_tax_npc_02sup_score = c(0.7, 0.7),
    feature_pred_tax_npc_03cla_val = c("Cla", "notClassified"),
    feature_pred_tax_npc_03cla_score = c(0.7, 0.9)
  )
  w_npc <- list(w_npc_pat = 1, w_npc_sup = 1, w_npc_cla = 1)
  out_npc <- compute_npclassifier_taxonomy(df_npc, w_npc)
  expect_identical(out_npc$label_npclassifier_predicted, c("Cla", "Sup"))
  expect_equal(out_npc$score_npclassifier, c(0.7, 0.7))

  expect_equal(nrow(compute_classyfire_taxonomy(df_cla[0, ], w_cla)), 0L)
  expect_equal(nrow(compute_npclassifier_taxonomy(df_npc[0, ], w_npc)), 0L)
})

test_that("build_mini_taxonomy_table prefers structure label on score equality and clears uncertainty score", {
  df_perc <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    score_weighted_chemo = c(0.80, 0.70),
    candidate_structure_inchikey_connectivity_layer = c("IK1", NA_character_),
    candidate_structure_tax_cla_04dirpar = c(NA_character_, NA_character_),
    candidate_structure_tax_cla_03cla = c("StrClass1", "StrClass2"),
    candidate_structure_tax_npc_03cla = c("StrNpc1", NA_character_),
    feature_pred_tax_cla_03cla_val = c("PredClass1", "PredClass2"),
    feature_pred_tax_cla_03cla_score = c(0.80, 0.75),
    feature_pred_tax_npc_03cla_val = c("PredNpc1", "PredNpc2"),
    feature_pred_tax_npc_03cla_score = c(0.85, 0.60)
  )
  out <- build_mini_taxonomy_table(
    df_percentile = df_perc,
    score_chemical_cla_kingdom = 1,
    score_chemical_cla_superclass = 1,
    score_chemical_cla_class = 1,
    score_chemical_cla_parent = 1,
    score_chemical_npc_pathway = 1,
    score_chemical_npc_superclass = 1,
    score_chemical_npc_class = 1
  )
  expect_identical(out$has_inchikey, c(TRUE, FALSE))
  # F1: cla structure score (0.80) == pred score (0.80) -> structure wins, score_classyfire is NA
  # F2: cla structure score (0.70) < pred score (0.75) -> prediction wins, score_classyfire is 0.75
  expect_identical(out$label_classyfire, c("StrClass1", "PredClass2"))
  expect_equal(out$score_classyfire, c(NA_real_, 0.75))
  # F1: npc structure score (0.80) < pred score (0.85) -> prediction wins
  expect_identical(out$label_npclassifier, c("PredNpc1", "PredNpc2"))
  expect_equal(out$score_npclassifier, c(0.85, 0.60))

  # Edge cases: validate_clean_chemo_inputs minimal_ms1_bio/chemo, 0-row filter_ms1_annotations,
  # missing similarity col, and 1-row rank_and_deduplicate
  df_dummy <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "IK1",
    score_biological = 0.5,
    score_chemical = 0.5,
    score_weighted_chemo = 0.5,
    candidate_score_pseudo_initial = 0.5
  )
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = df_dummy,
      candidates_final = 1L,
      best_percentile = 0.5,
      minimal_ms1_bio = -0.1,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      max_per_score = 1L,
      score_chemical_cla_kingdom = 0.5,
      score_chemical_cla_superclass = 0.5,
      score_chemical_cla_class = 0.5,
      score_chemical_cla_parent = 0.5,
      score_chemical_npc_pathway = 0.5,
      score_chemical_npc_superclass = 0.5,
      score_chemical_npc_class = 0.5
    ),
    "minimal_ms1_bio",
    class = "tima_validation_error"
  )
  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = df_dummy,
      candidates_final = 1L,
      best_percentile = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 1.5,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      max_per_score = 1L,
      score_chemical_cla_kingdom = 0.5,
      score_chemical_cla_superclass = 0.5,
      score_chemical_cla_class = 0.5,
      score_chemical_cla_parent = 0.5,
      score_chemical_npc_pathway = 0.5,
      score_chemical_npc_superclass = 0.5,
      score_chemical_npc_class = 0.5
    ),
    "minimal_ms1_chemo",
    class = "tima_validation_error"
  )
  expect_identical(
    nrow(filter_ms1_annotations(tidytable::tidytable(), 0.5, 0.5, "OR")),
    0L
  )
  expect_identical(
    nrow(filter_ms1_annotations(
      tidytable::tidytable(
        feature_id = "F1",
        score_biological = 0.8,
        score_chemical = 0.8
      ),
      0.5,
      0.5,
      "OR"
    )),
    1L
  )
  rk1 <- rank_and_deduplicate(tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "IK1",
    score_final = 0.9,
    candidate_score_pseudo_initial = 0.8
  ))
  expect_identical(rk1$rank_initial, 1L)
})
