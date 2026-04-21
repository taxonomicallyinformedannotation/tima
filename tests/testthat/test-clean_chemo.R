# Test Suite: clean_chemo ----

library(testthat)

# Test Fixtures ----

#' Create comprehensive test fixture for clean_chemo tests
#' @keywords internal
make_clean_chemo_fixture <- function(
  n_features = 5,
  n_candidates = 10,
  with_ms2 = TRUE,
  with_taxonomy = TRUE,
  seed = 123
) {
  set.seed(seed)

  feature_ids <- sprintf("F%03d", seq_len(n_features))

  # Generate annotations
  annot_table_wei_chemo <- tidytable::tidytable(
    feature_id = rep(feature_ids, each = n_candidates),
    candidate_structure_inchikey_connectivity_layer = paste0(
      "AAAAA",
      sprintf("%09d", seq_len(n_features * n_candidates))
    ),
    candidate_structure_name = paste0(
      "Compound_",
      seq_len(n_features * n_candidates)
    ),
    candidate_structure_smiles_no_stereo = paste0(
      "SMILES_",
      seq_len(n_features * n_candidates)
    ),
    candidate_library = paste0(
      "Library_",
      seq_len(n_features * n_candidates)
    ),
    score_weighted_chemo = runif(n_features * n_candidates, 0, 1),
    score_biological = runif(n_features * n_candidates, 0, 1),
    score_chemical = runif(n_features * n_candidates, 0, 1),
    candidate_adduct = paste0(
      "Adduct_",
      seq_len(n_features * n_candidates)
    ),
    candidate_score_pseudo_initial = runif(n_features * n_candidates, 0, 1),
    candidate_structure_organism_occurrence_closest = paste0(
      "Taxon ",
      seq_len(n_features * n_candidates)
    ),
    candidate_score_similarity = if (with_ms2) {
      c(runif(n_features * (n_candidates - 2), 0, 1), rep(NA, n_features * 2))
    } else {
      rep(NA, n_features * n_candidates)
    },
    candidate_structure_error_mz = rep(NA_real_, n_features * n_candidates),
    candidate_structure_error_rt = rep(NA_real_, n_features * n_candidates),
    candidate_score_sirius_csi = rep(NA_real_, n_features * n_candidates),
    candidate_structure_tax_cla_04dirpar = rep(
      "Par_",
      n_features * n_candidates
    ),
    candidate_structure_tax_cla_03cla = rep(
      "Cla_",
      n_features * n_candidates
    ),
    candidate_structure_tax_cla_02sup = rep(
      "Sup_",
      n_features * n_candidates
    ),
    candidate_structure_tax_cla_01kin = rep(
      "Kin_",
      n_features * n_candidates
    ),
    candidate_structure_tax_npc_03cla = rep(
      "Cla_",
      n_features * n_candidates
    ),
    candidate_structure_tax_npc_02sup = rep(
      "Sup_",
      n_features * n_candidates
    ),
    candidate_structure_tax_npc_01pat = rep(
      "Pat_",
      n_features * n_candidates
    )
  )

  # Add taxonomy columns if requested
  if (with_taxonomy) {
    annot_table_wei_chemo <- annot_table_wei_chemo |>
      tidytable::mutate(
        feature_pred_tax_cla_01kin_val = sample(
          c("Organic compounds", "Lipids", NA, NA),
          n(),
          replace = TRUE
        ),
        feature_pred_tax_cla_01kin_score = runif(n(), 0, 1),
        feature_pred_tax_cla_02sup_val = sample(
          c("Flavonoids", "Alkaloids", NA, NA),
          n(),
          replace = TRUE
        ),
        feature_pred_tax_cla_02sup_score = runif(n(), 0, 1),
        feature_pred_tax_cla_03cla_val = sample(
          c("Class1", "Class2", NA, NA),
          n(),
          replace = TRUE
        ),
        feature_pred_tax_cla_03cla_score = runif(n(), 0, 1),
        feature_pred_tax_cla_04dirpar_val = sample(
          c("Parent1", "Parent2", NA, NA),
          n(),
          replace = TRUE
        ),
        feature_pred_tax_cla_04dirpar_score = runif(n(), 0, 1),
        feature_pred_tax_npc_01pat_val = sample(
          c("Pathway1", "Pathway2", NA, NA),
          n(),
          replace = TRUE
        ),
        feature_pred_tax_npc_01pat_score = runif(n(), 0, 1),
        feature_pred_tax_npc_02sup_val = sample(
          c("NPCSuper1", "NPCSuper2", NA, NA),
          n(),
          replace = TRUE
        ),
        feature_pred_tax_npc_02sup_score = runif(n(), 0, 1),
        feature_pred_tax_npc_03cla_val = sample(
          c("NPCClass1", "NPCClass2", NA, NA),
          n(),
          replace = TRUE
        ),
        feature_pred_tax_npc_03cla_score = runif(n(), 0, 1)
      )
  }

  # Features table
  features_table <- tidytable::tidytable(
    feature_id = feature_ids,
    rt = runif(n_features, 1, 30),
    mz = runif(n_features, 100, 1000),
    feature_spectrum_entropy = runif(n_features, 0, 1),
    feature_spectrum_peaks = sample(10:200, n_features, replace = TRUE)
  )

  # Components table
  components_table <- tidytable::tidytable(
    feature_id = feature_ids,
    component_id = sample(1:3, n_features, replace = TRUE)
  )

  # Structure-organism pairs
  unique_inchikeys <- unique(
    annot_table_wei_chemo$candidate_structure_inchikey_connectivity_layer
  )
  structure_organism_pairs_table <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = rep(unique_inchikeys, each = 2),
    reference_doi = paste0(
      "10.1000/ref",
      seq_along(rep(unique_inchikeys, each = 2))
    ),
    organism_name = paste0(
      "Organism_",
      seq_along(rep(unique_inchikeys, each = 2))
    ),
    organism_taxonomy_ottid = sample(
      c(123, 456),
      length(unique_inchikeys) * 2,
      replace = TRUE
    ),
    organism_taxonomy_01domain = sample(
      c("Eukaryota", "Bacteria"),
      length(unique_inchikeys) * 2,
      replace = TRUE
    ),
    organism_taxonomy_02kingdom = sample(
      c("Plantae", "Fungi"),
      length(unique_inchikeys) * 2,
      replace = TRUE
    ),
    organism_taxonomy_03phylum = sample(
      paste0("Phylum_", 1:5),
      length(unique_inchikeys) * 2,
      replace = TRUE
    ),
    organism_taxonomy_04class = sample(
      paste0("Class_", 1:5),
      length(unique_inchikeys) * 2,
      replace = TRUE
    ),
    organism_taxonomy_05order = sample(
      paste0("Order_", 1:5),
      length(unique_inchikeys) * 2,
      replace = TRUE
    ),
    organism_taxonomy_06family = sample(
      paste0("Family_", 1:5),
      length(unique_inchikeys) * 2,
      replace = TRUE
    ),
    organism_taxonomy_07tribe = sample(
      paste0("Tribe_", 1:5),
      length(unique_inchikeys) * 2,
      replace = TRUE
    ),
    organism_taxonomy_08genus = sample(
      paste0("Genus_", 1:5),
      length(unique_inchikeys) * 2,
      replace = TRUE
    ),
    organism_taxonomy_09species = sample(
      paste0("Species_", 1:5),
      length(unique_inchikeys) * 2,
      replace = TRUE
    ),
    organism_taxonomy_10varietas = sample(
      paste0("Varietas_", 1:5),
      length(unique_inchikeys) * 2,
      replace = TRUE
    )
  )

  list(
    annot_table_wei_chemo = annot_table_wei_chemo,
    features_table = features_table,
    components_table = components_table,
    structure_organism_pairs_table = structure_organism_pairs_table
  )
}

# Unit Tests: Helper Functions ----

# Validation Tests ----
test_that("validate_clean_chemo_inputs works", {
  fixture <- make_clean_chemo_fixture()

  expect_silent(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = fixture$annot_table_wei_chemo,
      candidates_final = 5,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      max_per_score = 7L,
      score_chemical_cla_kingdom = 0.1,
      score_chemical_cla_superclass = 0.2,
      score_chemical_cla_class = 0.3,
      score_chemical_cla_parent = 0.4,
      score_chemical_npc_pathway = 0.1,
      score_chemical_npc_superclass = 0.2,
      score_chemical_npc_class = 0.3
    )
  )

  expect_error(
    validate_clean_chemo_inputs(
      annot_table_wei_chemo = 123,
      candidates_final = 5,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      max_per_score = 7L
    ),
    "data frame",
    class = "tima_validation_error"
  )
})

# Filtering Tests ----
test_that("filter_ms1_annotations filters with OR condition", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3", "F4"),
    score_biological = c(0.9, 0.1, 0.9, 0.1),
    score_chemical = c(0.1, 0.9, 0.1, 0.1),
    candidate_score_similarity = c(NA, NA, NA, 0.8),
    candidate_score_sirius_csi = c(NA, NA, NA, NA)
  )

  result <- filter_ms1_annotations(
    annot_table_wei_chemo = ann,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "OR"
  )

  # F1: bio=0.9 >= 0.5 OR chem=0.1 < 0.5 → PASS (bio meets)
  # F2: bio=0.1 < 0.5 OR chem=0.9 >= 0.5 → PASS (chem meets)
  # F3: bio=0.9 >= 0.5 OR chem=0.1 < 0.5 → PASS (bio meets)
  # F4: has MS2 (similarity=0.8) → PASS
  expect_equal(nrow(result), 4)
})

test_that("filter_ms1_annotations filters with AND condition", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.9, 0.1, 0.9),
    score_chemical = c(0.1, 0.9, 0.9),
    candidate_score_similarity = c(NA, NA, NA),
    candidate_score_sirius_csi = c(NA, NA, NA)
  )

  result <- filter_ms1_annotations(
    annot_table_wei_chemo = ann,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "AND"
  )

  # F1: bio=0.9 AND chem=0.1 → FAIL
  # F2: bio=0.1 AND chem=0.9 → FAIL
  # F3: bio=0.9 AND chem=0.9 → PASS
  expect_equal(nrow(result), 1)
  expect_equal(result$feature_id, "F3")
})

test_that("filter_ms1_annotations keeps all MS2 annotations regardless of scores", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    score_biological = c(0.1, 0.1, 0.1),
    score_chemical = c(0.1, 0.1, 0.1),
    candidate_score_similarity = c(0.8, NA, NA),
    candidate_score_sirius_csi = c(NA, 0.7, NA)
  )

  result <- filter_ms1_annotations(
    annot_table_wei_chemo = ann,
    minimal_ms1_bio = 0.9,
    minimal_ms1_chemo = 0.9,
    minimal_ms1_condition = "AND"
  )

  # F1 and F2 have MS2 data → should pass
  # F3 has no MS2 and fails both score thresholds → should fail
  expect_equal(nrow(result), 2)
  expect_true(all(c("F1", "F2") %in% result$feature_id))
})

test_that("filter_ms1_annotations handles character scores correctly", {
  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    score_biological = c("0.9", "0.1"),
    score_chemical = c("0.1", "0.9"),
    candidate_score_similarity = c(NA, NA),
    candidate_score_sirius_csi = c(NA, NA)
  )

  result <- filter_ms1_annotations(
    annot_table_wei_chemo = ann,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "OR"
  )

  expect_equal(nrow(result), 2)
})

# Ranking Tests ----
test_that("rank_and_deduplicate ranks candidates correctly", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C"),
    score_weighted_chemo = c("0.9", "0.8", "0.7"),
    candidate_score_pseudo_initial = c("0.85", "0.75", "0.80")
  )

  result <- rank_and_deduplicate(df)

  expect_true("rank_initial" %in% names(result))
  expect_true("rank_final" %in% names(result))
  expect_equal(nrow(result), 3) # All unique structures kept

  # Check ranking order
  expect_equal(
    result$rank_final[
      result$candidate_structure_inchikey_connectivity_layer == "A"
    ],
    1
  )
  expect_equal(
    result$rank_final[
      result$candidate_structure_inchikey_connectivity_layer == "B"
    ],
    2
  )
  expect_equal(
    result$rank_final[
      result$candidate_structure_inchikey_connectivity_layer == "C"
    ],
    3
  )
})

test_that("rank_and_deduplicate removes duplicate structures", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    candidate_structure_inchikey_connectivity_layer = c("A", "A", "B"),
    score_weighted_chemo = c("0.9", "0.8", "0.7"),
    candidate_score_pseudo_initial = c("0.85", "0.75", "0.80")
  )

  result <- rank_and_deduplicate(df)

  # Should keep only best scoring instance of "A" and "B"
  expect_equal(nrow(result), 2)
  expect_true(all(
    c("A", "B") %in% result$candidate_structure_inchikey_connectivity_layer
  ))
})

test_that("rank_and_deduplicate handles multiple features", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F2"),
    candidate_structure_inchikey_connectivity_layer = c("A", "B", "C", "D"),
    score_weighted_chemo = c("0.9", "0.8", "0.7", "0.6"),
    candidate_score_pseudo_initial = c("0.9", "0.8", "0.7", "0.6")
  )

  result <- rank_and_deduplicate(df)

  expect_equal(nrow(result), 4)
  # Each feature should have its own ranking starting from 1
  expect_true(all(result$rank_final[result$feature_id == "F1"] %in% c(1, 2)))
  expect_true(all(result$rank_final[result$feature_id == "F2"] %in% c(1, 2)))
})

# Percentile Filtering Tests ----
test_that("apply_percentile_filter keeps top percentile", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F1"),
    score_weighted_chemo = c("1.0", "0.9", "0.5", "0.1")
  )

  result <- apply_percentile_filter(df, best_percentile = 0.9)

  # 0.9 * 1.0 = 0.9 threshold
  # Keep: 1.0, 0.9
  # Filter: 0.5, 0.1
  expect_equal(nrow(result), 2)
})

test_that("apply_percentile_filter handles multiple features independently", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F2"),
    score_weighted_chemo = c("1.0", "0.5", "0.8", "0.4")
  )

  result <- apply_percentile_filter(df, best_percentile = 0.9)

  # F1: threshold = 0.9 * 1.0 = 0.9 → keep 1.0 only
  # F2: threshold = 0.9 * 0.8 = 0.72 → keep 0.8 only
  expect_equal(nrow(result), 2)
})

# test_that("apply_percentile_filter with percentile = 1.0 keeps all", {
#   df <- tidytable::tidytable(
#     feature_id = c("F1", "F1", "F1"),
#     score_weighted_chemo = c("1.0", "0.5", "0.1")
#   )
#
#   result <- apply_percentile_filter(df, best_percentile = 1.0)
#   expect_equal(nrow(result), 3)
# })

test_that("apply_percentile_filter with percentile = 0.0 keeps all", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    score_weighted_chemo = c("1.0", "0.5", "0.1")
  )

  result <- apply_percentile_filter(df, best_percentile = 0.0)
  expect_equal(nrow(result), 3)
})

# Counting Tests ----
test_that("count_candidates counts correctly", {
  df_ranked <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1", "F2", "F2")
  )

  df_percentile <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2")
  )

  result <- count_candidates(df_ranked, df_percentile)

  expect_equal(nrow(result), 2)
  expect_true(all(
    c("feature_id", "candidates_evaluated", "candidates_best") %in%
      names(result)
  ))

  f1 <- result[result$feature_id == "F1", ]
  expect_equal(f1$candidates_evaluated, 3)
  expect_equal(f1$candidates_best, 2)
})

test_that("count_candidates handles features not in percentile table", {
  df_ranked <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F2")
  )

  df_percentile <- tidytable::tidytable(
    feature_id = c("F1", "F1")
  )

  result <- count_candidates(df_ranked, df_percentile)

  expect_equal(nrow(result), 2)
  expect_true(is.na(result$candidates_best[result$feature_id == "F2"]))
})

# Taxonomy Tests ----
test_that("compute_classyfire_taxonomy selects highest weighted level", {
  weights <- list(
    w_cla_kin = 0.1,
    w_cla_sup = 0.2,
    w_cla_cla = 0.3,
    w_cla_par = 0.4
  )

  df_pred_tax <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_cla_01kin_val = "Kingdom1",
    feature_pred_tax_cla_01kin_score = "0.8",
    feature_pred_tax_cla_02sup_val = "Super1",
    feature_pred_tax_cla_02sup_score = "0.9",
    feature_pred_tax_cla_03cla_val = "Class1",
    feature_pred_tax_cla_03cla_score = "0.7",
    feature_pred_tax_cla_04dirpar_val = "Parent1",
    feature_pred_tax_cla_04dirpar_score = "0.95"
  )

  result <- compute_classyfire_taxonomy(df_pred_tax, weights)

  # Parent has highest weight (0.4) with score 0.95
  expect_equal(result$label_classyfire_predicted, "Parent1")
  expect_equal(as.numeric(result$score_classyfire), 0.95)
})

test_that("compute_classyfire_taxonomy handles NA values", {
  weights <- list(
    w_cla_kin = 0.1,
    w_cla_sup = 0.2,
    w_cla_cla = 0.3,
    w_cla_par = 0.4
  )

  df_pred_tax <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_cla_01kin_val = "Kingdom1",
    feature_pred_tax_cla_01kin_score = "0.8",
    feature_pred_tax_cla_02sup_val = NA_character_,
    feature_pred_tax_cla_02sup_score = NA_character_,
    feature_pred_tax_cla_03cla_val = NA_character_,
    feature_pred_tax_cla_03cla_score = NA_character_,
    feature_pred_tax_cla_04dirpar_val = NA_character_,
    feature_pred_tax_cla_04dirpar_score = NA_character_
  )

  result <- compute_classyfire_taxonomy(df_pred_tax, weights)

  # Only kingdom available
  expect_equal(result$label_classyfire_predicted, "Kingdom1")
  expect_equal(as.numeric(result$score_classyfire), 0.8)
})

test_that("compute_classyfire_taxonomy filters empty labels", {
  weights <- list(
    w_cla_kin = 0.1,
    w_cla_sup = 0.2,
    w_cla_cla = 0.3,
    w_cla_par = 0.4
  )

  df_pred_tax <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    feature_pred_tax_cla_01kin_val = c("Kingdom1", NA_character_),
    feature_pred_tax_cla_01kin_score = c("0.8", "0.9"),
    feature_pred_tax_cla_02sup_val = c(NA, NA),
    feature_pred_tax_cla_02sup_score = c(NA, NA),
    feature_pred_tax_cla_03cla_val = c(NA, NA),
    feature_pred_tax_cla_03cla_score = c(NA, NA),
    feature_pred_tax_cla_04dirpar_val = c(NA, NA),
    feature_pred_tax_cla_04dirpar_score = c(NA, NA)
  )

  result <- compute_classyfire_taxonomy(df_pred_tax, weights)

  # Only F1 should remain (F2 has NA label)
  expect_equal(nrow(result), 1)
  expect_equal(result$feature_id, "F1")
})

test_that("compute_classyfire_taxonomy ignores notClassified at all levels", {
  weights <- list(
    w_cla_kin = 0.4,
    w_cla_sup = 0.3,
    w_cla_cla = 0.2,
    w_cla_par = 0.1
  )

  df_pred_tax <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_cla_01kin_val = "Kingdom1",
    feature_pred_tax_cla_01kin_score = "0.8",
    feature_pred_tax_cla_02sup_val = "Super1",
    feature_pred_tax_cla_02sup_score = "0.7",
    feature_pred_tax_cla_03cla_val = "notClassified",
    feature_pred_tax_cla_03cla_score = "0.99",
    feature_pred_tax_cla_04dirpar_val = "notClassified",
    feature_pred_tax_cla_04dirpar_score = "0.99"
  )

  result <- compute_classyfire_taxonomy(df_pred_tax, weights)

  expect_equal(result$label_classyfire_predicted, "Kingdom1")
  expect_equal(as.numeric(result$score_classyfire), 0.8)
})

test_that("compute_classyfire_taxonomy drops rows when all levels are notClassified", {
  weights <- list(
    w_cla_kin = 0.1,
    w_cla_sup = 0.2,
    w_cla_cla = 0.3,
    w_cla_par = 0.4
  )

  df_pred_tax <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_cla_01kin_val = "notClassified",
    feature_pred_tax_cla_01kin_score = "0.8",
    feature_pred_tax_cla_02sup_val = "notClassified",
    feature_pred_tax_cla_02sup_score = "0.7",
    feature_pred_tax_cla_03cla_val = "notClassified",
    feature_pred_tax_cla_03cla_score = "0.6",
    feature_pred_tax_cla_04dirpar_val = "notClassified",
    feature_pred_tax_cla_04dirpar_score = "0.5"
  )

  result <- compute_classyfire_taxonomy(df_pred_tax, weights)
  expect_equal(nrow(result), 0)
})

test_that("compute_npclassifier_taxonomy selects highest weighted level", {
  weights <- list(
    w_npc_pat = 0.3,
    w_npc_sup = 0.2,
    w_npc_cla = 0.1
  )

  df_pred_tax <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_npc_01pat_val = "Pathway1",
    feature_pred_tax_npc_01pat_score = "0.95",
    feature_pred_tax_npc_02sup_val = "Super1",
    feature_pred_tax_npc_02sup_score = "0.8",
    feature_pred_tax_npc_03cla_val = "Class1",
    feature_pred_tax_npc_03cla_score = "0.7"
  )

  result <- compute_npclassifier_taxonomy(df_pred_tax, weights)

  # Pathway has highest weight (0.3) with score 0.95
  expect_equal(result$label_npclassifier_predicted, "Pathway1")
  expect_equal(as.numeric(result$score_npclassifier), 0.95)
})

test_that("compute_npclassifier_taxonomy handles NA values", {
  weights <- list(
    w_npc_pat = 0.3,
    w_npc_sup = 0.2,
    w_npc_cla = 0.1
  )

  df_pred_tax <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_npc_01pat_val = NA_character_,
    feature_pred_tax_npc_01pat_score = NA_character_,
    feature_pred_tax_npc_02sup_val = "Super1",
    feature_pred_tax_npc_02sup_score = "0.8",
    feature_pred_tax_npc_03cla_val = NA_character_,
    feature_pred_tax_npc_03cla_score = NA_character_
  )

  result <- compute_npclassifier_taxonomy(df_pred_tax, weights)

  # Only superclass available
  expect_equal(result$label_npclassifier_predicted, "Super1")
  expect_equal(as.numeric(result$score_npclassifier), 0.8)
})

test_that("compute_npclassifier_taxonomy ignores notClassified at all levels", {
  weights <- list(
    w_npc_pat = 0.5,
    w_npc_sup = 0.3,
    w_npc_cla = 0.1
  )

  df_pred_tax <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_npc_01pat_val = "Pathway1",
    feature_pred_tax_npc_01pat_score = "0.95",
    feature_pred_tax_npc_02sup_val = "Super1",
    feature_pred_tax_npc_02sup_score = "0.9",
    feature_pred_tax_npc_03cla_val = "notClassified",
    feature_pred_tax_npc_03cla_score = "0.2"
  )

  result <- compute_npclassifier_taxonomy(df_pred_tax, weights)

  # notClassified class is ignored; pathway wins among valid levels.
  expect_equal(result$label_npclassifier_predicted, "Pathway1")
  expect_equal(as.numeric(result$score_npclassifier), 0.95)
})

test_that("compute_npclassifier_taxonomy drops rows when all levels are notClassified", {
  weights <- list(
    w_npc_pat = 0.3,
    w_npc_sup = 0.2,
    w_npc_cla = 0.1
  )

  df_pred_tax <- tidytable::tidytable(
    feature_id = "F1",
    feature_pred_tax_npc_01pat_val = "notClassified",
    feature_pred_tax_npc_01pat_score = "0.95",
    feature_pred_tax_npc_02sup_val = "notClassified",
    feature_pred_tax_npc_02sup_score = "0.8",
    feature_pred_tax_npc_03cla_val = "notClassified",
    feature_pred_tax_npc_03cla_score = "0.7"
  )

  result <- compute_npclassifier_taxonomy(df_pred_tax, weights)
  expect_equal(nrow(result), 0)
})

# Compound Name Tests ----

test_that("remove_compound_names removes names when compounds_names=FALSE", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    )
  )

  result <- remove_compound_names(results_list, compounds_names = FALSE)

  expect_false("candidate_structure_name" %in% names(result$full))
  expect_false("candidate_structure_name" %in% names(result$filtered))
  expect_false("candidate_structure_name" %in% names(result$mini))
})

test_that("remove_compound_names keeps names when compounds_names=TRUE", {
  results_list <- list(
    full = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    ),
    filtered = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    ),
    mini = tidytable::tidytable(
      feature_id = "F1",
      candidate_structure_name = "Compound A"
    )
  )

  result <- remove_compound_names(results_list, compounds_names = TRUE)

  expect_true("candidate_structure_name" %in% names(result$full))
  expect_true("candidate_structure_name" %in% names(result$filtered))
  expect_true("candidate_structure_name" %in% names(result$mini))
})
# Integration Tests ----
# test_that("clean_chemo handles empty annotation table", {
#
#   # Create proper fixture with empty tables that have the right structure
#   components_table <- tidytable::tidytable(
#     feature_id = character(),
#     component_id = integer()
#   )
#
#   features_table <- tidytable::tidytable(
#     feature_id = character(),
#     rt = numeric(),
#     mz = numeric()
#   )
#
#   structure_organism_pairs_table <- tidytable::tidytable(
#     structure_inchikey_connectivity_layer = character(),
#     organism_name = character()
#   )
#
#   empty <- tidytable::tidytable()
#
#   result <- suppressWarnings(
#     clean_chemo(
#       annot_table_wei_chemo = empty,
#       components_table = components_table,
#       features_table = features_table,
#       structure_organism_pairs_table = structure_organism_pairs_table,
#       candidates_final = 5,
#       best_percentile = 0.9,
#       minimal_ms1_bio = 0.1,
#       minimal_ms1_chemo = 0.1,
#       minimal_ms1_condition = "OR",
#       compounds_names = TRUE,
#       high_confidence = FALSE,
#       remove_ties = FALSE,
#       summarize = FALSE,
#       max_per_score = 7L,
#       score_chemical_cla_kingdom = 0.1,
#       score_chemical_cla_superclass = 0.2,
#       score_chemical_cla_class = 0.3,
#       score_chemical_cla_parent = 0.4,
#       score_chemical_npc_pathway = 0.1,
#       score_chemical_npc_superclass = 0.2,
#       score_chemical_npc_class = 0.3
#     )
#   )
#
#   expect_s3_class(result, "data.frame")
#   expect_equal(nrow(result), 0)
# })

test_that("clean_chemo normal", {
  fixture <- make_clean_chemo_fixture()

  result <- clean_chemo(
    annot_table_wei_chemo = fixture$annot_table_wei_chemo,
    components_table = fixture$components_table,
    features_table = fixture$features_table,
    structure_organism_pairs_table = fixture$structure_organism_pairs_table,
    candidates_final = 5,
    best_percentile = 0.9,
    minimal_ms1_bio = 0.1,
    minimal_ms1_chemo = 0.1,
    minimal_ms1_condition = "OR",
    compounds_names = TRUE,
    high_confidence = FALSE,
    remove_ties = FALSE,
    summarize = FALSE,
    score_chemical_cla_kingdom = 0.1,
    score_chemical_cla_superclass = 0.2,
    score_chemical_cla_class = 0.3,
    score_chemical_cla_parent = 0.4,
    score_chemical_npc_pathway = 0.3,
    score_chemical_npc_superclass = 0.2,
    score_chemical_npc_class = 0.1,
    max_per_score = 7L
  )

  # Verify result structure
  expect_true(is.data.frame(result$full))
  expect_true(is.data.frame(result$filtered))
  expect_true(is.data.frame(result$mini))
})

test_that("clean_chemo validates inputs through main function", {
  fixture <- make_clean_chemo_fixture()

  # Invalid candidates_final
  expect_error(
    clean_chemo(
      annot_table_wei_chemo = fixture$annot_table_wei_chemo,
      components_table = fixture$components_table,
      features_table = fixture$features_table,
      structure_organism_pairs_table = fixture$structure_organism_pairs_table,
      candidates_final = -1,
      best_percentile = 0.9,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "positive integer",
    class = "tima_validation_error"
  )

  # Invalid best_percentile
  expect_error(
    clean_chemo(
      annot_table_wei_chemo = fixture$annot_table_wei_chemo,
      components_table = fixture$components_table,
      features_table = fixture$features_table,
      structure_organism_pairs_table = fixture$structure_organism_pairs_table,
      candidates_final = 5,
      best_percentile = 1.5,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR",
      compounds_names = TRUE,
      high_confidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE
    ),
    "between 0 and 1",
    class = "tima_validation_error"
  )
})
test_that("clean_chemo filters MS1-only annotations correctly with OR condition", {
  fixture <- make_clean_chemo_fixture(
    n_features = 3,
    n_candidates = 5,
    with_ms2 = FALSE
  )
  # Set specific scores for testing
  fixture$annot_table_wei_chemo <- fixture$annot_table_wei_chemo |>
    tidytable::mutate(
      score_biological = rep(c(0.9, 0.1, 0.5, 0.1, 0.9), 3),
      score_chemical = rep(c(0.1, 0.9, 0.1, 0.1, 0.1), 3),
      candidate_score_similarity = NA_real_,
      candidate_score_sirius_csi = NA_real_
    )
  # Mock environment weights
  score_chemical_cla_kingdom <- 0.1
  score_chemical_cla_superclass <- 0.2
  score_chemical_cla_class <- 0.3
  score_chemical_cla_parent <- 0.4
  score_chemical_npc_pathway <- 0.3
  score_chemical_npc_superclass <- 0.2
  score_chemical_npc_class <- 0.1
  result_df <- filter_ms1_annotations(
    annot_table_wei_chemo = fixture$annot_table_wei_chemo,
    minimal_ms1_bio = 0.5,
    minimal_ms1_chemo = 0.5,
    minimal_ms1_condition = "OR"
  )
  # Should keep rows with bio >= 0.5 OR chem >= 0.5
  # Rows 1, 2, 3, 5 per feature
  expect_equal(nrow(result_df), 12) # 4 rows * 3 features
})
test_that("clean_chemo sampling works when max_per_score is exceeded", {
  # Create fixture with many candidates having same score
  fixture <- make_clean_chemo_fixture(n_features = 1, n_candidates = 20)
  # Make 15 candidates have the exact same score
  fixture$annot_table_wei_chemo <- fixture$annot_table_wei_chemo |>
    tidytable::mutate(
      score_weighted_chemo = c(rep(0.9, 15), runif(5, 0.1, 0.8)),
      candidate_score_pseudo_initial = c(rep(0.9, 15), runif(5, 0.1, 0.8))
    )
  df_base <- filter_ms1_annotations(
    annot_table_wei_chemo = fixture$annot_table_wei_chemo,
    minimal_ms1_bio = 0.0,
    minimal_ms1_chemo = 0.0,
    minimal_ms1_condition = "OR"
  )
  df_ranked <- rank_and_deduplicate(df_base)
  # Add group sizes
  df_with_groups <- df_ranked |>
    tidytable::mutate(.n_per_group = n(), .by = c(feature_id, rank_final))
  # Apply sampling logic
  df_keep_all <- df_with_groups |>
    tidytable::filter(.n_per_group <= 7)
  set.seed(42)
  df_sampled <- df_with_groups |>
    tidytable::filter(.n_per_group > 7) |>
    tidytable::slice_sample(n = 7, by = c(feature_id, rank_final)) |>
    tidytable::mutate(
      annotation_note = paste0(
        "Sampled 7 of ",
        .n_per_group,
        " candidates with same score"
      )
    )
  df_result <- tidytable::bind_rows(df_keep_all, df_sampled) |>
    tidytable::select(-.n_per_group)
  # Should have at most 7 candidates with rank_final = 1
  rank1_count <- df_result |>
    tidytable::filter(rank_final == 1) |>
    nrow()
  expect_lte(rank1_count, 7)
})
test_that("clean_chemo handles character score columns", {
  fixture <- make_clean_chemo_fixture(n_features = 2, n_candidates = 3)
  # Convert scores to character (as they might be when loaded from TSV)
  fixture$annot_table_wei_chemo <- fixture$annot_table_wei_chemo |>
    tidytable::mutate(across(starts_with("score_"), as.character))
  # Mock environment weights
  score_chemical_cla_kingdom <- 0.1
  score_chemical_cla_superclass <- 0.2
  score_chemical_cla_class <- 0.3
  score_chemical_cla_parent <- 0.4
  score_chemical_npc_pathway <- 0.3
  score_chemical_npc_superclass <- 0.2
  score_chemical_npc_class <- 0.1
  # Should not error - function should convert to numeric internally
  expect_no_error({
    result_df <- filter_ms1_annotations(
      annot_table_wei_chemo = fixture$annot_table_wei_chemo,
      minimal_ms1_bio = 0.1,
      minimal_ms1_chemo = 0.1,
      minimal_ms1_condition = "OR"
    )
  })
})

test_that("coerce_score_columns converts only existing score columns", {
  ann <- tidytable::tidytable(
    feature_id = "F1",
    score_biological = "0.5",
    score_weighted_chemo = "0.9",
    non_score = "kept"
  )

  result <- coerce_score_columns(ann)

  expect_type(result$score_biological, "double")
  expect_type(result$score_weighted_chemo, "double")
  expect_type(result$non_score, "character")
  expect_equal(result$non_score, "kept")
})

test_that("prepare_ranked_candidates returns expected objects", {
  fixture <- make_clean_chemo_fixture(n_features = 2, n_candidates = 4)
  ann <- coerce_score_columns(fixture$annot_table_wei_chemo)

  out <- prepare_ranked_candidates(
    annot_table_wei_chemo = ann,
    minimal_ms1_bio = 0,
    minimal_ms1_chemo = 0,
    minimal_ms1_condition = "OR",
    best_percentile = 0.9,
    max_per_score = 7L
  )

  expect_true(is.data.frame(out$df_ranked))
  expect_true(is.data.frame(out$df_percentile))
  expect_true(is.data.frame(out$results_candidates))
  expect_true(is.data.frame(out$annotation_notes_lookup))
  expect_type(out$n_sampled_features, "integer")
})
test_that("prepare_ranked_candidates emits cross-feature M-anchor note end-to-end", {
  # Real-world scenario: the same molecular entity is detected as TWO
  # features under different adducts (F1 as [M+H]+, F2 as [M+Na]+).
  # F1 has a clear winner IK_A; F2's tied [M+Na]+ group must inherit
  # IK_A via the cross-feature neutral-mass anchor.
  mz_F1 <- 200
  M_target <- suppressWarnings(calculate_mass_of_m(mz_F1, "[M+H]+"))
  mz_F2 <- suppressWarnings(calculate_mz_from_mass(M_target, "[M+Na]+"))

  ann <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F2", "F2"),
    mz = c(mz_F1, mz_F2, mz_F2, mz_F2),
    candidate_adduct = c("[M+H]+", "[M+Na]+", "[M+Na]+", "[M+Na]+"),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_A",
      "IK_A",
      "IK_B",
      "IK_C"
    ),
    candidate_structure_name = c("A", "A", "B", "C"),
    score_weighted_chemo = c(0.9, 0.7, 0.7, 0.7),
    candidate_score_pseudo_initial = c(0.9, 0.7, 0.7, 0.7),
    score_biological = rep(0.5, 4L),
    score_chemical = rep(0.5, 4L),
    candidate_score_similarity = rep(0.8, 4L),
    candidate_score_sirius_csi = rep(NA_real_, 4L)
  )

  out <- prepare_ranked_candidates(
    annot_table_wei_chemo = ann,
    minimal_ms1_bio = 0,
    minimal_ms1_chemo = 0,
    minimal_ms1_condition = "OR",
    best_percentile = 0,
    max_per_score = 1L
  )

  notes <- out$annotation_notes_lookup
  expect_true(nrow(notes) >= 1L)
  expect_true(any(grepl(
    "sibling feature",
    notes$annotation_note
  )))

  # Only IK_A must survive in F2's tied group.
  f2_rows <- out$df_ranked |>
    tidytable::filter(feature_id == "F2")
  expect_equal(nrow(f2_rows), 1L)
  expect_equal(
    f2_rows$candidate_structure_inchikey_connectivity_layer[[1L]],
    "IK_A"
  )
})

test_that("build_mini_taxonomy_table returns mini taxonomy columns", {
  fixture <- make_clean_chemo_fixture(n_features = 2, n_candidates = 4)
  ann <- coerce_score_columns(fixture$annot_table_wei_chemo)

  ranked <- prepare_ranked_candidates(
    annot_table_wei_chemo = ann,
    minimal_ms1_bio = 0,
    minimal_ms1_chemo = 0,
    minimal_ms1_condition = "OR",
    best_percentile = 0.9,
    max_per_score = 7L
  )

  out <- build_mini_taxonomy_table(
    df_percentile = ranked$df_percentile,
    score_chemical_cla_kingdom = 0.1,
    score_chemical_cla_superclass = 0.2,
    score_chemical_cla_class = 0.3,
    score_chemical_cla_parent = 0.4,
    score_chemical_npc_pathway = 0.3,
    score_chemical_npc_superclass = 0.2,
    score_chemical_npc_class = 0.1
  )

  expect_true(is.data.frame(out))
  expect_true(all(
    c(
      "feature_id",
      "has_inchikey",
      "label_classyfire",
      "label_npclassifier",
      "score_classyfire",
      "score_npclassifier"
    ) %in%
      names(out)
  ))
})

test_that("build_mini_results_table assembles expected mini output", {
  features_table <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    rt = c(1.1, 2.2),
    mz = c(101.1, 202.2)
  )

  df_classes_mini <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    has_inchikey = c(TRUE, FALSE),
    label_classyfire = c("ClassA", "ClassB"),
    label_npclassifier = c("NPCA", "NPCB"),
    score_classyfire = c(NA_real_, 0.8),
    score_npclassifier = c(NA_real_, 0.6)
  )

  results_filtered <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidates_evaluated = c(3L, 2L),
    candidates_best = c(1L, 1L),
    annotation_note = c(NA_character_, "sampled")
  )

  df_filtered <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_name = c("Cmpd1", "Cmpd2"),
    candidate_adduct = c("[M+H]+", "[M+Na]+"),
    candidate_structure_smiles_no_stereo = c("CCO", "CCC"),
    candidate_structure_inchikey_connectivity_layer = c("IK1", NA_character_),
    candidate_library = c("lib1", "lib2"),
    candidate_structure_error_mz = c(0.1, 0.2),
    candidate_structure_error_rt = c(0.01, 0.02),
    candidate_structure_organism_occurrence_closest = c("Org1", "Org2"),
    candidate_structure_tag = c("reported", "predicted"),
    score_weighted_chemo = c(0.9, 0.4)
  )

  out <- build_mini_results_table(
    features_table = features_table,
    df_classes_mini = df_classes_mini,
    results_filtered = results_filtered,
    df_filtered = df_filtered,
    xrefs_table = NULL
  )

  expect_true(is.data.frame(out))
  expect_true(all(
    c(
      "feature_id",
      "rt",
      "mz",
      "label_classyfire",
      "label_npclassifier",
      "label_compound",
      "adduct",
      "smiles_no_stereo",
      "inchikey_connectivity_layer",
      "library",
      "error_mz",
      "error_rt",
      "organism_closest",
      "score",
      "candidates_evaluated",
      "candidates_best",
      "note"
    ) %in%
      names(out)
  ))
})

test_that("sample_candidates_per_group prioritizes non-NA RT errors", {
  df <- tidytable::tidytable(
    feature_id = rep("F1", 4),
    candidate_adduct = rep("[M+H]+", 4),
    rank_final = rep(1L, 4),
    candidate_structure_error_rt = c(NA, 0.1, NA, 0.2),
    candidate_structure_inchikey_connectivity_layer = paste0("IK", 1:4)
  )

  out <- sample_candidates_per_group(df = df, max_per_score = 2L, seed = 1L)

  expect_equal(nrow(out$df), 2L)
  expect_equal(out$n_sampled_features, 1L)
  expect_true(!anyNA(out$df$candidate_structure_error_rt))
  expect_equal(nrow(out$annotation_notes), 1L)
})

test_that("sample_candidates_per_group handles missing RT column", {
  df <- tidytable::tidytable(
    feature_id = rep("F1", 3),
    candidate_adduct = rep("[M+H]+", 3),
    rank_final = rep(1L, 3),
    candidate_structure_inchikey_connectivity_layer = paste0("IK", 1:3)
  )

  out <- sample_candidates_per_group(df = df, max_per_score = 1L, seed = 42L)

  expect_equal(nrow(out$df), 1L)
  expect_equal(out$n_sampled_features, 1L)
  expect_true("annotation_notes" %in% names(out))
})

test_that("sample_candidates_per_group returns empty result for empty input", {
  df <- tidytable::tidytable(
    feature_id = character(0),
    candidate_adduct = character(0),
    rank_final = integer(0)
  )

  out <- sample_candidates_per_group(df = df, max_per_score = 1L)
  expect_equal(nrow(out$df), 0L)
  expect_equal(out$n_sampled_features, 0L)
})

test_that("sample_candidates_per_group keeps cross-feature M-anchor InChIKey", {
  # Two DIFFERENT features represent the same molecular entity under
  # different adducts (same neutral mass M). F1 has an unambiguous winner
  # IK_BEST; F2's tied [M+Na]+ group must inherit IK_BEST via M-anchor.
  mz_F1 <- 200
  M_target <- suppressWarnings(calculate_mass_of_m(mz_F1, "[M+H]+"))
  mz_F2 <- suppressWarnings(calculate_mz_from_mass(M_target, "[M+Na]+"))

  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F2"),
    mz = c(mz_F1, mz_F2, mz_F2),
    candidate_adduct = c("[M+H]+", "[M+Na]+", "[M+Na]+"),
    rank_final = c(1L, 1L, 1L),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_BEST",
      "IK_BEST",
      "IK_OTHER"
    ),
    score_weighted_chemo = c(0.95, 0.7, 0.7),
    candidate_score_pseudo_initial = c(0.9, 0.7, 0.7)
  )

  out <- sample_candidates_per_group(df = df, max_per_score = 1L, seed = 42L)

  kept_f2 <- out$df |>
    tidytable::filter(feature_id == "F2")

  expect_equal(nrow(kept_f2), 1L)
  expect_equal(
    kept_f2$candidate_structure_inchikey_connectivity_layer[[1L]],
    "IK_BEST"
  )
  expect_true(any(grepl(
    "sibling feature",
    out$annotation_notes$annotation_note
  )))
})

test_that("sample_candidates_per_group isolates anchors per neutral mass", {
  # Two independent M-groups: (F1, F2) share M_A; (F3, F4) share M_B.
  # Each M-group's winner propagates only within its own M-group.
  mz_F1 <- 200
  M_A <- suppressWarnings(calculate_mass_of_m(mz_F1, "[M+H]+"))
  mz_F2 <- suppressWarnings(calculate_mz_from_mass(M_A, "[M+Na]+"))
  mz_F3 <- 350
  M_B <- suppressWarnings(calculate_mass_of_m(mz_F3, "[M+H]+"))
  mz_F4 <- suppressWarnings(calculate_mz_from_mass(M_B, "[M+Na]+"))

  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F2", "F3", "F4", "F4"),
    mz = c(mz_F1, mz_F2, mz_F2, mz_F3, mz_F4, mz_F4),
    candidate_adduct = c(
      "[M+H]+",
      "[M+Na]+",
      "[M+Na]+",
      "[M+H]+",
      "[M+Na]+",
      "[M+Na]+"
    ),
    rank_final = rep(1L, 6L),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_A",
      "IK_A",
      "IK_X",
      "IK_B",
      "IK_B",
      "IK_Y"
    ),
    score_weighted_chemo = c(0.95, 0.7, 0.7, 0.95, 0.7, 0.7),
    candidate_score_pseudo_initial = rep(0.8, 6L)
  )

  out <- sample_candidates_per_group(df = df, max_per_score = 1L, seed = 42L)

  kept_tied <- out$df |>
    tidytable::filter(candidate_adduct == "[M+Na]+") |>
    tidytable::arrange(feature_id)

  expect_equal(nrow(kept_tied), 2L)
  expect_equal(
    kept_tied$candidate_structure_inchikey_connectivity_layer,
    c("IK_A", "IK_B")
  )
})

test_that("sample_candidates_per_group falls back to sampling without cross-feature anchor", {
  # Single feature, tied candidates, no sibling feature sharing the M ->
  # no anchor can fire; must fall back to random / RT-priority sampling
  # and emit the "Sampled" note. Rows are NOT dropped.
  df <- tidytable::tidytable(
    feature_id = rep("F1", 4L),
    mz = rep(200, 4L),
    candidate_adduct = rep("[M+H]+", 4L),
    rank_final = rep(1L, 4L),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_A",
      "IK_B",
      "IK_C",
      "IK_D"
    ),
    score_weighted_chemo = rep(0.9, 4L),
    candidate_score_pseudo_initial = rep(0.8, 4L)
  )

  out <- sample_candidates_per_group(df = df, max_per_score = 2L, seed = 42L)

  expect_equal(nrow(out$df), 2L)
  expect_true(any(grepl("^Sampled ", out$annotation_notes$annotation_note)))
  expect_false(any(grepl(
    "sibling feature",
    out$annotation_notes$annotation_note
  )))
})

test_that("sample_candidates_per_group resolves tied group using another feature's unambiguous adduct (F151/F154 scenario)", {
  # Real-world F151/F154: F1 has an UNAMBIGUOUS rank-1 winner (IK_A) for
  # adduct [M+H4N]+. F2 has 3 tied rank-1 candidates (IK_A, IK_B, IK_C)
  # for adduct [2M+Na]+. Both features map to the same neutral mass M
  # (within tolerance). The anchor from F1 must collapse F2's tie to IK_A,
  # leaving F1 untouched (it's untied).
  mz_F1 <- 360.1495517
  mz_F2 <- 707.2199918

  df <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F2", "F2"),
    mz = c(mz_F1, mz_F2, mz_F2, mz_F2),
    candidate_adduct = c(
      "[M+H4N]+",
      "[2M+Na]+",
      "[2M+Na]+",
      "[2M+Na]+"
    ),
    rank_final = rep(1L, 4L),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK_A",
      "IK_A",
      "IK_B",
      "IK_C"
    ),
    score_weighted_chemo = c(0.98, 0.84, 0.84, 0.84),
    candidate_score_pseudo_initial = c(0.9, 0.8, 0.8, 0.8)
  )

  # max_per_score = 5 so F2 would not be sampled on size alone; the anchor
  # must still collapse its tie to IK_A.
  out <- sample_candidates_per_group(df = df, max_per_score = 5L, seed = 42L)

  kept <- out$df |>
    tidytable::arrange(feature_id) |>
    tidytable::select(
      feature_id,
      candidate_structure_inchikey_connectivity_layer
    )
  expect_equal(nrow(kept), 2L)
  expect_equal(
    kept$candidate_structure_inchikey_connectivity_layer,
    c("IK_A", "IK_A")
  )
  expect_true(any(grepl(
    "sibling feature",
    out$annotation_notes$annotation_note
  )))
})

test_that("sample_candidates_per_group handles malformed adduct strings", {
  df <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F1"),
    mz = c(145.03, 145.03, 145.03),
    candidate_adduct = c("[M+H]+", "not_a_valid_adduct", "not_a_valid_adduct"),
    rank_final = c(1L, 1L, 1L),
    candidate_structure_inchikey_connectivity_layer = c("IK_A", "IK_B", "IK_C"),
    score_weighted_chemo = c(0.95, 0.8, 0.8),
    candidate_score_pseudo_initial = c(0.9, 0.7, 0.7)
  )

  out <- suppressMessages(suppressWarnings(
    sample_candidates_per_group(df = df, max_per_score = 1L, seed = 42L)
  ))

  tied <- out$df |>
    tidytable::filter(candidate_adduct == "not_a_valid_adduct")

  expect_equal(nrow(tied), 1L)
  expect_false(
    ".candidate_M" %in% names(out$df)
  )
})

test_that("build_mini_results_table appends xrefs and renames id columns", {
  features_table <- tidytable::tidytable(feature_id = "F1", rt = 1, mz = 100)
  df_classes_mini <- tidytable::tidytable(
    feature_id = "F1",
    has_inchikey = TRUE,
    label_classyfire = "ClassA",
    label_npclassifier = "NPCA",
    score_classyfire = NA_real_,
    score_npclassifier = NA_real_
  )
  results_filtered <- tidytable::tidytable(
    feature_id = "F1",
    candidates_evaluated = 1L,
    candidates_best = 1L,
    annotation_note = NA_character_
  )
  df_filtered <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_name = "Cmpd1",
    candidate_adduct = "[M+H]+",
    candidate_structure_smiles_no_stereo = "CCO",
    candidate_structure_inchikey_connectivity_layer = "IK1",
    candidate_library = "lib1",
    candidate_structure_error_mz = 0.1,
    candidate_structure_error_rt = 0.01,
    candidate_structure_organism_occurrence_closest = "Org1",
    candidate_structure_tag = "reported",
    score_weighted_chemo = 0.9
  )

  xrefs_table <- tidytable::tidytable(
    inchikey = "IK1",
    prefix = "pubchem",
    id = "12345"
  )

  local_mocked_bindings(
    .add_xrefs_to_df = function(df, xrefs) {
      df$candidate_structure_id_pubchem <- "12345"
      df
    }
  )

  out <- build_mini_results_table(
    features_table = features_table,
    df_classes_mini = df_classes_mini,
    results_filtered = results_filtered,
    df_filtered = df_filtered,
    xrefs_table = xrefs_table
  )

  expect_true("id_pubchem" %in% names(out))
})

test_that("build_mini_taxonomy_table falls back to predicted labels when structure labels are placeholders", {
  df_percentile <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_inchikey_connectivity_layer = "IK1",
    candidate_structure_tax_cla_01kin = "notClassified",
    candidate_structure_tax_cla_02sup = "empty",
    candidate_structure_tax_cla_03cla = NA_character_,
    candidate_structure_tax_cla_04dirpar = "notClassified",
    candidate_structure_tax_npc_01pat = "notClassified",
    candidate_structure_tax_npc_02sup = "empty",
    candidate_structure_tax_npc_03cla = NA_character_,
    feature_pred_tax_cla_01kin_val = NA_character_,
    feature_pred_tax_cla_01kin_score = NA_character_,
    feature_pred_tax_cla_02sup_val = "Organic compounds",
    feature_pred_tax_cla_02sup_score = "0.8",
    feature_pred_tax_cla_03cla_val = NA_character_,
    feature_pred_tax_cla_03cla_score = NA_character_,
    feature_pred_tax_cla_04dirpar_val = NA_character_,
    feature_pred_tax_cla_04dirpar_score = NA_character_,
    feature_pred_tax_npc_01pat_val = "Alkaloids",
    feature_pred_tax_npc_01pat_score = "0.7",
    feature_pred_tax_npc_02sup_val = NA_character_,
    feature_pred_tax_npc_02sup_score = NA_character_,
    feature_pred_tax_npc_03cla_val = NA_character_,
    feature_pred_tax_npc_03cla_score = NA_character_
  )

  out <- build_mini_taxonomy_table(
    df_percentile = df_percentile,
    score_chemical_cla_kingdom = 0.1,
    score_chemical_cla_superclass = 0.2,
    score_chemical_cla_class = 0.3,
    score_chemical_cla_parent = 0.4,
    score_chemical_npc_pathway = 0.3,
    score_chemical_npc_superclass = 0.2,
    score_chemical_npc_class = 0.1
  )

  expect_equal(out$label_classyfire[[1L]], "Organic compounds")
  expect_equal(out$label_npclassifier[[1L]], "Alkaloids")
  expect_true(!is.na(out$score_classyfire[[1L]]))
  expect_true(!is.na(out$score_npclassifier[[1L]]))
})

test_that("build_mini_taxonomy_table keeps labels from highest-scoring structure candidate (pmax)", {
  # IK_UGLY has score 0.2, IK_GOOD has score 0.9 → IK_GOOD labels should win
  df_percentile <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    score_weighted_chemo = c(0.2, 0.9),
    candidate_score_pseudo_initial = c(0.2, 0.9),
    candidate_structure_inchikey_connectivity_layer = c("IK_UGLY", "IK_GOOD"),
    candidate_structure_tax_cla_01kin = c("KinUgly", "KinGood"),
    candidate_structure_tax_cla_02sup = c("SupUgly", "SupGood"),
    candidate_structure_tax_cla_03cla = c("ClaUgly", "ClaGood"),
    candidate_structure_tax_cla_04dirpar = c("ParentUgly", "ParentGood"),
    candidate_structure_tax_npc_01pat = c("PathUgly", "PathGood"),
    candidate_structure_tax_npc_02sup = c("NpcSupUgly", "NpcSupGood"),
    candidate_structure_tax_npc_03cla = c("NpcClaUgly", "NpcClaGood"),
    feature_pred_tax_cla_01kin_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_01kin_score = c(NA_character_, NA_character_),
    feature_pred_tax_cla_02sup_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_02sup_score = c(NA_character_, NA_character_),
    feature_pred_tax_cla_03cla_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_03cla_score = c(NA_character_, NA_character_),
    feature_pred_tax_cla_04dirpar_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_04dirpar_score = c(NA_character_, NA_character_),
    feature_pred_tax_npc_01pat_val = c(NA_character_, NA_character_),
    feature_pred_tax_npc_01pat_score = c(NA_character_, NA_character_),
    feature_pred_tax_npc_02sup_val = c(NA_character_, NA_character_),
    feature_pred_tax_npc_02sup_score = c(NA_character_, NA_character_),
    feature_pred_tax_npc_03cla_val = c(NA_character_, NA_character_),
    feature_pred_tax_npc_03cla_score = c(NA_character_, NA_character_)
  )

  out <- build_mini_taxonomy_table(
    df_percentile = df_percentile,
    score_chemical_cla_kingdom = 0.1,
    score_chemical_cla_superclass = 0.2,
    score_chemical_cla_class = 0.3,
    score_chemical_cla_parent = 0.4,
    score_chemical_npc_pathway = 0.3,
    score_chemical_npc_superclass = 0.2,
    score_chemical_npc_class = 0.1
  )

  expect_equal(out$label_classyfire[[1L]], "ParentGood")
  expect_equal(out$label_npclassifier[[1L]], "NpcClaGood")
  expect_true(is.na(out$score_classyfire[[1L]]))
  expect_true(is.na(out$score_npclassifier[[1L]]))
})

test_that("build_mini_taxonomy_table predicted label beats low-scoring structure label via pmax", {
  # Structure candidate has score_weighted_chemo = 0.1
  # Predicted label score_classyfire = 0.95 * 0.4 (weight) = 0.38 > 0.1 → predicted wins
  df_percentile <- tidytable::tidytable(
    feature_id = "F1",
    score_weighted_chemo = 0.1,
    candidate_score_pseudo_initial = 0.1,
    candidate_structure_inchikey_connectivity_layer = "IK_UGLY",
    candidate_structure_tax_cla_01kin = "KinUgly",
    candidate_structure_tax_cla_02sup = "SupUgly",
    candidate_structure_tax_cla_03cla = "ClaUgly",
    candidate_structure_tax_cla_04dirpar = "ParentUgly",
    candidate_structure_tax_npc_01pat = "PathUgly",
    candidate_structure_tax_npc_02sup = "NpcSupUgly",
    candidate_structure_tax_npc_03cla = "NpcClaUgly",
    feature_pred_tax_cla_01kin_val = NA_character_,
    feature_pred_tax_cla_01kin_score = NA_character_,
    feature_pred_tax_cla_02sup_val = "GoodSuperclass",
    feature_pred_tax_cla_02sup_score = "0.95",
    feature_pred_tax_cla_03cla_val = NA_character_,
    feature_pred_tax_cla_03cla_score = NA_character_,
    feature_pred_tax_cla_04dirpar_val = NA_character_,
    feature_pred_tax_cla_04dirpar_score = NA_character_,
    feature_pred_tax_npc_01pat_val = "GoodPathway",
    feature_pred_tax_npc_01pat_score = "0.92",
    feature_pred_tax_npc_02sup_val = NA_character_,
    feature_pred_tax_npc_02sup_score = NA_character_,
    feature_pred_tax_npc_03cla_val = NA_character_,
    feature_pred_tax_npc_03cla_score = NA_character_
  )

  out <- build_mini_taxonomy_table(
    df_percentile = df_percentile,
    score_chemical_cla_kingdom = 0.2,
    score_chemical_cla_superclass = 0.4,
    score_chemical_cla_class = 0.6,
    score_chemical_cla_parent = 0.8,
    score_chemical_npc_pathway = 0.25,
    score_chemical_npc_superclass = 0.5,
    score_chemical_npc_class = 0.75
  )

  # weighted predicted classyfire score = 0.95 * 0.4 = 0.38 > 0.1 → predicted wins
  expect_equal(out$label_classyfire[[1L]], "GoodSuperclass")
  expect_equal(out$label_npclassifier[[1L]], "GoodPathway")
  expect_false(is.na(out$score_classyfire[[1L]]))
  expect_false(is.na(out$score_npclassifier[[1L]]))
})
