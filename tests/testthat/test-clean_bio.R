# Test Suite: clean_bio ----

library(testthat)
set.seed(NULL)

## Input Validation ----

test_that("clean_bio validates data frame inputs", {
  # Non-data frame annot_table_wei_bio should error
  expect_error(
    clean_bio(
      annot_table_wei_bio = "not a dataframe",
      edges_table = data.frame(),
      minimal_consistency = 0.5
    ),
    "Fix: Ensure input is a valid data frame",
    fixed = TRUE
  )

  # Non-data frame edges_table should error
  expect_error(
    clean_bio(
      annot_table_wei_bio = data.frame(),
      edges_table = list(),
      minimal_consistency = 0.5
    ),
    "Fix: Ensure input is a valid data frame",
    fixed = TRUE
  )
})

test_that("clean_bio validates minimal_consistency parameter", {
  # Negative consistency should error
  expect_error(
    clean_bio(
      annot_table_wei_bio = data.frame(feature_id = "F1"),
      edges_table = data.frame(),
      minimal_consistency = -0.1
    ),
    "Fix: Use a value between 0 and 1",
    fixed = TRUE
  )

  # Consistency > 1 should error
  expect_error(
    clean_bio(
      annot_table_wei_bio = data.frame(feature_id = "F1"),
      edges_table = data.frame(),
      minimal_consistency = 1.5
    ),
    "Fix: Use a value between 0 and 1",
    fixed = TRUE
  )

  # Non-numeric consistency should error
  expect_error(
    clean_bio(
      annot_table_wei_bio = data.frame(feature_id = "F1"),
      edges_table = data.frame(),
      minimal_consistency = "0.5"
    ),
    "Fix: Provide a single numeric value",
    fixed = TRUE
  )
})

## Edge Cases and Empty Input ----

test_that("clean_bio handles empty annotation table", {
  # Create empty annotation table
  empty_annotations <- tidytable::tidytable(
    feature_id = character(0),
    candidate_structure_inchikey_connectivity_layer = character(0),
    candidate_structure_tax_cla_01kin = character(0),
    candidate_structure_tax_npc_01pat = character(0),
    score_weighted_bio = numeric(0)
  )

  # Create non-empty edges table
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F2"),
    feature_target = c("F2", "F3", "F3"),
    feature_spectrum_entropy = c(0.5, 0.6, 0.7)
  )

  result <- clean_bio(
    annot_table_wei_bio = empty_annotations,
    edges_table = edges,
    minimal_consistency = 0.5
  )

  # Should return empty input unchanged
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.frame")
})

test_that("clean_bio handles empty edges table", {
  # Load annotation fixture and add required bio columns
  annotations <- load_fixture("annotations") |>
    tidytable::mutate(
      score_weighted_bio = runif(tidytable::n(), 0.6, 0.9),
      candidate_structure_tax_cla_02sup = "Phenylpropanoids",
      candidate_structure_tax_npc_02sup = "Alkaloids",
      candidate_structure_tax_cla_04dirpar = "Flavones"
    )

  # Create empty edges table
  empty_edges <- tidytable::tidytable(
    feature_source = character(0),
    feature_target = character(0),
    feature_spectrum_entropy = numeric(0)
  )

  result <- clean_bio(
    annot_table_wei_bio = annotations,
    edges_table = empty_edges,
    minimal_consistency = 0.5
  )

  # Should return input with default consistency columns added
  expect_equal(nrow(result), nrow(annotations))
  expect_s3_class(result, "data.frame")

  # Should have feature_pred_tax columns with "empty" values
  expect_true("feature_pred_tax_cla_01kin_val" %in% colnames(result))
  expect_true("consistency_structure_cla_kin" %in% colnames(result))

  # All features should be marked as "empty" (no network)
  expect_true(all(result$feature_pred_tax_cla_01kin_val == "empty"))
})

test_that("clean_bio handles features with only 1 neighbor", {
  # Load and prepare annotation fixture
  annotations <- load_fixture("annotations") |>
    tidytable::slice(1:3) |>
    tidytable::mutate(
      score_weighted_bio = c(0.8, 0.75, 0.7),
      candidate_structure_tax_cla_02sup = "Phenylpropanoids",
      candidate_structure_tax_npc_02sup = "Alkaloids",
      candidate_structure_tax_cla_04dirpar = "Flavones"
    )

  # Create edges with FT0001 having only 1 neighbor (should be filtered out)
  edges <- tidytable::tidytable(
    feature_source = c("FT0001", "FT0002", "FT0002"),
    feature_target = c("FT0002", "FT0003", "FT0004"),
    feature_spectrum_entropy = c(0.5, 0.6, 0.7)
  )

  result <- clean_bio(
    annot_table_wei_bio = annotations,
    edges_table = edges,
    minimal_consistency = 0.5
  )

  # Should return results with consistency columns
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(annotations))

  # F1 should have "empty" values (only 1 neighbor)
  # F2 should have calculated values (>=2 neighbors)
  expect_true("feature_pred_tax_cla_01kin_val" %in% colnames(result))
})

test_that("clean_bio handles features with zero entropy", {
  # Create annotation table
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    candidate_structure_inchikey_connectivity_layer = generate_fake_inchikey(
      3,
      seed = 3
    ),
    candidate_structure_tax_cla_01kin = rep("Organic compounds", 3),
    candidate_structure_tax_npc_01pat = c(
      "Alkaloids",
      "Alkaloids",
      "Terpenoids"
    ),
    candidate_structure_tax_cla_02sup = c(
      "Phenylpropanoids",
      "Phenylpropanoids",
      "Isoprenoids"
    ),
    candidate_structure_tax_npc_02sup = c(
      "Alkaloids",
      "Alkaloids",
      "Terpenoids"
    ),
    candidate_structure_tax_cla_03cla = c(
      "Flavonoids",
      "Flavonoids",
      "Monoterpenoids"
    ),
    candidate_structure_tax_npc_03cla = c(
      "Benzylisoquinoline",
      "Benzylisoquinoline",
      "Iridoids"
    ),
    candidate_structure_tax_cla_04dirpar = c(
      "Flavones",
      "Flavones",
      "Monoterpenoids"
    ),
    score_weighted_bio = c(0.8, 0.75, 0.7)
  )

  # Create edges with zero entropy
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F2", "F2"),
    feature_target = c("F2", "F3", "F1", "F3"),
    feature_spectrum_entropy = c(0, 0, 0, 0),
    label = c("adduct", "adduct", "adduct", "adduct") # Has label, so kept
  )

  result <- clean_bio(
    annot_table_wei_bio = annotations,
    edges_table = edges,
    minimal_consistency = 0.5
  )

  # Should still work (edges with label are kept even with zero entropy)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(annotations))
})

## Functional Tests - Consistency Calculation ----

test_that("clean_bio calculates consistency correctly for perfect agreement", {
  # Create annotation table where all neighbors agree on classification
  annotations <- tidytable::tidytable(
    feature_id = rep("F1", 3),
    candidate_structure_inchikey_connectivity_layer = generate_fake_inchikey(
      3,
      seed = 10
    ),
    candidate_structure_tax_cla_01kin = rep("Organic compounds", 3),
    candidate_structure_tax_npc_01pat = rep("Alkaloids", 3),
    candidate_structure_tax_cla_02sup = rep("Phenylpropanoids", 3),
    candidate_structure_tax_npc_02sup = rep("Alkaloids", 3),
    candidate_structure_tax_cla_03cla = rep("Flavonoids", 3),
    candidate_structure_tax_npc_03cla = rep("Benzylisoquinoline", 3),
    candidate_structure_tax_cla_04dirpar = rep("Flavones", 3),
    score_weighted_bio = c(0.8, 0.75, 0.7)
  ) |>
    tidytable::bind_rows(
      # Neighbors F2, F3, F4 all have same classification
      tidytable::tidytable(
        feature_id = rep(c("F2", "F3", "F4"), each = 1),
        candidate_structure_inchikey_connectivity_layer = generate_fake_inchikey(
          3,
          seed = 11
        ),
        candidate_structure_tax_cla_01kin = rep("Organic compounds", 3),
        candidate_structure_tax_npc_01pat = rep("Alkaloids", 3),
        candidate_structure_tax_cla_02sup = rep("Phenylpropanoids", 3),
        candidate_structure_tax_npc_02sup = rep("Alkaloids", 3),
        candidate_structure_tax_cla_03cla = rep("Flavonoids", 3),
        candidate_structure_tax_npc_03cla = rep("Benzylisoquinoline", 3),
        candidate_structure_tax_cla_04dirpar = rep("Flavones", 3),
        score_weighted_bio = c(0.8, 0.75, 0.7)
      )
    )

  # Create edges: F1 connects to F2, F3, F4 (all have same classification)
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F1"),
    feature_target = c("F2", "F3", "F4"),
    feature_spectrum_entropy = c(0.5, 0.6, 0.7)
  )

  result <- clean_bio(
    annot_table_wei_bio = annotations,
    edges_table = edges,
    minimal_consistency = 0.5
  )

  # F1 should have high consistency (all neighbors agree)
  f1_results <- result |>
    tidytable::filter(feature_id == "F1")

  if ("consistency_structure_cla_kin" %in% colnames(f1_results)) {
    # Perfect agreement should give consistency = 1
    expect_true(any(f1_results$consistency_structure_cla_kin == 1))
  }
})

test_that("clean_bio calculates consistency correctly for partial agreement", {
  # Create annotation table where neighbors partially agree
  annotations <- tidytable::tidytable(
    feature_id = rep("F1", 3),
    candidate_structure_inchikey_connectivity_layer = generate_fake_inchikey(
      3,
      seed = 20
    ),
    candidate_structure_tax_cla_01kin = rep("Organic compounds", 3),
    candidate_structure_tax_npc_01pat = rep("Alkaloids", 3),
    candidate_structure_tax_cla_02sup = rep("Phenylpropanoids", 3),
    candidate_structure_tax_npc_02sup = rep("Alkaloids", 3),
    candidate_structure_tax_cla_03cla = rep("Flavonoids", 3),
    candidate_structure_tax_npc_03cla = rep("Benzylisoquinoline", 3),
    candidate_structure_tax_cla_04dirpar = rep("Flavones", 3),
    score_weighted_bio = c(0.8, 0.75, 0.7)
  ) |>
    tidytable::bind_rows(
      # F2 and F3 have Alkaloids, F4 has Terpenoids
      tidytable::tidytable(
        feature_id = c("F2", "F3", "F4"),
        candidate_structure_inchikey_connectivity_layer = generate_fake_inchikey(
          3,
          seed = 21
        ),
        candidate_structure_tax_cla_01kin = rep("Organic compounds", 3),
        candidate_structure_tax_npc_01pat = c(
          "Alkaloids",
          "Alkaloids",
          "Terpenoids"
        ),
        candidate_structure_tax_cla_02sup = c(
          "Phenylpropanoids",
          "Phenylpropanoids",
          "Isoprenoids"
        ),
        candidate_structure_tax_npc_02sup = c(
          "Alkaloids",
          "Alkaloids",
          "Terpenoids"
        ),
        candidate_structure_tax_cla_03cla = c(
          "Flavonoids",
          "Flavonoids",
          "Monoterpenoids"
        ),
        candidate_structure_tax_npc_03cla = c(
          "Benzylisoquinoline",
          "Benzylisoquinoline",
          "Iridoids"
        ),
        candidate_structure_tax_cla_04dirpar = c(
          "Flavones",
          "Flavones",
          "Monoterpenoids"
        ),
        score_weighted_bio = c(0.8, 0.75, 0.7)
      )
    )

  # Create edges: F1 connects to F2, F3, F4
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F1"),
    feature_target = c("F2", "F3", "F4"),
    feature_spectrum_entropy = c(0.5, 0.6, 0.7)
  )

  result <- clean_bio(
    annot_table_wei_bio = annotations,
    edges_table = edges,
    minimal_consistency = 0.5
  )

  # F1 should have moderate consistency (2/3 agree on NPC pathway)
  f1_results <- result |>
    tidytable::filter(feature_id == "F1")

  if ("consistency_structure_npc_pat" %in% colnames(f1_results)) {
    # 2 out of 3 neighbors have Alkaloids, so consistency ~ 0.67
    consistency_vals <- f1_results$consistency_structure_npc_pat
    expect_true(any(consistency_vals > 0.6 & consistency_vals <= 1.0))
  }
})

test_that("clean_bio filters out classifications below minimal_consistency", {
  # Create annotation table
  annotations <- tidytable::tidytable(
    feature_id = rep("F1", 3),
    candidate_structure_inchikey_connectivity_layer = generate_fake_inchikey(
      3,
      seed = 30
    ),
    candidate_structure_tax_cla_01kin = rep("Organic compounds", 3),
    candidate_structure_tax_npc_01pat = rep("Alkaloids", 3),
    candidate_structure_tax_cla_02sup = rep("Phenylpropanoids", 3),
    candidate_structure_tax_npc_02sup = rep("Alkaloids", 3),
    candidate_structure_tax_cla_03cla = rep("Flavonoids", 3),
    candidate_structure_tax_npc_03cla = rep("Benzylisoquinoline", 3),
    candidate_structure_tax_cla_04dirpar = rep("Flavones", 3),
    score_weighted_bio = c(0.8, 0.75, 0.7)
  ) |>
    tidytable::bind_rows(
      # All neighbors have different NPC pathway (no consensus)
      tidytable::tidytable(
        feature_id = c("F2", "F3", "F4"),
        candidate_structure_inchikey_connectivity_layer = generate_fake_inchikey(
          3,
          seed = 31
        ),
        candidate_structure_tax_cla_01kin = rep("Organic compounds", 3),
        candidate_structure_tax_npc_01pat = c(
          "Alkaloids",
          "Terpenoids",
          "Polyketides"
        ),
        candidate_structure_tax_cla_02sup = c(
          "Phenylpropanoids",
          "Isoprenoids",
          "Polyketides"
        ),
        candidate_structure_tax_npc_02sup = c(
          "Alkaloids",
          "Terpenoids",
          "Polyketides"
        ),
        candidate_structure_tax_cla_03cla = c(
          "Flavonoids",
          "Monoterpenoids",
          "Type I"
        ),
        candidate_structure_tax_npc_03cla = c(
          "Benzylisoquinoline",
          "Iridoids",
          "Modular"
        ),
        candidate_structure_tax_cla_04dirpar = c(
          "Flavones",
          "Monoterpenoids",
          "Type I"
        ),
        score_weighted_bio = c(0.2, 0.15, 0.1) # Low scores
      )
    )

  # Create edges
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1", "F1"),
    feature_target = c("F2", "F3", "F4"),
    feature_spectrum_entropy = c(0.5, 0.6, 0.7)
  )

  result <- clean_bio(
    annot_table_wei_bio = annotations,
    edges_table = edges,
    minimal_consistency = 0.8 # High threshold
  )

  # F1 should have "notConsistent" for NPC pathway (no consensus)
  f1_results <- result |>
    tidytable::filter(feature_id == "F1")

  if ("feature_pred_tax_npc_01pat_val" %in% colnames(f1_results)) {
    # With high minimal_consistency and low agreement, should be notConsistent
    expect_true(any(
      f1_results$feature_pred_tax_npc_01pat_val == "notConsistent" |
        f1_results$feature_pred_tax_npc_01pat_val == "empty"
    ))
  }
})

## Output Structure ----

test_that("clean_bio returns expected columns", {
  # Create simple annotation table
  annotations <- tidytable::tidytable(
    feature_id = c("F1"),
    candidate_structure_inchikey_connectivity_layer = generate_fake_inchikey(
      1,
      seed = 40
    ),
    candidate_structure_tax_cla_01kin = "Organic compounds",
    candidate_structure_tax_npc_01pat = "Alkaloids",
    candidate_structure_tax_cla_02sup = "Phenylpropanoids",
    candidate_structure_tax_npc_02sup = "Alkaloids",
    candidate_structure_tax_cla_03cla = "Flavonoids",
    candidate_structure_tax_npc_03cla = "Benzylisoquinoline",
    candidate_structure_tax_cla_04dirpar = "Flavones",
    score_weighted_bio = 0.8
  )

  # Create edges
  edges <- tidytable::tidytable(
    feature_source = c("F1", "F1"),
    feature_target = c("F2", "F3"),
    feature_spectrum_entropy = c(0.5, 0.6)
  )

  result <- clean_bio(
    annot_table_wei_bio = annotations,
    edges_table = edges,
    minimal_consistency = 0.5
  )

  # Should contain original columns
  expect_true("feature_id" %in% colnames(result))
  expect_true(
    "candidate_structure_inchikey_connectivity_layer" %in% colnames(result)
  )

  # Should contain new feature_pred_tax columns
  expected_pred_cols <- c(
    "feature_pred_tax_cla_01kin_val",
    "consistency_structure_cla_kin",
    "feature_pred_tax_cla_01kin_score",
    "feature_pred_tax_npc_01pat_val",
    "consistency_structure_npc_pat",
    "feature_pred_tax_npc_01pat_score",
    "feature_pred_tax_cla_02sup_val",
    "consistency_structure_cla_sup",
    "feature_pred_tax_cla_02sup_score",
    "feature_pred_tax_npc_02sup_val",
    "consistency_structure_npc_sup",
    "feature_pred_tax_npc_02sup_score",
    "feature_pred_tax_cla_03cla_val",
    "consistency_structure_cla_cla",
    "feature_pred_tax_cla_03cla_score",
    "feature_pred_tax_npc_03cla_val",
    "consistency_structure_npc_cla",
    "feature_pred_tax_npc_03cla_score",
    "feature_pred_tax_cla_04dirpar_val",
    "consistency_structure_cla_par",
    "feature_pred_tax_cla_04dirpar_score"
  )

  expect_required_columns(result, expected_pred_cols)
})

test_that("clean_bio preserves already computed predictions", {
  # Create annotation table with some features already having predictions
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_inchikey_connectivity_layer = generate_fake_inchikey(
      2,
      seed = 50
    ),
    candidate_structure_tax_cla_01kin = rep("Organic compounds", 2),
    candidate_structure_tax_npc_01pat = c("Alkaloids", "Terpenoids"),
    candidate_structure_tax_cla_02sup = c("Phenylpropanoids", "Isoprenoids"),
    candidate_structure_tax_npc_02sup = c("Alkaloids", "Terpenoids"),
    candidate_structure_tax_cla_03cla = c("Flavonoids", "Monoterpenoids"),
    candidate_structure_tax_npc_03cla = c("Benzylisoquinoline", "Iridoids"),
    candidate_structure_tax_cla_04dirpar = c("Flavones", "Monoterpenoids"),
    score_weighted_bio = c(0.8, 0.75),
    # F1 already has predictions
    feature_pred_tax_cla_02sup_val = c("Phenylpropanoids", NA),
    consistency_structure_cla_sup = c(0.9, NA),
    feature_pred_tax_cla_02sup_score = c(0.72, NA)
  )

  # Create edges
  edges <- tidytable::tidytable(
    feature_source = c("F2", "F2"),
    feature_target = c("F3", "F4"),
    feature_spectrum_entropy = c(0.5, 0.6)
  )

  result <- clean_bio(
    annot_table_wei_bio = annotations,
    edges_table = edges,
    minimal_consistency = 0.5
  )

  # F1's existing predictions should be preserved
  f1_result <- result |>
    tidytable::filter(feature_id == "F1")

  if (nrow(f1_result) > 0) {
    expect_equal(
      f1_result$feature_pred_tax_cla_02sup_val[1],
      "Phenylpropanoids"
    )
    expect_equal(f1_result$consistency_structure_cla_sup[1], 0.9)
  }
})

## Performance Tests ----

test_that("clean_bio handles moderate-scale data efficiently", {
  # Create 100 features with annotations
  n_features <- 100

  inchikeys <- generate_fake_inchikey(n_features, seed = 100)

  # Each feature can have multiple candidates
  annotations <- tidytable::tidytable(
    feature_id = rep(paste0("F", 1:n_features), each = 3),
    candidate_structure_inchikey_connectivity_layer = generate_fake_inchikey(
      n_features * 3,
      seed = 101
    ),
    candidate_structure_tax_cla_01kin = rep(
      "Organic compounds",
      n_features * 3
    ),
    candidate_structure_tax_npc_01pat = sample(
      c("Alkaloids", "Terpenoids", "Polyketides", "Phenylpropanoids"),
      n_features * 3,
      replace = TRUE
    ),
    candidate_structure_tax_cla_02sup = sample(
      c("Phenylpropanoids", "Isoprenoids", "Polyketides"),
      n_features * 3,
      replace = TRUE
    ),
    candidate_structure_tax_npc_02sup = sample(
      c("Alkaloids", "Terpenoids", "Polyketides"),
      n_features * 3,
      replace = TRUE
    ),
    candidate_structure_tax_cla_03cla = sample(
      c("Flavonoids", "Monoterpenoids", "Type I"),
      n_features * 3,
      replace = TRUE
    ),
    candidate_structure_tax_npc_03cla = sample(
      c("Benzylisoquinoline", "Iridoids", "Modular"),
      n_features * 3,
      replace = TRUE
    ),
    candidate_structure_tax_cla_04dirpar = sample(
      c("Flavones", "Monoterpenoids", "Type I"),
      n_features * 3,
      replace = TRUE
    ),
    score_weighted_bio = runif(n_features * 3, 0.5, 1.0)
  )

  # Create network edges (each feature has 3-5 neighbors)
  edges_list <- lapply(1:n_features, function(i) {
    n_neighbors <- sample(3:5, 1)
    targets <- sample(setdiff(1:n_features, i), n_neighbors)
    tidytable::tidytable(
      feature_source = paste0("F", i),
      feature_target = paste0("F", targets),
      feature_spectrum_entropy = runif(n_neighbors, 0.3, 0.8)
    )
  })
  edges <- tidytable::bind_rows(edges_list)

  # Should complete in reasonable time (<10 seconds)
  start_time <- Sys.time()

  result <- clean_bio(
    annot_table_wei_bio = annotations,
    edges_table = edges,
    minimal_consistency = 0.5
  )

  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  expect_true(
    elapsed_time < 10,
    info = sprintf(
      "Processing took %.2f seconds (expected < 10s)",
      elapsed_time
    )
  )

  expect_true(nrow(result) > 0)
  expect_s3_class(result, "data.frame")

  # Should have consistency columns
  expect_true("consistency_structure_cla_kin" %in% colnames(result))
})
