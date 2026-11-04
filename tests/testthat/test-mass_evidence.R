# Test Suite: mass_evidence — unified evidence-based ion species discovery

library(testthat)

# Build a tiny universe covering [M+H]+ / [M+Na]+ / [M+2H]2+ / [2M+H]+ / [M-H2O+H]+
.mini_universe <- function() {
  spec <- list(
    M = c(1L, 2L),
    charge_carriers = list(pos = c("H", "Na")),
    charges = list(pos = c(1L, 2L)),
    neutral_losses = c("H2O")
  )
  generate_adduct_hypotheses(spec, polarity = "pos")
}

test_that("empty_evidence_table returns the expected typed schema", {
  out <- empty_evidence_table()

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_named(
    out,
    c(
      "feature_id",
      "rt",
      "mz",
      "sample",
      "adduct",
      "n_mer",
      "z",
      "adduct_mass",
      "n_iso",
      "implied_M",
      "nearest_mass_error_ppm",
      "mass_cluster",
      "rt_cluster",
      "evidence_cluster",
      "n_evidence_features",
      "evidence_count",
      "evidence_score",
      "candidate_adduct_origin",
      "source"
    )
  )
  expect_type(out$feature_id, "character")
  expect_type(out$rt, "double")
  expect_type(out$n_mer, "integer")
})

test_that("implied_neutral_mass inverts m/z for canonical adducts", {
  M <- 180.0634
  u <- .mini_universe()
  for (a in c("[M+H]+", "[M+Na]+", "[2M+H]+", "[M+2H]2+")) {
    row <- u[u$adduct == a, ]
    mz <- calculate_mz_from_neutral_mass(M, row$n_mer, row$z, row$adduct_mass)
    M_back <- implied_neutral_mass(
      mz = mz,
      n_mer = row$n_mer,
      z = row$z,
      adduct_mass = row$adduct_mass,
      n_iso = 0L
    )
    expect_equal(M_back, M, tolerance = 1e-6, info = a)
  }
})

test_that("evidence engine clusters [M+H]+ and [M+Na]+ peers", {
  feats <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    rt = c(5.0, 5.005),
    mz = c(181.0707, 203.0526), # [M+H]+ and [M+Na]+ of M=180.0634
    sample = c("S", "S")
  )
  u <- .mini_universe()
  hyps <- build_evidence_supported_hypotheses(
    df_fea_min = feats,
    universe = u,
    tolerance_ppm = 10,
    tolerance_rt = 0.05,
    ms_mode = "pos"
  )
  # Both adducts found, both with evidence support >= 1
  f1 <- hyps[hyps$feature_id == "F1", ]
  f2 <- hyps[hyps$feature_id == "F2", ]
  expect_true("[M+H]+" %in% f1$adduct)
  expect_true("[M+Na]+" %in% f2$adduct)
  shared <- intersect(f1$evidence_cluster, f2$evidence_cluster)
  expect_gt(length(shared), 0L)
})

test_that("evidence engine discovers multimers ([M+H]+ vs [2M+H]+)", {
  feats <- tidytable::tidytable(
    feature_id = c("MONO", "DIMER"),
    rt = c(10.0, 10.0),
    mz = c(181.0707, 361.1339), # [M+H]+ and [2M+H]+ of M=180.0634
    sample = c("S", "S")
  )
  u <- .mini_universe()
  hyps <- build_evidence_supported_hypotheses(
    df_fea_min = feats,
    universe = u,
    tolerance_ppm = 10,
    tolerance_rt = 0.05,
    ms_mode = "pos"
  )
  mono <- hyps[hyps$feature_id == "MONO" & hyps$adduct == "[M+H]+", ]
  dimer <- hyps[hyps$feature_id == "DIMER" & hyps$adduct == "[2M+H]+", ]
  expect_equal(nrow(mono), 1L)
  expect_equal(nrow(dimer), 1L)
  expect_identical(mono$evidence_cluster, dimer$evidence_cluster)
  expect_gte(mono$evidence_score, 1L)
})

test_that("evidence engine discovers multicharged ([M+H]+ vs [M+2H]2+)", {
  M <- 400.0
  mz_singly <- M + 1.007276
  mz_doubly <- (M + 2 * 1.007276) / 2
  feats <- tidytable::tidytable(
    feature_id = c("Z1", "Z2"),
    rt = c(1.0, 1.0),
    mz = c(mz_singly, mz_doubly),
    sample = c("S", "S")
  )
  u <- .mini_universe()
  hyps <- build_evidence_supported_hypotheses(
    df_fea_min = feats,
    universe = u,
    tolerance_ppm = 10,
    tolerance_rt = 0.05,
    ms_mode = "pos"
  )
  singly <- hyps[hyps$feature_id == "Z1" & hyps$adduct == "[M+H]+", ]
  doubly <- hyps[hyps$feature_id == "Z2" & hyps$adduct == "[M+2H]2+", ]
  expect_equal(nrow(singly), 1L)
  expect_equal(nrow(doubly), 1L)
  expect_identical(singly$evidence_cluster, doubly$evidence_cluster)
})

test_that("evidence engine discovers neutral-loss adducts ([M+H]+ vs [M-H2O+H]+)", {
  M <- 180.0634
  mz_plain <- M + 1.007276
  mz_loss <- M - 18.010565 + 1.007276
  feats <- tidytable::tidytable(
    feature_id = c("INTACT", "LOSS"),
    rt = c(2.5, 2.5),
    mz = c(mz_plain, mz_loss),
    sample = c("S", "S")
  )
  u <- .mini_universe()
  hyps <- build_evidence_supported_hypotheses(
    df_fea_min = feats,
    universe = u,
    tolerance_ppm = 10,
    tolerance_rt = 0.05,
    ms_mode = "pos"
  )
  intact <- hyps[hyps$feature_id == "INTACT" & hyps$adduct == "[M+H]+", ]
  loss <- hyps[hyps$feature_id == "LOSS" & hyps$adduct == "[M-H2O+H]+", ]
  expect_equal(nrow(intact), 1L)
  expect_equal(nrow(loss), 1L)
  expect_identical(intact$evidence_cluster, loss$evidence_cluster)
})

test_that("evidence edges connect coadducted features", {
  feats <- tidytable::tidytable(
    feature_id = c("A", "B"),
    rt = c(1.0, 1.0),
    mz = c(181.0707, 203.0526),
    sample = c("S", "S")
  )
  u <- .mini_universe()
  hyps <- build_evidence_supported_hypotheses(
    df_fea_min = feats,
    universe = u,
    tolerance_ppm = 10,
    tolerance_rt = 0.05,
    ms_mode = "pos"
  )
  edges <- build_evidence_edges(hyps)
  expect_gt(nrow(edges), 0L)
  expect_true(all(
    c("feature_id", "adduct", "feature_id_dest", "adduct_dest") %in%
      names(edges)
  ))
  # Edges are directional: lower-mz feature is source
  expect_true(all(edges$feature_id == "A"))
})

test_that("build_evidence_edges returns an empty edge table for empty input", {
  edges <- build_evidence_edges(empty_evidence_table())

  expect_s3_class(edges, "data.frame")
  expect_equal(nrow(edges), 0L)
  expect_named(
    edges,
    c("feature_id", "adduct", "feature_id_dest", "adduct_dest")
  )
})

test_that("build_evidence_edges returns an empty edge table when no clusters are present", {
  hyps <- tidytable::tidytable(
    feature_id = c("A", "B"),
    mz = c(100, 200),
    adduct = c("[M+H]+", "[M+Na]+"),
    evidence_cluster = c(NA_character_, NA_character_),
    evidence_count = c(1L, 2L)
  )

  edges <- build_evidence_edges(hyps)

  expect_s3_class(edges, "data.frame")
  expect_equal(nrow(edges), 0L)
  expect_named(
    edges,
    c("feature_id", "adduct", "feature_id_dest", "adduct_dest")
  )
})

test_that("baseline hypothesis is enforced even without peer evidence", {
  feats <- tidytable::tidytable(
    feature_id = "LONE",
    rt = 5.0,
    mz = 251.1,
    sample = "S"
  )
  u <- .mini_universe()
  hyps <- build_evidence_supported_hypotheses(
    df_fea_min = feats,
    universe = u,
    tolerance_ppm = 10,
    tolerance_rt = 0.05,
    ms_mode = "pos"
  )
  baseline <- hyps[hyps$adduct == "[M+H]+", ]
  expect_equal(nrow(baseline), 1L)
  expect_equal(baseline$candidate_adduct_origin, "enforced")
  expect_equal(baseline$evidence_score, 0L)
})

test_that("exotic Cu/loss hypotheses are pruned without stronger core-supported evidence", {
  spec <- list(
    M = 1L,
    charge_carriers = list(pos = c("H", "Cu")),
    charges = list(pos = 1L),
    neutral_losses = c("H2O", "H3O4P")
  )
  u <- generate_adduct_hypotheses(spec, polarity = "pos")

  # Two features supporting the same neutral mass through [M+H]+ and [M-H2O+H]+.
  # A Cu+phosphoric-loss form should NOT survive because it is exotic and lacks
  # the stronger support threshold/core-backed cluster evidence we require.
  M <- 300
  feats <- tidytable::tidytable(
    feature_id = c("CORE", "LOSS", "WEIRD"),
    rt = c(5, 5, 7),
    mz = c(
      M + 1.007276,
      M - 18.010565 + 1.007276,
      M - 97.976896 + 62.929599
    ),
    sample = c("S", "S", "S")
  )

  hyps <- build_evidence_supported_hypotheses(
    df_fea_min = feats,
    universe = u,
    tolerance_ppm = 10,
    tolerance_rt = 0.05,
    ms_mode = "pos"
  )

  expect_true("[M+H]+" %in% hyps$adduct)
  expect_true("[M-H2O+H]+" %in% hyps$adduct)
  expect_false(any(grepl("Cu", hyps$adduct, fixed = TRUE)))
})

test_that("evidence engine honors requested hypothesis cap above 64 on small inputs", {
  feats <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    rt = c(1, 1),
    mz = c(101.007276, 101.007276),
    sample = c("S", "S")
  )

  universe <- tidytable::tidytable(
    adduct = c("[M+H]+", paste0("HYP_", seq_len(69L))),
    n_mer = rep(1L, 70L),
    n_iso = rep(0L, 70L),
    z = rep(1L, 70L),
    adduct_mass = rep(1.007276, 70L),
    adduct_mass_per_monomer = rep(0, 70L),
    carriers = rep(list(c(H = 1L)), 70L),
    clusters = rep(list(integer()), 70L),
    losses = rep(list(integer()), 70L)
  )

  hyps <- build_evidence_supported_hypotheses(
    df_fea_min = feats,
    universe = universe,
    tolerance_ppm = 10,
    tolerance_rt = 0.05,
    ms_mode = "pos",
    exact_masses = 100,
    max_hypotheses_per_feature = 70L
  )

  expect_equal(length(unique(hyps$adduct)), 70L)
  expect_equal(nrow(hyps), 140L)
})

test_that("evidence engine does not share support across samples", {
  feats <- tidytable::tidytable(
    feature_id = c("S1_H", "S2_NA"),
    rt = c(5, 5),
    mz = c(181.0707, 203.0526),
    sample = c("Sample_1", "Sample_2")
  )
  u <- .mini_universe()

  hyps <- build_evidence_supported_hypotheses(
    df_fea_min = feats,
    universe = u,
    tolerance_ppm = 10,
    tolerance_rt = 0.05,
    ms_mode = "pos"
  )

  s2_na <- hyps[hyps$feature_id == "S2_NA" & hyps$adduct == "[M+Na]+", ]
  expect_equal(nrow(s2_na), 0L)
  baseline_s2 <- hyps[hyps$feature_id == "S2_NA" & hyps$adduct == "[M+H]+", ]
  expect_equal(nrow(baseline_s2), 1L)
  expect_equal(baseline_s2$evidence_score, 0L)
})

test_that("evidence engine applies adaptive per-feature cap when cap_eff < cap_in without error", {
  # With n_feat = 20 and max_hypotheses_per_feature = 100000L:
  # cap_adaptive = floor(1.5e6 / 20) = 75000 < 100000, triggering the
  # cap_eff < cap_in code path while preserving baseline [M+H]+.
  feats <- tidytable::tidytable(
    feature_id = sprintf("F%02d", seq_len(20L)),
    rt = rep(1.0, 20L),
    mz = rep(181.0707, 20L),
    sample = rep("S", 20L)
  )
  u <- .mini_universe()

  hyps <- build_evidence_supported_hypotheses(
    df_fea_min = feats,
    universe = u,
    tolerance_ppm = 10,
    tolerance_rt = 0.05,
    ms_mode = "pos",
    exact_masses = 180.0634,
    max_hypotheses_per_feature = 100000L
  )

  expect_gt(nrow(hyps), 0L)
  expect_true(all(feats$feature_id %in% hyps$feature_id))
  expect_true(all(
    feats$feature_id %in% hyps$feature_id[hyps$adduct == "[M+H]+"]
  ))
})

test_that("build_evidence_edges selects top representative per feature and filters incompatible M", {
  hyps <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F3"),
    mz = c(100.0, 100.0, 122.0, 150.0),
    adduct = c("[M+Na]+", "[M+H]+", "[M+Na]+", "[M+K]+"),
    evidence_cluster = c("CL1", "CL1", "CL1", "CL1"),
    evidence_count = c(1L, 3L, 2L, 2L),
    implied_M = c(99.0, 99.0, 99.0001, 110.0)
  )

  edges <- build_evidence_edges(hyps, tolerance_ppm = 5)
  # F1 picks [M+H]+ (higher evidence_count = 3 > 1) and links to F2 (implied_M within 5 ppm),
  # while F2 -> F3 is rejected because 99.0001 vs 110.0 exceeds 5 ppm.
  expect_equal(nrow(edges), 1L)
  expect_equal(edges$feature_id[[1L]], "F1")
  expect_equal(edges$adduct[[1L]], "[M+H]+")
  expect_equal(edges$feature_id_dest[[1L]], "F2")
  expect_equal(edges$adduct_dest[[1L]], "[M+Na]+")
})

test_that(".has_library_match_within_tolerance respects exact window bounds and dalton floor", {
  exact <- c(100.0, 200.0, 500.0)
  # 10 ppm of query mass m around 100 Da:
  # Lower bound satisfies m * (1 + 10e-6) >= 100 -> m >= 100 / 1.00001 (~99.99900001)
  # Upper bound satisfies m * (1 - 10e-6) <= 100 -> m <= 100 / 0.99999 (~100.00100001)
  masses <- c(
    99.99901, # inside lower bound -> TRUE
    100.001, # inside upper bound -> TRUE
    99.9989, # just below lower bound -> FALSE
    100.0011, # just above upper bound -> FALSE
    NA_real_, # non-finite -> FALSE
    -5.0 # non-positive -> FALSE
  )
  res_ppm <- .has_library_match_within_tolerance(
    masses = masses,
    exact_masses_sorted = exact,
    tolerance_ppm = 10,
    tolerance_dalton = NULL
  )
  expect_identical(res_ppm, c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))

  # With tolerance_dalton = 0.005 Da (> 0.001 Da at 100 Da), 99.995 and 100.005 match
  res_da <- .has_library_match_within_tolerance(
    masses = c(99.995, 100.005, 99.994, -5.0),
    exact_masses_sorted = exact,
    tolerance_ppm = 10,
    tolerance_dalton = 0.005
  )
  expect_identical(res_da, c(TRUE, TRUE, FALSE, FALSE))
})

test_that("build_evidence_edges handles singleton clusters, same-feature duplicates, missing implied_M, and all-incompatible M", {
  # 1. Singleton clusters only (no duplicated evidence_cluster) -> empty edges
  hyps_single_cl <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    mz = c(100.0, 122.0),
    adduct = c("[M+H]+", "[M+Na]+"),
    evidence_cluster = c("CL1", "CL2"),
    evidence_count = c(2L, 2L),
    implied_M = c(99.0, 99.0)
  )
  expect_identical(nrow(build_evidence_edges(hyps_single_cl)), 0L)

  # 2. Multiple rows in same cluster but all for the SAME feature_id (n_rep < 2L) -> empty edges
  hyps_same_feat <- tidytable::tidytable(
    feature_id = c("F1", "F1"),
    mz = c(100.0, 100.0),
    adduct = c("[M+H]+", "[M+Na]+"),
    evidence_cluster = c("CL1", "CL1"),
    evidence_count = c(2L, 1L),
    implied_M = c(99.0, 77.0)
  )
  expect_identical(nrow(build_evidence_edges(hyps_same_feat)), 0L)

  # 3. All pairs in same cluster have incompatible implied_M (length(keep_idx) == 0L) -> empty edges
  hyps_incompat <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    mz = c(100.0, 200.0),
    adduct = c("[M+H]+", "[M+Na]+"),
    evidence_cluster = c("CL1", "CL1"),
    evidence_count = c(2L, 2L),
    implied_M = c(99.0, 177.0)
  )
  expect_identical(
    nrow(build_evidence_edges(hyps_incompat, tolerance_ppm = 5)),
    0L
  )

  # 4. Missing implied_M column defaults to NA_real_ and allows linking within cluster
  hyps_no_m <- tidytable::tidytable(
    feature_id = c("F2", "F1"),
    mz = c(122.0, 100.0),
    adduct = c("[M+Na]+", "[M+H]+"),
    evidence_cluster = c("CL1", "CL1"),
    evidence_count = c(NA_integer_, 2L)
  )
  edges_no_m <- build_evidence_edges(hyps_no_m)
  expect_identical(nrow(edges_no_m), 1L)
  expect_identical(edges_no_m$feature_id, "F1")
  expect_identical(edges_no_m$feature_id_dest, "F2")
})
