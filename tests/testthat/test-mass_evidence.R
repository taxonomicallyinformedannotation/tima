# Test Suite: mass_evidence — unified evidence-based ion species discovery

library(testthat)

# Build a tiny universe covering [M+H]+ / [M+Na]+ / [M+2H]2+ / [2M+H]+ / [M+H-H2O]+
.mini_universe <- function() {
  spec <- list(
    M = c(1L, 2L),
    charge_carriers = list(pos = c("H", "Na")),
    charges = list(pos = c(1L, 2L)),
    neutral_losses = c("H2O")
  )
  generate_adduct_hypotheses(spec, polarity = "pos")
}

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

test_that("evidence engine discovers neutral-loss adducts ([M+H]+ vs [M+H-H2O]+)", {
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
  loss <- hyps[hyps$feature_id == "LOSS" & hyps$adduct == "[M+H-H2O]+", ]
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
  expect_true("[M+H-H2O]+" %in% hyps$adduct)
  expect_false(any(grepl("Cu", hyps$adduct, fixed = TRUE)))
})
