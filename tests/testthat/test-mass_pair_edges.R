library(testthat)

build_feature_pairs_within_rt <- build_feature_pairs_within_rt
build_adduct_pair_differences <- build_adduct_pair_differences
join_couples_with_neutral_losses <- join_couples_with_neutral_losses
join_multi_with_addlossed <- join_multi_with_addlossed

# ── build_feature_pairs_within_rt ─────────────────────────────────────────────

test_that("build_feature_pairs_within_rt returns empty for no RT overlaps", {
  df_rt_tol <- data.table::data.table(
    feature_id = "F1",
    rt = 1.0,
    mz = 100.0,
    adduct = "[M+H]+",
    sample = "S1",
    rt_min = 0.9,
    rt_max = 1.1
  )
  df_fea_min <- data.table::data.table(
    feature_id = "F2",
    rt = 5.0, # completely outside RT window
    mz = 200.0,
    adduct = "[M+H]+",
    sample = "S1"
  )

  result <- build_feature_pairs_within_rt(
    df_rt_tol,
    df_fea_min,
    tolerance_ppm = 10
  )
  expect_equal(nrow(result), 0L)
})

test_that("build_feature_pairs_within_rt finds pairs within RT window", {
  df_rt_tol <- data.table::data.table(
    feature_id = "F1",
    rt = 1.0,
    mz = 100.0,
    adduct = "[M+H]+",
    sample = "S1",
    rt_min = 0.8,
    rt_max = 1.2
  )
  # F2 is within RT window and has higher mz
  df_fea_min <- data.table::data.table(
    feature_id = "F2",
    rt = 1.05,
    mz = 122.0,
    adduct = "[M+Na]+",
    sample = "S1"
  )

  result <- build_feature_pairs_within_rt(
    df_rt_tol,
    df_fea_min,
    tolerance_ppm = 10
  )
  expect_equal(nrow(result), 1L)
  expect_true("delta" %in% names(result))
  expect_true("delta_min" %in% names(result))
  expect_true("delta_max" %in% names(result))
  expect_equal(as.character(result$feature_id_dest[[1]]), "F2")
})

test_that("build_feature_pairs_within_rt excludes same-feature pairs", {
  df <- data.table::data.table(
    feature_id = "F1",
    rt = 1.0,
    mz = 100.0,
    adduct = "[M+H]+",
    sample = "S1",
    rt_min = 0.8,
    rt_max = 1.2
  )
  # df_fea_min also contains F1 — should be excluded
  df_fea_min <- data.table::data.table(
    feature_id = c("F1", "F2"),
    rt = c(1.0, 1.1),
    mz = c(100.0, 150.0),
    adduct = c("[M+H]+", "[M+H]+"),
    sample = c("S1", "S1")
  )

  result <- build_feature_pairs_within_rt(df, df_fea_min, tolerance_ppm = 10)
  # F1 should not appear as feature_id_dest
  if (nrow(result) > 0) {
    expect_false(any(result$feature_id == result$feature_id_dest))
  } else {
    expect_equal(nrow(result), 0L)
  }
})

test_that("build_feature_pairs_within_rt computes correct delta values", {
  df_rt_tol <- data.table::data.table(
    feature_id = "F1",
    rt = 1.0,
    mz = 100.0,
    adduct = "[M+H]+",
    sample = "S1",
    rt_min = 0.9,
    rt_max = 1.1
  )
  df_fea_min <- data.table::data.table(
    feature_id = "F2",
    rt = 1.0,
    mz = 122.0,
    adduct = "[M+Na]+",
    sample = "S1"
  )
  result <- build_feature_pairs_within_rt(
    df_rt_tol,
    df_fea_min,
    tolerance_ppm = 10
  )
  expect_equal(result$delta[[1]], 22.0, tolerance = 1e-6)
})

# ── build_adduct_pair_differences ─────────────────────────────────────────────

test_that("build_adduct_pair_differences finds H -> Na mass difference", {
  add_tbl <- tidytable::tidytable(
    adduct = c("[M+H]+", "[M+Na]+"),
    adduct_mass = c(1.007276, 22.989218)
  )
  result <- build_adduct_pair_differences(
    add_clu_table = add_tbl,
    tolerance_ppm = 5,
    max_mz = 2000
  )
  expect_true(nrow(result) >= 1L)
  expect_true("Distance" %in% names(result))
  expect_true("Group1" %in% names(result))
  expect_true("Group2" %in% names(result))
  # Na-H mass difference
  expected_dist <- 22.989218 - 1.007276
  expect_equal(result$Distance[[1]], expected_dist, tolerance = 1e-4)
})

test_that("build_adduct_pair_differences excludes identical adducts", {
  add_tbl <- tidytable::tidytable(
    adduct = c("[M+H]+"),
    adduct_mass = c(1.007276)
  )
  result <- build_adduct_pair_differences(add_tbl, 5, 2000)
  expect_equal(nrow(result), 0L)
})

# ── join_couples_with_neutral_losses ──────────────────────────────────────────

test_that("join_couples_with_neutral_losses matches neutral loss in delta window", {
  # couples_diff: feature pair with a delta window
  h2o_mass <- 18.010565
  df_couples_diff <- data.table::data.table(
    feature_id = "F1",
    adduct = "[M+H]+",
    feature_id_dest = "F2",
    adduct_dest = "[M+H]+",
    delta_min = h2o_mass - 0.01,
    delta_max = h2o_mass + 0.01
  )
  neutral_losses <- data.table::data.table(
    loss = "H2O",
    mass = h2o_mass
  )

  result <- join_couples_with_neutral_losses(df_couples_diff, neutral_losses)
  expect_equal(nrow(result), 1L)
  expect_equal(as.character(result$loss[[1]]), "H2O")
})

test_that("join_couples_with_neutral_losses returns empty when no match", {
  df_couples_diff <- data.table::data.table(
    feature_id = "F1",
    adduct = "[M+H]+",
    feature_id_dest = "F2",
    adduct_dest = "[M+H]+",
    delta_min = 0.0,
    delta_max = 1.0
  )
  neutral_losses <- data.table::data.table(
    loss = "H2O",
    mass = 18.01
  )

  result <- join_couples_with_neutral_losses(df_couples_diff, neutral_losses)
  expect_equal(nrow(result), 0L)
})

# ── join_multi_with_addlossed ─────────────────────────────────────────────────

test_that("join_multi_with_addlossed joins when mass and RT are in window", {
  target_mass <- 180.063
  df_multi <- data.table::data.table(
    feature_id = "F1",
    adduct = "[M+H]+",
    rt = 1.0,
    mz = 181.07,
    rt_min = 0.9,
    rt_max = 1.1,
    mass_min = target_mass - 0.02,
    mass_max = target_mass + 0.02
  )
  df_addlossed <- data.table::data.table(
    rt = 1.05,
    mass = target_mass
  )

  result <- join_multi_with_addlossed(df_multi, df_addlossed)
  expect_equal(nrow(result), 1L)
  expect_equal(as.character(result$feature_id[[1]]), "F1")
  expect_equal(result$mass[[1]], target_mass, tolerance = 1e-6)
})

test_that("join_multi_with_addlossed returns empty when outside window", {
  df_multi <- data.table::data.table(
    feature_id = "F1",
    adduct = "[M+H]+",
    rt = 1.0,
    mz = 181.07,
    rt_min = 0.9,
    rt_max = 1.1,
    mass_min = 179.0,
    mass_max = 181.0
  )
  df_addlossed <- data.table::data.table(
    rt = 5.0, # outside RT window
    mass = 180.0
  )

  result <- join_multi_with_addlossed(df_multi, df_addlossed)
  expect_equal(nrow(result), 0L)
})
