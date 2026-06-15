# Test Suite: annotate_masses_co2na ----
# Regression tests for formate+sodium adduct recovery around delta ~67.99 Da.

library(testthat)


test_that("[M-H]- to [M+CO2Na]- delta falls in the dominant ~67.99 Da bin", {
  neutral_mass <- 400

  mz_low <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M-H]-"
  )
  mz_high <- calculate_mz_from_mass(
    neutral_mass = neutral_mass,
    adduct_string = "[M+CO2Na]-"
  )

  delta <- mz_high - mz_low

  # Dominant bin reported in logs: (67.9834, 67.9925]
  expect_gt(delta, 67.9834)
  expect_lte(delta, 67.9925)
})


test_that("cluster expansion yields a state equivalent to [M+CO2Na]-", {
  base_hyps <- tidytable::tidytable(
    feature_id = "f1",
    adduct = "[M-H2+Na]-",
    source = "pair",
    is_preassigned = FALSE,
    candidate_adduct_origin = "supported"
  )

  # f1 is the source (base adduct known); f2 is the destination (cluster dest).
  cluster_edges <- tidytable::tidytable(
    feature_id = "f1",
    feature_id_dest = "f2",
    cluster = "CH2O2"
  )

  # Propagate [M-H2+Na]- + CH2O2 from source f1 to destination f2.
  out <- propagate_modifier_src_to_dest(
    base_hyps = base_hyps,
    edges = cluster_edges,
    mod_col = "cluster",
    mod_sign = "+"
  )

  # String notation can differ, but the physical state key must match CO2Na.
  expected_key <- adduct_to_state_key("[M+CO2Na]-")
  observed_keys <- vapply(out$adduct, adduct_to_state_key, character(1L))
  expect_true(expected_key %in% observed_keys)
})


test_that("modifier simplification keeps dehydration loss separate from proton carrier", {
  out <- .simplify_adduct_after_modifier("[M-H2O+H]+")
  expect_equal(out, "[M-H2O+H]+")
  expect_false(identical(out, "[M-HO]+"))
})


test_that("NH3/NH4 cancellation is preserved then reduced to +H carrier", {
  simplified <- .simplify_adduct_after_modifier("[M-H3N+C2H7N+H4N]+")
  expect_true(grepl("-H3N", simplified, fixed = TRUE))

  harmonized <- harmonize_adducts(
    tidytable::tidytable(adduct = simplified),
    adducts_translations = adducts_translations
  )
  expect_equal(harmonized$adduct[[1L]], "[M+C2H7N+H]+")
})


test_that("cluster propagation keeps carrier and cluster terms distinct in notation", {
  base_hyps <- tidytable::tidytable(
    feature_id = "f1",
    adduct = "[M-H2+Na]-",
    source = "pair",
    is_preassigned = FALSE,
    candidate_adduct_origin = "supported"
  )
  cluster_edges <- tidytable::tidytable(
    feature_id = "f1",
    feature_id_dest = "f2",
    cluster = "CH2O2"
  )

  out <- propagate_modifier_src_to_dest(
    base_hyps = base_hyps,
    edges = cluster_edges,
    mod_col = "cluster",
    mod_sign = "+"
  )

  expect_true("[M-H2+CH2O2+Na]-" %in% out$adduct)
})


test_that("cluster-edge path covers ~67.99 Da delta via two-step cluster pipeline", {
  # The ~67.99 Da delta ([M-H]- ↔ [M+CO2Na]-) is not a direct adduct-pair
  # delta. It is found via the two-step cluster pipeline:
  #   step 1: adduct-pair edge  [M-H]-  ↔ [M-H2+Na]-  (delta ≈ 21.98 Da)
  #   step 2: cluster edge      [M-H2+Na]- ↔ feature_C (delta ≈ 46.01 Da = CH2O2)
  #   result: propagate_modifier_src_to_dest propagates [M-H2+Na]- + CH2O2
  #           to feature_C → [M+CO2+Na]-, whose state key equals [M+CO2Na]-.
  #
  # This path does NOT pollute the adduct-pair graph with virtual states.

  neutral_mass <- 400
  mz_base <- calculate_mz_from_mass(neutral_mass, "[M-H]-")
  mz_sod <- calculate_mz_from_mass(neutral_mass, "[M-H2+Na]-")
  mz_co2na <- calculate_mz_from_mass(neutral_mass, "[M+CO2Na]-")

  # Step-1 delta (adduct pair): should be ~21.98 Da
  delta_step1 <- mz_sod - mz_base
  expect_gt(delta_step1, 21.97)
  expect_lte(delta_step1, 21.99)

  # Step-2 delta (cluster CH2O2): should be ~46.01 Da
  delta_step2 <- mz_co2na - mz_sod
  ch2o2_mass <- MetaboCoreUtils::calculateMass("CH2O2")
  expect_lt(abs(delta_step2 - ch2o2_mass), 0.005)

  # Total two-step delta covers the ~67.99 Da gap
  expect_gt(delta_step1 + delta_step2, 67.9834)
  expect_lte(delta_step1 + delta_step2, 67.9925)

  # Verify propagate_modifier_src_to_dest yields the correct state key for dest
  # feature_sod has [M-H2+Na]- (source); a cluster edge connects it to feature_c
  base_hyps <- tidytable::tidytable(
    feature_id = "feature_sod",
    adduct = "[M-H2+Na]-",
    source = "pair",
    is_preassigned = FALSE,
    candidate_adduct_origin = "supported"
  )
  cluster_edges <- tidytable::tidytable(
    feature_id = "feature_sod",
    feature_id_dest = "feature_c",
    cluster = "CH2O2"
  )
  out <- propagate_modifier_src_to_dest(
    base_hyps = base_hyps,
    edges = cluster_edges,
    mod_col = "cluster",
    mod_sign = "+"
  )

  expect_equal(out$feature_id, "feature_c")
  expected_key <- adduct_to_state_key("[M+CO2Na]-")
  observed_keys <- vapply(out$adduct, adduct_to_state_key, character(1L))
  expect_true(expected_key %in% observed_keys)
})
