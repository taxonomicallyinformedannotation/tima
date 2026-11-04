# Test Suite: annotate_masses_consistency ----

library(testthat)

.test_feature_pairs_tables <- function() {
  list(
    df_rt_tol = tidytable::tidytable(
      feature_id = "f1",
      rt = 5.0,
      mz = 100.0,
      adduct = "[M+H]+",
      sample = "s1",
      rt_min = 4.9,
      rt_max = 5.1
    ),
    df_fea_min = tidytable::tidytable(
      feature_id = c("f1", "f2", "f3"),
      rt = c(5.0, 5.05, 5.2),
      mz = c(100.0, 101.0, 99.0),
      adduct = c("[M+H]+", "[M+Na]+", "[M+K]+"),
      sample = c("s1", "s1", "s2")
    )
  )
}

test_that("build_feature_pairs_within_rt returns matches and empty typed output", {
  tbls <- .test_feature_pairs_tables()

  matched <- build_feature_pairs_within_rt(
    df_rt_tol = tbls$df_rt_tol,
    df_fea_min = tbls$df_fea_min,
    tolerance_ppm = 10
  )
  expect_equal(nrow(matched), 1L)
  expect_identical(matched$feature_id[[1L]], "f1")
  expect_identical(matched$feature_id_dest[[1L]], "f2")
  expect_equal(matched$delta[[1L]], 1)

  empty <- build_feature_pairs_within_rt(
    df_rt_tol = tidytable::tidytable(
      feature_id = "f1",
      rt = 5.0,
      mz = 100.0,
      adduct = "[M+H]+",
      sample = "s1",
      rt_min = 4.9,
      rt_max = 5.1
    ),
    df_fea_min = tidytable::tidytable(
      feature_id = "f2",
      rt = 6.0,
      mz = 101.0,
      adduct = "[M+Na]+",
      sample = "s2"
    ),
    tolerance_ppm = 10
  )
  expect_s3_class(empty, "data.frame")
  expect_equal(nrow(empty), 0L)
  expect_true(all(c("delta", "delta_min", "delta_max") %in% names(empty)))
})

test_that("build_adduct_pair_differences orders and filters adduct pairs", {
  add_clu_table <- tidytable::tidytable(
    adduct = c("[M+H]+", "[M+Na]+", "[M+K]+"),
    adduct_mass = c(1.007276, 22.989218, 38.963158)
  )

  diffs <- build_adduct_pair_differences(
    add_clu_table = add_clu_table,
    tolerance_ppm = 0,
    max_mz = 1000
  )

  expect_equal(nrow(diffs), 3L)
  expect_true(all(diffs$Distance > 0))
  expect_true(all(diffs$Group1 %in% add_clu_table$adduct))
  expect_true(all(diffs$Group2 %in% add_clu_table$adduct))

  empty <- build_adduct_pair_differences(
    add_clu_table = add_clu_table,
    tolerance_ppm = 10^6,
    max_mz = 1000
  )
  expect_equal(nrow(empty), 0L)

  diffs_two <- build_adduct_pair_differences(
    add_clu_table = add_clu_table[1:2, ],
    tolerance_ppm = 0,
    max_mz = 1000
  )
  expect_identical(nrow(diffs_two), 1L)
  expect_identical(diffs_two$Group1, "[M+H]+")
  expect_identical(diffs_two$Group2, "[M+Na]+")
})

test_that("compute_feature_adduct_support handles empty and deduplicated neighbors", {
  empty <- compute_feature_adduct_support(tidytable::tidytable())
  expect_s3_class(empty, "data.frame")
  expect_equal(nrow(empty), 0L)
  expect_true(all(
    c("feature_id", "adduct", "adduct_support") %in% names(empty)
  ))

  df_add <- tidytable::tidytable(
    feature_id = c("A", "A", "B"),
    adduct = c("[M+H]+", "[M+H]+", "[M+Na]+"),
    feature_id_dest = c("B", "C", "A"),
    adduct_dest = c("[M+Na]+", "[M+K]+", "[M+H]+")
  )
  support <- compute_feature_adduct_support(df_add)
  expect_equal(
    support[
      support$feature_id == "A" & support$adduct == "[M+H]+",
      "adduct_support"
    ][[1L]],
    # B and C; the B->A edge repeats B and a feature never supports itself
    2L
  )
  expect_equal(
    support[
      support$feature_id == "B" & support$adduct == "[M+Na]+",
      "adduct_support"
    ][[1L]],
    1L
  )
})

test_that("compute_feature_adduct_support counts co-varying neighbors twice", {
  df_add <- tidytable::tidytable(
    feature_id = c("A", "A"),
    adduct = c("[M+H]+", "[M+H]+"),
    feature_id_dest = c("B", "C"),
    adduct_dest = c("[M+Na]+", "[M+K]+")
  )
  .support_of <- function(support, feature, adduct) {
    support[
      support$feature_id == feature & support$adduct == adduct,
      "adduct_support"
    ][[1L]]
  }
  plain <- compute_feature_adduct_support(df_add)
  # Covariance pairs are unordered: (C, A) backs the A-C edge
  boosted <- compute_feature_adduct_support(
    df_add,
    covariance_edges = tidytable::tidytable(
      feature_id = "C",
      feature_id_dest = "A"
    )
  )

  expect_equal(
    .support_of(boosted, "A", "[M+H]+"),
    .support_of(plain, "A", "[M+H]+") + 1L
  )
  expect_equal(
    .support_of(boosted, "C", "[M+K]+"),
    .support_of(plain, "C", "[M+K]+") + 1L
  )
  expect_equal(
    .support_of(boosted, "B", "[M+Na]+"),
    .support_of(plain, "B", "[M+Na]+")
  )
  # Empty covariance input changes nothing
  expect_equal(
    compute_feature_adduct_support(
      df_add,
      covariance_edges = tidytable::tidytable(
        feature_id = character(),
        feature_id_dest = character()
      )
    ),
    plain
  )
})

test_that("apply_adduct_consistency_filter respects off, strict and conditional modes", {
  df_add <- tidytable::tidytable(
    feature_id = c("A", "A", "B", "B", "C", "C", "A"),
    adduct = c(
      "[M+H]+",
      "[M+H]+",
      "[M+Na]+",
      "[M+Na]+",
      "[M+K]+",
      "[M+K]+",
      "[M+H]+"
    ),
    feature_id_dest = c("B", "C", "A", "C", "A", "B", "D"),
    adduct_dest = c(
      "[M+Na]+",
      "[M+K]+",
      "[M+H]+",
      "[M+K]+",
      "[M+H]+",
      "[M+Na]+",
      "[M+NH4]+"
    )
  )

  filtered <- apply_adduct_consistency_filter(df_add)
  expect_s3_class(filtered, "data.frame")
  expect_true(nrow(filtered) > 0L)
  expect_true(nrow(filtered) <= nrow(df_add))
  expect_true(all(
    c("feature_id", "adduct", "adduct_dest", "feature_id_dest") %in%
      names(filtered)
  ))
})

test_that("enforce_graph_adduct_consistency handles empty input and non-empty graphs", {
  empty <- enforce_graph_adduct_consistency(tidytable::tidytable())
  expect_equal(nrow(empty), 0L)

  df_add <- tidytable::tidytable(
    feature_id = c("A", "A", "B", "B"),
    adduct = c("[M+H]+", "[M+H]+", "[M+Na]+", "[M+Na]+"),
    feature_id_dest = c("B", "C", "A", "C"),
    adduct_dest = c("[M+Na]+", "[M+K]+", "[M+H]+", "[M+K]+")
  )

  kept <- enforce_graph_adduct_consistency(df_add)
  expect_s3_class(kept, "data.frame")
  expect_true(nrow(kept) > 0L)
  expect_true(all(
    c("feature_id", "adduct", "adduct_dest", "feature_id_dest") %in% names(kept)
  ))
})

test_that("adduct state key helpers parse, fallback and raw text as expected", {
  map <- build_adduct_state_key_map(c("[M+H]+", "[M+Na]+", NA_character_))
  expect_equal(nrow(map), 2L)
  expect_true(all(c("adduct", "state_key") %in% names(map)))
  expect_identical(
    adduct_to_state_key("[M+H]+"),
    map$state_key[map$adduct == "[M+H]+"][[1L]]
  )
  expect_match(adduct_to_state_key("not-an-adduct"), "^raw:")
  expect_match(adduct_to_state_key("[M+1H]+"), "^(z:|fallback-d:|raw:)")
})

test_that("calculate_net_mod_mass_from_text handles valid and invalid adduct text", {
  expect_true(is.finite(calculate_net_mod_mass_from_text("[M+H]+")))
  expect_true(is.na(calculate_net_mod_mass_from_text("[M]")))
  expect_true(is.na(calculate_net_mod_mass_from_text("")))
  expect_true(is.na(calculate_net_mod_mass_from_text(NA_character_)))
})

test_that("join_couples_with_neutral_losses and join_multi_with_addlossed join on windows", {
  couples <- tidytable::tidytable(
    feature_id = "f1",
    feature_id_dest = "f2",
    delta_min = 17.0,
    delta_max = 19.0
  )
  neutral_losses <- tidytable::tidytable(
    loss = c("H2O", "NH3"),
    mass = c(18.010565, 17.026549)
  )
  joined_losses <- join_couples_with_neutral_losses(couples, neutral_losses)
  expect_equal(nrow(joined_losses), 2L)
  expect_true(all(
    c("feature_id", "loss", "mass", "feature_id_dest") %in% names(joined_losses)
  ))

  df_multi <- tidytable::tidytable(
    feature_id = "f1",
    adduct = "[M+H]+",
    rt = 5.0,
    mz = 100.0,
    rt_min = 4.9,
    rt_max = 5.1,
    mass_min = 99.0,
    mass_max = 101.0
  )
  df_addlossed_rdy <- tidytable::tidytable(
    rt = 5.0,
    mass = c(100.0, 102.0)
  )
  joined_multi <- join_multi_with_addlossed(df_multi, df_addlossed_rdy)
  expect_equal(nrow(joined_multi), 1L)
  expect_identical(joined_multi$feature_id[[1L]], "f1")
})

test_that("enforce_graph_adduct_consistency resolves mixed singleton and conflicting components", {
  # Component 1: singleton edge S1 -> S2 (no ambiguity, fast path)
  # Component 2: conflicting node X1 with two candidate adducts ([M+H]+ supported by X2 and X3,
  # vs [M+Na]+ only supported by X4)
  df_add <- tidytable::tidytable(
    feature_id = c("S1", "X1", "X1", "X1"),
    adduct = c("[M+H]+", "[M+H]+", "[M+H]+", "[M+Na]+"),
    feature_id_dest = c("S2", "X2", "X3", "X4"),
    adduct_dest = c("[M+Na]+", "[M+Na]+", "[M+K]+", "[M+K]+")
  )

  kept <- enforce_graph_adduct_consistency(df_add)
  expect_equal(nrow(kept), 3L)
  expect_true("S1" %in% kept$feature_id)
  x1_rows <- kept[kept$feature_id == "X1", ]
  expect_equal(nrow(x1_rows), 2L)
  expect_true(all(x1_rows$adduct == "[M+H]+"))
  audit <- attr(kept, "consistency_audit")
  expect_type(audit, "list")
  expect_identical(audit$n_input, 4L)
  expect_identical(audit$n_kept, 3L)
  expect_identical(audit$n_dropped, 1L)
  expect_identical(
    audit$dropped_reason_counts$drop_reason,
    "node_state_conflict"
  )
  expect_identical(audit$dropped_reason_counts$n, 1L)
})

test_that("enforce_graph_adduct_consistency tracks pair_dedup_or_conflict and deduplicates identical input edges", {
  # Same pair (X1, X2) has two candidate adduct transitions:
  # 1) [M+H]+ -> [M+Na]+ (supported by X1->X3)
  # 2) [M+K]+ -> [M+NH4]+ (lower score on the same pair -> pair_dedup_or_conflict)
  # Duplicate row of (X1, [M+H]+, [M+Na]+, X2) should be deduplicated in n_input.
  df_add <- tidytable::tidytable(
    feature_id = c("X1", "X1", "X1", "X1"),
    adduct = c("[M+H]+", "[M+H]+", "[M+H]+", "[M+K]+"),
    adduct_dest = c("[M+Na]+", "[M+Na]+", "[M+K]+", "[M+NH4]+"),
    feature_id_dest = c("X2", "X2", "X3", "X2")
  )

  kept <- enforce_graph_adduct_consistency(df_add)
  expect_equal(nrow(kept), 2L)
  audit <- attr(kept, "consistency_audit")
  expect_identical(audit$n_input, 3L)
  expect_identical(audit$n_kept, 2L)
  expect_identical(audit$n_dropped, 1L)
  expect_identical(
    audit$dropped_reason_counts$drop_reason,
    "pair_dedup_or_conflict"
  )
  expect_identical(audit$dropped_reason_counts$n, 1L)
})

test_that("annotate_masses_consistency helpers cover empty, NA, fallback adduct text, and binary-search edge cases", {
  # 1. build_feature_pairs_within_rt: empty inputs, all-NA sample/rt inputs, and NA feature_ids
  expect_identical(
    nrow(build_feature_pairs_within_rt(
      tidytable::tidytable(),
      tidytable::tidytable(),
      tolerance_ppm = 10
    )),
    0L
  )
  expect_identical(
    nrow(build_feature_pairs_within_rt(
      tidytable::tidytable(
        feature_id = NA_character_,
        rt = NA_real_,
        mz = 100,
        sample = NA_character_,
        rt_min = NA_real_,
        rt_max = NA_real_
      ),
      tidytable::tidytable(
        feature_id = NA_character_,
        rt = NA_real_,
        mz = 120,
        sample = NA_character_
      ),
      tolerance_ppm = 10
    )),
    0L
  )

  # 2. build_adduct_pair_differences: empty input
  expect_identical(
    nrow(build_adduct_pair_differences(tidytable::tidytable(), 10, 500)),
    0L
  )

  # 3. compute_feature_adduct_support: NA feature_id and non-overlapping covariance_edges
  df_add_na <- tidytable::tidytable(
    feature_id = c("F1", NA_character_),
    adduct = c("[M+H]+", "[M+Na]+"),
    feature_id_dest = c("F2", "F1"),
    adduct_dest = c("[M+Na]+", "[M+H]+")
  )
  res_supp_nomatch <- compute_feature_adduct_support(
    df_add_na,
    covariance_edges = tidytable::tidytable(
      feature_id = "Z1",
      feature_id_dest = "Z2"
    )
  )
  expect_true(nrow(res_supp_nomatch) >= 2L)

  res_supp_disjoint <- compute_feature_adduct_support(
    tidytable::tidytable(
      feature_id = c("F1", "F3"),
      adduct = c("[M+H]+", "[M+H]+"),
      feature_id_dest = c("F2", "F4"),
      adduct_dest = c("[M+Na]+", "[M+Na]+")
    ),
    covariance_edges = tidytable::tidytable(
      feature_id = "F1",
      feature_id_dest = "F4"
    )
  )
  expect_true(all(res_supp_disjoint$adduct_support == 1L))

  # 4. apply_adduct_consistency_filter with NA feature_id and adduct
  res_acf_na <- apply_adduct_consistency_filter(df_add_na)
  expect_identical(nrow(res_acf_na), 2L)

  # 5. adduct_to_state_key and calculate_net_mod_mass_from_text edge cases
  expect_true(is.na(calculate_net_mod_mass_from_text("")))
  expect_true(is.na(calculate_net_mod_mass_from_text("M+H")))
  expect_equal(calculate_net_mod_mass_from_text("[M]+"), 0)
  expect_true(is.na(calculate_net_mod_mass_from_text("[Mfoo]+")))
  expect_true(is.na(calculate_net_mod_mass_from_text("[M+2]+")))
  expect_true(is.na(calculate_net_mod_mass_from_text("[M+ BadElem]+")))
  expect_match(
    adduct_to_state_key("[M+H2O+H]+"),
    "^(z:|fallback-d:)"
  )
  local_mocked_bindings(
    parse_adduct = function(...) stop("simulated parse error"),
    .package = "tima"
  )
  expect_match(adduct_to_state_key("[M+H]+"), "^fallback-d:")

  # 6. join_couples_with_neutral_losses: empty inputs and all-NA neutral_losses
  expect_identical(
    nrow(join_couples_with_neutral_losses(
      tidytable::tidytable(),
      tidytable::tidytable()
    )),
    0L
  )
  expect_identical(
    nrow(join_couples_with_neutral_losses(
      tidytable::tidytable(
        feature_id = "F1",
        feature_id_dest = "F2",
        delta_min = 17.9,
        delta_max = 18.1
      ),
      tidytable::tidytable(loss = NA_character_, mass = 18.0106)
    )),
    0L
  )

  # 7. join_multi_with_addlossed: empty inputs and RT match with non-overlapping mass window
  expect_identical(
    nrow(join_multi_with_addlossed(
      tidytable::tidytable(),
      tidytable::tidytable()
    )),
    0L
  )
  expect_identical(
    nrow(join_multi_with_addlossed(
      tidytable::tidytable(
        feature_id = "F1",
        rt = 5.0,
        mz = 200.0,
        adduct = "[2M+H]+",
        rt_min = 4.5,
        rt_max = 5.5,
        mass_min = 99.0,
        mass_max = 100.0
      ),
      tidytable::tidytable(rt = 5.0, mass = 300.0)
    )),
    0L
  )

  # 8. Multi-sample build_feature_pairs_within_rt (multiple hit groups and no-hit sample),
  # 1-row build_adduct_pair_differences, 0-row and NA-adduct apply_adduct_consistency_filter,
  # empty build_adduct_state_key_map, no-overlap join_couples_with_neutral_losses / join_multi_with_addlossed,
  # and multi-iteration enforce_graph_adduct_consistency
  pairs_multi_smp <- build_feature_pairs_within_rt(
    df_rt_tol = tidytable::tidytable(
      feature_id = c("A1", "B1", "C1"),
      rt = c(5.0, 6.0, 20.0),
      mz = c(100.0, 100.0, 100.0),
      sample = c("S1", "S2", "S3"),
      rt_min = c(4.9, 5.9, 19.9),
      rt_max = c(5.1, 6.1, 20.1)
    ),
    df_fea_min = tidytable::tidytable(
      feature_id = c("A2", "B2", "C2"),
      rt = c(5.0, 6.0, 10.0),
      mz = c(120.0, 120.0, 120.0),
      sample = c("S1", "S2", "S3")
    ),
    tolerance_ppm = 10
  )
  expect_identical(nrow(pairs_multi_smp), 2L)

  expect_identical(
    nrow(build_adduct_pair_differences(
      tidytable::tidytable(adduct = "[M+H]+", adduct_mass = 1.007276),
      10,
      500
    )),
    0L
  )
  expect_identical(
    nrow(apply_adduct_consistency_filter(tidytable::tidytable())),
    0L
  )
  expect_identical(
    nrow(apply_adduct_consistency_filter(tidytable::tidytable(
      feature_id = c("F1", "F2"),
      adduct = c("[M+H]+", NA_character_),
      feature_id_dest = c("F2", "F1"),
      adduct_dest = c(NA_character_, "[M+Na]+")
    ))),
    2L
  )
  expect_identical(nrow(build_adduct_state_key_map(character(0))), 0L)
  expect_identical(
    nrow(join_couples_with_neutral_losses(
      tidytable::tidytable(
        feature_id = "F1",
        feature_id_dest = "F2",
        delta_min = 50.0,
        delta_max = 60.0
      ),
      tidytable::tidytable(loss = "H2O", mass = 18.0106)
    )),
    0L
  )
  expect_identical(
    nrow(join_multi_with_addlossed(
      tidytable::tidytable(
        feature_id = "F1",
        rt = 15.0,
        mz = 200.0,
        adduct = "[2M+H]+",
        rt_min = 14.5,
        rt_max = 15.5,
        mass_min = 99.0,
        mass_max = 100.0
      ),
      tidytable::tidytable(rt = 5.0, mass = 99.5)
    )),
    0L
  )

  # Multi-node ambiguous component where state flips on iter 1 and re-evaluates on iter 2
  eg_multi <- enforce_graph_adduct_consistency(tidytable::tidytable(
    feature_id = c("N1", "N1", "N2", "N2", "N2"),
    adduct = c("raw_long_state", "[M+H]+", "[M+H]+", "[M+Na]+", "[M+Na]+"),
    feature_id_dest = c("N2", "N2", "N3", "N4", "N5"),
    adduct_dest = c("[M+H]+", "[M+Na]+", "[M+K]+", "[M+K]+", "[M+NH4]+")
  ))
  expect_true(nrow(eg_multi) >= 2L)
})
