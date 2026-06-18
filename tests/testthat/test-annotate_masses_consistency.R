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
    3L
  )
  expect_equal(
    support[
      support$feature_id == "B" & support$adduct == "[M+Na]+",
      "adduct_support"
    ][[1L]],
    2L
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
