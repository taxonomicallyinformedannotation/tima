test_that("typed adduct lookup recovers inside-multimer neutral mass", {
  proton <- ATOMIC_MONOISOTOPIC_MASS[["H"]] - ELECTRON_MASS_DA
  h2o <- 2 * ATOMIC_MONOISOTOPIC_MASS[["H"]] + ATOMIC_MONOISOTOPIC_MASS[["O"]]
  M <- 212.0874
  mz_inside <- 2 * (M - h2o) + proton

  uni <- build_adduct_universe(
    adducts_list = list(
      M = c(1L, 2L),
      charge_carriers = list(pos = c("H")),
      charges = list(pos = c(1L))
    ),
    clusters_list = list(pos = character()),
    neutral_losses_list = c("H2O"),
    polarity = "pos"
  )
  lookup <- build_adduct_lookup(uni)

  recovered <- calculate_neutral_mass_from_lookup(
    mzs = mz_inside,
    adducts = "[2(M-H2O)+H]+",
    adduct_lookup = lookup,
    fallback = FALSE
  )

  expect_equal(recovered, M, tolerance = 1e-7)
})

test_that("direct adduct transitions exclude dimer deltas that depend on neutral mass", {
  uni <- build_adduct_universe(
    adducts_list = list(pos = c("[M+H]+", "[2M+H]+"), neg = c("[M-H]-")),
    clusters_list = list(pos = character(), neg = character()),
    neutral_losses_list = character(),
    polarity = "pos"
  )

  transitions <- build_universe_transition_tables(uni)
  expect_false(any(
    transitions$adduct_diffs$Group1 == "[M+H]+" &
      transitions$adduct_diffs$Group2 == "[2M+H]+"
  ))
})

test_that("cluster transitions scale modifier delta by charge state", {
  h2o <- 2 * ATOMIC_MONOISOTOPIC_MASS[["H"]] + ATOMIC_MONOISOTOPIC_MASS[["O"]]
  uni <- build_adduct_universe(
    adducts_list = list(
      M = c(1L),
      charge_carriers = list(pos = c("H")),
      charges = list(pos = c(2L))
    ),
    clusters_list = list(pos = c("H2O")),
    neutral_losses_list = character(),
    polarity = "pos"
  )

  transitions <- build_universe_transition_tables(uni)
  cluster_edges <- transitions$cluster_transitions |>
    tidytable::filter(adduct == "[M+2H]2+", adduct_dest == "[M+H2O+2H]2+")

  expect_equal(nrow(cluster_edges), 1L)
  expect_equal(cluster_edges$mass, h2o / 2, tolerance = 1e-9)
})
