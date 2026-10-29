# Test Suite: adduct_universe_matching ----

## Helpers ----

# Ion masses derived independently from atomic masses and the electron mass.
mass_h <- ATOMIC_MONOISOTOPIC_MASS[["H"]]
mass_na <- ATOMIC_MONOISOTOPIC_MASS[["Na"]]
mass_k <- ATOMIC_MONOISOTOPIC_MASS[["K"]]
mass_h2o <- 2 * mass_h + ATOMIC_MONOISOTOPIC_MASS[["O"]]
proton <- mass_h - ELECTRON_MASS_DA
sodium_ion <- mass_na - ELECTRON_MASS_DA

# Hand-built typed universe rows (same schema as empty_adduct_universe()).
universe_rows <- function(
  adduct,
  n_mer = 1L,
  n_iso = 0L,
  z = 1L,
  adduct_mass,
  clusters = NULL,
  losses = NULL,
  carriers = NULL
) {
  n <- length(adduct)
  as_list_col <- function(x) {
    x %||% rep(list(integer()), n)
  }
  tidytable::tidytable(
    adduct = adduct,
    n_mer = as.integer(rep_len(n_mer, n)),
    n_iso = as.integer(rep_len(n_iso, n)),
    z = as.integer(rep_len(z, n)),
    adduct_mass = adduct_mass,
    adduct_mass_per_monomer = 0,
    carriers = as_list_col(carriers),
    clusters = as_list_col(clusters),
    losses = as_list_col(losses)
  )
}

structured_universe <- function(
  n_mer = 1L,
  carriers = c("H", "Na"),
  charges = 1L,
  clusters = character(),
  losses = character()
) {
  build_adduct_universe(
    adducts_list = list(
      M = n_mer,
      charge_carriers = list(pos = carriers),
      charges = list(pos = charges)
    ),
    clusters_list = list(pos = clusters),
    neutral_losses_list = losses,
    polarity = "pos"
  )
}

## component_count_key ----

test_that("component_count_key is empty for NULL, empty or all-zero input", {
  expect_identical(component_count_key(NULL), "")
  expect_identical(component_count_key(integer()), "")
  expect_identical(component_count_key(c(H2O = 0L, Na = 0L)), "")
  expect_identical(component_count_key(c(H2O = NA_integer_)), "")
})

test_that("component_count_key drops zeros and unnamed terms and sorts names", {
  x <- c(Na = 1L, H2O = 0L, H = 2L)
  expect_identical(component_count_key(x), "H:2|Na:1")

  unnamed <- c(1L, 3L)
  names(unnamed) <- c("", "K")
  expect_identical(component_count_key(unnamed), "K:3")
})

test_that("component_count_key is order invariant and coerces doubles", {
  a <- c(CO2 = 1, H2O = 2)
  b <- c(H2O = 2L, CO2 = 1L)
  expect_identical(component_count_key(a), component_count_key(b))
  expect_identical(component_count_key(a), "CO2:1|H2O:2")
})

## single_component_increment ----

test_that("single_component_increment names the one term added by +1", {
  expect_identical(single_component_increment(NULL, c(H2O = 1L)), "H2O")
  expect_identical(
    single_component_increment(c(H2O = 1L), c(H2O = 2L)),
    "H2O"
  )
  expect_identical(
    single_component_increment(c(H2O = 1L), c(CH4O = 1L, H2O = 1L)),
    "CH4O"
  )
  # Doubles are coerced like integers.
  expect_identical(single_component_increment(c(H2O = 1), c(H2O = 2)), "H2O")
})

test_that("single_component_increment rejects anything but a single +1 step", {
  # No components at all.
  expect_identical(single_component_increment(NULL, NULL), NA_character_)
  expect_identical(
    single_component_increment(integer(), integer()),
    NA_character_
  )
  # Identical vectors (delta 0).
  expect_identical(
    single_component_increment(c(H2O = 1L), c(H2O = 1L)),
    NA_character_
  )
  # +2 of the same term.
  expect_identical(
    single_component_increment(NULL, c(H2O = 2L)),
    NA_character_
  )
  # Two different terms each +1.
  expect_identical(
    single_component_increment(NULL, c(H2O = 1L, CH4O = 1L)),
    NA_character_
  )
  # A decrease (reverse direction).
  expect_identical(
    single_component_increment(c(H2O = 1L), NULL),
    NA_character_
  )
  # Swap: one term removed, another added (net +0).
  expect_identical(
    single_component_increment(c(H2O = 1L), c(CH4O = 1L)),
    NA_character_
  )
})

## calculate_mz_from_lookup ----

test_that("lookup m/z matches hand-computed values and recycles scalar M", {
  lookup <- build_adduct_lookup(structured_universe(
    n_mer = c(1L, 2L),
    charges = c(1L, 2L)
  ))
  m <- 180.0634
  adducts <- c("[M+H]+", "[M+Na]+", "[2M+H]+", "[M+2H]2+")
  expected <- c(
    m + proton,
    m + sodium_ion,
    2 * m + proton,
    (m + 2 * proton) / 2
  )

  # Scalar neutral mass is recycled over all adducts.
  out <- calculate_mz_from_lookup(
    neutral_mass = m,
    adducts = adducts,
    adduct_lookup = lookup,
    fallback = FALSE
  )
  expect_equal(out, expected, tolerance = 1e-9)
})

test_that("calculate_mz_from_lookup adds n_iso isotope shifts per row", {
  m <- 250.1
  uni <- universe_rows(
    adduct = c("[M+H]+", "[M1+H]+", "[M2+H]+", "[M1+2H]2+"),
    n_iso = c(0L, 1L, 2L, 1L),
    z = c(1L, 1L, 1L, 2L),
    adduct_mass = c(proton, proton, proton, 2 * proton)
  )
  lookup <- build_adduct_lookup(uni)

  out <- calculate_mz_from_lookup(
    neutral_mass = c(m, m, m, m),
    adducts = uni$adduct,
    adduct_lookup = lookup,
    fallback = FALSE
  )
  expect_equal(
    out,
    c(
      m + proton,
      m + proton + ISOTOPE_MASS_SHIFT_DALTONS,
      m + proton + 2 * ISOTOPE_MASS_SHIFT_DALTONS,
      (m + 2 * proton) / 2 + ISOTOPE_MASS_SHIFT_DALTONS
    ),
    tolerance = 1e-9
  )
  # The lookup path agrees with the string-parsing path for isotopologues.
  expect_equal(
    out,
    unname(vapply(
      uni$adduct,
      calculate_mz_from_mass,
      numeric(1L),
      neutral_mass = m
    )),
    tolerance = 1e-9
  )
  # And the neutral-mass lookup inverts it.
  expect_equal(
    calculate_neutral_mass_from_lookup(
      mzs = out,
      adducts = uni$adduct,
      adduct_lookup = lookup,
      fallback = FALSE
    ),
    rep(m, 4L),
    tolerance = 1e-9
  )
})

test_that("calculate_mz_from_lookup uses parsing fallback only when asked", {
  lookup <- build_adduct_lookup(structured_universe(carriers = "H"))
  m <- 300.2
  adducts <- c("[M+H]+", "[M+K]+", NA_character_)

  with_fallback <- calculate_mz_from_lookup(
    neutral_mass = m,
    adducts = adducts,
    adduct_lookup = lookup,
    fallback = TRUE
  )
  expect_equal(with_fallback[[1L]], m + proton, tolerance = 1e-9)
  expect_equal(
    with_fallback[[2L]],
    m + mass_k - ELECTRON_MASS_DA,
    tolerance = 1e-9
  )
  expect_true(is.na(with_fallback[[3L]]))

  without_fallback <- calculate_mz_from_lookup(
    neutral_mass = m,
    adducts = adducts,
    adduct_lookup = lookup,
    fallback = FALSE
  )
  expect_equal(without_fallback[[1L]], m + proton, tolerance = 1e-9)
  expect_true(is.na(without_fallback[[2L]]))
  expect_true(is.na(without_fallback[[3L]]))
})

test_that("lookup m/z and neutral-mass conversions are mutual inverses", {
  uni <- structured_universe(
    n_mer = c(1L, 2L),
    charges = c(1L, 2L),
    clusters = c("H2O"),
    losses = c("H2O")
  )
  lookup <- build_adduct_lookup(uni)
  m <- 212.0874

  mz <- calculate_mz_from_lookup(
    neutral_mass = m,
    adducts = lookup$adduct,
    adduct_lookup = lookup,
    fallback = FALSE
  )
  expect_false(anyNA(mz))
  back <- calculate_neutral_mass_from_lookup(
    mzs = mz,
    adducts = lookup$adduct,
    adduct_lookup = lookup,
    fallback = FALSE
  )
  expect_equal(back, rep(m, nrow(lookup)), tolerance = 1e-9)
})

## build_universe_transition_tables ----

test_that("transition tables are empty for an empty universe", {
  tr <- build_universe_transition_tables(empty_adduct_universe())
  expect_identical(nrow(tr$adduct_diffs), 0L)
  expect_identical(nrow(tr$cluster_transitions), 0L)
  expect_identical(nrow(tr$loss_transitions), 0L)
})

test_that("transition tables are empty for a single-adduct universe", {
  tr <- build_universe_transition_tables(
    structured_universe(carriers = "H")
  )
  expect_identical(nrow(tr$adduct_diffs), 0L)
  expect_identical(nrow(tr$cluster_transitions), 0L)
  expect_identical(nrow(tr$loss_transitions), 0L)
})

test_that("adduct diffs equal the carrier mass difference at any M", {
  uni <- structured_universe(clusters = c("H2O"), losses = c("CO2"))
  lookup <- build_adduct_lookup(uni)
  tr <- build_universe_transition_tables(uni)
  diffs <- tr$adduct_diffs

  # [M+H]+ -> [M+Na]+ is present with Distance = Na - H.
  hit <- diffs[diffs$Group1 == "[M+H]+" & diffs$Group2 == "[M+Na]+", ]
  expect_identical(nrow(hit), 1L)
  expect_equal(hit$Distance, mass_na - mass_h, tolerance = 1e-9)

  # One H -> Na edge per decoration (none, +H2O, -CO2, -CO2+H2O), and every
  # edge only swaps the carrier while keeping the cluster/loss decoration.
  expect_identical(nrow(diffs), 4L)
  key <- stats::setNames(
    paste(lookup$cluster_key, lookup$loss_key),
    lookup$adduct
  )
  expect_identical(
    unname(key[diffs$Group1]),
    unname(key[diffs$Group2])
  )

  # Distance is the m/z difference for any neutral mass (slope 1).
  for (m in c(150, 600)) {
    mz1 <- calculate_mz_from_lookup(m, diffs$Group1, lookup, fallback = FALSE)
    mz2 <- calculate_mz_from_lookup(m, diffs$Group2, lookup, fallback = FALSE)
    expect_equal(mz2 - mz1, diffs$Distance, tolerance = 1e-9)
  }
})

test_that("adduct diffs do not depend on alphabetical order of adducts", {
  uni <- structured_universe(carriers = c("Na", "K"))
  tr <- build_universe_transition_tables(uni)
  hit <- tr$adduct_diffs[
    tr$adduct_diffs$Group1 == "[M+Na]+" &
      tr$adduct_diffs$Group2 == "[M+K]+",
  ]
  expect_identical(nrow(hit), 1L)
  expect_equal(hit$Distance, mass_k - mass_na, tolerance = 1e-9)
})

test_that("cluster transitions keep only single-term cluster increments", {
  # z = 2 keeps the bare adduct sorting before the clustered one.
  uni <- structured_universe(
    carriers = "H",
    charges = 2L,
    clusters = c("H2O", "H4N")
  )
  tr <- build_universe_transition_tables(uni)
  ct <- tr$cluster_transitions

  # The two clustered variants differ by a swap (-H2O +H4N), which is not a
  # single +1 increment, so every edge starts from the bare ion.
  expect_true(all(ct$adduct == "[M+2H]2+"))
  expect_setequal(ct$cluster, c("H2O", "H4N"))
  h4n <- 4 * mass_h + ATOMIC_MONOISOTOPIC_MASS[["N"]]
  expect_equal(
    ct$mass[match(c("H2O", "H4N"), ct$cluster)],
    c(mass_h2o, h4n) / 2,
    tolerance = 1e-9
  )
})

test_that("singly charged cluster transitions are found regardless of order", {
  uni <- structured_universe(carriers = "H", clusters = c("H2O"))
  ct <- build_universe_transition_tables(uni)$cluster_transitions
  hit <- ct[ct$adduct == "[M+H]+" & ct$adduct_dest == "[M+H2O+H]+", ]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$cluster, "H2O")
  expect_equal(hit$mass, mass_h2o, tolerance = 1e-9)
})

test_that("loss transitions link the loss ion to its intact parent", {
  uni <- structured_universe(carriers = "H", losses = c("H2O"))
  lt <- build_universe_transition_tables(uni)$loss_transitions
  hit <- lt[lt$adduct == "[M-H2O+H]+" & lt$adduct_dest == "[M+H]+", ]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$loss, "H2O")
  expect_equal(hit$mass, mass_h2o, tolerance = 1e-9)
})
