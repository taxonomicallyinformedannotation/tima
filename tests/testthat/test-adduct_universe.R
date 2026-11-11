test_that("adduct_to_string formats canonical strings correctly", {
  expect_equal(
    adduct_to_string(1, c(H = 1), integer(), integer(), 1),
    "[M+H]+"
  )
  expect_equal(
    adduct_to_string(1, c(H = -1), integer(), integer(), -1),
    "[M-H]-"
  )
  expect_equal(
    adduct_to_string(1, c(H = 2), integer(), integer(), 2),
    "[M+2H]2+"
  )
  expect_equal(
    adduct_to_string(1, c(H = 1, Na = 1), integer(), integer(), 2),
    "[M+H+Na]2+"
  )
  expect_equal(
    adduct_to_string(1, c(H = 3), integer(), integer(), 3),
    "[M+3H]3+"
  )
  expect_equal(
    adduct_to_string(2, c(Na = 1), integer(), integer(), 1),
    "[2M+Na]+"
  )
  # [M+Na-2H]- : Na contributes +1, -2H contributes -2 -> z=-1
  expect_equal(
    adduct_to_string(1, c(Na = 1, H = -2), integer(), integer(), -1),
    "[M-2H+Na]-"
  )
  # [M+Fe-2H]+ : Fe contributes +3, -2H contributes -2 -> z=+1
  expect_equal(
    adduct_to_string(1, c(Fe = 1, H = -2), integer(), integer(), 1),
    "[M-2H+Fe]+"
  )
  expect_equal(
    adduct_to_string(1, c(H4N = 1), integer(), integer(), 1),
    "[M+H4N]+"
  )
  expect_equal(
    adduct_to_string(1, c(H = 1), c(H2O = 1), integer(), 1),
    "[M+H2O+H]+"
  )
})

test_that("parse_carrier_token decomposes compound carriers", {
  pc <- parse_carrier_token("H")
  expect_equal(unname(pc$symbols), 1L)
  expect_equal(pc$z_contribution, 1L)

  pc <- parse_carrier_token("Na-2H")
  expect_equal(as.list(pc$symbols), list(Na = 1L, H = -2L))
  expect_equal(pc$z_contribution, -1L)

  pc <- parse_carrier_token("Fe-2H")
  expect_equal(as.list(pc$symbols), list(Fe = 1L, H = -2L))
  expect_equal(pc$z_contribution, 1L)

  pc <- parse_carrier_token("H4N")
  expect_equal(pc$z_contribution, 1L)
})

test_that("generate_adduct_hypotheses returns expected rows for a small spec", {
  spec <- list(
    M = c(1L, 2L),
    charge_carriers = list(pos = c("H", "Na"), neg = c("H", "Cl", "Na")),
    charges = list(pos = c(1L, 2L), neg = c(-1L)),
    clusters = list(pos = c("H2O"), neg = character()),
    neutral_losses = c("H2O")
  )
  u <- generate_adduct_hypotheses(spec, polarity = "pos")
  expect_true("[M+H]+" %in% u$adduct)
  expect_true("[M+Na]+" %in% u$adduct)
  expect_true("[2M+H]+" %in% u$adduct)
  expect_true("[M+2H]2+" %in% u$adduct)
  expect_true("[M+H+Na]2+" %in% u$adduct)
  expect_true("[M+H2O+H]+" %in% u$adduct)
  expect_true("[M-H2O+H]+" %in% u$adduct)

  u_neg <- generate_adduct_hypotheses(spec, polarity = "neg")
  expect_true("[M-H]-" %in% u_neg$adduct)
  expect_true("[M-2H+Na]-" %in% u_neg$adduct)

  # adduct_mass independent of m/z
  expect_true(all(is.finite(u$adduct_mass)))
})

test_that("adduct_mass applies electron-mass correction with signed charge", {
  spec <- list(
    M = 1L,
    charge_carriers = list(pos = c("H"), neg = c("H")),
    charges = list(pos = 1L, neg = -1L)
  )

  u_pos <- generate_adduct_hypotheses(spec, polarity = "pos")
  u_neg <- generate_adduct_hypotheses(spec, polarity = "neg")

  mass_h <- ATOMIC_MONOISOTOPIC_MASS[["H"]]
  expect_equal(
    u_pos$adduct_mass[u_pos$adduct == "[M+H]+"],
    mass_h - ELECTRON_MASS_DA,
    tolerance = 1e-12
  )
  expect_equal(
    u_neg$adduct_mass[u_neg$adduct == "[M-H]-"],
    -mass_h + ELECTRON_MASS_DA,
    tolerance = 1e-12
  )
})

test_that("calculate_neutral_mass round-trips canonical adducts", {
  spec <- list(
    M = c(1L, 2L),
    charge_carriers = list(
      pos = c("H", "Na", "K", "NH4", "Fe"),
      neg = c("H", "F", "Cl", "Br")
    ),
    charges = list(pos = c(1L, 2L, 3L), neg = c(-1L, -2L, -3L))
  )
  u_pos <- generate_adduct_hypotheses(spec, polarity = "pos")
  u_neg <- generate_adduct_hypotheses(spec, polarity = "neg")
  u <- tidytable::bind_rows(u_pos, u_neg)

  M <- 122.45
  test_set <- c(
    "[M+H]+",
    "[M-H]-",
    "[M+2H]2+",
    "[M+H+Na]2+",
    "[M+3H]3+",
    "[2M+Na]+",
    "[M+H4N]+"
  )
  for (a in test_set) {
    expect_true(
      a %in% u$adduct,
      info = paste("missing canonical adduct in universe:", a)
    )
    row <- u[u$adduct == a, ]
    mz <- calculate_mz_from_neutral_mass(
      M,
      row$n_mer,
      row$z,
      row$adduct_mass
    )
    M_back <- calculate_neutral_mass(
      mz,
      row$n_mer,
      row$z,
      row$adduct_mass
    )
    expect_equal(M_back, M, tolerance = 1e-6, info = a)
  }
})

test_that("generate_adduct_hypotheses agrees with parse_adduct on neutral mass", {
  spec <- list(
    M = c(1L, 2L),
    charge_carriers = list(
      pos = c("H", "Na", "K", "NH4"),
      neg = c("H", "Cl")
    ),
    charges = list(pos = c(1L, 2L), neg = c(-1L))
  )
  u <- generate_adduct_hypotheses(spec, polarity = "pos")
  M <- 200.0
  for (i in seq_len(nrow(u))) {
    a <- u$adduct[[i]]
    mz <- calculate_mz_from_neutral_mass(
      M,
      u$n_mer[[i]],
      u$z[[i]],
      u$adduct_mass[[i]]
    )
    M_legacy <- calculate_mass_of_m(mz, a)
    # parse_adduct uses neutral-formula arithmetic; allow electron-mass
    # rounding tolerance.
    expect_equal(M_legacy, M, tolerance = 1e-3, info = a)
  }
})

test_that("build_adduct_universe handles legacy flat list", {
  u <- build_adduct_universe(
    adducts_list = list(pos = c("[M+H]+", "[M+Na]+", "[M+2H]2+")),
    clusters_list = list(pos = c("H2O")),
    neutral_losses_list = c("H2O"),
    polarity = "pos"
  )
  expect_true("[M+H]+" %in% u$adduct)
  expect_true("[M+Na]+" %in% u$adduct)
  expect_true("[M+2H]2+" %in% u$adduct)
  # Cluster expansion variant present
  expect_true(any(grepl("H2O", u$adduct, fixed = TRUE)))
})

test_that("build_adduct_universe_from_legacy preserves modifier expansions", {
  u <- build_adduct_universe_from_legacy(
    adducts = "[M+H]+",
    clusters = "H2O",
    neutral_losses = "CO2",
    polarity = "positive"
  )

  expect_true("[M+H]+" %in% u$adduct)
  expect_true(any(grepl("H2O", u$adduct, fixed = TRUE)))
  expect_true(any(grepl("CO", u$adduct, fixed = TRUE)))
  expect_equal(nrow(u), 4L)
})

test_that("build_adduct_universe handles structured form", {
  u <- build_adduct_universe(
    adducts_list = list(
      M = c(1L, 2L),
      charge_carriers = list(pos = c("H", "Na"), neg = c("H")),
      charges = list(pos = c(1L), neg = c(-1L))
    ),
    clusters_list = list(pos = c("H2O"), neg = character()),
    neutral_losses_list = character(),
    polarity = "pos"
  )
  expect_true("[M+H]+" %in% u$adduct)
  expect_true("[M+Na]+" %in% u$adduct)
  expect_true("[2M+H]+" %in% u$adduct)
})

test_that("calculate_neutral_mass supports universe lookup, per-monomer mass, and rejects missing params", {
  u <- tidytable::tidytable(
    adduct = c("[M+H]+", "[2M+Na]+"),
    n_mer = c(1L, 2L),
    z = c(1L, 1L),
    adduct_mass = c(1.007276, 22.989218),
    adduct_mass_per_monomer = c(0.0, 18.010565)
  )

  M <- 150.0
  mz1 <- calculate_mz_from_neutral_mass(M, 1L, 1L, 1.007276, 0.0)
  mz2 <- calculate_mz_from_neutral_mass(M, 2L, 1L, 22.989218, 18.010565)

  # Lookup via (universe, adduct) with adduct_mass_per_monomer column present
  m_lookup <- calculate_neutral_mass(
    mz = c(mz1, mz2),
    universe = u,
    adduct = c("[M+H]+", "[2M+Na]+")
  )
  expect_equal(m_lookup, c(M, M), tolerance = 1e-10)

  # Lookup via (universe, adduct) when adduct_mass_per_monomer column is absent
  u_no_pm <- u[, c("adduct", "n_mer", "z", "adduct_mass")]
  mz2_no_pm <- calculate_mz_from_neutral_mass(M, 2L, 1L, 22.989218, 0.0)
  m_no_pm <- calculate_neutral_mass(
    mz = c(mz1, mz2_no_pm),
    universe = u_no_pm,
    adduct = c("[M+H]+", "[2M+Na]+")
  )
  expect_equal(m_no_pm, c(M, M), tolerance = 1e-10)

  # Missing n_mer / z / adduct_mass individually aborts with tima_validation_error
  expect_error(
    calculate_neutral_mass(mz = 100.0, n_mer = NULL, z = 1L, adduct_mass = 1.0),
    regexp = "supply \\(n_mer, z, adduct_mass\\) or \\(universe, adduct\\)",
    class = "tima_validation_error"
  )
  expect_error(
    calculate_neutral_mass(mz = 100.0, n_mer = 1L, z = NULL, adduct_mass = 1.0),
    regexp = "supply \\(n_mer, z, adduct_mass\\) or \\(universe, adduct\\)",
    class = "tima_validation_error"
  )
  expect_error(
    calculate_neutral_mass(mz = 100.0, n_mer = 1L, z = 1L, adduct_mass = NULL),
    regexp = "supply \\(n_mer, z, adduct_mass\\) or \\(universe, adduct\\)",
    class = "tima_validation_error"
  )
})

test_that(".normalize_negative_mode_carrier normalizes +1/+2/+3 cations and preserves signed/anion tokens", {
  expect_identical(
    .normalize_negative_mode_carrier(NA_character_),
    NA_character_
  )
  expect_identical(.normalize_negative_mode_carrier(""), "")
  expect_identical(.normalize_negative_mode_carrier("-H"), "-H")
  expect_identical(.normalize_negative_mode_carrier("Na-2H"), "Na-2H")
  expect_identical(.normalize_negative_mode_carrier("Cl"), "Cl")
  expect_identical(
    .normalize_negative_mode_carrier("UnknownToken"),
    "UnknownToken"
  )
  expect_identical(.normalize_negative_mode_carrier("H"), "-H")
  expect_identical(.normalize_negative_mode_carrier("Na"), "Na-2H")
  expect_identical(.normalize_negative_mode_carrier("Ca"), "Ca-3H")
  expect_identical(.normalize_negative_mode_carrier("Fe"), "Fe-4H")
})

test_that("formula token validation and legacy universe edge cases return expected structures", {
  expect_identical(.validate_formula_tokens(character(0)), character(0))
  expect_identical(.validate_formula_tokens(c("  ", "")), character(0))
  expect_identical(
    .validate_formula_tokens(c("H2O (water)", "C2H3N", "???invalid???")),
    c("H2O", "C2H3N")
  )

  expect_null(.parse_single_legacy_adduct_row("not_an_adduct"))
  expect_null(.parse_single_legacy_adduct_row("[0M+H]+"))

  empty_u <- empty_adduct_universe()
  expect_identical(.assemble_legacy_universe(list()), empty_u)
  expect_identical(
    build_adduct_universe_from_legacy(
      character(0),
      character(0),
      character(0),
      "pos"
    ),
    empty_u
  )
  expect_identical(
    build_adduct_universe_from_legacy(
      c("invalid1", "[0M+H]+"),
      "H2O",
      "CO2",
      "pos"
    ),
    empty_u
  )

  # Flat clusters_list (not nested by polarity) in build_adduct_universe
  u_flat <- build_adduct_universe(
    adducts_list = list(pos = c("[M+H]+")),
    clusters_list = c("H2O"),
    neutral_losses_list = character(0),
    polarity = "pos"
  )
  expect_true("[M+H]+" %in% u_flat$adduct)
  expect_true(any(grepl("H2O", u_flat$adduct, fixed = TRUE)))

  # %||% operator fallback
  expect_identical(NULL %||% "fallback", "fallback")
  expect_identical(character(0) %||% "fallback", "fallback")
  expect_identical("present" %||% "fallback", "present")

  # Formula and carrier helper edge cases
  expect_identical(parse_atomic_formula(""), integer())
  expect_identical(formula_mass(integer()), 0)
  expect_null(parse_carrier_token(NULL))
  expect_null(parse_carrier_token(""))
  expect_null(parse_carrier_token("UnknownCarrier"))
  expect_true(is.na(carrier_token_mass(NULL)))
  expect_true(is.na(carrier_token_mass(list(symbols = c(UnknownEl = 1L)))))
  expect_identical(.format_neutrals(c(H2O = 0L), "+"), "")
})
