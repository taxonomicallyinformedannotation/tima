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
    adduct_to_string(1, c(NH4 = 1), integer(), integer(), 1),
    "[M+NH4]+"
  )
  expect_equal(
    adduct_to_string(1, c(H = 1), c(H2O = 1), integer(), 1),
    "[M+H+H2O]+"
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

  pc <- parse_carrier_token("NH4")
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
  expect_true("[M+H+H2O]+" %in% u$adduct)
  expect_true("[M+H-H2O]+" %in% u$adduct)

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
    "[M+NH4]+"
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
