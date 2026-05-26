# Tests for strengthened multimer/multicharge annotation.
#
# The typed adduct universe must emit BOTH interpretations whenever a
# multimer combines with a neutral cluster or neutral loss:
#
#   * Outside-multimer (legacy): the cluster/loss is applied to the
#     assembled multimer, e.g. [2M-H2O+H]+ or [2M+NaCl+H]+.
#   * Inside-multimer (new):      the cluster/loss decorates each monomer
#     BEFORE multimerization, e.g. [2(M-H2O)+H]+ or [2(M+NaCl)+H]+.
#
# These have *different* implied neutral masses, so the engine must be
# able to test both for the same observed m/z.

# ---------------------------------------------------------------------------
# Canonical string rendering
# ---------------------------------------------------------------------------

test_that("adduct_to_string renders inside-multimer losses with parentheses", {
  s_outside <- adduct_to_string(
    n_mer = 2L,
    carriers = c(H = 1L),
    clusters = integer(),
    losses = c(H2O = 1L),
    z = 1L
  )
  s_inside <- adduct_to_string(
    n_mer = 2L,
    carriers = c(H = 1L),
    clusters = integer(),
    losses = c(H2O = 1L),
    z = 1L,
    loss_inside_multimer = TRUE
  )
  # Canonical ordering inside the brackets: M, then carriers, then clusters,
  # then losses. Inside-multimer wraps M+inside-bits in parentheses.
  expect_equal(s_outside, "[2M+H-H2O]+")
  expect_equal(s_inside, "[2(M-H2O)+H]+")
})

test_that("adduct_to_string renders inside-multimer clusters with parentheses", {
  s_outside <- adduct_to_string(
    n_mer = 2L,
    carriers = c(H = 1L),
    clusters = c(NaCl = 1L),
    losses = integer(),
    z = 1L
  )
  s_inside <- adduct_to_string(
    n_mer = 2L,
    carriers = c(H = 1L),
    clusters = c(NaCl = 1L),
    losses = integer(),
    z = 1L,
    cluster_inside_multimer = TRUE
  )
  expect_equal(s_outside, "[2M+H+NaCl]+")
  expect_equal(s_inside, "[2(M+NaCl)+H]+")
})

test_that("adduct_to_string ignores the inside flag for monomers (no-op)", {
  s <- adduct_to_string(
    n_mer = 1L,
    carriers = c(H = 1L),
    clusters = integer(),
    losses = c(H2O = 1L),
    z = 1L,
    loss_inside_multimer = TRUE
  )
  # For a monomer, "inside" and "outside" are mathematically identical,
  # so we render the plain canonical form.
  expect_equal(s, "[M+H-H2O]+")
})

# ---------------------------------------------------------------------------
# Mass arithmetic — the two variants give DIFFERENT implied neutral masses
# ---------------------------------------------------------------------------

test_that("inside-multimer loss yields a different implied mass than outside", {
  # Using a hypothetical neutral mass M = 200 Da and water (H2O ~ 18.0106).
  proton <- ATOMIC_MONOISOTOPIC_MASS[["H"]] - ELECTRON_MASS_DA
  h2o <- 2 * ATOMIC_MONOISOTOPIC_MASS[["H"]] + ATOMIC_MONOISOTOPIC_MASS[["O"]]
  M <- 200

  # Outside: [2M-H2O+H]+ => m/z = (2M - H2O + proton) / 1
  mz_outside <- 2 * M - h2o + proton
  # Inside:  [2(M-H2O)+H]+ => m/z = (2*(M - H2O) + proton) / 1
  mz_inside <- 2 * (M - h2o) + proton

  # The two interpretations differ by exactly one H2O.
  expect_equal(mz_outside - mz_inside, h2o, tolerance = 1e-9)

  # Reverse: given the m/z, recover M with the right per-monomer term.
  recovered_outside <- calculate_neutral_mass(
    mz = mz_outside,
    n_mer = 2L,
    z = 1L,
    adduct_mass = proton - h2o, # cluster/loss OUTSIDE the multimer
    adduct_mass_per_monomer = 0
  )
  recovered_inside <- calculate_neutral_mass(
    mz = mz_inside,
    n_mer = 2L,
    z = 1L,
    adduct_mass = proton, # only the carrier outside
    adduct_mass_per_monomer = -h2o # H2O lost per monomer
  )
  expect_equal(recovered_outside, M, tolerance = 1e-9)
  expect_equal(recovered_inside, M, tolerance = 1e-9)
})

test_that("inside-multimer cluster yields a different implied mass than outside", {
  proton <- ATOMIC_MONOISOTOPIC_MASS[["H"]] - ELECTRON_MASS_DA
  nacl <- ATOMIC_MONOISOTOPIC_MASS[["Na"]] + ATOMIC_MONOISOTOPIC_MASS[["Cl"]]
  M <- 300

  mz_outside <- 2 * M + nacl + proton # [2M+NaCl+H]+
  mz_inside <- 2 * (M + nacl) + proton # [2(M+NaCl)+H]+
  expect_equal(mz_inside - mz_outside, nacl, tolerance = 1e-9)

  rec_outside <- calculate_neutral_mass(
    mz = mz_outside,
    n_mer = 2L,
    z = 1L,
    adduct_mass = proton + nacl,
    adduct_mass_per_monomer = 0
  )
  rec_inside <- calculate_neutral_mass(
    mz = mz_inside,
    n_mer = 2L,
    z = 1L,
    adduct_mass = proton,
    adduct_mass_per_monomer = nacl
  )
  expect_equal(rec_outside, M, tolerance = 1e-9)
  expect_equal(rec_inside, M, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Universe enumeration — both variants are emitted
# ---------------------------------------------------------------------------

test_that("typed universe emits both inside- and outside-multimer loss rows", {
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
  expect_true("[2M+H-H2O]+" %in% uni$adduct)
  expect_true("[2(M-H2O)+H]+" %in% uni$adduct)

  # And the per-monomer term is correctly populated for the inside variant.
  outside <- uni[uni$adduct == "[2M+H-H2O]+", ]
  inside <- uni[uni$adduct == "[2(M-H2O)+H]+", ]
  expect_equal(outside$adduct_mass_per_monomer, 0)
  h2o <- 2 * ATOMIC_MONOISOTOPIC_MASS[["H"]] + ATOMIC_MONOISOTOPIC_MASS[["O"]]
  expect_equal(inside$adduct_mass_per_monomer, -h2o, tolerance = 1e-9)
})

test_that("typed universe emits both inside- and outside-multimer cluster rows", {
  uni <- build_adduct_universe(
    adducts_list = list(
      M = c(1L, 2L),
      charge_carriers = list(pos = c("H")),
      charges = list(pos = c(1L))
    ),
    clusters_list = list(pos = c("NaCl")),
    neutral_losses_list = character(),
    polarity = "pos"
  )
  expect_true("[2M+H+NaCl]+" %in% uni$adduct)
  expect_true("[2(M+NaCl)+H]+" %in% uni$adduct)

  outside <- uni[uni$adduct == "[2M+H+NaCl]+", ]
  inside <- uni[uni$adduct == "[2(M+NaCl)+H]+", ]
  expect_equal(outside$adduct_mass_per_monomer, 0)
  nacl <- ATOMIC_MONOISOTOPIC_MASS[["Na"]] + ATOMIC_MONOISOTOPIC_MASS[["Cl"]]
  expect_equal(inside$adduct_mass_per_monomer, nacl, tolerance = 1e-9)
})

test_that("monomers never get an inside-multimer duplicate", {
  uni <- build_adduct_universe(
    adducts_list = list(
      M = c(1L),
      charge_carriers = list(pos = c("H")),
      charges = list(pos = c(1L))
    ),
    clusters_list = list(pos = c("NaCl")),
    neutral_losses_list = c("H2O"),
    polarity = "pos"
  )
  # No parenthesised string for n_mer == 1.
  expect_false(any(grepl("\\(", uni$adduct, perl = TRUE)))
  # All rows have per-monomer mass == 0 since the inside variant is a
  # mathematical no-op for monomers.
  expect_true(all(uni$adduct_mass_per_monomer == 0))
})

# ---------------------------------------------------------------------------
# End-to-end: a dimer of (M - H2O) and a NaCl-per-monomer dimer both annotate
# back to the same neutral M via library matching.
# ---------------------------------------------------------------------------

test_that("evidence engine annotates [2(M-H2O)+H]+ back to the same neutral M", {
  proton <- ATOMIC_MONOISOTOPIC_MASS[["H"]] - ELECTRON_MASS_DA
  h2o <- 2 * ATOMIC_MONOISOTOPIC_MASS[["H"]] + ATOMIC_MONOISOTOPIC_MASS[["O"]]
  M <- 199.0925
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

  row <- uni[uni$adduct == "[2(M-H2O)+H]+", ]
  expect_equal(nrow(row), 1L)

  recovered <- implied_neutral_mass(
    mz = mz_inside,
    n_mer = row$n_mer,
    z = row$z,
    adduct_mass = row$adduct_mass,
    n_iso = 0L,
    adduct_mass_per_monomer = row$adduct_mass_per_monomer
  )
  expect_equal(recovered, M, tolerance = 1e-7)
})

test_that("evidence engine annotates [2(M+NaCl)+H]+ back to the same neutral M", {
  proton <- ATOMIC_MONOISOTOPIC_MASS[["H"]] - ELECTRON_MASS_DA
  nacl <- ATOMIC_MONOISOTOPIC_MASS[["Na"]] + ATOMIC_MONOISOTOPIC_MASS[["Cl"]]
  M <- 300.123
  mz_inside <- 2 * (M + nacl) + proton

  uni <- build_adduct_universe(
    adducts_list = list(
      M = c(1L, 2L),
      charge_carriers = list(pos = c("H")),
      charges = list(pos = c(1L))
    ),
    clusters_list = list(pos = c("NaCl")),
    neutral_losses_list = character(),
    polarity = "pos"
  )

  row <- uni[uni$adduct == "[2(M+NaCl)+H]+", ]
  expect_equal(nrow(row), 1L)

  recovered <- implied_neutral_mass(
    mz = mz_inside,
    n_mer = row$n_mer,
    z = row$z,
    adduct_mass = row$adduct_mass,
    n_iso = 0L,
    adduct_mass_per_monomer = row$adduct_mass_per_monomer
  )
  expect_equal(recovered, M, tolerance = 1e-7)
})

test_that("outside- and inside-variants both round-trip distinct m/z values", {
  proton <- ATOMIC_MONOISOTOPIC_MASS[["H"]] - ELECTRON_MASS_DA
  h2o <- 2 * ATOMIC_MONOISOTOPIC_MASS[["H"]] + ATOMIC_MONOISOTOPIC_MASS[["O"]]
  M <- 250

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

  out_row <- uni[uni$adduct == "[2M+H-H2O]+", ]
  in_row <- uni[uni$adduct == "[2(M-H2O)+H]+", ]

  mz_out <- calculate_mz_from_neutral_mass(
    neutral_mass = M,
    n_mer = out_row$n_mer,
    z = out_row$z,
    adduct_mass = out_row$adduct_mass,
    adduct_mass_per_monomer = out_row$adduct_mass_per_monomer
  )
  mz_in <- calculate_mz_from_neutral_mass(
    neutral_mass = M,
    n_mer = in_row$n_mer,
    z = in_row$z,
    adduct_mass = in_row$adduct_mass,
    adduct_mass_per_monomer = in_row$adduct_mass_per_monomer
  )
  # Distinct interpretations => distinct m/z, differing by exactly one H2O.
  expect_equal(mz_out - mz_in, h2o, tolerance = 1e-9)
})
