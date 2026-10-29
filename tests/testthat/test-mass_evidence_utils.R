test_that(".has_library_match_within_tolerance uses combined dalton+ppm tolerance", {
  # Library exact masses - need to be close enough to query masses
  lib <- c(100.0, 200.0, 500.0, 1000.0)

  # Query masses very close to library masses (within 0.01 Da)
  masses <- c(100.005, 200.005, 500.005, 1000.005)

  # With dalton=0.01: all should match (0.005 < 0.01)
  hit <- .has_library_match_within_tolerance(
    masses,
    lib,
    tolerance_ppm = 10,
    tolerance_dalton = 0.01
  )
  expect_true(all(hit))

  # With dalton=NULL (ppm only, 10 ppm):
  # At 100 Da: 10 ppm = 0.001 Da. 100.005 is 0.005 away = 50 ppm > 10 ppm -> FALSE
  # At 500 Da: 10 ppm = 0.005 Da. 500.005 is 0.005 away = exactly 10 ppm -> TRUE (interval is (lo, hi])
  # At 1000 Da: 10 ppm = 0.01 Da. 1000.005 is 0.005 away = 5 ppm < 10 ppm -> TRUE
  hit <- .has_library_match_within_tolerance(
    masses,
    lib,
    tolerance_ppm = 10,
    tolerance_dalton = NULL
  )
  expect_equal(hit, c(FALSE, FALSE, TRUE, TRUE))

  # With dalton=0.001 (smaller than ppm at high mass):
  # At 1000 Da: 10 ppm = 0.01 Da. max(0.001, 0.01) = 0.01. 1000.005 is within 0.01 -> TRUE
  # At 100 Da: 10 ppm = 0.001 Da. max(0.001, 0.001) = 0.001. 100.005 is 0.005 away -> FALSE
  hit <- .has_library_match_within_tolerance(
    c(1000.005),
    lib,
    tolerance_ppm = 10,
    tolerance_dalton = 0.001
  )
  expect_true(hit)

  hit <- .has_library_match_within_tolerance(
    c(100.005),
    lib,
    tolerance_ppm = 10,
    tolerance_dalton = 0.001
  )
  expect_false(hit)

  # Edge cases
  expect_equal(
    .has_library_match_within_tolerance(numeric(0), lib, 10, 0.01),
    logical(0)
  )
  expect_equal(
    .has_library_match_within_tolerance(c(100.0), numeric(0), 10, 0.01),
    c(FALSE)
  )
})

test_that("backward compatible .has_library_match_within_ppm works", {
  lib <- c(100.0, 200.0)
  hit <- .has_library_match_within_ppm(c(100.0005, 100.02), lib, 10)
  expect_equal(hit, c(TRUE, FALSE))
})

# --- implied_neutral_mass -------------------------------------------------

test_that("implied_neutral_mass inverts the neutral-mass formula exactly", {
  # [M+H]+ of glucose: M = mz - m(H+)
  expect_equal(
    implied_neutral_mass(181.0707, 1L, 1L, 1.00728),
    180.06342
  )
  # [M+Na]+ of the same M
  expect_equal(
    implied_neutral_mass(203.0526, 1L, 1L, 22.98922),
    180.06338
  )
  # Multimer / charge scaling: (|z| * mz - adduct_mass) / n_mer
  # (2 * 190.0 - 2.01456) / 2 = 188.99272
  expect_equal(
    implied_neutral_mass(190.0, 2L, 2L, 2.01456),
    188.99272
  )
  # Sign of the charge is irrelevant (abs(z)), so z = -2 gives the same answer
  expect_equal(
    implied_neutral_mass(190.0, 2L, -2L, 2.01456),
    188.99272
  )
})

test_that("implied_neutral_mass subtracts the 13C isotope shift per n_iso", {
  # The M1 isotopologue of 181.0707 is 1.003355 Da heavier; shifting it back
  # must recover exactly the same neutral mass as the monoisotopic peak.
  mono <- implied_neutral_mass(181.0707, 1L, 1L, 1.00728, n_iso = 0L)
  m1 <- implied_neutral_mass(
    181.0707 + EVIDENCE_ISOTOPE_SHIFT_DA,
    1L,
    1L,
    1.00728,
    n_iso = 1L
  )
  expect_equal(m1, mono, tolerance = 1e-9)

  # n_iso = 1 on the *monoisotopic* m/z must shift the recovered mass down by
  # exactly one isotope unit (kills any wrong arithmetic on the shift).
  expect_equal(
    implied_neutral_mass(181.0707, 1L, 1L, 1.00728, n_iso = 1L),
    mono - EVIDENCE_ISOTOPE_SHIFT_DA,
    tolerance = 1e-9
  )
  # The shift is multiplicative in n_iso, not additive.
  expect_equal(
    implied_neutral_mass(181.0707, 1L, 1L, 1.00728, n_iso = 2L),
    mono - 2 * EVIDENCE_ISOTOPE_SHIFT_DA,
    tolerance = 1e-9
  )
  # ... and it is removed *before* dividing by n_mer.
  expect_equal(
    implied_neutral_mass(
      190.0 + EVIDENCE_ISOTOPE_SHIFT_DA,
      2L,
      2L,
      2.01456,
      n_iso = 1L
    ),
    188.99272,
    tolerance = 1e-9
  )
})

test_that("implied_neutral_mass applies the per-monomer adduct loss", {
  # [2(M-H2O)+H]+: the water loss is inside the dimer, so it is subtracted
  # once per monomer after the division.
  expect_equal(
    implied_neutral_mass(200.0, 1L, 1L, 1.00728, 0L, 18.01056),
    180.98216
  )
  # The per-monomer term is subtracted *after* the division by n_mer, so it is
  # applied exactly once whatever the oligomer order.
  expect_equal(
    implied_neutral_mass(200.0, 2L, 1L, 1.00728, 0L, 18.01056),
    (200.0 - 1.00728) / 2 - 18.01056
  )
  # Default per-monomer loss of 0 keeps the outside-only legacy behaviour.
  expect_equal(
    implied_neutral_mass(200.0, 1L, 1L, 1.00728),
    200.0 - 1.00728
  )
})

test_that("EVIDENCE_ISOTOPE_SHIFT_DA is the 13C/12C difference", {
  expect_identical(EVIDENCE_ISOTOPE_SHIFT_DA, ISOTOPE_MASS_SHIFT_DALTONS)
})

# --- .nearest_exact_mass_ppm ---------------------------------------------

test_that(".nearest_exact_mass_ppm reports ppm error against the closer mass", {
  lib <- c(100.0, 200.0, 500.0)

  expect_equal(.nearest_exact_mass_ppm(100.0, lib), 0)
  # 0.0005 Da at 100 Da = 5 ppm
  expect_equal(.nearest_exact_mass_ppm(100.0005, lib), 5, tolerance = 1e-6)
  # 0.001 Da at 100 Da = 10 ppm
  expect_equal(.nearest_exact_mass_ppm(100.001, lib), 10, tolerance = 1e-6)
  # The denominator is the *library* mass of the closer match, not the query:
  # 150.0 is 50 Da from both 100 and 200; ties resolve to the lower one, so
  # 50 / 100 * 1e6.
  expect_equal(.nearest_exact_mass_ppm(150.0, lib), 5e5)
  # 600 is 100 Da from 500 and 400 from nothing -> 100 / 500 * 1e6
  expect_equal(.nearest_exact_mass_ppm(600.0, lib), 2e5)
  # Below the library range the first entry is used
  expect_equal(.nearest_exact_mass_ppm(50.0, lib), 5e5)
  # A query *between* two library masses: 400 is 200 Da from 200 but only
  # 100 Da from 500, so the closer one (500) is both the numerator's reference
  # and the distance measure. 100 / 500 * 1e6.
  expect_equal(.nearest_exact_mass_ppm(400.0, lib), 2e5)
  # Mirror case: 300 is 100 Da from 200 and 200 Da from 500 -> 100 / 200 * 1e6
  expect_equal(.nearest_exact_mass_ppm(300.0, lib), 5e5)
  # The upstream neighbour must not be measured with a positive offset: 190 is
  # 10 Da from 200, not 390 Da.
  expect_equal(.nearest_exact_mass_ppm(190.0, c(100.0, 200.0)), 5e4)
  expect_equal(.nearest_exact_mass_ppm(110.0, c(100.0, 200.0)), 1e5)
})

test_that(".nearest_exact_mass_ppm returns Inf when no library mass exists", {
  lib <- c(100.0, 200.0)
  expect_equal(.nearest_exact_mass_ppm(c(1, 2), numeric(0)), c(Inf, Inf))
  expect_identical(.nearest_exact_mass_ppm(numeric(0), lib), numeric(0))
  expect_identical(.nearest_exact_mass_ppm(numeric(0), numeric(0)), numeric(0))
})

# --- .nearest_exact_mass_index -------------------------------------------

test_that(".nearest_exact_mass_index indexes the closest library mass", {
  lib <- c(100.0, 200.0, 500.0)

  expect_equal(
    .nearest_exact_mass_index(c(100.0, 150.0, 400.0), lib),
    c(1L, 1L, 3L)
  )
  # Ties resolve to the lower index.
  expect_equal(.nearest_exact_mass_index(150.0, lib), 1L)
  # Out-of-range queries clamp to the first / last library entry.
  expect_equal(.nearest_exact_mass_index(c(50.0, 600.0), lib), c(1L, 3L))
  # A query just below an entry still maps to that entry.
  expect_equal(.nearest_exact_mass_index(99.999, lib), 1L)
  # Single-entry library: every query maps to it.
  expect_equal(.nearest_exact_mass_index(c(1, 99, 1e6), 42.0), c(1L, 1L, 1L))
})

test_that(".nearest_exact_mass_index returns 0 for an empty library", {
  lib <- c(100.0, 200.0)
  expect_identical(.nearest_exact_mass_index(numeric(0), lib), integer(0))
  expect_equal(.nearest_exact_mass_index(c(1, 2), numeric(0)), c(0L, 0L))
})

# --- .ppm_cluster_sorted --------------------------------------------------

test_that(".ppm_cluster_sorted splits on gaps beyond the ppm tolerance", {
  v <- c(100, 100.0005, 100.002)

  # 10 ppm at ~100 Da is 0.001 Da: the 0.0005 gap joins, the 0.0015 gap splits.
  expect_equal(.ppm_cluster_sorted(v, 10), c(1L, 1L, 2L))
  # 1 ppm is 0.0001 Da: every gap exceeds it.
  expect_equal(.ppm_cluster_sorted(v, 1), c(1L, 2L, 3L))
  # 2000 ppm swallows both gaps.
  expect_equal(.ppm_cluster_sorted(v, 2000), c(1L, 1L, 1L))
  # The comparison is strict: a gap exactly equal to the tolerance joins.
  expect_equal(.ppm_cluster_sorted(c(100, 100.001), 10), c(1L, 1L))
  expect_equal(.ppm_cluster_sorted(c(100, 100.0011), 10), c(1L, 2L))
})

test_that(".ppm_cluster_sorted gives every NA its own singleton cluster", {
  expect_equal(
    .ppm_cluster_sorted(c(100, 100.0001, NA_real_, NA_real_), 10),
    c(1L, 1L, 2L, 3L)
  )
  expect_equal(
    .ppm_cluster_sorted(c(NA_real_, NA_real_), 10),
    c(1L, 2L)
  )
  # A gap across an NA never merges the neighbouring real values.
  expect_equal(
    .ppm_cluster_sorted(c(100, 100.0001, NA_real_, 100.0002), 10),
    c(1L, 1L, 2L, 3L)
  )
})

test_that(".ppm_cluster_sorted handles degenerate inputs", {
  expect_identical(.ppm_cluster_sorted(numeric(0), 10), integer(0))
  expect_equal(.ppm_cluster_sorted(100, 10), 1L)
  expect_equal(.ppm_cluster_sorted(NA_real_, 10), 1L)
})

# --- annotate_adduct_universe_metadata -----------------------------------

.tier_universe <- function() {
  tidytable::tidytable(
    adduct = c(
      "[M+H]+",
      "[M+Na]+",
      "[2M+H]+",
      "[M+2H]2+",
      "[M-H2O+H]+",
      "[M+C3H6O+H]+",
      "[M+H+CH3CN]+",
      "[3M+Na]+",
      "[4M+H]+",
      "[M+Fe]+",
      "[M+3H]3+",
      "[M+CH3CN+CH3CN]+",
      "[M+Cl]-",
      "[M-H]-",
      "[M-H-H2O]-",
      "[M+H+NaCl]+"
    ),
    z = c(1L, 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 3L, 1L, -1L, -1L, -1L, 1L),
    n_mer = c(
      1L,
      1L,
      2L,
      1L,
      1L,
      1L,
      1L,
      3L,
      4L,
      1L,
      1L,
      1L,
      1L,
      1L,
      1L,
      1L
    ),
    carriers = I(list(
      c(H = "1"),
      c(Na = "1"),
      c(H = "1"),
      c(H = "2"),
      c(H = "1"),
      c(H = "1"),
      c(H = "1"),
      c(Na = "1"),
      c(H = "1"),
      c(Fe = "1"),
      c(H = "3"),
      c(H = "1"),
      c(Cl = "-1"),
      c(H = "-1"),
      c(H = "-1"),
      c(H = "1", Na = "1")
    )),
    losses = I(list(
      numeric(0),
      numeric(0),
      numeric(0),
      numeric(0),
      c(H2O = "-1"),
      c(C3H6O = "-1"),
      numeric(0),
      numeric(0),
      numeric(0),
      numeric(0),
      numeric(0),
      numeric(0),
      numeric(0),
      numeric(0),
      c(H2O = "-1"),
      numeric(0)
    )),
    clusters = I(list(
      numeric(0),
      numeric(0),
      numeric(0),
      numeric(0),
      numeric(0),
      numeric(0),
      c(C2H3N = "1"),
      numeric(0),
      numeric(0),
      numeric(0),
      numeric(0),
      c(C2H3N = "1", C2H7N = "1"),
      numeric(0),
      numeric(0),
      numeric(0),
      c(NaCl = "1")
    ))
  )
}

test_that("annotate_adduct_universe_metadata assigns adduct tiers in positive mode", {
  out <- as.data.frame(annotate_adduct_universe_metadata(
    .tier_universe(),
    "pos"
  ))
  rownames(out) <- NULL

  # tier 0: the baseline adduct only
  expect_equal(out$adduct_tier[out$adduct == "[M+H]+"], 0L)
  expect_true(out$is_baseline[out$adduct == "[M+H]+"])

  # tier 1: common carrier, no loss, no cluster, |z| == 1, at most a dimer
  expect_equal(out$adduct_tier[out$adduct == "[M+Na]+"], 1L)
  expect_equal(out$adduct_tier[out$adduct == "[2M+H]+"], 1L)
  # A positively charged adduct is not the baseline in positive mode.
  expect_equal(out$adduct_tier[out$adduct == "[M-H]-"], 1L)

  # tier 2: one common neutral loss, or |z| == 2
  expect_equal(out$adduct_tier[out$adduct == "[M-H2O+H]+"], 2L)
  expect_equal(out$adduct_tier[out$adduct == "[M+2H]2+"], 2L)
  expect_equal(out$adduct_tier[out$adduct == "[M-H-H2O]-"], 2L)

  # tier 3: one uncommon loss, one cluster, or a trimer
  expect_equal(out$adduct_tier[out$adduct == "[M+C3H6O+H]+"], 3L)
  expect_false(out$has_only_common_losses[out$adduct == "[M+C3H6O+H]+"])
  expect_equal(out$adduct_tier[out$adduct == "[M+H+CH3CN]+"], 3L)
  expect_true(out$has_cluster[out$adduct == "[M+H+CH3CN]+"])
  expect_equal(out$adduct_tier[out$adduct == "[3M+Na]+"], 3L)
  expect_equal(out$adduct_tier[out$adduct == "[M+H+NaCl]+"], 3L)

  # tier 4: exotic carrier, |z| >= 3, tetramer, two clusters, or a negative
  # adduct seen in positive mode
  expect_equal(out$adduct_tier[out$adduct == "[M+Fe]+"], 4L)
  expect_true(out$has_exotic_carrier[out$adduct == "[M+Fe]+"])
  expect_equal(out$adduct_tier[out$adduct == "[M+3H]3+"], 4L)
  expect_equal(out$adduct_tier[out$adduct == "[4M+H]+"], 4L)
  expect_equal(out$adduct_tier[out$adduct == "[M+CH3CN+CH3CN]+"], 4L)
  expect_equal(out$adduct_tier[out$adduct == "[M+Cl]-"], 4L)

  expect_setequal(out$adduct_tier, 0:4)
})

test_that("annotate_adduct_universe_metadata is polarity dependent", {
  out <- as.data.frame(annotate_adduct_universe_metadata(
    .tier_universe(),
    "neg"
  ))
  rownames(out) <- NULL

  # Na is a core carrier in positive mode but exotic in negative mode.
  expect_equal(out$adduct_tier[out$adduct == "[M+Na]+"], 4L)
  expect_equal(out$adduct_tier[out$adduct == "[3M+Na]+"], 4L)
  expect_true(out$has_exotic_carrier[out$adduct == "[M+Na]+"])
  # ... and Cl is the mirror image.
  expect_equal(out$adduct_tier[out$adduct == "[M+Cl]-"], 1L)
  # [M-H]- becomes the baseline, [M+H]+ is no longer core.
  expect_true(out$is_baseline[out$adduct == "[M-H]-"])
  expect_equal(out$adduct_tier[out$adduct == "[M-H]-"], 0L)
  expect_false(out$is_baseline[out$adduct == "[M+H]+"])
  expect_equal(out$adduct_tier[out$adduct == "[M+H]+"], 1L)
  # A trimer is core in positive mode (Na allowed) but not in negative mode.
  expect_equal(out$adduct_tier[out$adduct == "[M+H+NaCl]+"], 4L)
  # Losses, clusters and charges are polarity independent.
  expect_equal(out$adduct_tier[out$adduct == "[M-H2O+H]+"], 2L)
  expect_equal(out$adduct_tier[out$adduct == "[M-H-H2O]-"], 2L)
  expect_equal(out$adduct_tier[out$adduct == "[M+C3H6O+H]+"], 3L)
})

test_that("annotate_adduct_universe_metadata maps tiers to support requirements", {
  out <- as.data.frame(annotate_adduct_universe_metadata(
    .tier_universe(),
    "pos"
  ))
  rownames(out) <- NULL

  # Core adducts (tier 0/1) need no peer support and are not "core support".
  expect_equal(
    out$min_cluster_support[out$adduct_tier <= 1L],
    rep(1L, sum(out$adduct_tier <= 1L))
  )
  expect_false(any(out$requires_core_support[out$adduct_tier <= 1L]))
  expect_true(all(out$is_core_adduct[out$adduct_tier <= 1L]))

  # Tiers 2/3/4 need 2/3/4 supporting features respectively.
  expect_equal(
    out$min_cluster_support[out$adduct_tier == 2L],
    rep(2L, sum(out$adduct_tier == 2L))
  )
  expect_equal(
    out$min_cluster_support[out$adduct_tier == 3L],
    rep(3L, sum(out$adduct_tier == 3L))
  )
  expect_equal(
    out$min_cluster_support[out$adduct_tier == 4L],
    rep(4L, sum(out$adduct_tier == 4L))
  )
  expect_true(all(out$requires_core_support[out$adduct_tier >= 2L]))
  expect_false(any(out$is_core_adduct[out$adduct_tier >= 2L]))

  # min_cluster_support is exactly the tier for tiers >= 2 and 1 for the rest.
  expect_equal(
    out$min_cluster_support,
    ifelse(out$adduct_tier <= 1L, 1L, out$adduct_tier)
  )
})

test_that("annotate_adduct_universe_metadata derives term counts and flags", {
  out <- as.data.frame(annotate_adduct_universe_metadata(
    .tier_universe(),
    "pos"
  ))
  rownames(out) <- NULL
  u <- as.data.frame(.tier_universe())

  expect_equal(out$n_carrier_terms, lengths(u$carriers))
  expect_equal(out$n_loss_terms, lengths(u$losses))
  expect_equal(out$n_cluster_terms, lengths(u$clusters))
  expect_equal(out$has_loss, out$n_loss_terms > 0L)
  expect_equal(out$has_cluster, out$n_cluster_terms > 0L)
  expect_equal(out$has_exotic_carrier, !out$has_only_common_carriers)
  # Positive-mode common carriers are H/Na/K/NH4/Mg/Ca only.
  expect_false(out$has_only_common_carriers[out$adduct == "[M+Fe]+"])
  expect_true(out$has_only_common_carriers[out$adduct == "[M+H]+"])
  # Clusters must be common for the row to be flagged as such.
  expect_true(out$has_only_common_clusters[out$adduct == "[M+H+CH3CN]+"])
  expect_true(out$has_only_common_clusters[out$adduct == "[M+CH3CN+CH3CN]+"])
})

test_that("annotate_adduct_universe_metadata yields NA tiers for an unknown polarity", {
  out <- as.data.frame(annotate_adduct_universe_metadata(
    .tier_universe(),
    "other"
  ))
  expect_true(all(is.na(out$is_baseline)))
  expect_true(all(is.na(out$adduct_tier)))
})

# A one-row universe carrying a single named modifier, used to probe the
# common/exotic classification of each token individually.
.one_modifier_universe <- function(
  adduct = "[M-X+H]+",
  z = 1L,
  carrier = "H",
  loss = NULL,
  cluster = NULL
) {
  tidytable::tidytable(
    adduct = adduct,
    z = z,
    n_mer = 1L,
    carriers = I(list(setNames(1, carrier))),
    losses = I(list(if (is.null(loss)) numeric(0) else setNames(-1, loss))),
    clusters = I(list(
      if (is.null(cluster)) numeric(0) else setNames(1, cluster)
    ))
  )
}

test_that("every documented positive-mode carrier is treated as common", {
  # Each of these must stay in the "core" set; dropping any one of them makes
  # the adduct exotic and pushes it from tier 1 to tier 4.
  for (cn in c("H", "Na", "K", "H4N", "Mg", "Ca")) {
    u <- .one_modifier_universe(adduct = "[M+C]+", carrier = cn)
    out <- as.data.frame(annotate_adduct_universe_metadata(u, "pos"))
    expect_true(out$has_only_common_carriers[[1L]], info = cn)
    expect_false(out$has_exotic_carrier[[1L]], info = cn)
    expect_equal(out$adduct_tier[[1L]], 1L, info = cn)
  }

  for (cn in c("Fe", "Cu", "Zn", "Ag")) {
    u <- .one_modifier_universe(adduct = "[M+C]+", carrier = cn)
    out <- as.data.frame(annotate_adduct_universe_metadata(u, "pos"))
    expect_false(out$has_only_common_carriers[[1L]], info = cn)
    expect_true(out$has_exotic_carrier[[1L]], info = cn)
    expect_equal(out$adduct_tier[[1L]], 4L, info = cn)
  }
})

test_that("every documented negative-mode carrier is treated as common", {
  for (cn in c("H", "F", "Cl", "Br")) {
    u <- .one_modifier_universe(adduct = "[M-C]-", z = -1L, carrier = cn)
    out <- as.data.frame(annotate_adduct_universe_metadata(u, "neg"))
    expect_true(out$has_only_common_carriers[[1L]], info = cn)
    expect_equal(out$adduct_tier[[1L]], 1L, info = cn)
  }

  # The positive-mode carriers that are not halides are exotic in negative mode.
  for (cn in c("Na", "K", "Mg", "Ca")) {
    u <- .one_modifier_universe(adduct = "[M-C]-", z = -1L, carrier = cn)
    out <- as.data.frame(annotate_adduct_universe_metadata(u, "neg"))
    expect_true(out$has_exotic_carrier[[1L]], info = cn)
    expect_equal(out$adduct_tier[[1L]], 4L, info = cn)
  }
})

test_that("every documented common neutral loss yields tier 2", {
  for (f in c(
    "H2O",
    "H4O2",
    "H8O4",
    "H3N",
    "CO",
    "CO2",
    "SO2",
    "SO3",
    "HF",
    "CF2",
    "CF2O",
    "CF3",
    "CHF2",
    "C2F2",
    "C2F4",
    "C3F6"
  )) {
    out <- as.data.frame(annotate_adduct_universe_metadata(
      .one_modifier_universe(loss = f),
      "pos"
    ))
    expect_true(out$has_only_common_losses[[1L]], info = f)
    expect_equal(out$adduct_tier[[1L]], 2L, info = f)
    expect_equal(out$min_cluster_support[[1L]], 2L, info = f)
  }
})

test_that("an undocumented neutral loss is exotic and costs a tier", {
  for (f in c("C7H14O2", "CH4O", "C3H8O2")) {
    out <- as.data.frame(annotate_adduct_universe_metadata(
      .one_modifier_universe(loss = f),
      "pos"
    ))
    expect_false(out$has_only_common_losses[[1L]], info = f)
    expect_equal(out$adduct_tier[[1L]], 3L, info = f)
    expect_equal(out$min_cluster_support[[1L]], 3L, info = f)
  }
})

test_that("every documented common cluster yields tier 3", {
  for (f in c("C2H3N", "C2H7N", "NaCl", "C2H6OS")) {
    out <- as.data.frame(annotate_adduct_universe_metadata(
      .one_modifier_universe(cluster = f),
      "pos"
    ))
    expect_true(out$has_only_common_clusters[[1L]], info = f)
    expect_true(out$has_cluster[[1L]], info = f)
    expect_equal(out$adduct_tier[[1L]], 3L, info = f)
    expect_equal(out$min_cluster_support[[1L]], 3L, info = f)
  }
})

test_that("an undocumented cluster is flagged but does not change the tier", {
  for (f in c("C4H8O", "C5H11N")) {
    out <- as.data.frame(annotate_adduct_universe_metadata(
      .one_modifier_universe(cluster = f),
      "pos"
    ))
    expect_false(out$has_only_common_clusters[[1L]], info = f)
    # One cluster is allowed in tier 3 regardless of what it is.
    expect_equal(out$adduct_tier[[1L]], 3L, info = f)
  }
})

test_that("an absent modifier counts as common", {
  out <- as.data.frame(annotate_adduct_universe_metadata(
    .one_modifier_universe(adduct = "[M+Na]+"),
    "pos"
  ))
  expect_equal(out$n_loss_terms[[1L]], 0L)
  expect_equal(out$n_cluster_terms[[1L]], 0L)
  expect_false(out$has_loss[[1L]])
  expect_false(out$has_cluster[[1L]])
  expect_true(out$has_only_common_losses[[1L]])
  expect_true(out$has_only_common_clusters[[1L]])
  expect_equal(out$adduct_tier[[1L]], 1L)
})

test_that("a completely bare adduct row is treated as all-common", {
  # Zero carriers is not an exotic carrier set.
  u <- tidytable::tidytable(
    adduct = c("[M]+", "[M+Na]+"),
    z = c(1L, 1L),
    n_mer = c(1L, 1L),
    carriers = I(list(numeric(0), c(Na = "1"))),
    losses = I(list(numeric(0), numeric(0))),
    clusters = I(list(numeric(0), numeric(0)))
  )
  out <- as.data.frame(annotate_adduct_universe_metadata(u, "pos"))
  expect_equal(out$n_carrier_terms, c(0L, 1L))
  expect_true(all(out$has_only_common_carriers))
  expect_false(any(out$has_exotic_carrier))
  expect_equal(out$adduct_tier, c(1L, 1L))
})

test_that("the modifier slots are read by name, whatever the coefficient", {
  # Classification keys on the formula name only, so a positive coefficient
  # must not change the tier.
  for (coef in c(-1, 1)) {
    u <- tidytable::tidytable(
      adduct = "[M-X+H]+",
      z = 1L,
      n_mer = 1L,
      carriers = list(c(H = "1")),
      losses = list(c(H2O = coef)),
      clusters = list(numeric(0))
    )
    out <- as.data.frame(annotate_adduct_universe_metadata(u, "pos"))
    expect_equal(out$n_loss_terms[[1L]], 1L, info = coef)
    expect_true(out$has_loss[[1L]], info = coef)
    expect_true(out$has_only_common_losses[[1L]], info = coef)
    expect_equal(out$adduct_tier[[1L]], 2L, info = coef)
  }
})

test_that("a factor adduct column is still matched against the baseline", {
  u <- tidytable::tidytable(
    adduct = factor(c("[M+H]+", "[M+Na]+")),
    z = c(1L, 1L),
    n_mer = c(1L, 1L),
    carriers = I(list(c(H = "1"), c(Na = "1"))),
    losses = I(list(numeric(0), numeric(0))),
    clusters = I(list(numeric(0), numeric(0)))
  )
  out <- as.data.frame(annotate_adduct_universe_metadata(u, "pos"))
  expect_true(out$is_baseline[[1L]])
  expect_equal(out$adduct_tier[[1L]], 0L)
  expect_false(out$is_baseline[[2L]])
  expect_equal(out$adduct_tier[[2L]], 1L)
})
