library(testthat)

implied_neutral_mass <- implied_neutral_mass
has_library_match <- .has_library_match_within_ppm
nearest_ppm <- .nearest_exact_mass_ppm
nearest_idx <- .nearest_exact_mass_index
ppm_cluster <- .ppm_cluster_sorted
annotate_universe_meta <- annotate_adduct_universe_metadata
EVIDENCE_ISOTOPE_SHIFT_DA <- EVIDENCE_ISOTOPE_SHIFT_DA
build_adduct_universe <- build_adduct_universe

# implied_neutral_mass ----
test_that("implied_neutral_mass computes basic [M+H]+ case correctly", {
  # [M+H]+: z=1, n_mer=1, adduct_mass = mass of H = 1.007276
  h_mass <- 1.007276
  mz <- 200.0
  expected <- (1L * (mz - 0) - h_mass) / 1L - 0
  result <- implied_neutral_mass(
    mz = mz,
    n_mer = 1L,
    z = 1L,
    adduct_mass = h_mass
  )
  expect_equal(result, expected, tolerance = 1e-9)
})

test_that("implied_neutral_mass applies isotope shift", {
  h_mass <- 1.007276
  mz <- 200.0
  # n_iso = 1: M1 isotopologue
  result_no_iso <- implied_neutral_mass(mz, 1L, 1L, h_mass, n_iso = 0L)
  result_iso <- implied_neutral_mass(mz, 1L, 1L, h_mass, n_iso = 1L)
  expect_equal(
    result_iso,
    result_no_iso - EVIDENCE_ISOTOPE_SHIFT_DA,
    tolerance = 1e-9
  )
})

test_that("implied_neutral_mass handles dimers (n_mer = 2)", {
  h_mass <- 1.007276
  mz <- 350.0
  result <- implied_neutral_mass(mz, n_mer = 2L, z = 1L, adduct_mass = h_mass)
  expected <- (1L * mz - h_mass) / 2L
  expect_equal(result, expected, tolerance = 1e-9)
})

test_that("implied_neutral_mass handles negative charge", {
  h_loss <- -1.007276
  mz <- 199.0
  # [M-H]-: z = -1, adduct_mass = -1.007276
  result <- implied_neutral_mass(mz, n_mer = 1L, z = -1L, adduct_mass = h_loss)
  expected <- (1L * mz - h_loss) / 1L
  expect_equal(result, expected, tolerance = 1e-9)
})

# .has_library_match_within_ppm ----
test_that(".has_library_match_within_ppm detects matches within tolerance", {
  lib_masses <- sort(c(100.0, 200.0, 300.0))
  # query at 200.001 ~ 5 ppm from 200.0
  query <- c(100.0, 200.001, 350.0)
  result <- has_library_match(query, lib_masses, tolerance_ppm = 10)
  expect_equal(result, c(TRUE, TRUE, FALSE))
})

test_that(".has_library_match_within_ppm returns all FALSE for empty library", {
  result <- has_library_match(
    c(100.0, 200.0),
    exact_masses_sorted = numeric(0),
    10
  )
  expect_equal(result, c(FALSE, FALSE))
})

test_that(".has_library_match_within_ppm returns all FALSE for empty queries", {
  result <- has_library_match(numeric(0), c(100.0), 10)
  expect_length(result, 0)
  expect_false(any(result))
})

test_that(".has_library_match_within_ppm is tight at exact tolerance boundary", {
  lib <- sort(c(200.0))
  # 200 * 5e-6 = 0.001 Da -- exact boundary should match
  query_in <- 200.0 + 200.0 * 5e-6
  query_out <- 200.0 + 200.0 * 6e-6
  expect_true(has_library_match(query_in, lib, 5)[1])
  expect_false(has_library_match(query_out, lib, 5)[1])
})

# .nearest_exact_mass_ppm ----
test_that(".nearest_exact_mass_ppm returns Inf for empty library", {
  result <- nearest_ppm(c(100.0, 200.0), numeric(0))
  expect_equal(result, c(Inf, Inf))
})

test_that(".nearest_exact_mass_ppm computes correct ppm errors", {
  lib <- sort(c(100.0, 200.0, 300.0))
  query <- c(100.001, 200.0)
  result <- nearest_ppm(query, lib)
  # ppm for 100.001 vs 100.0: 0.001/100 * 1e6 = 10 ppm
  expect_equal(result[1], 10, tolerance = 0.01)
  expect_equal(result[2], 0, tolerance = 1e-9)
})

test_that(".nearest_exact_mass_ppm returns correct length", {
  lib <- sort(c(100.0, 200.0))
  expect_length(nearest_ppm(numeric(0), lib), 0)
})

# .ppm_cluster_sorted ----
test_that(".ppm_cluster_sorted returns empty for empty input", {
  expect_equal(ppm_cluster(numeric(0), 10), integer(0))
})

test_that(".ppm_cluster_sorted returns 1 for single value", {
  expect_equal(ppm_cluster(c(100.0), 10), 1L)
})

test_that(".ppm_cluster_sorted clusters tight values together", {
  vals <- sort(c(100.000, 100.0005, 200.000))
  clusters <- ppm_cluster(vals, tolerance_ppm = 10)
  # 100.000 and 100.0005 are 5 ppm apart, should be in same cluster
  expect_equal(clusters[1], clusters[2])
  expect_true(clusters[3] > clusters[2])
})

test_that(".ppm_cluster_sorted assigns separate clusters to distant values", {
  vals <- sort(c(100.0, 200.0, 300.0))
  clusters <- ppm_cluster(vals, tolerance_ppm = 10)
  expect_equal(length(unique(clusters)), 3L)
})

test_that(".ppm_cluster_sorted NA values each get their own cluster", {
  vals <- c(NA_real_, 100.0, NA_real_)
  # NAs should be boundaries
  clusters <- ppm_cluster(vals, tolerance_ppm = 10)
  expect_equal(length(clusters), 3L)
  # The two NAs should be singletons (different clusters)
  expect_false(clusters[1] == clusters[3])
})

# annotate_adduct_universe_metadata ----
test_that("annotate_adduct_universe_metadata adds expected columns for pos", {
  universe <- build_adduct_universe(
    adducts_list = list(pos = c("[M+H]+", "[M+Na]+")),
    clusters_list = list(pos = c("H2O")),
    neutral_losses_list = character(),
    polarity = "pos"
  )
  result <- annotate_universe_meta(universe, "pos")
  expect_true("adduct_tier" %in% names(result))
  expect_true("is_core_adduct" %in% names(result))
  expect_true("min_cluster_support" %in% names(result))
  expect_true("is_baseline" %in% names(result))
})

test_that("annotate_adduct_universe_metadata baseline [M+H]+ gets tier 0", {
  universe <- build_adduct_universe(
    adducts_list = list(pos = c("[M+H]+", "[M+Na]+")),
    clusters_list = list(pos = character()),
    neutral_losses_list = character(),
    polarity = "pos"
  )
  result <- annotate_universe_meta(universe, "pos")
  baseline_row <- result[result$adduct == "[M+H]+", ]
  if (nrow(baseline_row) > 0) {
    expect_true(all(baseline_row$is_baseline))
    expect_equal(baseline_row$adduct_tier[[1]], 0L)
  } else {
    skip("[M+H]+ not in generated universe")
  }
})
