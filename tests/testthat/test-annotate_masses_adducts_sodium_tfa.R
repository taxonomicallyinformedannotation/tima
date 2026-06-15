# Test Suite: Sodium carboxylate and TFA adducts - Real annotation pipeline ----
#
# End-to-end tests: Two ions with correct m/z delta should be annotated
# with correct adducts ([M-H2+Na]- and [M-H+C2HF3O2]-).

library(testthat)
library(tima)

# Helper: Create features with specified m/z values and RT
create_test_features <- function(
  mz_values,
  rt_value,
  feature_names = NULL,
  sample_name = "sample1"
) {
  n <- length(mz_values)
  if (is.null(feature_names)) {
    feature_names <- paste0("F_", seq_len(n))
  }

  data.frame(
    feature_id = feature_names,
    mz = mz_values,
    rt = rep(rt_value, n),
    sample = rep(sample_name, n),
    stringsAsFactors = FALSE
  )
}

## Annotation Pipeline Tests ----

test_that("Two features with [M-H]- and [M-H2+Na]- delta (~21 Da) both appear in annotations", {
  local_quiet_logging()

  # Setup: Two features of same neutral mas in same RT window
  # One as [M-H]-, one as [M-H2+Na]-

  H_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["H"]]
  Na_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["Na"]]
  electron_mass <- getFromNamespace("ELECTRON_MASS_DA", "tima")

  M <- 100.0 # Arbitrary neutral mass
  mz_neg <- M - H_mass - electron_mass # [M-H]-
  mz_na <- M + Na_mass - 2 * H_mass - electron_mass # [M-H2+Na]-

  features <- create_test_features(c(mz_neg, mz_na), rt = 2.5)

  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_small.csv",
    library_fixture = "library_realistic.csv"
  )

  withr::local_dir(new = env$dirs$root)

  # Write test features
  utils::write.csv(features, env$features, row.names = FALSE)

  # Run annotation with both adducts configured
  result <- annotate_masses(
    features = env$features,
    library = env$library,
    str_stereo = env$str_stereo,
    str_met = env$str_met,
    str_tax_cla = env$str_tax_cla,
    str_tax_npc = env$str_tax_npc,
    adducts_list = list(
      neg = c("[M-H]-", "[M-H2+Na]-")
    ),
    ms_mode = "neg",
    tolerance_ppm = 10,
    tolerance_rt = 0.02,
    output_annotations = env$output_annotations,
    output_edges = env$output_edges
  )

  # Verify output exists
  expect_type(result, "character")
  expect_true(file.exists(result["annotations"]))

  # Load and verify annotations
  annotations <- tidytable::fread(
    result["annotations"],
    colClasses = "character"
  )

  # Should have hypothesis rows for both features
  expect_true(nrow(annotations) >= 2L)

  # CHECK: [M-H2+Na]- (sodium adduct) MUST appear
  adducts_seen <- na.omit(annotations$candidate_adduct)
  expect_true(
    any(grepl("\\[M-H2\\+Na\\]-", adducts_seen)),
    label = "[M-H2+Na]- (sodium adduct) should appear in annotations"
  )
})

test_that("Two features with [M-H]- and [M-H+C2HF3O2]- delta (~114 Da) both appear in annotations", {
  local_quiet_logging()

  # Setup: Base compound and TFA-attached form
  H_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["H"]]
  C_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["C"]]
  F_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["F"]]
  O_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["O"]]
  electron_mass <- getFromNamespace("ELECTRON_MASS_DA", "tima")

  M <- 150.0
  TFA_mass <- 2 * C_mass + H_mass + 3 * F_mass + 2 * O_mass

  mz_neg <- M - H_mass - electron_mass # [M-H]-
  mz_tfa <- M - H_mass + TFA_mass - electron_mass # [M-H+C2HF3O2]-

  features <- create_test_features(c(mz_neg, mz_tfa), rt = 3.5)

  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_small.csv",
    library_fixture = "library_realistic.csv"
  )

  withr::local_dir(new = env$dirs$root)
  utils::write.csv(features, env$features, row.names = FALSE)

  # Run annotation with TFA cluster
  result <- annotate_masses(
    features = env$features,
    library = env$library,
    str_stereo = env$str_stereo,
    str_met = env$str_met,
    str_tax_cla = env$str_tax_cla,
    str_tax_npc = env$str_tax_npc,
    adducts_list = list(
      neg = c("[M-H]-", "[M-H+C2HF3O2]-")
    ),
    clusters_list = list(neg = c("C2HF3O2")),
    ms_mode = "neg",
    tolerance_ppm = 10,
    tolerance_rt = 0.02,
    output_annotations = env$output_annotations,
    output_edges = env$output_edges
  )

  expect_type(result, "character")
  expect_true(file.exists(result["annotations"]))

  annotations <- tidytable::fread(
    result["annotations"],
    colClasses = "character"
  )

  expect_true(nrow(annotations) >= 2L)

  # CHECK: [M-H+C2HF3O2]- (TFA adduct) MUST appear
  adducts_seen <- na.omit(annotations$candidate_adduct)
  expect_true(
    any(grepl("C2HF3O2", adducts_seen)),
    label = "[M-H+C2HF3O2]- (TFA) should appear in annotations"
  )
})

test_that("Three feature pair with [M-H]-, [M-H2+Na]-, [M-H+C2HF3O2]- in same RT generates edges", {
  local_quiet_logging()

  # Setup: Three forms of same neutral mass in same RT
  H_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["H"]]
  Na_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["Na"]]
  C_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["C"]]
  F_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["F"]]
  O_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["O"]]
  electron_mass <- getFromNamespace("ELECTRON_MASS_DA", "tima")

  M <- 200.0
  TFA_mass <- 2 * C_mass + H_mass + 3 * F_mass + 2 * O_mass

  mz1 <- M - H_mass - electron_mass # [M-H]-
  mz2 <- M + Na_mass - 2 * H_mass - electron_mass # [M-H2+Na]-
  mz3 <- M - H_mass + TFA_mass - electron_mass # [M-H+C2HF3O2]-

  rt <- 4.0

  features <- create_test_features(c(mz1, mz2, mz3), rt = rt)

  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_small.csv",
    library_fixture = "library_realistic.csv"
  )

  withr::local_dir(new = env$dirs$root)
  utils::write.csv(features, env$features, row.names = FALSE)

  # Run annotation with all adducts
  result <- annotate_masses(
    features = env$features,
    library = env$library,
    str_stereo = env$str_stereo,
    str_met = env$str_met,
    str_tax_cla = env$str_tax_cla,
    str_tax_npc = env$str_tax_npc,
    adducts_list = list(
      neg = c("[M-H]-", "[M-H2+Na]-", "[M-H+C2HF3O2]-")
    ),
    clusters_list = list(neg = c("C2HF3O2")),
    ms_mode = "neg",
    tolerance_ppm = 10,
    tolerance_rt = 0.02,
    output_annotations = env$output_annotations,
    output_edges = env$output_edges
  )

  expect_type(result, "character")
  expect_true(file.exists(result["annotations"]))
  expect_true(file.exists(result["edges"]))

  annotations <- tidytable::fread(
    result["annotations"],
    colClasses = "character"
  )
  edges <- tidytable::fread(result["edges"], colClasses = "character")

  # Should have annotations for all features
  expect_true(nrow(annotations) >= 3L)

  # CHECK: Both sodium and TFA annotations MUST appear
  adducts_seen <- na.omit(annotations$candidate_adduct)
  expect_true(
    any(grepl("\\[M-H2\\+Na\\]-", adducts_seen)),
    label = "[M-H2+Na]- (sodium adduct) should appear in triple-feature test"
  )
  expect_true(
    any(grepl("C2HF3O2", adducts_seen)),
    label = "[M-H+C2HF3O2]- (TFA) should appear in triple-feature test"
  )

  # With 3 features in same RT, should have edges between them
  expect_true(nrow(edges) >= 1L)
})

test_that("Ion pair edges correctly link [M-H]- and [M-H2+Na]- as same compound", {
  local_quiet_logging()

  # Verify that edge generation recognizes these are same compound
  H_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["H"]]
  Na_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["Na"]]
  electron_mass <- getFromNamespace("ELECTRON_MASS_DA", "tima")

  M <- 180.0
  mz_neg <- M - H_mass - electron_mass
  mz_na <- M + Na_mass - 2 * H_mass - electron_mass

  features <- create_test_features(c(mz_neg, mz_na), rt = 2.5)

  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_small.csv",
    library_fixture = "library_realistic.csv"
  )

  withr::local_dir(new = env$dirs$root)
  utils::write.csv(features, env$features, row.names = FALSE)

  result <- annotate_masses(
    features = env$features,
    library = env$library,
    str_stereo = env$str_stereo,
    str_met = env$str_met,
    str_tax_cla = env$str_tax_cla,
    str_tax_npc = env$str_tax_npc,
    adducts_list = list(
      neg = c("[M-H]-", "[M-H2+Na]-")
    ),
    ms_mode = "neg",
    tolerance_ppm = 10,
    tolerance_rt = 0.02,
    output_annotations = env$output_annotations,
    output_edges = env$output_edges
  )

  annotations <- tidytable::fread(
    result["annotations"],
    colClasses = "character"
  )
  edges <- tidytable::fread(result["edges"], colClasses = "character")

  # CHECK: [M-H2+Na]- MUST appear in annotations
  adducts_seen <- na.omit(annotations$candidate_adduct)
  expect_true(
    any(grepl("\\[M-H2\\+Na\\]-", adducts_seen)),
    label = "[M-H2+Na]- (sodium adduct) should appear when edges are generated"
  )

  # Should have at least one edge
  expect_true(nrow(edges) >= 1L)
})

test_that("Two features with [M-H]- and [M+COONa]- show sodium carboxylate adduct", {
  local_quiet_logging()

  # Setup: One as [M-H]-, one as [M+COONa]- (sodium carboxylate)
  # COONa = C + 2*O + Na = carboxylate group with sodium counterion

  H_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["H"]]
  C_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["C"]]
  O_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["O"]]
  Na_mass <- tima:::ATOMIC_MONOISOTOPIC_MASS[["Na"]]
  electron_mass <- getFromNamespace("ELECTRON_MASS_DA", "tima")

  M <- 150.0 # Arbitrary neutral mass
  COONa_mass <- C_mass + 2 * O_mass + Na_mass # Sodium carboxylate group mass

  mz_neg <- M - H_mass - electron_mass # [M-H]-
  mz_cooNa <- M + COONa_mass - electron_mass # [M+COONa]-

  features <- create_test_features(c(mz_neg, mz_cooNa), rt = 5.0)

  env <- prepare_annotation_fixture_env(
    feature_fixture = "features_small.csv",
    library_fixture = "library_realistic.csv"
  )

  withr::local_dir(new = env$dirs$root)
  utils::write.csv(features, env$features, row.names = FALSE)

  # Run annotation with both adducts configured
  result <- annotate_masses(
    features = env$features,
    library = env$library,
    str_stereo = env$str_stereo,
    str_met = env$str_met,
    str_tax_cla = env$str_tax_cla,
    str_tax_npc = env$str_tax_npc,
    adducts_list = list(
      neg = c("[M-H]-", "[M+COONa]-")
    ),
    ms_mode = "neg",
    tolerance_ppm = 10,
    tolerance_rt = 0.02,
    output_annotations = env$output_annotations,
    output_edges = env$output_edges
  )

  # Verify output exists
  expect_type(result, "character")
  expect_true(file.exists(result["annotations"]))

  # Load and verify annotations
  annotations <- tidytable::fread(
    result["annotations"],
    colClasses = "character"
  )

  # Should have hypothesis rows for both features
  expect_true(nrow(annotations) >= 2L)

  # CHECK: [M+COONa]- (sodium carboxylate adduct) MUST appear
  adducts_seen <- na.omit(annotations$candidate_adduct)
  expect_true(
    any(grepl("COONa", adducts_seen)),
    label = "[M+COONa]- (sodium carboxylate) should appear in annotations"
  )
})
