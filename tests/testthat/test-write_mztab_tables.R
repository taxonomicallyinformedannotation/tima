# Test Suite: write_mztab_tables ----
# Tests for internal mzTab table serialization helpers in write_mztab_tables.R

library(testthat)

ns <- asNamespace("tima")
mztab_pluck <- get(".mztab_pluck", envir = ns)
mztab_pluck_na <- get(".mztab_pluck_na", envir = ns)
mztab_expand_summarized <- get(".mztab_expand_summarized", envir = ns)

# ── .mztab_pluck ─────────────────────────────────────────────────────────────

test_that(".mztab_pluck returns column as character when present", {
  df <- data.frame(a = c(1, 2), b = c("x", "y"), stringsAsFactors = FALSE)
  result <- mztab_pluck(df, "a")
  expect_equal(result, c("1", "2"))

  result_b <- mztab_pluck(df, "b")
  expect_equal(result_b, c("x", "y"))
})

test_that(".mztab_pluck returns 'null' vector when column absent", {
  df <- data.frame(a = c(1, 2), stringsAsFactors = FALSE)
  result <- mztab_pluck(df, "missing_col")
  expect_equal(result, c("null", "null"))
  expect_length(result, 2L)
})

# ── .mztab_pluck_na ───────────────────────────────────────────────────────────

test_that(".mztab_pluck_na replaces NA and empty with 'null'", {
  df <- data.frame(
    a = c("hello", NA_character_, "", "null", "world"),
    stringsAsFactors = FALSE
  )
  result <- mztab_pluck_na(df, "a")
  expect_equal(result, c("hello", "null", "null", "null", "world"))
})

test_that(".mztab_pluck_na returns all 'null' for absent column", {
  df <- data.frame(a = 1:3)
  result <- mztab_pluck_na(df, "b")
  expect_equal(result, rep("null", 3L))
})

# ── .mztab_expand_summarized ──────────────────────────────────────────────────

test_that(".mztab_expand_summarized returns unchanged df when no pipe-sep columns", {
  df <- data.frame(
    feature_id = c("f1", "f2"),
    candidate_score = c("0.8", "0.7"),
    stringsAsFactors = FALSE
  )
  result <- mztab_expand_summarized(df)
  expect_equal(nrow(result), 2L)
  expect_equal(result$feature_id, c("f1", "f2"))
})

test_that(".mztab_expand_summarized expands pipe-separated candidate columns", {
  df <- data.frame(
    feature_id = c("f1"),
    candidate_smiles = c("CC|CCC"),
    rank_1 = c("1|2"),
    stringsAsFactors = FALSE
  )
  result <- mztab_expand_summarized(df)
  expect_equal(nrow(result), 2L)
  expect_equal(result$feature_id, c("f1", "f1"))
  expect_equal(trimws(result$candidate_smiles), c("CC", "CCC"))
})

test_that(".mztab_expand_summarized handles rows with and without pipe", {
  df <- data.frame(
    feature_id = c("f1", "f2"),
    candidate_smiles = c("CC|CCC", "CCC"),
    stringsAsFactors = FALSE
  )
  result <- mztab_expand_summarized(df)
  expect_equal(nrow(result), 3L)
})

test_that(".mztab_expand_summarized pads shorter multi-value rows with NA", {
  df <- data.frame(
    feature_id = c("f1", "f2"),
    candidate_smiles = c("CC|CCC", "DDD"),
    rank_final = c("1|2", "1"),
    score_final = c("0.8|0.7", "0.6"),
    stringsAsFactors = FALSE
  )

  result <- mztab_expand_summarized(df)

  expect_equal(nrow(result), 3L)
  expect_equal(result$feature_id, c("f1", "f1", "f2"))
  expect_equal(result$candidate_smiles, c("CC", "CCC", "DDD"))
  expect_equal(result$rank_final, c("1", "2", "1"))
  expect_equal(result$score_final, c("0.8", "0.7", "0.6"))
})

test_that(".mztab_expand_summarized returns df unchanged when no candidate/rank/score cols", {
  df <- data.frame(
    feature_id = c("f1"),
    mz = c(100.0),
    stringsAsFactors = FALSE
  )
  result <- mztab_expand_summarized(df)
  expect_equal(nrow(result), 1L)
  expect_equal(result$feature_id, "f1")
})

mztab_build_smf <- get(".mztab_build_smf", envir = ns)
mztab_build_sme <- get(".mztab_build_sme", envir = ns)
mztab_build_sml <- get(".mztab_build_sml", envir = ns)

test_that(".mztab_build_smf preserves feature-level metadata and defaults", {
  results <- data.frame(
    feature_id = c("F1", "F1", "F2"),
    feature_mz = c("100.0", "100.0", "200.0"),
    feature_rt = c("1.2", "1.2", "2.3"),
    component_id = c("C1", "C1", "C2"),
    candidates_evaluated = c("2", "2", "1"),
    stringsAsFactors = FALSE
  )

  smf <- mztab_build_smf(results)

  expect_equal(nrow(smf), 2L)
  expect_equal(smf$feature_id, c("F1", "F2"))
  expect_equal(smf$opt_global_component_id, c("C1", "C2"))
  expect_equal(smf$SMF_ID, c("1", "2"))
  expect_equal(smf$SML_ID_REFS, c(NA_character_, NA_character_))
  expect_equal(smf$exp_mass_to_charge, c("100.0", "200.0"))
})

test_that(".mztab_build_sme handles empty and populated annotation tables", {
  empty_smf <- data.frame(
    SMF_ID = character(0),
    SML_ID_REFS = character(0),
    SME_ID_REFS = character(0),
    SME_ID_REF_ambiguity_code = character(0),
    exp_mass_to_charge = character(0),
    charge = character(0),
    retention_time_in_seconds = character(0),
    feature_id = character(0),
    stringsAsFactors = FALSE
  )

  empty_sme <- mztab_build_sme(
    results = data.frame(feature_id = character(0), stringsAsFactors = FALSE),
    smf_table = empty_smf
  )

  expect_equal(nrow(empty_sme), 0L)

  results <- data.frame(
    feature_id = c("F1", "F1"),
    feature_mz = c("100.1", "100.1"),
    candidate_structure_name = c("CmpdA", "CmpdB"),
    candidate_structure_molecular_formula = c("C10H10", "C11H12"),
    candidate_structure_smiles_no_stereo = c("CCO", "CCN"),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2"),
    candidate_structure_exact_mass = c("100.1", "101.1"),
    candidate_adduct = c("[M+H]+", "[M+Na]+"),
    candidate_library = c("spectral", "spectral"),
    rank_final = c("1", "2"),
    score_final = c("0.9", "0.1"),
    score_biological = c("0.8", "0.05"),
    score_chemical = c("0.7", "0.02"),
    stringsAsFactors = FALSE
  )

  smf <- data.frame(
    SMF_ID = "1",
    SML_ID_REFS = NA_character_,
    SME_ID_REFS = NA_character_,
    SME_ID_REF_ambiguity_code = "null",
    exp_mass_to_charge = "100.1",
    charge = "null",
    retention_time_in_seconds = "null",
    feature_id = "F1",
    stringsAsFactors = FALSE
  )

  sme <- mztab_build_sme(results, smf)

  expect_equal(nrow(sme), 2L)
  expect_equal(sme$evidence_input_id, c("1", "1"))
  expect_equal(sme$rank, c("1", "2"))
  expect_equal(sme$chemical_name, c("CmpdA", "CmpdB"))
  expect_equal(sme$adduct_ion, c("[M+H]+", "[M+Na]+"))
  expect_false("id_confidence_measure[4]" %in% names(sme))
})

test_that(".mztab_build_sml emits one row per feature and aligns ambiguity values", {
  smf <- data.frame(
    SMF_ID = c("1", "2"),
    SML_ID_REFS = NA_character_,
    SME_ID_REFS = NA_character_,
    SME_ID_REF_ambiguity_code = "null",
    exp_mass_to_charge = c("100.0", "200.0"),
    charge = "null",
    retention_time_in_seconds = c("72", "144"),
    feature_id = c("F1", "F2"),
    stringsAsFactors = FALSE
  )

  sme <- data.frame(
    SME_ID = c(1L, 2L),
    evidence_input_id = c("1", "1"),
    database_identifier = c("IK1", "IK2"),
    chemical_formula = c("C10H10", "C11H12"),
    smiles = c("CCO", "CCN"),
    inchi = c("null", "null"),
    chemical_name = c("CmpdA", "CmpdB"),
    uri = c("null", "null"),
    derivatized_form = c("null", "null"),
    adduct_ion = c("[M+H]+", "[M+Na]+"),
    exp_mass_to_charge = c("100.0", "100.0"),
    charge = c("1", "1"),
    theoretical_mass_to_charge = c("100.0", "101.0"),
    spectra_ref = c("null", "null"),
    identification_method = c(
      "[MS, MS:1002825, spectral library match, ]",
      "[MS, MS:1002825, spectral library match, ]"
    ),
    ms_level = c(
      "[MS, MS:1000579, MS1 spectrum, ]",
      "[MS, MS:1000579, MS1 spectrum, ]"
    ),
    rank = c("1", "2"),
    `id_confidence_measure[1]` = c("0.9", "0.1"),
    `id_confidence_measure[2]` = c("null", "null"),
    `id_confidence_measure[3]` = c("null", "null"),
    feature_id = c("F1", "F1"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  sml <- mztab_build_sml(
    results = data.frame(
      feature_id = "F1",
      candidate_structure_exact_mass = "100.1",
      score_final = "0.9",
      stringsAsFactors = FALSE
    ),
    smf_table = smf,
    sme_table = sme
  )

  expect_equal(nrow(sml), 1L)
  expect_equal(sml$SMF_ID_REFS, "1")
  expect_equal(sml$SME_ID_REFS, "1|2")
  expect_equal(sml$chemical_name, "CmpdA|CmpdB")
  expect_equal(sml$adduct_ions, "[M+H]+|[M+Na]+")
  expect_equal(sml$best_id_evidence_value, "0.9")
})

test_that(".mztab_build_sml returns a skeleton row for empty SME data", {
  sml <- mztab_build_sml(
    results = data.frame(feature_id = character(0), stringsAsFactors = FALSE),
    smf_table = data.frame(
      SMF_ID = "1",
      feature_id = "F1",
      stringsAsFactors = FALSE
    ),
    sme_table = tidytable::tidytable(
      SME_ID = integer(0),
      feature_id = character(0),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(sml), 1L)
  expect_equal(sml$SMF_ID_REFS, "1")
  expect_equal(sml$SME_ID_REFS, "null")
  expect_equal(sml$database_identifier, "null")
})
