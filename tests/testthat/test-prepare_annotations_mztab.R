# Test Suite: prepare_annotations_mztab ----

library(testthat)

.create_mztab_lookup_fixtures <- function(dir) {
  stereo <- file.path(dir, "stereo.tsv")
  met <- file.path(dir, "met.tsv")
  cla <- file.path(dir, "cla.tsv")
  npc <- file.path(dir, "npc.tsv")

  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = "RYYVLZVUVIJVGH",
      structure_inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
      structure_smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
      structure_smiles_no_stereo = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"
    ),
    stereo,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = "RYYVLZVUVIJVGH",
      structure_inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
      structure_smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
      structure_name = "Caffeine",
      structure_molecular_formula = "C8H10N4O2",
      structure_exact_mass = 194.0804,
      structure_xlogp = -0.07
    ),
    met,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = "RYYVLZVUVIJVGH",
      structure_inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
      structure_smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
      structure_tax_cla_chemontid = "CHEMONTID:0000001",
      structure_tax_cla_01kin = "Organic compounds",
      structure_tax_cla_02sup = "Organoheterocyclic compounds",
      structure_tax_cla_03cla = "Imidazopyrimidines",
      structure_tax_cla_04dirpar = "Xanthines"
    ),
    cla,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
      structure_tax_npc_01pat = "Alkaloids",
      structure_tax_npc_02sup = "Pseudoalkaloids",
      structure_tax_npc_03cla = "Purine alkaloids"
    ),
    npc,
    sep = "\t"
  )
  list(stereo = stereo, met = met, cla = cla, npc = npc)
}

test_that("prepare_annotations_mztab exports empty annotations when input is NULL, empty, or missing", {
  tmp_dir <- withr::local_tempdir()
  fx <- .create_mztab_lookup_fixtures(tmp_dir)

  for (bad_in in list(NULL, "", file.path(tmp_dir, "missing.mztab"))) {
    out_file <- tempfile(tmpdir = tmp_dir, fileext = ".tsv")
    res <- prepare_annotations_mztab(
      input = bad_in,
      output = out_file,
      str_stereo = fx$stereo,
      str_met = fx$met,
      str_tax_cla = fx$cla,
      str_tax_npc = fx$npc
    )
    expect_identical(res, out_file)
    expect_true(file.exists(out_file))
    tbl <- tidytable::fread(out_file)
    expect_identical(nrow(tbl), 1L)
    expect_true(all(is.na(tbl$feature_id)))
  }
})

test_that("prepare_annotations_mztab maps pipe-separated SME_ID_REFS, handles fallback IDs, and supports SMF-only tables", {
  tmp_dir <- withr::local_tempdir()
  fx <- .create_mztab_lookup_fixtures(tmp_dir)
  dummy_in <- file.path(tmp_dir, "dummy.mztab")
  writeLines("MTD\tmzTab-version\t2.0.0-M", dummy_in)

  # 1. Pipe-separated SME_ID_REFS + unmapped SME row falling back to evidence_input_id
  local_mocked_bindings(
    read_mztab_tables = function(input) {
      list(
        mtd = tidytable::tidytable(),
        sml = tidytable::tidytable(
          SML_ID = c("10", "20"),
          SME_ID_REFS = c("1|2", "null")
        ),
        smf = tidytable::tidytable(),
        sme = tidytable::tidytable(
          SME_ID = c("1", "2", "3", "4"),
          evidence_input_id = c("E1", "E2", "fallback_feat", "null"),
          identification_method = c(
            "[, , GNPS spectral library matching, ]",
            "[, , exact mass database search, ]",
            "",
            NA_character_
          ),
          `id_confidence_measure[4]` = c("0.92", "0.80", "0.75", "bad"),
          chemical_name = rep("Caffeine", 4L),
          smiles = rep("CN1C=NC2=C1C(=O)N(C(=O)N2C)C", 4L),
          database_identifier = rep("RYYVLZVUVIJVGH", 4L),
          chemical_formula = rep("C8H10N4O2", 4L),
          theoretical_neutral_mass = rep("194.0804", 4L),
          adduct_ion = rep("[M+H]+", 4L),
          rank = c("1", "1", "2", "3")
        )
      )
    },
    validate_mztab_tables = function(tabs, strict = FALSE) invisible(TRUE)
  )

  out_sme <- file.path(tmp_dir, "out_sme.tsv")
  prepare_annotations_mztab(
    input = dummy_in,
    output = out_sme,
    str_stereo = fx$stereo,
    str_met = fx$met,
    str_tax_cla = fx$cla,
    str_tax_npc = fx$npc
  )
  tbl_sme <- tidytable::fread(out_sme)
  expect_true(all(
    c("10", "fallback_feat") %in% as.character(tbl_sme$feature_id)
  ))

  # 2. All SME_ID_REFS null/empty -> length(refs_flat) == 0L branch
  local_mocked_bindings(
    read_mztab_tables = function(input) {
      list(
        mtd = tidytable::tidytable(),
        sml = tidytable::tidytable(
          SML_ID = "10",
          SME_ID_REFS = "null"
        ),
        smf = tidytable::tidytable(),
        sme = tidytable::tidytable(
          SME_ID = "1",
          evidence_input_id = "F_FALLBACK",
          identification_method = "gnps",
          best_id_confidence_value = "0.88",
          smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
          database_identifier = "RYYVLZVUVIJVGH"
        )
      )
    },
    validate_mztab_tables = function(tabs, strict = FALSE) invisible(TRUE)
  )
  out_null_refs <- file.path(tmp_dir, "out_null_refs.tsv")
  prepare_annotations_mztab(
    input = dummy_in,
    output = out_null_refs,
    str_stereo = fx$stereo,
    str_met = fx$met,
    str_tax_cla = fx$cla,
    str_tax_npc = fx$npc
  )
  tbl_null_refs <- tidytable::fread(out_null_refs)
  expect_identical(as.character(tbl_null_refs$feature_id), "F_FALLBACK")

  # 3. SMF-only table when SME and SML are empty (id_col = "SMF_ID")
  local_mocked_bindings(
    read_mztab_tables = function(input) {
      list(
        mtd = tidytable::tidytable(),
        sml = tidytable::tidytable(),
        sme = tidytable::tidytable(),
        smf = tidytable::tidytable(
          SMF_ID = "SMF_42",
          smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
          database_identifier = "RYYVLZVUVIJVGH",
          exp_mass_to_charge = "195.0877",
          adduct_ion = "[M+H]+"
        )
      )
    },
    validate_mztab_tables = function(tabs, strict = FALSE) invisible(TRUE)
  )
  out_smf <- file.path(tmp_dir, "out_smf.tsv")
  prepare_annotations_mztab(
    input = dummy_in,
    output = out_smf,
    str_stereo = fx$stereo,
    str_met = fx$met,
    str_tax_cla = fx$cla,
    str_tax_npc = fx$npc
  )
  tbl_smf <- tidytable::fread(out_smf)
  expect_identical(as.character(tbl_smf$feature_id), "SMF_42")
})
