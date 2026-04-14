# Test Suite: complement_metadata_structures ----

library(testthat)

test_that("complement_metadata_structures handles empty input", {
  result <- complement_metadata_structures(tidytable::tidytable())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("complement_metadata_structures errors on invalid file paths", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "INKX",
    candidate_structure_smiles_no_stereo = "C"
  )
  expect_error(
    complement_metadata_structures(
      df,
      str_stereo = "missing_stereo.tsv",
      str_met = "missing_met.tsv",
      str_tax_cla = "missing_cla.tsv",
      str_tax_npc = "missing_npc.tsv"
    ),
    "Please verify file paths and ensure all required files are present."
  )
})

test_that("complement_metadata_structures validates single character path inputs", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "INK1",
    candidate_structure_smiles_no_stereo = "C"
  )

  # Use fixtures for structure files
  st_file <- temp_test_path("st.tsv")
  met_file <- temp_test_path("met.tsv")
  cla_file <- temp_test_path("cla.tsv")
  npc_file <- temp_test_path("npc.tsv")

  # Copy fixtures to temp location using helper
  copy_fixture_to("structures_stereo.csv", st_file)
  copy_fixture_to("structures_metadata.csv", met_file)
  copy_fixture_to("structures_taxonomy_cla.csv", cla_file)
  copy_fixture_to("structures_taxonomy_npc.csv", npc_file)

  # Pass a vector for one path
  expect_error(
    complement_metadata_structures(
      df,
      str_stereo = c(st_file, st_file),
      str_met = met_file,
      str_tax_cla = cla_file,
      str_tax_npc = npc_file
    ),
    "single character"
  )
})

test_that("complement_metadata_structures collapses multiple names with separator", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "INK1",
    candidate_structure_smiles_no_stereo = "C"
  )

  # Use fixtures for structure files
  st_file <- temp_test_path("st.tsv")
  met_file <- temp_test_path("met.tsv")
  cla_file <- temp_test_path("cla.tsv")
  npc_file <- temp_test_path("npc.tsv")

  # Copy fixtures using helper
  copy_fixture_to("structures_stereo.csv", st_file)
  copy_fixture_to("structures_metadata.csv", met_file)
  copy_fixture_to("structures_taxonomy_cla.csv", cla_file)
  copy_fixture_to("structures_taxonomy_npc.csv", npc_file)

  result <- complement_metadata_structures(
    df,
    str_stereo = st_file,
    str_met = met_file,
    str_tax_cla = cla_file,
    str_tax_npc = npc_file
  )
  expect_true("candidate_structure_name" %in% names(result))
  # INK1 does not match any fixture entry, so name will be NA
})

test_that("complement_metadata_structures errors when required columns missing", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "INK3",
    candidate_structure_smiles_no_stereo = "CCC"
  )

  # Use fixtures for structure files
  st_file <- temp_test_path("st.tsv")
  met_file <- temp_test_path("met.tsv")
  cla_file <- temp_test_path("cla.tsv")
  npc_file <- temp_test_path("npc.tsv")

  # Stereo missing structure_smiles_no_stereo column - create incomplete data
  stereo <- tidytable::tidytable(structure_inchikey_connectivity_layer = "INK3")
  tidytable::fwrite(x = stereo, file = st_file, sep = "\t")

  # Use fixtures for the others
  copy_fixture_to("structures_metadata.csv", met_file)
  copy_fixture_to("structures_taxonomy_cla.csv", cla_file)
  copy_fixture_to("structures_taxonomy_npc.csv", npc_file)

  # Expect error due to missing column usage inside joins/select
  expect_error(
    complement_metadata_structures(
      df,
      str_stereo = st_file,
      str_met = met_file,
      str_tax_cla = cla_file,
      str_tax_npc = npc_file
    )
  )
})

.write_min_lookup_files <- function(
  st_file,
  met_file,
  cla_file,
  npc_file,
  inchikey,
  smiles,
  mass = "100",
  formula = "C10H20O",
  xlogp = "2.1",
  tag = "clean",
  name = "LibName"
) {
  # Full inchikey for taxonomy and bridge lookups
  full_inchikey <- paste0(inchikey, "-AAAAAAAAAA-N")
  inchikey_no_stereo <- paste0(inchikey, "-N")

  # Stereo file: includes all structural identifiers + name, tag, xlogp
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = full_inchikey,
      structure_inchikey_connectivity_layer = inchikey,
      structure_inchikey_no_stereo = inchikey_no_stereo,
      structure_smiles = smiles,
      structure_smiles_no_stereo = smiles,
      structure_xlogp = xlogp,
      structure_name = name,
      structure_tag = tag
    ),
    st_file,
    sep = "\t"
  )

  # Metadata: keyed by inchikey_no_stereo, only formula + mass
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_no_stereo = inchikey_no_stereo,
      structure_exact_mass = mass,
      structure_molecular_formula = formula
    ),
    met_file,
    sep = "\t"
  )

  # ClassyFire taxonomy: keyed by full inchikey
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = full_inchikey,
      structure_tax_cla_chemontid = "CHEMONTID:0000000",
      structure_tax_cla_01kin = "Organic compounds",
      structure_tax_cla_02sup = "Lipids",
      structure_tax_cla_03cla = "Fatty acyls",
      structure_tax_cla_04dirpar = "Fatty acids"
    ),
    cla_file,
    sep = "\t"
  )

  # NPClassifier taxonomy: keyed by structure_smiles (with stereo)
  tidytable::fwrite(
    tidytable::tidytable(
      structure_smiles = smiles,
      structure_tax_npc_01pat = "Pathway",
      structure_tax_npc_02sup = "Superclass",
      structure_tax_npc_03cla = "Class"
    ),
    npc_file,
    sep = "\t"
  )
}

test_that("clean lookup values are always used before existing candidate metadata", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK1"),
    candidate_structure_smiles_no_stereo = c("C", "C"),
    candidate_structure_exact_mass = c("999", "888"),
    candidate_structure_molecular_formula = c("OLD1", "OLD2"),
    candidate_structure_xlogp = c("9.9", "8.8"),
    candidate_structure_tag = c("legacy", "legacy"),
    candidate_structure_name = c("OldName1", "OldName2")
  )

  st_file <- temp_test_path("prio_st.tsv")
  met_file <- temp_test_path("prio_met.tsv")
  cla_file <- temp_test_path("prio_cla.tsv")
  npc_file <- temp_test_path("prio_npc.tsv")

  .write_min_lookup_files(
    st_file = st_file,
    met_file = met_file,
    cla_file = cla_file,
    npc_file = npc_file,
    inchikey = "IK1",
    smiles = "C",
    mass = "100",
    formula = "C10H20O",
    xlogp = "2.1",
    tag = "clean",
    name = "LibName"
  )

  out <- complement_metadata_structures(
    df,
    str_stereo = st_file,
    str_met = met_file,
    str_tax_cla = cla_file,
    str_tax_npc = npc_file
  )

  expect_true(all(out$candidate_structure_exact_mass == "100"))
  expect_true(all(out$candidate_structure_molecular_formula == "C10H20O"))
  ## xlogp is now enriched from stereo reference
  expect_true(all(out$candidate_structure_xlogp == "2.1"))
  expect_true(all(out$candidate_structure_tag == "clean"))
  expect_true(all(out$candidate_structure_name == "LibName"))
})

test_that("existing candidate metadata is used only when no lookup values exist", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "IK2",
    candidate_structure_smiles_no_stereo = "N",
    candidate_structure_exact_mass = "777",
    candidate_structure_molecular_formula = "C7H7N",
    candidate_structure_xlogp = "1.7",
    candidate_structure_tag = "existing",
    candidate_structure_name = "ExistingName"
  )

  st_file <- temp_test_path("fallback_st.tsv")
  met_file <- temp_test_path("fallback_met.tsv")
  cla_file <- temp_test_path("fallback_cla.tsv")
  npc_file <- temp_test_path("fallback_npc.tsv")

  # Only unrelated lookup entries: no library match for IK2/N.
  .write_min_lookup_files(
    st_file = st_file,
    met_file = met_file,
    cla_file = cla_file,
    npc_file = npc_file,
    inchikey = "OTHER",
    smiles = "CC"
  )

  out <- complement_metadata_structures(
    df,
    str_stereo = st_file,
    str_met = met_file,
    str_tax_cla = cla_file,
    str_tax_npc = npc_file
  )

  ## Computable properties (formula, mass) fall back to existing values
  ## when no lookup match is found.
  expect_equal(out$candidate_structure_exact_mass[[1]], "777")
  expect_equal(out$candidate_structure_molecular_formula[[1]], "C7H7N")
  ## Non-computable properties are preserved from existing values via coalesce
  expect_equal(out$candidate_structure_xlogp[[1]], "1.7")
  expect_equal(out$candidate_structure_tag[[1]], "existing")
  expect_equal(out$candidate_structure_name[[1]], "ExistingName")
})

test_that("one canonical value is returned per inchikey across rows", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("IK3", "IK3", "IK3"),
    candidate_structure_smiles_no_stereo = c("O", "O", "O"),
    candidate_structure_exact_mass = c("901", "902", "903"),
    candidate_structure_molecular_formula = c("OLD_A", "OLD_B", "OLD_C"),
    candidate_structure_xlogp = c("0.1", "0.2", "0.3"),
    candidate_structure_tag = c("legacy_a", "legacy_b", "legacy_c"),
    candidate_structure_name = c("OldA", "OldB", "OldC")
  )

  st_file <- temp_test_path("canon_st.tsv")
  met_file <- temp_test_path("canon_met.tsv")
  cla_file <- temp_test_path("canon_cla.tsv")
  npc_file <- temp_test_path("canon_npc.tsv")

  .write_min_lookup_files(
    st_file = st_file,
    met_file = met_file,
    cla_file = cla_file,
    npc_file = npc_file,
    inchikey = "IK3",
    smiles = "O",
    mass = "123.45",
    formula = "C6H6O",
    xlogp = "1.2",
    tag = "clean_canon",
    name = "CanonicalName"
  )

  out <- complement_metadata_structures(
    df,
    str_stereo = st_file,
    str_met = met_file,
    str_tax_cla = cla_file,
    str_tax_npc = npc_file
  )

  expect_equal(length(unique(out$candidate_structure_exact_mass)), 1L)
  expect_equal(length(unique(out$candidate_structure_molecular_formula)), 1L)
  ## xlogp is now enriched from stereo reference; all rows get the same value
  expect_equal(length(unique(out$candidate_structure_xlogp)), 1L)
  expect_equal(length(unique(out$candidate_structure_tag)), 1L)
  expect_equal(length(unique(out$candidate_structure_name)), 1L)

  expect_equal(unique(out$candidate_structure_exact_mass), "123.45")
  expect_equal(unique(out$candidate_structure_molecular_formula), "C6H6O")
  expect_equal(unique(out$candidate_structure_xlogp), "1.2")
  expect_equal(unique(out$candidate_structure_tag), "clean_canon")
  expect_equal(unique(out$candidate_structure_name), "CanonicalName")
})

test_that("name harmonization is case-insensitive within complement_metadata_structures", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = c("IK4", "IK4"),
    candidate_structure_smiles_no_stereo = c("CCO", "CCO"),
    candidate_structure_name = c("SUCROSE", "sucrose")
  )

  st_file <- temp_test_path("name_harm_st.tsv")
  met_file <- temp_test_path("name_harm_met.tsv")
  cla_file <- temp_test_path("name_harm_cla.tsv")
  npc_file <- temp_test_path("name_harm_npc.tsv")

  full_inchikey <- "IK4AAAAAAAAAA-BBBBBBBBBB-N"
  inchikey_no_stereo <- "IK4AAAAAAAAAA-N"

  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = full_inchikey,
      structure_inchikey_connectivity_layer = "IK4",
      structure_inchikey_no_stereo = inchikey_no_stereo,
      structure_smiles = "CCO",
      structure_smiles_no_stereo = "CCO",
      structure_xlogp = "-3.0",
      structure_name = c("SUCROSE", "sucrose"),
      structure_tag = "clean"
    ),
    st_file,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_no_stereo = inchikey_no_stereo,
      structure_exact_mass = "180.16",
      structure_molecular_formula = "C12H22O11"
    ),
    met_file,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = full_inchikey,
      structure_tax_cla_chemontid = "CHEMONTID:0000000",
      structure_tax_cla_01kin = "Organic compounds",
      structure_tax_cla_02sup = "Carbohydrates",
      structure_tax_cla_03cla = "Oligosaccharides",
      structure_tax_cla_04dirpar = "Disaccharides"
    ),
    cla_file,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_smiles = "CCO",
      structure_tax_npc_01pat = "Pathway",
      structure_tax_npc_02sup = "Superclass",
      structure_tax_npc_03cla = "Class"
    ),
    npc_file,
    sep = "\t"
  )

  out <- complement_metadata_structures(
    df,
    str_stereo = st_file,
    str_met = met_file,
    str_tax_cla = cla_file,
    str_tax_npc = npc_file
  )

  expect_equal(length(unique(out$candidate_structure_name)), 1L)
  expect_equal(unique(out$candidate_structure_name), "SUCROSE")
})

test_that("metadata and names keyed by structure_inchikey are mapped via stereo", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "IK5AAAAABBBBBB",
    candidate_structure_smiles_no_stereo = "CNO"
  )

  st_file <- temp_test_path("inchikey_only_st.tsv")
  met_file <- temp_test_path("inchikey_only_met.tsv")
  cla_file <- temp_test_path("inchikey_only_cla.tsv")
  npc_file <- temp_test_path("inchikey_only_npc.tsv")

  full_inchikey <- "IK5AAAAABBBBBB-CCCCCCCCCC-N"
  inchikey_no_stereo <- "IK5AAAAABBBBBB-N"

  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = full_inchikey,
      structure_inchikey_connectivity_layer = "IK5AAAAABBBBBB",
      structure_inchikey_no_stereo = inchikey_no_stereo,
      structure_smiles = "C(=O)NO",
      structure_smiles_no_stereo = "CNO",
      structure_xlogp = "1.5",
      structure_name = "MappedByInchikey",
      structure_tag = "clean_lookup"
    ),
    st_file,
    sep = "\t"
  )

  # Metadata keyed by inchikey_no_stereo
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_no_stereo = inchikey_no_stereo,
      structure_exact_mass = "250.12",
      structure_molecular_formula = "C10H18N2O5"
    ),
    met_file,
    sep = "\t"
  )

  # ClassyFire keyed by full inchikey
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = full_inchikey,
      structure_tax_cla_chemontid = "CHEMONTID:0000000",
      structure_tax_cla_01kin = "Organic compounds",
      structure_tax_cla_02sup = "Alkaloids",
      structure_tax_cla_03cla = "Amines",
      structure_tax_cla_04dirpar = "Primary amines"
    ),
    cla_file,
    sep = "\t"
  )

  # NPClassifier keyed by structure_smiles (with stereo)
  tidytable::fwrite(
    tidytable::tidytable(
      structure_smiles = "C(=O)NO",
      structure_tax_npc_01pat = "Pathway",
      structure_tax_npc_02sup = "Superclass",
      structure_tax_npc_03cla = "Class"
    ),
    npc_file,
    sep = "\t"
  )

  out <- complement_metadata_structures(
    df,
    str_stereo = st_file,
    str_met = met_file,
    str_tax_cla = cla_file,
    str_tax_npc = npc_file
  )

  expect_equal(out$candidate_structure_exact_mass[[1]], "250.12")
  expect_equal(out$candidate_structure_molecular_formula[[1]], "C10H18N2O5")
  ## xlogp is now enriched from stereo reference
  expect_equal(out$candidate_structure_xlogp[[1]], "1.5")
  expect_equal(out$candidate_structure_tag[[1]], "clean_lookup")
  expect_equal(out$candidate_structure_name[[1]], "MappedByInchikey")
})

test_that("complement_metadata_structures keeps existing taxonomy when lookup is placeholder", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "IK_PLACEHOLDER",
    candidate_structure_smiles_no_stereo = "CCO",
    candidate_structure_tax_cla_02sup = "ExistingSuperclass",
    candidate_structure_tax_npc_01pat = "ExistingPathway"
  )

  st_file <- temp_test_path("st_placeholder.tsv")
  met_file <- temp_test_path("met_placeholder.tsv")
  cla_file <- temp_test_path("cla_placeholder.tsv")
  npc_file <- temp_test_path("npc_placeholder.tsv")

  full_inchikey <- "IK_PLACEHOLDER-AAAAAAAAAA-N"
  inchikey_no_stereo <- "IK_PLACEHOLDER-N"

  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = full_inchikey,
      structure_inchikey_connectivity_layer = "IK_PLACEHOLDER",
      structure_inchikey_no_stereo = inchikey_no_stereo,
      structure_smiles = "CCO",
      structure_smiles_no_stereo = "CCO",
      structure_xlogp = NA_character_,
      structure_name = "",
      structure_tag = ""
    ),
    st_file,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_no_stereo = inchikey_no_stereo,
      structure_molecular_formula = "C2H6O",
      structure_exact_mass = "46.0419"
    ),
    met_file,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = full_inchikey,
      structure_tax_cla_chemontid = "CHEMONTID:0000000",
      structure_tax_cla_01kin = "Organic compounds",
      structure_tax_cla_02sup = "notClassified",
      structure_tax_cla_03cla = "empty",
      structure_tax_cla_04dirpar = ""
    ),
    cla_file,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      structure_smiles = "CCO",
      structure_tax_npc_01pat = "notClassified",
      structure_tax_npc_02sup = "empty",
      structure_tax_npc_03cla = ""
    ),
    npc_file,
    sep = "\t"
  )

  out <- complement_metadata_structures(
    df,
    str_stereo = st_file,
    str_met = met_file,
    str_tax_cla = cla_file,
    str_tax_npc = npc_file
  )

  expect_equal(
    out$candidate_structure_tax_cla_02sup[[1L]],
    "ExistingSuperclass"
  )
  expect_equal(out$candidate_structure_tax_npc_01pat[[1L]], "ExistingPathway")
})

test_that("complement_metadata_structures normalizes empty existing values before fallback", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "IK_EMPTY",
    candidate_structure_smiles_no_stereo = "CCN",
    candidate_structure_exact_mass = "",
    candidate_structure_molecular_formula = "",
    candidate_structure_xlogp = "",
    candidate_structure_tag = "",
    candidate_structure_name = ""
  )

  st_file <- temp_test_path("st_empty_existing.tsv")
  met_file <- temp_test_path("met_empty_existing.tsv")
  cla_file <- temp_test_path("cla_empty_existing.tsv")
  npc_file <- temp_test_path("npc_empty_existing.tsv")

  .write_min_lookup_files(
    st_file = st_file,
    met_file = met_file,
    cla_file = cla_file,
    npc_file = npc_file,
    inchikey = "IK_EMPTY",
    smiles = "CCN",
    mass = "45.08",
    formula = "C2H7N",
    xlogp = "-0.4",
    tag = "lookup_tag",
    name = "ethylamine"
  )

  out <- complement_metadata_structures(
    df,
    str_stereo = st_file,
    str_met = met_file,
    str_tax_cla = cla_file,
    str_tax_npc = npc_file
  )

  expect_equal(out$candidate_structure_exact_mass[[1L]], "45.08")
  expect_equal(out$candidate_structure_molecular_formula[[1L]], "C2H7N")
  ## xlogp is now enriched from stereo reference
  expect_equal(out$candidate_structure_xlogp[[1L]], "-0.4")
  expect_equal(out$candidate_structure_tag[[1L]], "lookup_tag")
  expect_equal(out$candidate_structure_name[[1L]], "ethylamine")
})

test_that("complement_metadata_structures converts placeholder existing values to NA when no lookup exists", {
  df <- tidytable::tidytable(
    candidate_structure_inchikey_connectivity_layer = "IK_NOLOOKUP",
    candidate_structure_smiles_no_stereo = "CCC",
    candidate_structure_exact_mass = "",
    candidate_structure_molecular_formula = "null",
    candidate_structure_xlogp = "NA",
    candidate_structure_tag = "empty",
    candidate_structure_name = "notClassified"
  )

  st_file <- temp_test_path("st_no_lookup.tsv")
  met_file <- temp_test_path("met_no_lookup.tsv")
  cla_file <- temp_test_path("cla_no_lookup.tsv")
  npc_file <- temp_test_path("npc_no_lookup.tsv")

  # Write unrelated lookup records so no enrichment row matches IK_NOLOOKUP/CCC.
  .write_min_lookup_files(
    st_file = st_file,
    met_file = met_file,
    cla_file = cla_file,
    npc_file = npc_file,
    inchikey = "OTHER",
    smiles = "CO"
  )

  out <- complement_metadata_structures(
    df,
    str_stereo = st_file,
    str_met = met_file,
    str_tax_cla = cla_file,
    str_tax_npc = npc_file
  )

  expect_true(is.na(out$candidate_structure_exact_mass[[1L]]))
  expect_true(is.na(out$candidate_structure_molecular_formula[[1L]]))
  ## xlogp: the literal "NA" string is normalized to NA by .normalize_enrichment_text
  ## when no enriched value exists, the existing value goes through .pick_enrichment
  ## which normalizes "NA" to NA_character_
  expect_true(is.na(out$candidate_structure_xlogp[[1L]]))
  expect_true(is.na(out$candidate_structure_tag[[1L]]))
  expect_true(is.na(out$candidate_structure_name[[1L]]))
})
