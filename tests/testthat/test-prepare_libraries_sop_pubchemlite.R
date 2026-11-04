# Test Suite: prepare_libraries_sop_pubchemlite ----

library(testthat)

test_that("fake_pubchemlite writes a valid header-only CSV", {
  temp_file <- tempfile(fileext = ".csv")

  result <- getFromNamespace("fake_pubchemlite", "tima")(export = temp_file)

  expect_equal(result, temp_file)
  expect_true(file.exists(temp_file))

  header <- readLines(temp_file, n = 1L)
  expect_match(header, "Identifier")
  expect_match(header, "CompoundName")
  expect_match(header, "pred_CCS_A2_\\[M\\+H\\]\\+")
})

test_that("prepare_libraries_sop_pubchemlite standardizes PubChem Lite rows", {
  input_file <- tempfile(fileext = ".csv")
  output_file <- tempfile(fileext = ".tsv.gz")

  utils::write.csv(
    data.frame(
      Identifier = c("3", "4"),
      FirstBlock = c("INCSWYKICIYAHB", "HXKKHQJGJAFBHI"),
      PubMed_Count = c("11", "77"),
      Patent_Count = c("199", "114242"),
      Related_CIDs = c("9964159 23615184", "4 111033"),
      Synonym = c("foo", "bar"),
      MolecularFormula = c("C7H8O4", "C3H9NO"),
      SMILES = c("C1=CC(C(C(=C1)C(=O)O)O)O", "CC(CN)O"),
      InChI = c("InChI=1S/C7H8O4", "InChI=1S/C3H9NO"),
      InChIKey = c(
        "INCSWYKICIYAHB-UHFFFAOYSA-N",
        "HXKKHQJGJAFBHI-UHFFFAOYSA-N"
      ),
      MonoisotopicMass = c("156.04225873", "75.068413911"),
      XLogP = c("-0.3", "-1.0"),
      CompoundName = c(
        "(5S,6S)-5,6-dihydroxycyclohexa-1,3-diene-1-carboxylic acid",
        "1-aminopropan-2-ol"
      ),
      AnnoTypeCount = c("2", "8"),
      AgroChemInfo = c("0", "0"),
      BioPathway = c("1", "1"),
      DrugMedicInfo = c("0", "0"),
      FoodRelated = c("0", "5"),
      PharmacoInfo = c("2", "2"),
      SafetyInfo = c("0", "12"),
      ToxicityInfo = c("0", "3"),
      KnownUse = c("0", "4"),
      DisorderDisease = c("0", "0"),
      Identification = c("0", "2"),
      ChemClass = c("0", "5"),
      `pred_CCS_A2_[M+H]+` = c("128.6", "114.0"),
      `pred_CCS_A2_[M+Na]+` = c("136.2", "120.9"),
      `pred_CCS_A2_[M-H]-` = c("128.7", "112.8"),
      `pred_CCS_A2_[M+NH4]+` = c("147.6", "137.0"),
      `pred_CCS_A2_[M+K]+` = c("134.3", "121.1"),
      `pred_CCS_A2_[M+H-H2O]+` = c("124.0", "109.9"),
      `pred_CCS_A2_[M+HCOO]-` = c("147.9", "136.8"),
      `pred_CCS_A2_[M+CH3COO]-` = c("168.5", "162.3"),
      `pred_CCS_A2_[M+Na-2H]-` = c("132.7", "119.9"),
      `pred_CCS_A2_[M]+` = c("125.9", "111.1"),
      `pred_CCS_A2_[M]-` = c("125.9", "111.1"),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    input_file,
    row.names = FALSE,
    quote = TRUE
  )

  result <- getFromNamespace("prepare_libraries_sop_pubchemlite", "tima")(
    input = input_file,
    output = output_file
  )

  expect_equal(result, output_file)
  expect_true(file.exists(output_file))

  out <- tidytable::fread(output_file)
  expect_equal(nrow(out), 2L)
  expect_true(all(
    c("structure_name", "structure_inchikey", "structure_smiles") %in%
      names(out)
  ))
  expect_true(all(out$structure_tag == "xenobiotic"))
  expect_true(all(out$organism_name == "xenobiotic"))
  expect_true(all(out$organism_taxonomy_ottid == "93302"))
  expect_true(is.numeric(out$structure_exact_mass))
  expect_true(is.numeric(out$structure_xlogp))
})

test_that("prepare_libraries_sop_pubchemlite deduplicates identical structures and normalizes empty strings", {
  input_file <- tempfile(fileext = ".csv")
  output_file <- tempfile(fileext = ".tsv.gz")
  on.exit(unlink(c(input_file, output_file)), add = TRUE)

  utils::write.csv(
    data.frame(
      Identifier = c("1", "2", "3"),
      CompoundName = c("CompA", "CompA", ""),
      InChIKey = c(
        "INCSWYKICIYAHB-UHFFFAOYSA-N",
        "INCSWYKICIYAHB-UHFFFAOYSA-N",
        "HXKKHQJGJAFBHI-UHFFFAOYSA-N"
      ),
      SMILES = c("C1=CC=CC=C1", "C1=CC=CC=C1", "CC(CN)O"),
      MolecularFormula = c("C6H6", "C6H6", "C3H9NO"),
      MonoisotopicMass = c("78.046950192", "78.046950192", "75.068413911"),
      XLogP = c("2.1345678", "2.1345678", ""),
      stringsAsFactors = FALSE
    ),
    input_file,
    row.names = FALSE
  )

  prepare_libraries_sop_pubchemlite(input = input_file, output = output_file)
  out <- tidytable::fread(output_file, na.strings = c("", "NA"))

  expect_equal(nrow(out), 2L)
  expect_equal(out$structure_name, c("CompA", NA_character_))
  expect_equal(out$structure_exact_mass, c(78.04695, 75.06841))
  expect_equal(out$structure_xlogp, c(2.13457, NA_real_))
})

test_that("prepare_libraries_sop_pubchemlite validates arguments, handles missing/empty inputs, missing columns, and cache hit", {
  expect_error(
    prepare_libraries_sop_pubchemlite(input = 123, output = "out.tsv"),
    class = "tima_validation_error"
  )
  expect_error(
    prepare_libraries_sop_pubchemlite(
      input = c("a.csv", "b.csv"),
      output = "out.tsv"
    ),
    class = "tima_validation_error"
  )
  expect_error(
    prepare_libraries_sop_pubchemlite(input = "in.csv", output = NULL),
    class = "tima_validation_error"
  )
  expect_error(
    prepare_libraries_sop_pubchemlite(
      input = "in.csv",
      output = c("o1.tsv", "o2.tsv")
    ),
    class = "tima_validation_error"
  )

  tmp_dir <- withr::local_tempdir()

  # 1. Missing input file exports fake_sop_columns()
  out_missing <- file.path(tmp_dir, "missing_out.tsv")
  res_missing <- prepare_libraries_sop_pubchemlite(
    input = file.path(tmp_dir, "does_not_exist.csv"),
    output = out_missing
  )
  expect_identical(res_missing, out_missing)
  expect_true(file.exists(out_missing))
  tbl_missing <- tidytable::fread(out_missing)
  expect_identical(nrow(tbl_missing), 1L)
  expect_true(all(is.na(tbl_missing$structure_inchikey)))

  # 2. Header-only (0-row) CSV exports fake_sop_columns()
  in_empty <- file.path(tmp_dir, "header_only.csv")
  fake_pubchemlite(export = in_empty)
  out_empty <- file.path(tmp_dir, "empty_out.tsv")
  res_empty <- prepare_libraries_sop_pubchemlite(
    input = in_empty,
    output = out_empty
  )
  expect_identical(res_empty, out_empty)
  tbl_empty <- tidytable::fread(out_empty)
  expect_identical(nrow(tbl_empty), 1L)
  expect_true(all(is.na(tbl_empty$structure_inchikey)))

  # 3. CSV missing an optional column (e.g., XLogP) fills NA via clean_col(NULL)
  in_no_xlogp <- file.path(tmp_dir, "no_xlogp.csv")
  utils::write.csv(
    data.frame(
      CompoundName = "Caffeine",
      InChIKey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
      SMILES = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
      MolecularFormula = "C8H10N4O2",
      MonoisotopicMass = "194.080376",
      stringsAsFactors = FALSE
    ),
    in_no_xlogp,
    row.names = FALSE
  )
  out_no_xlogp <- file.path(tmp_dir, "no_xlogp_out.tsv")
  prepare_libraries_sop_pubchemlite(
    input = in_no_xlogp,
    output = out_no_xlogp
  )
  tbl_no_xlogp <- tidytable::fread(out_no_xlogp)
  expect_identical(nrow(tbl_no_xlogp), 1L)
  expect_true(is.na(tbl_no_xlogp$structure_xlogp))

  # 4. Existing output file < 100,000 bytes is overwritten
  out_small <- file.path(tmp_dir, "small_out.tsv")
  writeLines(paste(rep("s", 50L), collapse = ""), out_small)
  prepare_libraries_sop_pubchemlite(
    input = in_no_xlogp,
    output = out_small
  )
  tbl_small <- tidytable::fread(out_small)
  expect_identical(nrow(tbl_small), 1L)
  expect_identical(tbl_small$structure_name, "Caffeine")

  # 5. Existing output file >= 100,000 bytes is reused without overwriting
  out_cached <- file.path(tmp_dir, "cached_out.tsv")
  big_content <- paste(rep("x", 100005L), collapse = "")
  writeLines(big_content, out_cached)
  size_before <- file.size(out_cached)
  res_cached <- prepare_libraries_sop_pubchemlite(
    input = in_no_xlogp,
    output = out_cached
  )
  expect_identical(res_cached, out_cached)
  expect_identical(file.size(out_cached), size_before)
})
