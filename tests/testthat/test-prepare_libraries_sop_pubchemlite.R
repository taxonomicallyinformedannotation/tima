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
