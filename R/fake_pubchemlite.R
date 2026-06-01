#' @title Fake PubChem Lite
#'
#' @description Create a minimal placeholder PubChem Lite CSV when the download
#'     fails, so downstream SOP preparation still receives a valid header-only
#'     file.
#'
#' @param export Character string path where the fake CSV should be written
#'
#' @return Character string path to the created fake file
#'
#' @keywords internal
fake_pubchemlite <- function(export) {
  if (missing(export) || !is.character(export) || length(export) != 1L) {
    cli::cli_abort(
      "export path must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  log_warn("PubChem Lite download failed. Creating empty placeholder CSV.")

  dir.create(dirname(export), recursive = TRUE, showWarnings = FALSE)

  header <- paste(
    c(
      "Identifier",
      "FirstBlock",
      "PubMed_Count",
      "Patent_Count",
      "Related_CIDs",
      "Synonym",
      "MolecularFormula",
      "SMILES",
      "InChI",
      "InChIKey",
      "MonoisotopicMass",
      "XLogP",
      "CompoundName",
      "AnnoTypeCount",
      "AgroChemInfo",
      "BioPathway",
      "DrugMedicInfo",
      "FoodRelated",
      "PharmacoInfo",
      "SafetyInfo",
      "ToxicityInfo",
      "KnownUse",
      "DisorderDisease",
      "Identification",
      "ChemClass",
      "pred_CCS_A2_[M+H]+",
      "pred_CCS_A2_[M+Na]+",
      "pred_CCS_A2_[M-H]-",
      "pred_CCS_A2_[M+NH4]+",
      "pred_CCS_A2_[M+K]+",
      "pred_CCS_A2_[M+H-H2O]+",
      "pred_CCS_A2_[M+HCOO]-",
      "pred_CCS_A2_[M+CH3COO]-",
      "pred_CCS_A2_[M+Na-2H]-",
      "pred_CCS_A2_[M]+",
      "pred_CCS_A2_[M]-"
    ),
    collapse = ","
  )

  writeLines(header, con = export, sep = "\n")
  export
}
