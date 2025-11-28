# Helper fixtures for weight_annotations tests ----

wa_create_minimal_files <- function(root = NULL) {
  # Use temp directory if root not provided
  if (is.null(root)) {
    root <- get_test_root()
  }

  dir.create(
    file.path(root, "data/interim/annotations"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(root, "data/interim/features"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(root, "data/interim/libraries/sop/merged"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(root, "data/interim/metadata"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  files <- list(
    library = file.path(root, "data/interim/libraries/sop/merged/keys.tsv"),
    components = file.path(root, "data/interim/features/components.tsv"),
    edges = file.path(root, "data/interim/features/edges.tsv"),
    taxa = file.path(root, "data/interim/metadata/taxa.tsv"),
    ann = file.path(root, "data/interim/annotations/ann.tsv"),
    ann2 = file.path(root, "data/interim/annotations/ann2.tsv")
  )
  lapply(files, function(p) writeLines("", p))
  files
}

wa_min_annotation_table <- function() {
  tidytable::tidytable(
    feature_id = c("F1", "F1", "F2"),
    feature_mz = c("100", "100", "150"),
    feature_rt = c("1.0", "1.0", "2.0"),
    candidate_library = c("libA", "libA", "libB"),
    candidate_spectrum_id = c("S1", "S2", "S3"),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+H]+"),
    candidate_count_similarity_peaks_matched = c("10", "8", "12"),
    candidate_score_similarity = c("0.9", "0.85", "0.7"),
    candidate_structure_name = c("Cmpd1", "Cmpd1", "Cmpd2"),
    candidate_structure_exact_mass = c("100.0", "100.0", "150.0"),
    candidate_structure_molecular_formula = c("C5H12", "C5H12", "C7H14"),
    candidate_structure_xlogp = c("1.0", "1.0", "2.0"),
    candidate_structure_inchikey_connectivity_layer = c(
      "AAAAAAA",
      "AAAAAAA",
      "BBBBBBB"
    ),
    candidate_structure_smiles_no_stereo = c("CCCCC", "CCCCC", "CCCCCCC"),
    candidate_structure_tax_cla_chemontid = NA,
    candidate_structure_tax_cla_01kin = NA,
    candidate_structure_tax_cla_02sup = NA,
    candidate_structure_tax_cla_03cla = NA,
    candidate_structure_tax_cla_04dirpar = NA,
    candidate_structure_tax_npc_01pat = NA,
    candidate_structure_tax_npc_02sup = NA,
    candidate_structure_tax_npc_03cla = NA,
    candidate_structure_organism_occurrence_closest = NA,
    candidate_structure_organism_occurrence_reference = NA,
    candidate_structure_error_mz = c("0.1", "0.2", "0.3"),
    candidate_structure_error_rt = c("0.05", "0.04", "0.03"),
    candidate_score_sirius_csi = c("0.4", NA, "0.6"),
    candidate_score_sirius_confidence = c("0.5", NA, "0.7"),
    candidate_score_sirius_msnovelist = c("0.2", NA, "0.3"),
    candidate_spectrum_entropy = c("1.2", "1.1", "1.5")
  )
}

wa_min_formula_table <- function() {
  tidytable::tidytable(
    feature_id = c("F1", "F2"),
    candidate_structure_molecular_formula = c("C5H12", "C7H14"),
    candidate_score_sirius_zodiac = c("0.8", "0.6")
  )
}

wa_min_canopus_table <- function() {
  tidytable::tidytable(
    feature_id = c("F1", "F2"),
    feature_pred_tax_npc_01pat_val = c("PathA", "PathB"),
    feature_pred_tax_npc_01pat_score = c("0.9", "0.7")
  )
}

wa_min_edges_table <- function() {
  tidytable::tidytable(
    feature_source = c("F1", "F2"),
    feature_target = c("F3", "F4"),
    candidate_score_similarity = c("0.9", "0.8"),
    feature_spectrum_entropy = c("1.3", "1.4"),
    feature_spectrum_peaks = c("150", "180")
  )
}

wa_stub_get_default_paths <- function(tmp) {
  list(
    data = list(
      processed = list(path = file.path("data/processed"))
    )
  )
}
