# Helper fixtures for clean_chemo tests ----

# Build a minimal annotated table for clean_chemo pipeline
cc_min_annot_table <- function() {
  tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F3"),
    candidate_structure_inchikey_connectivity_layer = c(
      "AAAA",
      "AAAA",
      "BBBB",
      NA
    ),
    candidate_structure_smiles_no_stereo = c("C", "C", "CC", NA),
    candidate_structure_name = c("CmpdA", "CmpdA", "CmpdB", NA),
    candidate_library = c("lib", "lib", "lib", "lib"),
    candidate_structure_error_mz = c("0.01", "0.02", "0.03", "0.04"),
    candidate_structure_error_rt = c("0.1", "0.1", "0.2", "0.3"),
    candidate_score_pseudo_initial = c("0.95", "0.90", "0.85", "0.50"),
    score_weighted_chemo = c("0.90", "0.85", "0.70", "0.40"),
    score_biological = c("0.60", "0.55", "0.30", "0.80"),
    score_chemical = c("0.65", "0.45", "0.35", "0.70"),
    candidate_score_similarity = c("0.80", NA, NA, NA),
    candidate_score_sirius_csi = c(NA, "0.70", NA, NA),
    feature_pred_tax_cla_01kin_val = c("K1", "K1", "K2", "K3"),
    feature_pred_tax_cla_01kin_score = c("0.2", "0.2", "0.3", "0.4"),
    feature_pred_tax_cla_02sup_val = c("S1", "S1", "S2", "S3"),
    feature_pred_tax_cla_02sup_score = c("0.3", "0.3", "0.4", "0.2"),
    feature_pred_tax_cla_03cla_val = c("C1", "C1", "C2", "C3"),
    feature_pred_tax_cla_03cla_score = c("0.4", "0.4", "0.2", "0.1"),
    feature_pred_tax_cla_04dirpar_val = c("P1", "P1", "P2", "P3"),
    feature_pred_tax_cla_04dirpar_score = c("0.5", "0.5", "0.1", "0.05"),
    feature_pred_tax_npc_01pat_val = c("NP1", "NP1", "NP2", "NP3"),
    feature_pred_tax_npc_01pat_score = c("0.6", "0.6", "0.3", "0.2"),
    feature_pred_tax_npc_02sup_val = c("NS1", "NS1", "NS2", "NS3"),
    feature_pred_tax_npc_02sup_score = c("0.7", "0.7", "0.2", "0.1"),
    feature_pred_tax_npc_03cla_val = c("NC1", "NC1", "NC2", "NC3"),
    feature_pred_tax_npc_03cla_score = c("0.8", "0.8", "0.1", "0.05")
  )
}

cc_features_table <- function() {
  tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    rt = c("1.0", "2.0", "3.0"),
    mz = c("100", "150", "200")
  )
}
cc_components_table <- function() {
  tidytable::tidytable(
    feature_id = c("F1", "F2", "F3"),
    component_id = c("C1", "C1", "C2")
  )
}
cc_sop_table <- function() {
  tidytable::tidytable(
    structure_inchikey_connectivity_layer = c("AAAA", "BBBB"),
    organism_name = c("Org1", "Org2")
  )
}

# Build predicted taxonomy DF directly for compute_* tests
cc_pred_tax_table <- function() {
  tidytable::tidytable(
    feature_id = c("F1", "F2"),
    feature_pred_tax_cla_01kin_val = c("K1", "K2"),
    feature_pred_tax_cla_01kin_score = c("0.1", "0.3"),
    feature_pred_tax_cla_02sup_val = c("S1", "S2"),
    feature_pred_tax_cla_02sup_score = c("0.2", "0.4"),
    feature_pred_tax_cla_03cla_val = c("C1", "C2"),
    feature_pred_tax_cla_03cla_score = c("0.5", "0.2"),
    feature_pred_tax_cla_04dirpar_val = c("P1", "P2"),
    feature_pred_tax_cla_04dirpar_score = c("0.6", "0.1"),
    feature_pred_tax_npc_01pat_val = c("NP1", "NP2"),
    feature_pred_tax_npc_01pat_score = c("0.3", "0.2"),
    feature_pred_tax_npc_02sup_val = c("NS1", "NS2"),
    feature_pred_tax_npc_02sup_score = c("0.4", "0.1"),
    feature_pred_tax_npc_03cla_val = c("NC1", "NC2"),
    feature_pred_tax_npc_03cla_score = c("0.5", "0.05")
  )
}

cc_weights_list <- function() {
  list(
    w_cla_kin = 0.1,
    w_cla_sup = 0.2,
    w_cla_cla = 0.3,
    w_cla_par = 0.4,
    w_npc_pat = 0.2,
    w_npc_sup = 0.3,
    w_npc_cla = 0.5
  )
}
