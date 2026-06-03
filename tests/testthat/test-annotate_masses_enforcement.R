# Test Suite: annotate_masses_enforcement ----

library(testthat)

.test_structure_table <- function() {
  tidytable::tidytable(
    structure_name = c("Compound A", "Compound B"),
    structure_inchikey_connectivity_layer = c("AAAA", "BBBB"),
    structure_inchikey_no_stereo = c("AAAAAA", "BBBBBB"),
    structure_smiles_no_stereo = c("CCO", "CCN"),
    structure_molecular_formula = c("C2H6O", "C2H7N"),
    structure_exact_mass = c(46.041864812, 45.057849223),
    structure_xlogp = c(-0.3, -0.2),
    structure_tax_npc_01pat = c("PathA", "PathB"),
    structure_tax_cla_chemontid = c("CHEMONTID:1", "CHEMONTID:2")
  )
}

test_that("enrich_with_structure_metadata handles empty and non-empty inputs", {
  empty <- enrich_with_structure_metadata(
    combined = tidytable::tidytable(),
    structure_table = .test_structure_table()
  )
  expect_s3_class(empty, "data.frame")
  expect_equal(nrow(empty), 0L)
  expect_true(all(
    c(
      "feature_id",
      "candidate_structure_error_mz",
      "candidate_structure_name",
      "candidate_structure_inchikey_connectivity_layer",
      "candidate_structure_smiles_no_stereo",
      "candidate_library",
      "candidate_adduct"
    ) %in%
      names(empty)
  ))

  combined <- tidytable::tidytable(
    feature_id = "F1",
    error_mz = 0.01,
    structure_name = "Compound A",
    structure_inchikey_connectivity_layer = "AAAA",
    structure_inchikey_no_stereo = "AAAAAA",
    structure_smiles_no_stereo = "CCO",
    structure_molecular_formula = "C2H6O",
    structure_exact_mass = 46.041864812,
    structure_xlogp = "-0.3",
    candidate_library = "lib1",
    candidate_adduct = "[M+H]+",
    structure_tax_npc_01pat = "PathA",
    structure_tax_cla_chemontid = "CHEMONTID:1"
  )

  enriched <- enrich_with_structure_metadata(
    combined = combined,
    structure_table = .test_structure_table()
  )

  expect_equal(nrow(enriched), 1L)
  expect_identical(enriched$candidate_structure_name[[1L]], "Compound A")
  expect_identical(enriched$candidate_library[[1L]], "TIMA MS1")
  expect_null(enriched$candidate_structure_exact_mass)
})

test_that("loss atom requirement helpers parse formulas and adducts", {
  expect_identical(loss_term_atom_requirements(NULL), integer())
  expect_identical(loss_term_atom_requirements(NA_character_), integer())
  expect_identical(loss_term_atom_requirements(""), integer())
  expect_identical(loss_term_atom_requirements("H2O"), c(H = 2L, O = 1L))
  expect_identical(
    loss_term_atom_requirements("2C6H10O5 (hexose)"),
    c(C = 12L, H = 20L, O = 10L)
  )

  expect_identical(adduct_loss_atom_requirements(NULL), integer())
  expect_identical(adduct_loss_atom_requirements("[M+H]+"), integer())
  expect_identical(adduct_loss_atom_requirements("[M-H2O]+"), c(H = 2L, O = 1L))

  expect_true(formula_satisfies_loss_requirements("C2H6O", c(H = 2L, O = 1L)))
  expect_false(formula_satisfies_loss_requirements("C2H4", c(H = 2L, O = 1L)))
  expect_true(is.na(formula_satisfies_loss_requirements(
    NA_character_,
    c(H = 1L)
  )))
})

test_that("enforce_loss_formula_compatibility demotes incompatible structural matches", {
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    adduct = c("[M-H2O]+", "[M+H]+"),
    candidate_structure_molecular_formula = c("C2H6", "C2H6O"),
    candidate_structure_error_mz = c(0.01, 0.02),
    source = c("loss", "pair"),
    loss_term = c("H2O", NA_character_),
    candidate_library = c("lib1", "lib1"),
    candidate_structure_name = c("Compound A", "Compound B")
  )

  out <- enforce_loss_formula_compatibility(annotations)
  expect_identical(out$candidate_structure_name[[1L]], NA_character_)
  expect_identical(out$candidate_structure_name[[2L]], "Compound B")

  untouched <- enforce_loss_formula_compatibility(
    tidytable::tidytable(feature_id = "F1", adduct = "[M+H]+")
  )
  expect_equal(nrow(untouched), 1L)
})

test_that("enforce_annotation_edge_adduct_agreement keeps supported and baseline annotations", {
  annotations <- tidytable::tidytable(
    feature_id = c("F1", "F1", "F2", "F3"),
    adduct = c("[M+H]+", "[M+Na]+", "[M+H]+", "[M+H]+"),
    source = c("pair", "pair", "baseline", "preassigned"),
    candidate_adduct_origin = c("", "", "baseline", "preassigned"),
    is_preassigned = c(FALSE, FALSE, FALSE, TRUE)
  )
  adduct_edges <- tidytable::tidytable(
    feature_id = c("F1"),
    adduct = c("[M+H]+"),
    feature_id_dest = c("F1"),
    adduct_dest = c("[M+Na]+")
  )

  out <- enforce_annotation_edge_adduct_agreement(
    annotations = annotations,
    adduct_edges = adduct_edges,
    baseline_adduct = "[M+H]+"
  )

  expect_true(any(out$feature_id == "F1" & out$adduct == "[M+H]+"))
  expect_true(any(out$feature_id == "F1" & out$adduct == "[M+Na]+"))
  expect_true(any(out$feature_id == "F2" & out$adduct == "[M+H]+"))
  expect_true(any(out$feature_id == "F3" & out$adduct == "[M+H]+"))
})

test_that("build_output_edges combines edge sets and preserves isolated features", {
  adduct_edges <- tidytable::tidytable(
    feature_id = "A",
    adduct = "[M+H]+",
    feature_id_dest = "B",
    adduct_dest = "[M+Na]+"
  )
  loss_edges <- tidytable::tidytable(
    feature_id = "B",
    loss = "H2O",
    feature_id_dest = "C"
  )
  cluster_edges <- tidytable::tidytable(
    feature_id = "C",
    cluster = "1",
    feature_id_dest = "D"
  )
  features_table <- tidytable::tidytable(
    feature_id = c("A", "B", "C", "D", "E")
  )

  edges <- build_output_edges(
    adduct_edges = adduct_edges,
    loss_edges = loss_edges,
    cluster_edges = cluster_edges,
    features_table = features_table,
    name_source = "CLUSTERID1",
    name_target = "CLUSTERID2"
  )

  expect_true(all(c("CLUSTERID1", "CLUSTERID2", "label") %in% names(edges)))
  expect_true(any(edges$CLUSTERID1 == "A" & edges$CLUSTERID2 == "B"))
  expect_true(any(edges$CLUSTERID1 == "C" & edges$CLUSTERID2 == "B"))
  expect_true(any(edges$CLUSTERID1 == "C" & edges$CLUSTERID2 == "D"))
  expect_true(any(edges$CLUSTERID1 == "E" & edges$CLUSTERID2 == "E"))
})

test_that("log_consistency_audit is safe for null and populated audits", {
  expect_no_error(log_consistency_audit(NULL))
  expect_no_error(log_consistency_audit(list(n_kept = 1L, n_dropped = 2L)))
})

test_that("propagate_annotations_across_m_cliques only propagates to adduct-compatible targets", {
  annotations <- tidytable::tidytable(
    feature_id = "F1",
    adduct = "[M+NH4]+",
    mass = 300,
    mz = calculate_mz_from_mass(300, "[M+NH4]+"),
    rt = 1,
    structure_exact_mass = 300,
    source = "pair"
  )

  node_hypotheses <- tidytable::tidytable(
    feature_id = c("F1", "F2", "F2"),
    adduct = c("[M+NH4]+", "[M+H]+", "[M+Na]+"),
    mass = c(300, 300, 300),
    mz = c(
      calculate_mz_from_mass(300, "[M+NH4]+"),
      calculate_mz_from_mass(300, "[M+H]+"),
      calculate_mz_from_mass(300, "[M+Na]+")
    ),
    rt = c(1, 1, 1)
  )
  attr(node_hypotheses, "component_membership") <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    component_id = c("C1", "C1")
  )
  attr(node_hypotheses, "feature_m_map") <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    component_id = c("C1", "C1"),
    neutral_mass = c(300, 300)
  )

  out <- propagate_annotations_across_m_cliques(annotations, node_hypotheses)

  # F2 has no [M+NH4]+ hypothesis, so no propagated row should be created.
  expect_false(any(out$feature_id == "F2"))
})
