# Test Suite: annotate_masses_baseline_losses ----
#
# A neutral-loss edge to a precursor without adduct-graph support still gives
# the product its loss ion (from the precursor's plain baseline ions), as a
# baseline-strength hypothesis that ties with the primary baseline ion.

library(testthat)

test_that("is_plain_monomer_ion keeps single-charge monomers without modifiers", {
  ions <- c(
    "[M+H]+",
    "[M+H4N]+",
    "[M+Na]+",
    "[M-H]-",
    "[M-H2]2-",
    "[M+H2]2+",
    "[2M+H]+",
    "[M-H+CH2O2]-",
    "[M-H-H2O]-",
    "[M]+",
    NA_character_
  )
  expect_identical(
    is_plain_monomer_ion(ions),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("a loss edge to an unsupported precursor gives the product its loss ion", {
  m <- 300.0
  mz_parent <- as.numeric(calculate_mz_from_mass(m, "[M-H]-"))
  features <- tidytable::tidytable(
    feature_id = c("P", "L"),
    mz = c(mz_parent, mz_parent - 18.010565),
    rt = c(1.0, 1.0)
  )
  empty_edges <- tidytable::tidytable(
    feature_id = character(),
    adduct = character(),
    feature_id_dest = character(),
    adduct_dest = character()
  )
  hyps <- collect_node_adduct_hypotheses(
    adduct_edges = empty_edges,
    cluster_edges = tidytable::tidytable(
      feature_id = character(),
      cluster = character(),
      feature_id_dest = character()
    ),
    # Oriented from the product (lighter) to the precursor (heavier).
    loss_edges = tidytable::tidytable(
      feature_id = "L",
      loss = "H2O (water)",
      feature_id_dest = "P"
    ),
    evidence_hypotheses = NULL,
    preassigned = tidytable::tidytable(
      feature_id = character(),
      adduct = character()
    ),
    baseline_adducts = c("[M-H2]2-", "[M-H]-", "[M-H+CH2O2]-"),
    features_table = features
  )
  loss_rows <- hyps[hyps$source == "loss", ]
  # Only the plain baseline ion carries the loss: no `[M-H2-H2O]2-` and no
  # formate cluster minus water.
  expect_identical(loss_rows$feature_id, "L")
  expect_identical(loss_rows$adduct, "[M-H-H2O]-")
  expect_identical(loss_rows$candidate_adduct_origin, "baseline")
  expect_identical(loss_rows$loss_term, "H2O")
  # The product keeps its own baseline ions; the precursor gains nothing.
  expect_true("[M-H]-" %in% hyps$adduct[hyps$feature_id == "L"])
  expect_false(any(hyps$source[hyps$feature_id == "P"] == "loss"))
})

test_that("a supported precursor still passes losses as supported", {
  mz_h <- as.numeric(calculate_mz_from_mass(300.0, "[M+H]+"))
  mz_na <- as.numeric(calculate_mz_from_mass(300.0, "[M+Na]+"))
  hyps <- collect_node_adduct_hypotheses(
    adduct_edges = tidytable::tidytable(
      feature_id = "P",
      adduct = "[M+H]+",
      feature_id_dest = "N",
      adduct_dest = "[M+Na]+"
    ),
    cluster_edges = tidytable::tidytable(
      feature_id = character(),
      cluster = character(),
      feature_id_dest = character()
    ),
    loss_edges = tidytable::tidytable(
      feature_id = "L",
      loss = "H2O (water)",
      feature_id_dest = "P"
    ),
    evidence_hypotheses = NULL,
    preassigned = tidytable::tidytable(
      feature_id = character(),
      adduct = character()
    ),
    baseline_adducts = c("[M+H]+", "[M+Na]+"),
    features_table = tidytable::tidytable(
      feature_id = c("P", "N", "L"),
      mz = c(mz_h, mz_na, mz_h - 18.010565),
      rt = c(1.0, 1.0, 1.0)
    )
  )
  loss_rows <- hyps[hyps$source == "loss", ]
  expect_identical(loss_rows$adduct, "[M-H2O+H]+")
  expect_identical(loss_rows$candidate_adduct_origin, "supported")
})

test_that("a baseline-origin loss ties with the primary baseline ion", {
  annotations <- function(loss_origin) {
    tidytable::tidytable(
      feature_id = c("f1", "f1", "f2", "f3"),
      adduct = c("[M-H]-", "[M-H-H2O]-", "[M-H]-", "[M+Cl]-"),
      source = c("baseline", "loss", "pair", "pair"),
      candidate_adduct_origin = c(
        "baseline",
        loss_origin,
        "supported",
        "supported"
      ),
      adduct_support = c(0L, 0L, 1L, 1L),
      candidate_structure_error_mz = c(0.001, 0.001, 0.001, 0.001)
    )
  }
  # An adduct edge elsewhere, so the filter runs; f1 is unconstrained.
  edges <- tidytable::tidytable(
    feature_id = "f2",
    adduct = "[M-H]-",
    feature_id_dest = "f3",
    adduct_dest = "[M+Cl]-"
  )
  kept_f1 <- function(loss_origin) {
    out <- enforce_non_conflicting_annotation_states(
      annotations = annotations(loss_origin),
      adduct_edges = edges,
      baseline_adduct = "[M-H]-"
    )
    sort(out$adduct[out$feature_id == "f1"])
  }
  expect_identical(kept_f1("baseline"), c("[M-H-H2O]-", "[M-H]-"))
  # A graph-supported loss still outranks the baseline ion.
  expect_identical(kept_f1("supported"), "[M-H-H2O]-")
})
