# Test Suite: write_mztab ----

library(testthat)

# ── Helpers ───────────────────────────────────────────────────────────────────

# Build a minimal TIMA results data frame and write it to a temp TSV.
.make_tima_results <- function(
  tmpdir,
  n_features = 3L,
  n_candidates = 2L
) {
  rows <- vector("list", n_features * n_candidates)
  k <- 0L
  for (f in seq_len(n_features)) {
    for (c in seq_len(n_candidates)) {
      k <- k + 1L
      ik <- paste0(
        "ABCDEFGHIJKLMN",
        sprintf("%02d", (f - 1L) * n_candidates + c)
      )
      rows[[k]] <- data.frame(
        feature_id = as.character(f),
        feature_mz = as.character(200.0 + f * 1.1 + c * 0.01),
        feature_rt = as.character(1.5 + f * 0.3),
        candidate_structure_name = paste0("Compound_F", f, "C", c),
        candidate_structure_inchikey_connectivity_layer = ik,
        candidate_structure_smiles_no_stereo = paste0("CC", c, "C(=O)O"),
        candidate_structure_molecular_formula = paste0(
          "C",
          4 + c,
          "H",
          6 + c,
          "O2"
        ),
        candidate_structure_exact_mass = as.character(88.0 + c * 14.0),
        candidate_adduct = ifelse(c == 1L, "[M+H]+", "[M+Na]+"),
        candidate_library = ifelse(c == 1L, "spectral", "TIMA MS1"),
        score_final = as.character(round(0.8 - c * 0.15, 4)),
        score_biological = as.character(round(0.6 - c * 0.1, 4)),
        score_chemical = as.character(round(0.5 - c * 0.05, 4)),
        rank_final = as.character(c),
        stringsAsFactors = FALSE
      )
    }
  }
  df <- do.call(rbind, rows)
  path <- file.path(tmpdir, "annotations.tsv")
  write.table(df, path, sep = "\t", row.names = FALSE, quote = FALSE)
  path
}

# ── Validation tests ──────────────────────────────────────────────────────────

test_that("write_mztab validates input parameter", {
  tmp <- withr::local_tempfile(fileext = ".mztab")
  expect_error(
    write_mztab(input = NULL, output = tmp),
    class = "tima_error"
  )
  expect_error(
    write_mztab(input = 123, output = tmp),
    class = "tima_error"
  )
  expect_error(
    write_mztab(input = "nonexistent_file.tsv", output = tmp),
    class = "tima_error"
  )
})

test_that("write_mztab validates output parameter", {
  tmp_in <- withr::local_tempfile(fileext = ".tsv")
  writeLines("feature_id\tfeature_mz\n1\t100", tmp_in)
  expect_error(
    write_mztab(input = tmp_in, output = NULL),
    class = "tima_error"
  )
  expect_error(
    write_mztab(input = tmp_in, output = 123),
    class = "tima_error"
  )
})

test_that("write_mztab coerces vector-valued scalar parameters", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 1L, n_candidates = 1L)
  out <- file.path(tmpdir, "vector_params.mztab")

  ret <- write_mztab(
    input = c(results, "ignored.tsv"),
    output = c(out, "ignored.mztab"),
    ms_run_location = c("file:///run1.mzML", "file:///run2.mzML")
  )

  expect_identical(ret, out)
  expect_true(file.exists(out))
})

test_that("write_mztab writes output in the same folder as input", {
  local_test_project(copy = TRUE)

  input_dir <- withr::local_tempdir()
  other_dir <- withr::local_tempdir()
  results <- .make_tima_results(input_dir, n_features = 1L, n_candidates = 1L)
  requested_out <- file.path(other_dir, "elsewhere.mztab")

  ret <- write_mztab(input = results, output = requested_out)

  expected_out <- file.path(dirname(results), "elsewhere.mztab")
  expect_identical(ret, expected_out)
  expect_true(file.exists(expected_out))
  expect_false(file.exists(requested_out))
})

# ── Core behaviour tests ──────────────────────────────────────────────────────

test_that("write_mztab produces a file for a typical TIMA results table", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir)
  out <- file.path(tmpdir, "out.mztab")

  ret <- write_mztab(input = results, output = out)

  expect_identical(ret, out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 0L)
})

test_that("write_mztab produces valid MTD section", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir)
  out <- file.path(tmpdir, "annotations.mztab")

  write_mztab(input = results, output = out)

  lines <- readLines(out, warn = FALSE)
  mtd <- grep("^MTD\t", lines, value = TRUE)

  expect_true(any(grepl("mzTab-version\t2\\.1\\.0-M", mtd)))
  expect_true(any(grepl("mzTab-mode\tSummary", mtd)))
  expect_true(any(grepl("mzTab-type\tIdentification", mtd)))
  # id_confidence_measure uses TIMA user-namespace Param syntax
  expect_true(any(grepl(
    "id_confidence_measure\\[1\\].*\\[TIMA, TIMA:001",
    mtd
  )))
  expect_true(any(grepl(
    "id_confidence_measure\\[2\\].*\\[TIMA, TIMA:002",
    mtd
  )))
  expect_true(any(grepl(
    "id_confidence_measure\\[3\\].*\\[TIMA, TIMA:003",
    mtd
  )))
  # database URI should be the LOTUS URL
  expect_true(any(grepl("database\\[1\\]-uri.*naturalproducts\\.net", mtd)))
})

test_that("write_mztab emits ontology-aligned experiment metadata defaults", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir)
  out <- file.path(tmpdir, "metadata_defaults.mztab")

  write_mztab(input = results, output = out)

  lines <- readLines(out, warn = FALSE)
  mtd <- grep("^MTD\\t", lines, value = TRUE)

  expect_true(any(grepl(
    "quantification_method\\t\\[MS, MS:1001834, LC-MS label-free quantitation analysis, \\]",
    mtd
  )))
  expect_true(any(grepl(
    "assay\\[1\\]-quantification_reagent\\t\\[MS, MS:1002038, unlabeled sample, \\]",
    mtd
  )))
  expect_true(any(grepl(
    "sample\\[1\\]-description\\tTIMA metabolomics sample",
    mtd
  )))
  expect_true(any(grepl(
    "study_variable\\[1\\]-assay_refs\\tassay\\[1\\]",
    mtd
  )))
  expect_true(any(grepl(
    "study_variable_group\\[1\\]\\t\\[, , annotation_group, \\]",
    mtd
  )))
  expect_true(any(grepl(
    "study_variable\\[1\\]-group_ref\\tstudy_variable_group\\[1\\]",
    mtd
  )))
  expect_true(any(grepl(
    "small_molecule-identification_reliability\\t\\[MS, MS:1000932, identification reliability, \\]",
    mtd
  )))
})

test_that("write_mztab emits publication and instrument metadata when provided", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir)
  out <- file.path(tmpdir, "metadata_custom.mztab")

  write_mztab(
    input = results,
    output = out,
    publication = "doi:10.0000/example.1",
    instrument = "[MS, MS:1001742, LTQ Orbitrap Velos, ]",
    sample_name = "Liver extract"
  )

  lines <- readLines(out, warn = FALSE)
  mtd <- grep("^MTD\\t", lines, value = TRUE)

  expect_true(any(grepl("publication\\[1\\]\\tdoi:10\\.0000/example\\.1", mtd)))
  expect_true(any(grepl(
    "instrument\\[1\\]-name\\t\\[MS, MS:1001742, LTQ Orbitrap Velos, \\]",
    mtd
  )))
  expect_true(any(grepl("sample\\[1\\]-description\\tLiver extract", mtd)))
})

test_that("write_mztab omits publication metadata for empty-like publication inputs", {
  local_test_project(copy = TRUE)

  for (pub in list(NULL, "null")) {
    tmpdir <- withr::local_tempdir()
    results <- .make_tima_results(tmpdir)
    out <- file.path(
      tmpdir,
      paste0("metadata_no_publication_", tempfile(), ".mztab")
    )

    ret <- write_mztab(
      input = results,
      output = out,
      publication = pub
    )

    lines <- readLines(ret, warn = FALSE)
    mtd <- grep("^MTD\\t", lines, value = TRUE)

    expect_false(any(grepl("^MTD\\tpublication\\[1\\]\\t", mtd)))
  }
})

test_that("write_mztab retains TIMA result columns as canonical or opt_global fields", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "coverage.tsv")
  out <- file.path(tmpdir, "coverage.mztab")

  df <- data.frame(
    feature_id = "64",
    feature_mz = "175.0",
    feature_rt = "2.0",
    feature_spectrum_entropy = "2.38",
    feature_spectrum_peaks = "44",
    feature_pred_tax_cla_01kin_val = "Organic compounds",
    feature_pred_tax_cla_01kin_score = "1.000",
    feature_pred_tax_npc_01pat_val = "Amino acids and Peptides",
    feature_pred_tax_npc_01pat_score = "1.000",
    component_id = "2",
    candidate_spectrum_entropy = "2.38",
    candidate_structure_molecular_formula = "C6H14N4O2",
    candidate_score_sirius_tree = "66.437",
    candidate_score_sirius_confidence = "0.955",
    candidate_library = "gnps",
    candidate_spectrum_id = "CCMSLIB00005884102",
    candidate_adduct = "[M+H]+",
    candidate_score_similarity = "0.81",
    candidate_score_similarity_forward = "0.85",
    candidate_score_similarity_reverse = "0.94",
    candidate_structure_exact_mass = "174.111675688",
    candidate_structure_xlogp = "-1.5481",
    candidate_structure_inchikey_connectivity_layer = "ODKSFYDXXFIFQN",
    candidate_structure_inchikey_no_stereo = "ODKSFYDXXFIFQN-N",
    candidate_structure_smiles_no_stereo = "NC(N)=NCCCC(N)C(=O)O",
    candidate_structure_tax_cla_01kin = "Organic compounds",
    candidate_structure_tax_npc_01pat = "Amino acids and Peptides",
    candidate_structure_organism_occurrence_closest = "Biota",
    candidate_structure_id_CHEBI = "16467",
    candidate_structure_id_cas = "74-79-3",
    candidate_structure_id_chembl.compound = "CHEMBL1485",
    candidate_structure_id_hmdb = "HMDB0000517",
    candidate_structure_id_kegg = "C00062",
    candidate_structure_id_pubchem.compound = "28782",
    candidate_structure_id_wikidata = "Q173670",
    candidate_structure_id_lipidmaps = "LMFA00000001",
    candidate_structure_id_drugbank = "DB00125",
    candidate_structure_id_SLM = "SLM:0001",
    rank_initial = "2",
    rank_final = "1",
    score_initial = "0.885086322494474",
    score_biological = "1.007276",
    score_chemical = "1",
    score_final = "0.965337082473133",
    annotation_note = "ok",
    candidates_evaluated = "618",
    candidates_best = "618",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out)
  lines <- readLines(out, warn = FALSE)

  sfh_cols <- strsplit(
    lines[grep("^SFH\\t", lines)[[1L]]],
    "\t",
    fixed = TRUE
  )[[1L]][-1L]
  seh_cols <- strsplit(
    lines[grep("^SEH\\t", lines)[[1L]]],
    "\t",
    fixed = TRUE
  )[[1L]][-1L]
  smh_cols <- strsplit(
    lines[grep("^SMH\\t", lines)[[1L]]],
    "\t",
    fixed = TRUE
  )[[1L]][-1L]
  all_cols <- unique(c(sfh_cols, seh_cols, smh_cols))

  canonical_map <- c(
    feature_mz = "exp_mass_to_charge",
    feature_rt = "retention_time_in_seconds",
    candidate_structure_inchikey_connectivity_layer = "database_identifier",
    candidate_structure_molecular_formula = "chemical_formula",
    candidate_structure_smiles_no_stereo = "smiles",
    candidate_adduct = "adduct_ion",
    candidate_structure_exact_mass = "theoretical_mass_to_charge",
    candidate_library = "identification_method",
    candidate_spectrum_id = "spectra_ref",
    rank_final = "rank",
    score_final = "id_confidence_measure[1]",
    score_biological = "id_confidence_measure[2]",
    score_chemical = "id_confidence_measure[3]",
    candidate_score_similarity = "id_confidence_measure[4]"
  )

  src_cols <- colnames(df)
  dropped <- src_cols[
    !vapply(
      src_cols,
      function(col) {
        opt <- tima:::.mztab_opt_colname(col)
        canonical <- if (col %in% names(canonical_map)) {
          canonical_map[[col]]
        } else {
          NULL
        }
        col %in%
          c("feature_id") ||
          (!is.null(canonical) && canonical %in% all_cols) ||
          (opt %in% all_cols)
      },
      logical(1L)
    )
  ]

  expect_equal(
    length(dropped),
    0L,
    info = paste("Dropped columns:", paste(dropped, collapse = ", "))
  )
})

test_that("write_mztab covers full final-results schema and emits provenance COM comments", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "final_schema.tsv")
  out <- file.path(tmpdir, "final_schema.mztab")

  final_cols <- c(
    "feature_id",
    "feature_mz",
    "feature_rt",
    "feature_spectrum_entropy",
    "feature_spectrum_peaks",
    "feature_pred_tax_cla_01kin_val",
    "feature_pred_tax_cla_01kin_score",
    "feature_pred_tax_cla_02sup_val",
    "feature_pred_tax_cla_02sup_score",
    "feature_pred_tax_cla_03cla_val",
    "feature_pred_tax_cla_03cla_score",
    "feature_pred_tax_cla_04dirpar_val",
    "feature_pred_tax_cla_04dirpar_score",
    "feature_pred_tax_npc_01pat_val",
    "feature_pred_tax_npc_01pat_score",
    "feature_pred_tax_npc_02sup_val",
    "feature_pred_tax_npc_02sup_score",
    "feature_pred_tax_npc_03cla_val",
    "feature_pred_tax_npc_03cla_score",
    "component_id",
    "candidate_spectrum_entropy",
    "candidate_structure_molecular_formula",
    "candidate_count_sirius_peaks_explained",
    "candidate_score_sirius_intensity",
    "candidate_score_sirius_isotope",
    "candidate_score_sirius_sirius",
    "candidate_score_sirius_tree",
    "candidate_score_sirius_zodiac",
    "candidate_score_sirius_confidence",
    "candidate_score_sirius_csi",
    "candidate_score_sirius_msnovelist",
    "candidate_library",
    "candidate_spectrum_id",
    "candidate_adduct",
    "candidate_count_similarity_peaks_matched",
    "candidate_score_similarity",
    "candidate_score_similarity_forward",
    "candidate_score_similarity_reverse",
    "candidate_structure_exact_mass",
    "candidate_structure_xlogp",
    "candidate_structure_inchikey_connectivity_layer",
    "candidate_structure_inchikey_no_stereo",
    "candidate_structure_smiles_no_stereo",
    "candidate_structure_tax_cla_chemontid",
    "candidate_structure_tax_cla_01kin",
    "candidate_structure_tax_cla_02sup",
    "candidate_structure_tax_cla_03cla",
    "candidate_structure_tax_cla_04dirpar",
    "candidate_structure_tax_npc_01pat",
    "candidate_structure_tax_npc_02sup",
    "candidate_structure_tax_npc_03cla",
    "candidate_structure_organism_occurrence_closest",
    "candidate_structure_organism_occurrence_reference",
    "candidate_structure_tag",
    "candidate_structure_error_mz",
    "candidate_structure_id_CHEBI",
    "candidate_structure_id_cas",
    "candidate_structure_id_chembl.compound",
    "candidate_structure_id_chemspider",
    "candidate_structure_id_hmdb",
    "candidate_structure_id_kegg",
    "candidate_structure_id_pubchem.compound",
    "candidate_structure_id_surechembl",
    "candidate_structure_id_wikidata",
    "candidate_structure_id_lipidmaps",
    "candidate_structure_id_drugbank",
    "candidate_structure_id_SLM",
    "rank_initial",
    "rank_final",
    "score_initial",
    "score_biological",
    "score_chemical",
    "score_final",
    "annotation_note",
    "candidates_evaluated",
    "candidates_best"
  )

  vals <- as.list(rep("1", length(final_cols)))
  names(vals) <- final_cols
  vals$feature_id <- "FT001"
  vals$feature_mz <- "175.0"
  vals$feature_rt <- "2.0"
  vals$candidate_adduct <- "[M+H]+"
  vals$candidate_library <- "gnps"
  vals$candidate_spectrum_id <- "CCMSLIB00005884102"
  vals$candidate_structure_exact_mass <- "174.111675688"
  vals$candidate_structure_inchikey_connectivity_layer <- "ODKSFYDXXFIFQN"
  vals$rank_final <- "1"
  vals$score_biological <- "1.0"
  vals$score_chemical <- "1.0"
  vals$score_final <- "0.9"
  vals$candidate_score_similarity <- "0.81"

  df <- as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out)
  lines <- readLines(out, warn = FALSE)

  expect_true(any(grepl("^COM\\tTIMA export:", lines)))

  sfh_cols <- strsplit(
    lines[grep("^SFH\\t", lines)[[1L]]],
    "\t",
    fixed = TRUE
  )[[1L]][-1L]
  seh_cols <- strsplit(
    lines[grep("^SEH\\t", lines)[[1L]]],
    "\t",
    fixed = TRUE
  )[[1L]][-1L]
  smh_cols <- strsplit(
    lines[grep("^SMH\\t", lines)[[1L]]],
    "\t",
    fixed = TRUE
  )[[1L]][-1L]
  all_cols <- unique(c(sfh_cols, seh_cols, smh_cols))

  canonical_map <- c(
    feature_mz = "exp_mass_to_charge",
    feature_rt = "retention_time_in_seconds",
    candidate_structure_inchikey_connectivity_layer = "database_identifier",
    candidate_structure_molecular_formula = "chemical_formula",
    candidate_structure_smiles_no_stereo = "smiles",
    candidate_adduct = "adduct_ion",
    candidate_structure_exact_mass = "theoretical_mass_to_charge",
    candidate_library = "identification_method",
    candidate_spectrum_id = "spectra_ref",
    rank_final = "rank",
    score_final = "id_confidence_measure[1]",
    score_biological = "id_confidence_measure[2]",
    score_chemical = "id_confidence_measure[3]",
    candidate_score_similarity = "id_confidence_measure[4]"
  )

  dropped <- final_cols[
    !vapply(
      final_cols,
      function(col) {
        opt <- tima:::.mztab_opt_colname(col)
        canonical <- if (col %in% names(canonical_map)) {
          canonical_map[[col]]
        } else {
          NULL
        }
        col %in%
          c("feature_id") ||
          (!is.null(canonical) && canonical %in% all_cols) ||
          (opt %in% all_cols)
      },
      logical(1L)
    )
  ]

  expect_equal(
    length(dropped),
    0L,
    info = paste("Dropped columns:", paste(dropped, collapse = ", "))
  )
})

test_that("write_mztab embeds optional edges table as COM extension lines", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 2L, n_candidates = 1L)
  edges_path <- file.path(tmpdir, "edges.tsv")
  out <- file.path(tmpdir, "with_edges.mztab")

  edges <- data.frame(
    source = c("FT001", "FT001"),
    target = c("FT002", "FT003"),
    score = c("0.91", "0.72"),
    stringsAsFactors = FALSE
  )
  write.table(edges, edges_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = results, output = out, edges_file = edges_path)

  lines <- readLines(out, warn = FALSE)
  expect_true(any(grepl(
    "^COM\\tTIMA edges\\trows=2; cols=source\\|target\\|score$",
    lines
  )))
  edge_lines <- grep("^COM\\tTIMA edge\\t", lines, value = TRUE)
  expect_length(edge_lines, 2L)
  expect_true(any(grepl('"source":"FT001"', edge_lines, fixed = TRUE)))
  expect_true(any(grepl('"target":"FT002"', edge_lines, fixed = TRUE)))
  expect_true(any(grepl('"score":"0.91"', edge_lines, fixed = TRUE)))
})

test_that("write_mztab embeds optional edges table in JSON exports", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 2L, n_candidates = 1L)
  edges_path <- file.path(tmpdir, "edges_json.tsv")
  out <- file.path(tmpdir, "with_edges.json")

  edges <- data.frame(
    source = c("FT001", "FT001"),
    target = c("FT002", "FT003"),
    score = c("0.91", "0.72"),
    stringsAsFactors = FALSE
  )
  write.table(edges, edges_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = results, output = out, edges_file = edges_path)

  payload <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_true("tima_edges" %in% names(payload))
  expect_length(payload$tima_edges, 2L)
  expect_equal(payload$tima_edges[[1]]$source, "FT001")
  expect_equal(payload$tima_edges[[1]]$target, "FT002")
  expect_equal(payload$tima_edges[[1]]$score, "0.91")
})

test_that("write_mztab produces SMF section with one row per feature", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  n_feat <- 4L
  results <- .make_tima_results(tmpdir, n_features = n_feat)
  out <- file.path(tmpdir, "out.mztab")

  write_mztab(input = results, output = out)

  lines <- readLines(out, warn = FALSE)
  sfh_idx <- grep("^SFH\t", lines)
  smf_idx <- grep("^SMF\t", lines)

  expect_length(sfh_idx, 1L) # exactly one header
  expect_length(smf_idx, n_feat) # one row per feature
})

test_that("write_mztab produces SME section with one row per annotation", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  n_feat <- 3L
  n_cand <- 2L
  results <- .make_tima_results(
    tmpdir,
    n_features = n_feat,
    n_candidates = n_cand
  )
  out <- file.path(tmpdir, "out.mztab")

  write_mztab(input = results, output = out)

  lines <- readLines(out, warn = FALSE)
  seh_idx <- grep("^SEH\t", lines)
  sme_idx <- grep("^SME\t", lines)

  expect_length(seh_idx, 1L)
  expect_length(sme_idx, n_feat * n_cand)
})

test_that("write_mztab produces SML section with correct compound grouping", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  # 2 features × 2 candidates = 4 unique InChIKeys
  results <- .make_tima_results(tmpdir, n_features = 2L, n_candidates = 2L)
  out <- file.path(tmpdir, "out.mztab")

  write_mztab(input = results, output = out)

  lines <- readLines(out, warn = FALSE)
  smh_idx <- grep("^SMH\t", lines)
  sml_idx <- grep("^SML\t", lines)

  expect_length(smh_idx, 1L)
  # 2 features × 2 candidates = 4 unique compounds in this fixture
  expect_length(sml_idx, 4L)
})

test_that("write_mztab uses 'null' for missing optional fields", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  # Minimal results without score columns
  minimal_path <- file.path(tmpdir, "minimal.tsv")
  writeLines(
    c(
      "feature_id\tfeature_mz\tfeature_rt",
      "FT001\t200.1\t1.2",
      "FT002\t350.5\t2.5"
    ),
    minimal_path
  )

  out <- file.path(tmpdir, "minimal.mztab")
  write_mztab(input = minimal_path, output = out)

  expect_true(file.exists(out))
  lines <- readLines(out, warn = FALSE)
  # Should still have MTD and SMF sections
  expect_true(any(grepl("^MTD\t", lines)))
  expect_true(any(grepl("^SMF\t", lines)))
})

test_that("write_mztab MTD references custom title and description", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir)
  out <- file.path(tmpdir, "custom_title.mztab")

  write_mztab(
    input = results,
    output = out,
    title = "My custom study title",
    description = "Description for the test experiment."
  )

  lines <- readLines(out, warn = FALSE)
  expect_true(any(grepl("My custom study title", lines, fixed = TRUE)))
  expect_true(any(grepl(
    "Description for the test experiment",
    lines,
    fixed = TRUE
  )))
})

test_that("write_mztab reliability scoring follows MSI levels", {
  expect_equal(.mztab_score_to_reliability(0.85, "spectral"), 1L)
  expect_equal(.mztab_score_to_reliability(0.75, "spectral"), 1L)
  expect_equal(.mztab_score_to_reliability(0.65, "spectral"), 2L)
  expect_equal(.mztab_score_to_reliability(0.55, "TIMA MS1"), 2L)
  expect_equal(.mztab_score_to_reliability(0.30, "TIMA MS1"), 3L)
  expect_equal(.mztab_score_to_reliability(0.10, "TIMA MS1"), 4L)
  expect_equal(.mztab_score_to_reliability(NA, "TIMA MS1"), 4L)
})

test_that(".mztab_library_to_identification_method returns proper Param syntax", {
  fmt <- function(lib) tima:::.mztab_library_to_identification_method(lib)

  # All results must be Param strings
  expect_match(fmt("spectral"), "^\\[.*,.*,.*,.*\\]$")
  expect_match(fmt("TIMA MS1"), "^\\[.*,.*,.*,.*\\]$")
  expect_match(fmt("sirius"), "^\\[.*,.*,.*,.*\\]$")
  expect_match(fmt("gnps"), "^\\[.*,.*,.*,.*\\]$")
  expect_match(fmt("mzmine_lib"), "^\\[.*,.*,.*,.*\\]$")

  # Specific expected names
  expect_match(fmt("spectral"), "spectral library matching", fixed = FALSE)
  expect_match(fmt("TIMA MS1"), "exact mass database search", fixed = FALSE)
  expect_match(fmt("sirius"), "SIRIUS", fixed = FALSE)
  expect_match(fmt("gnps"), "GNPS", fixed = FALSE)

  # Already-formatted Param passes through unchanged
  already <- "[MS, MS:1003082, MS-DIAL, 5.5]"
  expect_identical(fmt(already), already)
})

# ── New ontology/CV-alignment tests ──────────────────────────────────────────

test_that("write_mztab uses TIMA:004 (not PSM-specific MS:1001143) for spectral similarity", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "with_sim.tsv")
  out <- file.path(tmpdir, "with_sim.mztab")

  df <- data.frame(
    feature_id = "F1",
    feature_mz = "180.1",
    feature_rt = "1.0",
    candidate_structure_name = "TestCmpd",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_structure_molecular_formula = "C6H12O6",
    candidate_adduct = "[M+H]+",
    candidate_library = "spectral",
    candidate_score_similarity = "0.91",
    score_final = "0.88",
    rank_final = "1",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out)
  lines <- readLines(out, warn = FALSE)
  mtd <- grep("^MTD\t", lines, value = TRUE)

  # id_confidence_measure[4] must be in the TIMA user namespace, NOT PSI-MS PSM
  measure4_lines <- grep("id_confidence_measure\\[4\\]", mtd, value = TRUE)
  expect_gt(length(measure4_lines), 0L)
  expect_true(
    all(grepl("TIMA:004", measure4_lines)),
    info = paste("Expected TIMA:004, got:", measure4_lines)
  )
  # Must NOT reference the PSMs-specific accession MS:1001143
  expect_false(any(grepl("MS:1001143", measure4_lines)))
})

test_that("write_mztab spectra_ref is formatted as 'ms_run[1]:{id}'", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "spectraref.tsv")
  out <- file.path(tmpdir, "spectraref.mztab")

  df <- data.frame(
    feature_id = "F1",
    feature_mz = "200.1",
    feature_rt = "1.5",
    candidate_structure_name = "Cmpd",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_structure_molecular_formula = "C8H10O2",
    candidate_adduct = "[M+H]+",
    candidate_library = "spectral",
    candidate_spectrum_id = "CCMSLIB00001234",
    score_final = "0.75",
    rank_final = "1",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out)
  lines <- readLines(out, warn = FALSE)

  seh_line <- lines[grep("^SEH\t", lines)[[1L]]]
  seh_cols <- strsplit(seh_line, "\t", fixed = TRUE)[[1L]][-1L]
  sme_line <- lines[grep("^SME\t", lines)[[1L]]]
  sme_vals <- strsplit(sme_line, "\t", fixed = TRUE)[[1L]][-1L]

  ref_pos <- which(seh_cols == "spectra_ref")
  expect_length(ref_pos, 1L)
  ref_val <- sme_vals[ref_pos]
  expect_match(ref_val, "^ms_run\\[1\\]:", info = paste("Got:", ref_val))
  expect_true(grepl("CCMSLIB00001234", ref_val))
})

test_that("write_mztab emits ms_run[1]-scan_polarity with PSI-MS CV term", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 1L, n_candidates = 1L)
  out_pos <- file.path(tmpdir, "pos.mztab")
  out_neg <- file.path(tmpdir, "neg.mztab")
  out_null <- file.path(tmpdir, "unknown.mztab")

  write_mztab(input = results, output = out_pos, polarity = "positive")
  write_mztab(input = results, output = out_neg, polarity = "negative")
  write_mztab(input = results, output = out_null, polarity = NULL)

  lines_pos <- readLines(out_pos, warn = FALSE)
  lines_neg <- readLines(out_neg, warn = FALSE)
  lines_null <- readLines(out_null, warn = FALSE)

  mtd_pos <- grep("^MTD\t", lines_pos, value = TRUE)
  mtd_neg <- grep("^MTD\t", lines_neg, value = TRUE)
  mtd_null <- grep("^MTD\t", lines_null, value = TRUE)

  # Positive polarity → MS:1000130
  expect_true(any(grepl("MS:1000130", mtd_pos)))
  # Negative polarity → MS:1000129
  expect_true(any(grepl("MS:1000129", mtd_neg)))
  # Unknown polarity → no scan_polarity line
  expect_false(any(grepl("scan_polarity", mtd_null)))
})

test_that("write_mztab polarity abbreviations are accepted", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 1L, n_candidates = 1L)
  out <- file.path(tmpdir, "abbr.mztab")

  # "pos" should be expanded to "positive"
  write_mztab(input = results, output = out, polarity = "pos")
  lines <- readLines(out, warn = FALSE)
  mtd <- grep("^MTD\t", lines, value = TRUE)
  expect_true(any(grepl("MS:1000130", mtd)))
})

test_that("write_mztab emits software[1]-setting[1] with TIMA repository URL", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 1L, n_candidates = 1L)
  out <- file.path(tmpdir, "sw.mztab")

  write_mztab(input = results, output = out)
  lines <- readLines(out, warn = FALSE)
  mtd <- grep("^MTD\t", lines, value = TRUE)

  expect_true(any(grepl(
    "software\\[1\\]-setting\\[1\\].*github.*tima",
    mtd,
    ignore.case = TRUE
  )))
})

test_that("write_mztab emits colunit-small_molecule for neutral mass (UO:0000221)", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 1L, n_candidates = 1L)
  out <- file.path(tmpdir, "colunit.mztab")

  write_mztab(input = results, output = out)
  lines <- readLines(out, warn = FALSE)
  mtd <- grep("^MTD\t", lines, value = TRUE)

  expect_true(any(grepl("UO:0000221", mtd)), info = "Dalton colunit missing")
})

test_that("write_mztab emits colunit-small_molecule_evidence for id_confidence_measure[1]", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 1L, n_candidates = 1L)
  out <- file.path(tmpdir, "colunit_ev.mztab")

  write_mztab(input = results, output = out)
  lines <- readLines(out, warn = FALSE)
  mtd <- grep("^MTD\t", lines, value = TRUE)

  expect_true(any(grepl("colunit-small_molecule_evidence", mtd)))
  expect_true(any(grepl("id_confidence_measure\\[1\\]", mtd)))
})

test_that("write_mztab contact info is emitted when provided", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 1L, n_candidates = 1L)
  out <- file.path(tmpdir, "contact.mztab")

  write_mztab(
    input = results,
    output = out,
    contact = list(
      name = "Jane Doe",
      email = "jane@example.com",
      affiliation = "Example University"
    )
  )

  lines <- readLines(out, warn = FALSE)
  mtd <- grep("^MTD\t", lines, value = TRUE)
  expect_true(any(grepl("contact\\[1\\]-name.*Jane Doe", mtd)))
  expect_true(any(grepl("contact\\[1\\]-email.*jane@example.com", mtd)))
  expect_true(any(grepl("contact\\[1\\]-affiliation.*Example University", mtd)))
})

test_that("write_mztab xrefs enrichment populates uri from Wikidata", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "xrefs_ann.tsv")
  out <- file.path(tmpdir, "xrefs.mztab")
  xrefs_tsv <- file.path(tmpdir, "xrefs.tsv")

  # Annotation with a known InChIKey connectivity layer
  df <- data.frame(
    feature_id = "F1",
    feature_mz = "180.1",
    feature_rt = "1.0",
    candidate_structure_name = "Caffeine",
    candidate_structure_inchikey_connectivity_layer = "RYYVLZVUVIJVGH",
    candidate_structure_molecular_formula = "C8H10N4O2",
    candidate_adduct = "[M+H]+",
    candidate_library = "spectral",
    score_final = "0.9",
    rank_final = "1",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  # Minimal xrefs fixture (InChIKey connectivity layer → Wikidata QID)
  xrefs_df <- data.frame(
    inchikey = "RYYVLZVUVIJVGH",
    prefix = "wikidata",
    id = "Q60235",
    stringsAsFactors = FALSE
  )
  write.table(xrefs_df, xrefs_tsv, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out, xrefs_file = xrefs_tsv)
  lines <- readLines(out, warn = FALSE)

  # SME uri should contain the Wikidata URL
  seh_line <- lines[grep("^SEH\t", lines)[[1L]]]
  seh_cols <- strsplit(seh_line, "\t", fixed = TRUE)[[1L]][-1L]
  sme_line <- lines[grep("^SME\t", lines)[[1L]]]
  sme_vals <- strsplit(sme_line, "\t", fixed = TRUE)[[1L]][-1L]

  uri_pos <- which(seh_cols == "uri")
  expect_length(uri_pos, 1L)
  uri_val <- sme_vals[uri_pos]
  expect_true(
    grepl("wikidata", uri_val, ignore.case = TRUE) ||
      grepl("Q60235", uri_val),
    info = paste("Expected Wikidata URI, got:", uri_val)
  )
})

test_that("write_mztab xrefs adds additional database[n] blocks for ChEBI", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "db_ann.tsv")
  out <- file.path(tmpdir, "db_extra.mztab")
  xrefs_tsv <- file.path(tmpdir, "db_xrefs.tsv")

  df <- data.frame(
    feature_id = "F1",
    feature_mz = "181.1",
    feature_rt = "1.0",
    candidate_structure_name = "Caffeine",
    candidate_structure_inchikey_connectivity_layer = "RYYVLZVUVIJVGH",
    candidate_structure_molecular_formula = "C8H10N4O2",
    candidate_adduct = "[M+H]+",
    candidate_library = "spectral",
    score_final = "0.9",
    rank_final = "1",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  # Provide ChEBI xref → should trigger database[2] registration
  xrefs_df <- data.frame(
    inchikey = "RYYVLZVUVIJVGH",
    prefix = "chebi",
    id = "27732",
    stringsAsFactors = FALSE
  )
  write.table(xrefs_df, xrefs_tsv, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out, xrefs_file = xrefs_tsv)
  lines <- readLines(out, warn = FALSE)
  mtd <- grep("^MTD\t", lines, value = TRUE)

  expect_true(
    any(grepl("database\\[2\\]\\t", mtd)),
    info = "Expected database[2] registration for ChEBI"
  )
  expect_true(any(grepl("ChEBI|chebi|CHEBI", mtd, ignore.case = TRUE)))
})

test_that(".mztab_build_xrefs_index returns NULL for missing file", {
  expect_null(tima:::.mztab_build_xrefs_index(NULL))
  expect_null(tima:::.mztab_build_xrefs_index("nonexistent_file.tsv"))
})

test_that(".mztab_pick_best_uri prefers Wikidata over ChEBI", {
  rows <- data.frame(
    prefix = c("chebi", "wikidata"),
    id = c("12345", "Q99999"),
    stringsAsFactors = FALSE
  )
  uri <- tima:::.mztab_pick_best_uri(rows)
  expect_match(uri, "wikidata", ignore.case = TRUE)
})

test_that(".mztab_pick_best_uri handles single row correctly", {
  rows <- data.frame(
    prefix = "hmdb",
    id = "HMDB0000001",
    stringsAsFactors = FALSE
  )
  uri <- tima:::.mztab_pick_best_uri(rows)
  expect_match(uri, "hmdb", ignore.case = TRUE)
  expect_match(uri, "HMDB0000001")
})

test_that("write_mztab ms_run_format and ms_run_id_format are emitted when set", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 1L, n_candidates = 1L)
  out <- file.path(tmpdir, "format.mztab")

  write_mztab(
    input = results,
    output = out,
    ms_run_format = "[MS, MS:1000584, mzML file, ]",
    ms_run_id_format = "[MS, MS:1000776, scan number only nativeID format, ]"
  )

  lines <- readLines(out, warn = FALSE)
  mtd <- grep("^MTD\t", lines, value = TRUE)

  expect_true(any(grepl("ms_run\\[1\\]-format.*MS:1000584", mtd)))
  expect_true(any(grepl("ms_run\\[1\\]-id_format.*MS:1000776", mtd)))
})

test_that("write_mztab theoretical_neutral_mass is rounded to 6 decimal places when available", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "mass.tsv")
  out <- file.path(tmpdir, "mass.mztab")

  df <- data.frame(
    feature_id = "F1",
    feature_mz = "195.09",
    feature_rt = "2.0",
    candidate_structure_name = "Cmpd",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_structure_molecular_formula = "C9H10N2O",
    candidate_structure_exact_mass = "162.0793",
    candidate_adduct = "[M+H]+",
    candidate_library = "spectral",
    score_final = "0.8",
    rank_final = "1",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out)
  lines <- readLines(out, warn = FALSE)

  smh_line <- lines[grep("^SMH\t", lines)[[1L]]]
  smh_cols <- strsplit(smh_line, "\t", fixed = TRUE)[[1L]][-1L]
  sml_line <- lines[grep("^SML\t", lines)[[1L]]]
  sml_vals <- strsplit(sml_line, "\t", fixed = TRUE)[[1L]][-1L]

  mass_pos <- which(smh_cols == "theoretical_neutral_mass")
  expect_length(mass_pos, 1L)
  mass_val <- sml_vals[mass_pos]
  # Should be a numeric string, not "null"
  expect_false(mass_val == "null", info = "Neutral mass should not be null")
  expect_true(nchar(mass_val) > 0L)
  expect_true(!is.na(suppressWarnings(as.numeric(mass_val))))
})

test_that("write_mztab SMF exp_mass_to_charge matches feature_mz", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 2L, n_candidates = 1L)
  out <- file.path(tmpdir, "out.mztab")

  write_mztab(input = results, output = out)

  lines <- readLines(out, warn = FALSE)
  sfh_line <- lines[grep("^SFH\t", lines)[[1L]]]
  sfh_cols <- strsplit(sfh_line, "\t", fixed = TRUE)[[1L]][-1L]
  mz_pos <- which(sfh_cols == "exp_mass_to_charge")

  smf_lines <- lines[grep("^SMF\t", lines)]
  for (ln in smf_lines) {
    vals <- strsplit(ln, "\t", fixed = TRUE)[[1L]][-1L]
    expect_true(nzchar(vals[mz_pos]) && vals[mz_pos] != "null")
  }
})

test_that("write_mztab complements an existing mzTab without erasing unmanaged lines", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 2L, n_candidates = 1L)

  base <- file.path(tmpdir, "base.mztab")
  writeLines(
    c(
      "COM\tkept comment line",
      "MSH\tlegacy section we do not manage",
      "MSM\tlegacy row"
    ),
    base
  )

  out <- file.path(tmpdir, "merged.mztab")
  write_mztab(input = results, output = out, base_mztab = base)

  lines <- readLines(out, warn = FALSE)
  expect_true(any(grepl("^COM\\tkept comment line$", lines)))
  expect_true(any(grepl("^MSH\\tlegacy section we do not manage$", lines)))
  expect_true(any(grepl("^MSM\\tlegacy row$", lines)))
  expect_true(any(grepl("^SMH\\t", lines)))
  expect_true(any(grepl("^SFH\\t", lines)))
  expect_true(any(grepl("^SEH\\t", lines)))
})

test_that("write_mztab merge mode is idempotent and does not duplicate existing rows", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 2L, n_candidates = 1L)
  base <- file.path(tmpdir, "seed.mztab")
  merged1 <- file.path(tmpdir, "merged1.mztab")
  merged2 <- file.path(tmpdir, "merged2.mztab")

  write_mztab(input = results, output = base)
  write_mztab(input = results, output = merged1, base_mztab = base)
  write_mztab(input = results, output = merged2, base_mztab = merged1)

  lines1 <- readLines(merged1, warn = FALSE)
  lines2 <- readLines(merged2, warn = FALSE)

  expect_identical(lines1, lines2)
  expect_length(grep("^SMF\\t", lines2), 2L)
  expect_length(grep("^SME\\t", lines2), 2L)
  expect_length(grep("^SML\\t", lines2), 2L)
})

test_that("write_mztab writes JSON mzTab output that round-trips through the parser", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 2L, n_candidates = 2L)
  out <- file.path(tmpdir, "out.json")

  write_mztab(input = results, output = out)

  expect_true(file.exists(out))

  raw_json <- jsonlite::fromJSON(out, simplifyVector = FALSE)
  expect_true("metadata" %in% names(raw_json))
  expect_true("smallMoleculeSummary" %in% names(raw_json))
  expect_true("smallMoleculeFeature" %in% names(raw_json))
  expect_true("smallMoleculeEvidence" %in% names(raw_json))

  tabs <- read_mztab_tables(out)
  expect_true(nrow(tabs$metadata) > 0L)
  expect_equal(nrow(tabs$smf), 2L)
  expect_equal(nrow(tabs$sme), 4L)
  expect_equal(nrow(tabs$sml), 4L)
})

test_that("write_mztab JSON merge mode is idempotent and avoids duplicate rows", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 2L, n_candidates = 1L)
  base <- file.path(tmpdir, "seed.json")
  merged1 <- file.path(tmpdir, "merged1.json")
  merged2 <- file.path(tmpdir, "merged2.json")

  write_mztab(input = results, output = base)
  write_mztab(input = results, output = merged1, base_mztab = base)
  write_mztab(input = results, output = merged2, base_mztab = merged1)

  tabs1 <- read_mztab_tables(merged1)
  tabs2 <- read_mztab_tables(merged2)

  smf_core <- c(
    "SMF_ID",
    "exp_mass_to_charge",
    "charge",
    "retention_time_in_seconds"
  )
  expect_equal(
    as.data.frame(tabs1$smf)[, smf_core, drop = FALSE],
    as.data.frame(tabs2$smf)[, smf_core, drop = FALSE]
  )

  sme_core <- c(
    "SME_ID",
    "database_identifier",
    "exp_mass_to_charge",
    "charge",
    "theoretical_mass_to_charge",
    "identification_method",
    "ms_level",
    "rank"
  )
  expect_equal(
    as.data.frame(tabs1$sme)[, sme_core, drop = FALSE],
    as.data.frame(tabs2$sme)[, sme_core, drop = FALSE]
  )

  expect_equal(
    as.character(tabs1$sml$database_identifier),
    as.character(tabs2$sml$database_identifier)
  )
  expect_equal(nrow(tabs2$smf), 2L)
  expect_equal(nrow(tabs2$sme), 2L)
  expect_equal(nrow(tabs2$sml), 2L)
})

test_that("write_mztab JSON round-trip with TIMA annotation fixture preserves SME semantics", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 2L, n_candidates = 2L)
  out1 <- file.path(tmpdir, "roundtrip_1.json")
  out2 <- file.path(tmpdir, "roundtrip_2.json")

  write_mztab(input = results, output = out1)
  tabs1 <- read_mztab_tables(out1)
  expect_no_error(validate_mztab_tables(tabs1, strict = TRUE))

  # Rewriting with the same TIMA annotations and a JSON base must be stable.
  write_mztab(input = results, output = out2, base_mztab = out1)
  tabs2 <- read_mztab_tables(out2)
  expect_no_error(validate_mztab_tables(tabs2, strict = TRUE))

  smf_core <- c(
    "SMF_ID",
    "exp_mass_to_charge",
    "charge",
    "retention_time_in_seconds"
  )
  expect_equal(
    as.data.frame(tabs1$smf)[, smf_core, drop = FALSE],
    as.data.frame(tabs2$smf)[, smf_core, drop = FALSE]
  )

  sme_core <- c(
    "SME_ID",
    "database_identifier",
    "exp_mass_to_charge",
    "charge",
    "theoretical_mass_to_charge",
    "identification_method",
    "ms_level",
    "rank"
  )
  expect_equal(
    as.data.frame(tabs1$sme)[, sme_core, drop = FALSE],
    as.data.frame(tabs2$sme)[, sme_core, drop = FALSE]
  )

  expect_equal(
    as.character(tabs1$sml$database_identifier),
    as.character(tabs2$sml$database_identifier)
  )

  expect_equal(nrow(tabs2$smf), 2L)
  expect_equal(nrow(tabs2$sme), 4L)
  expect_equal(nrow(tabs2$sml), 4L)
  expect_true(all(nzchar(as.character(tabs2$sme$identification_method))))
  expect_true(all(nzchar(as.character(tabs2$sme$ms_level))))
})

test_that("write_mztab SME identification_method is valid Param syntax", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 2L, n_candidates = 2L)
  out <- file.path(tmpdir, "out.mztab")

  write_mztab(input = results, output = out)

  lines <- readLines(out, warn = FALSE)
  seh_line <- lines[grep("^SEH\t", lines)[[1L]]]
  seh_cols <- strsplit(seh_line, "\t", fixed = TRUE)[[1L]][-1L]
  id_pos <- which(seh_cols == "identification_method")

  sme_lines <- lines[grep("^SME\t", lines)]
  for (ln in sme_lines) {
    vals <- strsplit(ln, "\t", fixed = TRUE)[[1L]][-1L]
    method <- vals[id_pos]
    # Must be a Param: starts with "[" and ends with "]"
    expect_match(
      method,
      "^\\[.*\\]$",
      info = paste("bad identification_method:", method)
    )
  }
})

test_that("write_mztab merge mode preserves COM lines from after managed sections", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 1L, n_candidates = 1L)
  base <- file.path(tmpdir, "base_com.mztab")
  out <- file.path(tmpdir, "merged_com.mztab")

  writeLines(
    c(
      "COM\tpre-section comment",
      "MTD\tmzTab-version\t2.0.0-M",
      "SMH\tSML_ID\tSMF_ID_REFS\tSME_ID_REFS\tdatabase_identifier",
      "SML\t1\tnull\tnull\tBASEKEY",
      "COM\tpost-SML comment from masster",
      "SFH\tSMF_ID\tSML_ID_REFS\texp_mass_to_charge\tcharge\tretention_time_in_seconds",
      "SMF\t1\t1\t100\t1\t60"
    ),
    base
  )

  write_mztab(input = results, output = out, base_mztab = base)
  lines <- readLines(out, warn = FALSE)

  expect_true(any(grepl("pre-section comment", lines, fixed = TRUE)))
  expect_true(any(grepl("post-SML comment from masster", lines, fixed = TRUE)))
})

test_that("write_mztab merge mode preserves optional columns from base tables", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 1L, n_candidates = 1L)
  base <- file.path(tmpdir, "base_opt.mztab")
  out <- file.path(tmpdir, "merged_opt.mztab")

  writeLines(
    c(
      "MTD\tmzTab-version\t2.0.0-M",
      "SMH\tSML_ID\tSMF_ID_REFS\tSME_ID_REFS\tdatabase_identifier\topt_global_note",
      "SML\t1\tnull\tnull\tBASEKEY\tlegacy-note",
      "SFH\tSMF_ID\tSML_ID_REFS\texp_mass_to_charge\tcharge\tretention_time_in_seconds",
      "SMF\t1\t1\t100\t1\t60",
      "SEH\tSME_ID\tevidence_input_id\tdatabase_identifier\texp_mass_to_charge\tcharge\ttheoretical_mass_to_charge\tspectra_ref\tidentification_method\tms_level\trank",
      "SME\t1\t1\tBASEKEY\t100\t1\t100\tnull\tnull\t[MS, MS:1000580, MS2 spectrum, ]\t1"
    ),
    base
  )

  write_mztab(input = results, output = out, base_mztab = base)
  lines <- readLines(out, warn = FALSE)

  smh <- lines[grep("^SMH\\t", lines)[[1L]]]
  expect_true(grepl("opt_global_note", smh, fixed = TRUE))
  expect_true(any(grepl("legacy-note", lines, fixed = TRUE)))
})

test_that("write_mztab exports extra filtered-result columns as opt_global fields", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "ann_extra.tsv")
  out <- file.path(tmpdir, "out_extra.mztab")

  df <- data.frame(
    feature_id = "F1",
    feature_mz = "101.0",
    feature_rt = "1.0",
    candidate_structure_name = "Cmpd",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_structure_molecular_formula = "C5H10O",
    candidate_adduct = "[M+H]+",
    candidate_library = "spectral",
    candidate_score_sirius_tree = "66.4",
    candidate_structure_organism_occurrence_closest = "Biota",
    score_final = "0.9",
    rank_final = "1",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out)
  lines <- readLines(out, warn = FALSE)

  seh_line <- lines[grep("^SEH\\t", lines)[[1L]]]
  expect_true(grepl(
    "opt_global_candidate_score_sirius_tree",
    seh_line,
    fixed = TRUE
  ))
  expect_true(grepl(
    "opt_global_candidate_structure_organism_occurrence_closest",
    seh_line,
    fixed = TRUE
  ))
})

test_that("write_mztab maps InChI and derives charge from adduct", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "ann.tsv")
  out <- file.path(tmpdir, "out.mztab")

  df <- data.frame(
    feature_id = "F1",
    feature_mz = "101.0",
    feature_rt = "1.0",
    candidate_structure_name = "Cmpd",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_structure_molecular_formula = "C5H10O",
    candidate_structure_exact_mass = "86.07",
    candidate_structure_inchi = "InChI=1S/C5H10O/c1-2-3-4-5-6/h2-5H2,1H3",
    candidate_adduct = "[M+2H]2+",
    candidate_library = "spectral",
    score_final = "0.9",
    rank_final = "1",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out)
  lines <- readLines(out, warn = FALSE)

  seh_line <- lines[grep("^SEH\\t", lines)[[1L]]]
  seh_cols <- strsplit(seh_line, "\t", fixed = TRUE)[[1L]][-1L]
  sme_line <- lines[grep("^SME\\t", lines)[[1L]]]
  sme_vals <- strsplit(sme_line, "\t", fixed = TRUE)[[1L]][-1L]

  pos_inchi <- which(seh_cols == "inchi")
  pos_charge <- which(seh_cols == "charge")
  expect_true(length(pos_inchi) == 1L)
  expect_true(length(pos_charge) == 1L)
  expect_true(grepl("^InChI=", sme_vals[pos_inchi]))
  expect_identical(sme_vals[pos_charge], "2")
})

test_that("write_mztab expands summarized pipe-separated candidates into separate SME rows", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "summarized.tsv")
  out <- file.path(tmpdir, "summarized.mztab")

  df <- data.frame(
    feature_id = "F1",
    feature_mz = "250.123",
    feature_rt = "2.0",
    candidate_structure_name = "CmpdA|CmpdB",
    candidate_structure_inchikey_connectivity_layer = "AAAAAAAAAAAAAA|BBBBBBBBBBBBBB",
    candidate_structure_molecular_formula = "C10H12O2|C11H14O2",
    candidate_structure_exact_mass = "164.0837|178.0994",
    candidate_adduct = "[M+H]+|[M+Na]+",
    candidate_library = "spectral|TIMA MS1",
    score_final = "0.92|0.41",
    score_biological = "0.8|0.2",
    score_chemical = "0.7|0.3",
    rank_final = "1|2",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out)
  lines <- readLines(out, warn = FALSE)

  expect_length(grep("^SMF\\t", lines), 1L)
  expect_length(grep("^SME\\t", lines), 2L)
})

test_that("write_mztab exports id_confidence_measure[4] only when spectral similarity exists", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_yes <- file.path(tmpdir, "with_similarity.tsv")
  out_yes <- file.path(tmpdir, "with_similarity.mztab")
  in_no <- file.path(tmpdir, "without_similarity.tsv")
  out_no <- file.path(tmpdir, "without_similarity.mztab")

  df_yes <- data.frame(
    feature_id = "F1",
    feature_mz = "150.1",
    feature_rt = "1.0",
    candidate_structure_name = "Cmpd",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_structure_molecular_formula = "C6H6O",
    candidate_adduct = "[M+H]+",
    candidate_library = "spectral",
    candidate_score_similarity = "0.88",
    score_final = "0.9",
    rank_final = "1",
    stringsAsFactors = FALSE
  )
  write.table(df_yes, in_yes, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_yes, output = out_yes)
  lines_yes <- readLines(out_yes, warn = FALSE)
  mtd_yes <- grep("^MTD\\t", lines_yes, value = TRUE)
  seh_yes <- lines_yes[grep("^SEH\\t", lines_yes)[[1L]]]
  expect_true(any(grepl("^MTD\\tid_confidence_measure\\[4\\]\\t", mtd_yes)))
  expect_true(grepl("id_confidence_measure[4]", seh_yes, fixed = TRUE))

  df_no <- subset(df_yes, select = -candidate_score_similarity)
  write.table(df_no, in_no, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_no, output = out_no)
  lines_no <- readLines(out_no, warn = FALSE)
  mtd_no <- grep("^MTD\\t", lines_no, value = TRUE)
  seh_no <- lines_no[grep("^SEH\\t", lines_no)[[1L]]]
  expect_false(any(grepl("^MTD\\tid_confidence_measure\\[4\\]\\t", mtd_no)))
  expect_false(grepl("id_confidence_measure[4]", seh_no, fixed = TRUE))
})

test_that("write_mztab keeps feature_* optional columns in SMF, not SME", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "feature_opt.tsv")
  out <- file.path(tmpdir, "feature_opt.mztab")

  df <- data.frame(
    feature_id = "F1",
    feature_mz = "120.0",
    feature_rt = "0.5",
    feature_entropy = "0.42",
    candidate_structure_name = "Cmpd",
    candidate_structure_inchikey_connectivity_layer = "ABCDEFGHIJKLMN",
    candidate_adduct = "[M+H]+",
    candidate_library = "spectral",
    candidate_score_custom = "12.3",
    score_final = "0.95",
    rank_final = "1",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out)
  lines <- readLines(out, warn = FALSE)

  sfh <- lines[grep("^SFH\\t", lines)[[1L]]]
  seh <- lines[grep("^SEH\\t", lines)[[1L]]]

  expect_true(grepl("opt_global_feature_entropy", sfh, fixed = TRUE))
  expect_false(grepl("opt_global_feature_entropy", seh, fixed = TRUE))
  expect_true(grepl("opt_global_candidate_score_custom", seh, fixed = TRUE))
})

test_that("write_mztab expands summarized rows even when score_final is absent", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "summarized_no_score_final.tsv")
  out <- file.path(tmpdir, "summarized_no_score_final.mztab")

  df <- data.frame(
    feature_id = "F1",
    feature_mz = "333.3",
    feature_rt = "1.0",
    candidate_structure_name = "Cmpd1|Cmpd2",
    candidate_structure_inchikey_connectivity_layer = "AAAAAA|BBBBBB",
    rank_final = "1|2",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out)
  lines <- readLines(out, warn = FALSE)
  expect_length(grep("^SME\\t", lines), 2L)
})

test_that("write_mztab merge mode preserves unmanaged non-COM lines after managed sections", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  results <- .make_tima_results(tmpdir, n_features = 1L, n_candidates = 1L)
  base <- file.path(tmpdir, "base_unmanaged_tail.mztab")
  out <- file.path(tmpdir, "merged_unmanaged_tail.mztab")

  writeLines(
    c(
      "MTD\tmzTab-version\t2.0.0-M",
      "SMH\tSML_ID\tSMF_ID_REFS\tSME_ID_REFS\tdatabase_identifier",
      "SML\t1\t1\t1\tBASEKEY",
      "CUSTOM\tpost-managed extension row"
    ),
    base
  )

  write_mztab(input = results, output = out, base_mztab = base)
  lines <- readLines(out, warn = FALSE)

  expect_true(any(grepl("^CUSTOM\\tpost-managed extension row$", lines)))
})

test_that("write_mztab creates SME rows from sparse annotation signals beyond name and InChIKey", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "sparse_annotation.tsv")
  out <- file.path(tmpdir, "sparse_annotation.mztab")

  df <- data.frame(
    feature_id = "F1",
    feature_mz = "180.1",
    feature_rt = "1.2",
    candidate_structure_molecular_formula = "C7H8O",
    candidate_adduct = "[M+H]+",
    candidate_library = "TIMA MS1",
    rank_final = "1",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  write_mztab(input = in_path, output = out)
  lines <- readLines(out, warn = FALSE)

  expect_length(grep("^SME\\t", lines), 1L)
})

test_that("write_mztab merge mode avoids false SMF dedup when only opt columns differ", {
  local_test_project(copy = TRUE)

  tmpdir <- withr::local_tempdir()
  in_path <- file.path(tmpdir, "feature_tag.tsv")
  base <- file.path(tmpdir, "base_smf_key.mztab")
  out <- file.path(tmpdir, "merged_smf_key.mztab")

  # New TIMA row has the same mz/rt/charge signature but a distinct opt field.
  df <- data.frame(
    feature_id = "F_NEW",
    feature_mz = "100.0",
    feature_rt = "1.0",
    feature_tag = "B",
    candidate_structure_name = "CmpdB",
    candidate_structure_inchikey_connectivity_layer = "BBBBBBBBBBBBBB",
    candidate_structure_molecular_formula = "C5H10O",
    candidate_adduct = "[M+H]+",
    candidate_library = "spectral",
    score_final = "0.8",
    rank_final = "1",
    stringsAsFactors = FALSE
  )
  write.table(df, in_path, sep = "\t", row.names = FALSE, quote = FALSE)

  writeLines(
    c(
      "MTD\tmzTab-version\t2.0.0-M",
      "SMH\tSML_ID\tSMF_ID_REFS\tSME_ID_REFS\tdatabase_identifier",
      "SML\t1\t1\t1\tBASEKEY",
      "SFH\tSMF_ID\tSML_ID_REFS\texp_mass_to_charge\tcharge\tretention_time_in_seconds\topt_global_feature_tag",
      "SMF\t1\t1\t100.0\tnull\t60\tA",
      "SEH\tSME_ID\tevidence_input_id\tdatabase_identifier\tchemical_formula\tsmiles\tinchi\tchemical_name\turi\tderivatized_form\tadduct_ion\texp_mass_to_charge\tcharge\ttheoretical_mass_to_charge\tspectra_ref\tidentification_method\tms_level\trank",
      "SME\t1\t1\tBASEKEY\tC5H10O\tnull\tnull\tBaseCmpd\tnull\tnull\t[M+H]+\t100.0\tnull\t100.0\tnull\t[, , spectral library matching, ]\t[MS, MS:1000580, MS2 spectrum, ]\t1"
    ),
    base
  )

  write_mztab(input = in_path, output = out, base_mztab = base)
  lines <- readLines(out, warn = FALSE)

  expect_length(grep("^SMF\\t", lines), 2L)
})
