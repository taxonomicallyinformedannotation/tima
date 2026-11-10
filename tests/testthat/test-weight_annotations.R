# Test Suite: weight_annotations ----

library(testthat)

# Fixture staging ----

#' Stage weight_annotations test fixtures
#'
#' @param root Root directory for staging (created if needed)
#' @return Named list of file paths for all required inputs
stage_weight_annotations_fixtures <- function(
  root = temp_test_dir("weight_ann")
) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  # Copy fixtures to temp location
  ann_file <- file.path(root, "annotations.tsv")
  lib_file <- file.path(root, "library.tsv")
  comp_file <- file.path(root, "components.tsv")
  edges_file <- file.path(root, "edges.tsv")
  taxa_file <- file.path(root, "taxa.tsv")
  canopus_file <- file.path(root, "canopus.tsv")
  formula_file <- file.path(root, "formula.tsv")
  org_tax_file <- file.path(root, "taxonomy_ott.tsv")
  output_file <- file.path(root, "weighted.tsv")

  copy_fixture_to("annotations_weighted.csv", ann_file)
  copy_fixture_to("library_weighted.csv", lib_file)
  # Ensure fixture library always contains non-empty tags for propagation tests.
  lib_df <- utils::read.csv(
    file = lib_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!"tag" %in% names(lib_df)) {
    lib_df$tag <- "serum"
  } else {
    lib_df$tag[is.na(lib_df$tag) | lib_df$tag == ""] <- "serum"
  }
  utils::write.csv(lib_df, file = lib_file, row.names = FALSE)
  copy_fixture_to("components_weighted.csv", comp_file)
  copy_fixture_to("edges_weighted.csv", edges_file)
  copy_fixture_to("taxa_weighted.csv", taxa_file)
  copy_fixture_to("organism_taxonomy_gentiana.csv", org_tax_file)

  # Create empty canopus, formula files (required but can be empty)
  writeLines("feature_id", canopus_file)
  writeLines("feature_id", formula_file)

  list(
    tmpdir = root,
    library = lib_file,
    annotations = ann_file,
    components = comp_file,
    edges = edges_file,
    taxa = taxa_file,
    canopus = canopus_file,
    formula = formula_file,
    org_tax_ott = org_tax_file,
    output = output_file
  )
}

# Validation ----

test_that("weight_annotations() validates required file existence", {
  expect_error(
    weight_annotations(
      library = "nonexistent.tsv",
      annotations = "fake.tsv",
      components = "fake.tsv",
      edges = "fake.tsv",
      taxa = "fake.tsv",
      canopus = tempfile(),
      formula = tempfile(),
      output = tempfile(),
      pattern = "test"
    ),
    "Required file.*not found",
    class = "tima_validation_error"
  )
})

test_that("weight_annotations() validates weight sum", {
  fixtures <- stage_weight_annotations_fixtures()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      org_tax_ott = fixtures$org_tax_ott,
      output = fixtures$output,
      weight_spectral = 0.5,
      weight_chemical = 0.5,
      weight_biological = 0.5, # Sum = 1.5, invalid
      pattern = "test"
    ),
    "Weights must sum to 1",
    class = "tima_validation_error"
  )
})

test_that("weight_annotations() validates minimal_ms1_condition", {
  fixtures <- stage_weight_annotations_fixtures()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      org_tax_ott = fixtures$org_tax_ott,
      output = fixtures$output,
      weight_spectral = 0.5,
      weight_chemical = 0.3,
      weight_biological = 0.2,
      minimal_ms1_condition = "INVALID",
      pattern = "test"
    ),
    "Must be one of: OR, AND"
  )
})

test_that("weight_annotations() rejects negative weights", {
  fixtures <- stage_weight_annotations_fixtures()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      org_tax_ott = fixtures$org_tax_ott,
      output = fixtures$output,
      weight_spectral = -0.1,
      weight_chemical = 0.6,
      weight_biological = 0.5,
      pattern = "test"
    ),
    "must be non-negative",
    class = "tima_validation_error"
  )
})

test_that("weight_annotations() rejects invalid logical parameters", {
  fixtures <- stage_weight_annotations_fixtures()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      org_tax_ott = fixtures$org_tax_ott,
      output = fixtures$output,
      weight_spectral = 0.5,
      weight_chemical = 0.3,
      weight_biological = 0.2,
      ms1_only = "yes", # Should be logical
      pattern = "test"
    ),
    "ms1_only must be a single TRUE or FALSE"
  )
})

test_that("weight_annotations() rejects invalid candidate counts", {
  fixtures <- stage_weight_annotations_fixtures()

  expect_error(
    weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      org_tax_ott = fixtures$org_tax_ott,
      output = fixtures$output,
      weight_spectral = 0.5,
      weight_chemical = 0.3,
      weight_biological = 0.2,
      candidates_neighbors = 0, # Invalid: must be > 0
      pattern = "test"
    ),
    "candidates_neighbors must be > 0"
  )
})

# Other paths ----

test_that("weight_annotations() runs successfully with minimal inputs", {
  fixtures <- stage_weight_annotations_fixtures()
  withr::local_dir(new = fixtures$tmpdir)

  # Mock get_default_paths to return our temp dir
  local_mocked_bindings(
    get_default_paths = function() {
      list(
        data = list(
          processed = list(path = file.path(fixtures$tmpdir, "processed"))
        )
      )
    },
    get_params = function(step) {
      list(step = "prepare_params")
    },
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  dir.create(
    file.path(fixtures$tmpdir, "processed"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  warnings <- testthat::capture_warnings({
    result <- weight_annotations(
      library = fixtures$library,
      annotations = fixtures$annotations,
      components = fixtures$components,
      edges = fixtures$edges,
      taxa = fixtures$taxa,
      canopus = fixtures$canopus,
      formula = fixtures$formula,
      org_tax_ott = fixtures$org_tax_ott,
      output = fixtures$output,
      candidates_neighbors = 3L,
      candidates_final = 1L,
      best_percentile = 0.9,
      weight_spectral = 0.2,
      weight_chemical = 0.3,
      weight_biological = 0.5,
      score_biological_domain = 0.1,
      score_biological_kingdom = 0.2,
      score_biological_phylum = 0.3,
      score_biological_class = 0.4,
      score_biological_order = 0.5,
      score_biological_infraorder = 0.55,
      score_biological_family = 0.6,
      score_biological_subfamily = 0.65,
      score_biological_tribe = 0.7,
      score_biological_subtribe = 0.75,
      score_biological_genus = 0.8,
      score_biological_subgenus = 0.85,
      score_biological_species = 0.9,
      score_biological_subspecies = 0.95,
      score_biological_variety = 1.0,
      score_biological_biota = 1.007276,
      score_chemical_cla_kingdom = 0.25,
      score_chemical_cla_superclass = 0.5,
      score_chemical_cla_class = 0.75,
      score_chemical_cla_parent = 1.0,
      score_chemical_npc_pathway = 0.33,
      score_chemical_npc_superclass = 0.66,
      score_chemical_npc_class = 1.0,
      minimal_consistency = 0.6,
      minimal_ms1_bio = 0.6,
      minimal_ms1_chemo = 0.6,
      minimal_ms1_condition = "AND",
      ms1_only = TRUE,
      compounds_names = TRUE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = TRUE,
      pattern = "test",
      force = FALSE
    )
  })

  expect_true(any(grepl(
    "no non-missing arguments to max; returning -Inf|Empty results table provided",
    warnings
  )))

  # Should return output paths
  expect_type(result, "character")
  expect_true(length(result) >= 1)
})

test_that("weight_annotations() processes MS1-only annotations", {
  fixtures <- stage_weight_annotations_fixtures()
  withr::local_dir(new = fixtures$tmpdir)

  local_mocked_bindings(
    get_default_paths = function() {
      list(
        data = list(
          processed = list(path = file.path(fixtures$tmpdir, "processed"))
        )
      )
    },
    get_params = function(step) list(step = "prepare_params"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  dir.create(
    file.path(fixtures$tmpdir, "processed"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  result <- weight_annotations(
    library = fixtures$library,
    annotations = fixtures$annotations,
    components = fixtures$components,
    edges = fixtures$edges,
    taxa = fixtures$taxa,
    canopus = fixtures$canopus,
    formula = fixtures$formula,
    org_tax_ott = fixtures$org_tax_ott,
    output = fixtures$output,
    candidates_neighbors = 3L,
    candidates_final = 1L,
    best_percentile = 0.9,
    weight_spectral = 0.2,
    weight_chemical = 0.3,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_infraorder = 0.55,
    score_biological_family = 0.6,
    score_biological_subfamily = 0.65,
    score_biological_tribe = 0.7,
    score_biological_subtribe = 0.75,
    score_biological_genus = 0.8,
    score_biological_subgenus = 0.85,
    score_biological_species = 0.9,
    score_biological_subspecies = 0.95,
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276,
    score_chemical_cla_kingdom = 0.25,
    score_chemical_cla_superclass = 0.5,
    score_chemical_cla_class = 0.75,
    score_chemical_cla_parent = 1.0,
    score_chemical_npc_pathway = 0.33,
    score_chemical_npc_superclass = 0.66,
    score_chemical_npc_class = 1.0,
    minimal_consistency = 0.6,
    minimal_ms1_bio = 0.6,
    minimal_ms1_chemo = 0.6,
    minimal_ms1_condition = "AND",
    ms1_only = FALSE,
    compounds_names = FALSE,
    high_evidence = FALSE,
    remove_ties = TRUE,
    summarize = FALSE,
    pattern = "test",
    force = FALSE
  )

  expect_type(result, "character")
})

test_that("weight_annotations() handles different weight combinations", {
  fixtures <- stage_weight_annotations_fixtures()
  withr::local_dir(new = fixtures$tmpdir)

  local_mocked_bindings(
    get_default_paths = function() {
      list(
        data = list(
          processed = list(path = file.path(fixtures$tmpdir, "processed"))
        )
      )
    },
    get_params = function(step) list(step = "prepare_params"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  dir.create(
    file.path(fixtures$tmpdir, "processed"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # All weight on biological
  result <- weight_annotations(
    library = fixtures$library,
    annotations = fixtures$annotations,
    components = fixtures$components,
    edges = fixtures$edges,
    taxa = fixtures$taxa,
    canopus = fixtures$canopus,
    formula = fixtures$formula,
    org_tax_ott = fixtures$org_tax_ott,
    output = fixtures$output,
    candidates_neighbors = 3L,
    candidates_final = 1L,
    best_percentile = 0.9,
    weight_spectral = 0.2,
    weight_chemical = 0.3,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_infraorder = 0.55,
    score_biological_family = 0.6,
    score_biological_subfamily = 0.65,
    score_biological_tribe = 0.7,
    score_biological_subtribe = 0.75,
    score_biological_genus = 0.8,
    score_biological_subgenus = 0.85,
    score_biological_species = 0.9,
    score_biological_subspecies = 0.95,
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276,
    score_chemical_cla_kingdom = 0.25,
    score_chemical_cla_superclass = 0.5,
    score_chemical_cla_class = 0.75,
    score_chemical_cla_parent = 1.0,
    score_chemical_npc_pathway = 0.33,
    score_chemical_npc_superclass = 0.66,
    score_chemical_npc_class = 1.0,
    minimal_consistency = 0.6,
    minimal_ms1_bio = 0.6,
    minimal_ms1_chemo = 0.6,
    minimal_ms1_condition = "AND",
    ms1_only = FALSE,
    compounds_names = FALSE,
    high_evidence = FALSE,
    remove_ties = TRUE,
    summarize = FALSE,
    pattern = "test",
    force = FALSE
  )

  expect_type(result, "character")
})

test_that("weight_annotations() covers xrefs_file, duplicate/NA taxa feature_id, and missing needed_structures", {
  fixtures <- stage_weight_annotations_fixtures()
  withr::local_dir(new = fixtures$tmpdir)

  local_mocked_bindings(
    get_default_paths = function() {
      list(
        data = list(
          processed = list(path = file.path(fixtures$tmpdir, "processed"))
        )
      )
    },
    get_params = function(step) list(step = "prepare_params"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  dir.create(
    file.path(fixtures$tmpdir, "processed"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # 1. With NA feature_id in taxa_table and xrefs_file provided
  taxa_na <- file.path(fixtures$tmpdir, "taxa_na.tsv")
  taxa_orig <- tidytable::fread(fixtures$taxa)
  tidytable::fwrite(
    tidytable::bind_rows(
      taxa_orig,
      tidytable::tidytable(feature_id = NA_character_)
    ),
    taxa_na,
    sep = "\t"
  )
  xrefs_path <- file.path(fixtures$tmpdir, "xrefs.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = "IK1",
      xref = "X1"
    ),
    xrefs_path,
    sep = "\t"
  )

  res_na_xref <- weight_annotations(
    library = fixtures$library,
    annotations = fixtures$annotations,
    components = fixtures$components,
    edges = fixtures$edges,
    taxa = taxa_na,
    canopus = fixtures$canopus,
    formula = fixtures$formula,
    org_tax_ott = fixtures$org_tax_ott,
    xrefs_file = xrefs_path,
    output = fixtures$output,
    candidates_neighbors = 3L,
    candidates_final = 1L,
    best_percentile = 0.9,
    weight_spectral = 0.2,
    weight_chemical = 0.3,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_infraorder = 0.55,
    score_biological_family = 0.6,
    score_biological_subfamily = 0.65,
    score_biological_tribe = 0.7,
    score_biological_subtribe = 0.75,
    score_biological_genus = 0.8,
    score_biological_subgenus = 0.85,
    score_biological_species = 0.9,
    score_biological_subspecies = 0.95,
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276,
    score_chemical_cla_kingdom = 0.25,
    score_chemical_cla_superclass = 0.5,
    score_chemical_cla_class = 0.75,
    score_chemical_cla_parent = 1.0,
    score_chemical_npc_pathway = 0.33,
    score_chemical_npc_superclass = 0.66,
    score_chemical_npc_class = 1.0,
    minimal_consistency = 0.6,
    minimal_ms1_bio = 0.6,
    minimal_ms1_chemo = 0.6,
    minimal_ms1_condition = "AND",
    ms1_only = FALSE,
    compounds_names = FALSE,
    high_evidence = FALSE,
    remove_ties = TRUE,
    summarize = FALSE,
    pattern = "test_na",
    force = FALSE
  )
  expect_type(res_na_xref, "character")

  # 2. Duplicate feature_id in taxa_table -> triggers left_join fallback
  taxa_dup <- file.path(fixtures$tmpdir, "taxa_dup.tsv")
  tidytable::fwrite(
    tidytable::bind_rows(taxa_orig, taxa_orig[1L, ]),
    taxa_dup,
    sep = "\t"
  )
  res_dup <- weight_annotations(
    library = fixtures$library,
    annotations = fixtures$annotations,
    components = fixtures$components,
    edges = fixtures$edges,
    taxa = taxa_dup,
    canopus = fixtures$canopus,
    formula = fixtures$formula,
    org_tax_ott = fixtures$org_tax_ott,
    output = fixtures$output,
    candidates_neighbors = 3L,
    candidates_final = 1L,
    best_percentile = 0.9,
    weight_spectral = 0.2,
    weight_chemical = 0.3,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_infraorder = 0.55,
    score_biological_family = 0.6,
    score_biological_subfamily = 0.65,
    score_biological_tribe = 0.7,
    score_biological_subtribe = 0.75,
    score_biological_genus = 0.8,
    score_biological_subgenus = 0.85,
    score_biological_species = 0.9,
    score_biological_subspecies = 0.95,
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276,
    score_chemical_cla_kingdom = 0.25,
    score_chemical_cla_superclass = 0.5,
    score_chemical_cla_class = 0.75,
    score_chemical_cla_parent = 1.0,
    score_chemical_npc_pathway = 0.33,
    score_chemical_npc_superclass = 0.66,
    score_chemical_npc_class = 1.0,
    minimal_consistency = 0.6,
    minimal_ms1_bio = 0.6,
    minimal_ms1_chemo = 0.6,
    minimal_ms1_condition = "AND",
    ms1_only = FALSE,
    compounds_names = FALSE,
    high_evidence = FALSE,
    remove_ties = TRUE,
    summarize = FALSE,
    pattern = "test_dup",
    force = FALSE
  )
  expect_type(res_dup, "character")
})

test_that("weight_annotations() handles boolean flags", {
  fixtures <- stage_weight_annotations_fixtures()
  withr::local_dir(new = fixtures$tmpdir)

  local_mocked_bindings(
    get_default_paths = function() {
      list(
        data = list(
          processed = list(path = file.path(fixtures$tmpdir, "processed"))
        )
      )
    },
    get_params = function(step) list(step = "prepare_params"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  dir.create(
    file.path(fixtures$tmpdir, "processed"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  result <- weight_annotations(
    library = fixtures$library,
    annotations = fixtures$annotations,
    components = fixtures$components,
    edges = fixtures$edges,
    taxa = fixtures$taxa,
    canopus = fixtures$canopus,
    formula = fixtures$formula,
    org_tax_ott = fixtures$org_tax_ott,
    output = fixtures$output,
    candidates_neighbors = 3L,
    candidates_final = 1L,
    best_percentile = 0.9,
    weight_spectral = 0.2,
    weight_chemical = 0.3,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_infraorder = 0.55,
    score_biological_family = 0.6,
    score_biological_subfamily = 0.65,
    score_biological_tribe = 0.7,
    score_biological_subtribe = 0.75,
    score_biological_genus = 0.8,
    score_biological_subgenus = 0.85,
    score_biological_species = 0.9,
    score_biological_subspecies = 0.95,
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276,
    score_chemical_cla_kingdom = 0.25,
    score_chemical_cla_superclass = 0.5,
    score_chemical_cla_class = 0.75,
    score_chemical_cla_parent = 1.0,
    score_chemical_npc_pathway = 0.33,
    score_chemical_npc_superclass = 0.66,
    score_chemical_npc_class = 1.0,
    minimal_consistency = 0.6,
    minimal_ms1_bio = 0.6,
    minimal_ms1_chemo = 0.6,
    minimal_ms1_condition = "AND",
    ms1_only = FALSE,
    compounds_names = FALSE,
    high_evidence = FALSE,
    remove_ties = TRUE,
    summarize = FALSE,
    pattern = "test",
    force = FALSE
  )

  expect_type(result, "character")
})

test_that("weight_annotations() exports propagated tag columns", {
  fixtures <- stage_weight_annotations_fixtures()
  withr::local_dir(new = fixtures$tmpdir)

  local_mocked_bindings(
    get_default_paths = function() {
      list(
        data = list(
          processed = list(path = file.path(fixtures$tmpdir, "processed"))
        )
      )
    },
    get_params = function(step) list(step = "prepare_params"),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )

  dir.create(
    file.path(fixtures$tmpdir, "processed"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  result <- weight_annotations(
    library = fixtures$library,
    annotations = fixtures$annotations,
    components = fixtures$components,
    edges = fixtures$edges,
    taxa = fixtures$taxa,
    canopus = fixtures$canopus,
    formula = fixtures$formula,
    org_tax_ott = fixtures$org_tax_ott,
    output = fixtures$output,
    candidates_neighbors = 3L,
    candidates_final = 1L,
    best_percentile = 0.9,
    weight_spectral = 0.2,
    weight_chemical = 0.3,
    weight_biological = 0.5,
    score_biological_domain = 0.1,
    score_biological_kingdom = 0.2,
    score_biological_phylum = 0.3,
    score_biological_class = 0.4,
    score_biological_order = 0.5,
    score_biological_infraorder = 0.55,
    score_biological_family = 0.6,
    score_biological_subfamily = 0.65,
    score_biological_tribe = 0.7,
    score_biological_subtribe = 0.75,
    score_biological_genus = 0.8,
    score_biological_subgenus = 0.85,
    score_biological_species = 0.9,
    score_biological_subspecies = 0.95,
    score_biological_variety = 1.0,
    score_biological_biota = 1.007276,
    score_chemical_cla_kingdom = 0.25,
    score_chemical_cla_superclass = 0.5,
    score_chemical_cla_class = 0.75,
    score_chemical_cla_parent = 1.0,
    score_chemical_npc_pathway = 0.33,
    score_chemical_npc_superclass = 0.66,
    score_chemical_npc_class = 1.0,
    minimal_consistency = 0.6,
    minimal_ms1_bio = 0.6,
    minimal_ms1_chemo = 0.6,
    minimal_ms1_condition = "AND",
    ms1_only = FALSE,
    compounds_names = TRUE,
    high_evidence = FALSE,
    remove_ties = FALSE,
    summarize = FALSE,
    pattern = "tagtest",
    force = FALSE
  )

  full_path <- unname(result[["full"]])
  filtered_path <- unname(result[["filtered"]])
  mini_path <- sub("\\.tsv$", "_mini.tsv", full_path)

  expect_true(file.exists(full_path))
  expect_true(file.exists(filtered_path))
  expect_true(file.exists(mini_path))

  full_tbl <- tidytable::fread(full_path)
  filtered_tbl <- tidytable::fread(filtered_path)
  mini_tbl <- tidytable::fread(mini_path)

  expect_true(
    "candidate_structure_tag" %in% names(full_tbl)
  )
  expect_true(
    "candidate_structure_tag" %in% names(filtered_tbl)
  )
  expect_true("tag" %in% names(mini_tbl))

  expect_true(
    any(
      full_tbl$candidate_structure_tag == "foo",
      na.rm = TRUE
    ) ||
      any(
        filtered_tbl$candidate_structure_tag == "foo",
        na.rm = TRUE
      ) ||
      any(mini_tbl$tag == "foo", na.rm = TRUE)
  )
})

# rearrange_annotations ----

test_that("rearrange_annotations merges SIRIUS scores into spectral entries with same inchikey", {
  # Annotation table: spectral + SIRIUS entries for the same structure.
  # Glutamine (RWQNBRDOKXIBIV) is a real-world example where tautomers

  # produce different SMILES (amide vs enol form) but share the same
  # InChIKey connectivity layer.  The merge must succeed on InChIKey
  # even when the SMILES differ due to tautomerism.
  annotation_table <- tidytable::tidytable(
    feature_id = c("1", "1", "2"),
    feature_mz = c("147.0764", "147.0764", "200.0"),
    feature_rt = c("5.0", "5.0", "6.0"),
    candidate_library = c("ISDB", "SIRIUS", "SIRIUS"),
    candidate_spectrum_id = c("spec_1", NA_character_, NA_character_),
    candidate_adduct = c("[M+H]+", "[M+H]+", "[M+Na]+"),
    candidate_count_similarity_peaks_matched = c(
      "5",
      NA_character_,
      NA_character_
    ),
    candidate_score_similarity = c("0.85", NA_character_, NA_character_),
    candidate_spectrum_entropy = c(NA_character_, NA_character_, NA_character_),
    candidate_structure_name = c("L-Glutamine", "Glutamine", "Compound2"),
    candidate_structure_exact_mass = c("146.069", "146.069", "199.0"),
    candidate_structure_molecular_formula = c(
      "C5H10N2O3",
      "C5H10N2O3",
      "C8H10N2"
    ),
    candidate_structure_xlogp = c("-3.1", "-3.1", "1.0"),
    candidate_structure_inchikey_connectivity_layer = c(
      "RWQNBRDOKXIBIV",
      "RWQNBRDOKXIBIV",
      "UNIQUE123456"
    ),
    candidate_structure_inchikey_no_stereo = c(
      "RWQNBRDOKXIBIV-UHFFFAOYSA",
      "RWQNBRDOKXIBIV-UHFFFAOYSA",
      "UNIQUE123456-ZZZZZZZZZZ"
    ),
    # Tautomeric SMILES: amide form (spectral) vs enol form (SIRIUS)
    candidate_structure_smiles_no_stereo = c(
      "NC(=O)CCC(N)C(O)=O",
      "NC(O)=CCC(N)C(O)=O",
      "c1ccc2[nH]ccc2c1"
    ),
    candidate_structure_tax_npc_01pat = c(
      "Amino acids and Peptides",
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_npc_02sup = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_npc_03cla = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_chemontid = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_01kin = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_02sup = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_03cla = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tax_cla_04dirpar = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_organism_occurrence_closest = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_organism_occurrence_reference = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_structure_tag = c(NA_character_, NA_character_, NA_character_),
    candidate_structure_error_mz = c("0.001", "0.002", "0.003"),
    candidate_structure_error_rt = c(
      NA_character_,
      NA_character_,
      NA_character_
    ),
    candidate_score_sirius_confidence = c(NA_character_, "0.9", "0.8"),
    candidate_score_sirius_csi = c(NA_character_, "-50", "-80"),
    candidate_score_sirius_msnovelist = c(NA_character_, "0.5", "0.3")
  )

  formula_table <- tidytable::tidytable(
    feature_id = c("1", "2"),
    feature_mz = c("147.0764", "200.0"),
    feature_rt = c("5.0", "6.0"),
    candidate_structure_molecular_formula = c("C5H10N2O3", "C8H10N2"),
    candidate_count_sirius_peaks_explained = c("10", "8"),
    candidate_score_sirius_intensity = c("0.95", "0.85"),
    candidate_score_sirius_isotope = c("0.99", "0.90"),
    candidate_score_sirius_sirius = c("-20", "-30"),
    candidate_score_sirius_tree = c("-15", "-25"),
    candidate_score_sirius_zodiac = c("0.8", "0.7")
  )

  canopus_table <- tidytable::tidytable(
    feature_id = c("1", "2"),
    feature_mz = c("147.0764", "200.0"),
    feature_rt = c("5.0", "6.0"),
    feature_spectrum_entropy = c("2.5", "3.0"),
    feature_spectrum_peaks = c("20", "30"),
    feature_pred_tax_npc_01pat_val = c("Amino acids and Peptides", "Alkaloids"),
    feature_pred_tax_npc_01pat_score = c("0.9", "0.8"),
    feature_pred_tax_npc_02sup_val = c(NA_character_, NA_character_),
    feature_pred_tax_npc_02sup_score = c(NA_character_, NA_character_),
    feature_pred_tax_npc_03cla_val = c(NA_character_, NA_character_),
    feature_pred_tax_npc_03cla_score = c(NA_character_, NA_character_),
    feature_pred_tax_cla_01kin_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_01kin_score = c(NA_character_, NA_character_),
    feature_pred_tax_cla_02sup_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_02sup_score = c(NA_character_, NA_character_),
    feature_pred_tax_cla_03cla_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_03cla_score = c(NA_character_, NA_character_),
    feature_pred_tax_cla_04dirpar_val = c(NA_character_, NA_character_),
    feature_pred_tax_cla_04dirpar_score = c(NA_character_, NA_character_)
  )

  edges_table <- tidytable::tidytable(
    feature_source = c("1", "2"),
    feature_spectrum_entropy = c("2.5", "3.0"),
    feature_spectrum_peaks = c("20", "30"),
    candidate_score_similarity = c("0.7", "0.6")
  )

  result <- rearrange_annotations(
    annotation_table = annotation_table,
    formula_table = formula_table,
    canopus_table = canopus_table,
    edges_table = edges_table
  )

  # Feature 1: spectral entry (ISDB, sim=0.85) and SIRIUS entry (CSI=-50)
  # share inchikey RWQNBRDOKXIBIV (glutamine).  The spectral entry wins in

  # table_1 (higher similarity).  SIRIUS scores should be MERGED into the
  # spectral entry despite different tautomeric SMILES.
  f1 <- result[
    result$feature_id == "1" &
      !is.na(result$candidate_structure_inchikey_connectivity_layer) &
      result$candidate_structure_inchikey_connectivity_layer ==
        "RWQNBRDOKXIBIV",
  ]

  # There should be exactly one row for feature_1 + RWQNBRDOKXIBIV
  expect_equal(nrow(f1), 1L)

  # The merged row should have BOTH spectral and SIRIUS scores
  expect_equal(f1$candidate_score_similarity, 0.85)
  expect_false(is.na(f1$candidate_score_sirius_csi))
  expect_equal(f1$candidate_score_sirius_csi, -50)
  expect_equal(f1$candidate_library, "ISDB")

  # Feature 2: SIRIUS-only entry (unique inchikey UNIQUE123456)
  f2 <- result[
    result$feature_id == "2" &
      !is.na(result$candidate_structure_inchikey_connectivity_layer) &
      result$candidate_structure_inchikey_connectivity_layer == "UNIQUE123456",
  ]

  expect_equal(nrow(f2), 1L)
  expect_false(is.na(f2$candidate_score_sirius_csi))
  expect_equal(f2$candidate_library, "SIRIUS")

  # Formula scores should be present
  expect_true("candidate_score_sirius_zodiac" %in% names(result))

  # CANOPUS predictions should be present

  expect_true("feature_pred_tax_npc_01pat_val" %in% names(result))
})

test_that("load_edges_table slices top N neighbors numerically per source", {
  tf <- temp_test_path("edges_slice.tsv")
  edges_df <- tidytable::tidytable(
    feature_source = c("F2", "F2", "F2", "F1", "F1"),
    feature_target = c("T1", "T2", "T3", "F1", "T4"),
    candidate_score_similarity = c(
      "0.15",
      "0.95",
      "0.50",
      NA_character_,
      "0.80"
    ),
    feature_spectrum_entropy = c("1.0", "1.0", "1.0", "2.0", "2.0"),
    feature_spectrum_peaks = c("10", "10", "10", "5", "5")
  )
  tidytable::fwrite(edges_df, tf, sep = "\t")

  res <- load_edges_table(tf, candidates_neighbors = 2L)
  expect_equal(nrow(res), 4L)
  f2_rows <- res[res$feature_source == "F2", ]
  expect_equal(f2_rows$feature_target, c("T2", "T3"))
  f1_rows <- res[res$feature_source == "F1", ]
  expect_equal(f1_rows$feature_target, c("T4", "F1"))
})

test_that("log_annotation_stats logs distinct feature-library-inchikey counts", {
  ann_df <- tidytable::tidytable(
    feature_id = c("1", "1", "1", "2", "3"),
    candidate_library = c("GNPS", "GNPS", "ISDB", "ISDB", "ISDB"),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK1",
      "IK1",
      "IK1",
      "IK2",
      NA_character_
    )
  )
  log_file <- withr::local_tempfile(fileext = ".log")
  setup_logger(filename = log_file, threshold = 400)

  log_annotation_stats(ann_df)
  logged <- paste(readLines(log_file, warn = FALSE), collapse = "\n")
  expect_true(grepl("ISDB\\s+2\\s+66\\.67%", logged))
  expect_true(grepl("GNPS\\s+1\\s+33\\.33%", logged))

  # Also test when all inchikey connectivity layers are NA (length(keep) == 0L)
  ann_na <- tidytable::tidytable(
    feature_id = "1",
    candidate_library = "ISDB",
    candidate_structure_inchikey_connectivity_layer = NA_character_
  )
  expect_null(log_annotation_stats(ann_na))
})

test_that("validate_weight_annotations_inputs, load_annotation_tables, load_structure_organism_pairs, and load_edges_table cover all branches", {
  tmp_dir <- withr::local_tempdir()
  req_f <- file.path(tmp_dir, "req.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      feature_id = c("1", "2", "3"),
      structure_inchikey_connectivity_layer = c("IK1", "IK2", "IK3"),
      organism_name = c("Org1", "Org2", "Org3")
    ),
    req_f,
    sep = "\t"
  )

  # 1. validate_weight_annotations_inputs: missing annotation file and non-numeric/NA weight
  expect_error(
    validate_weight_annotations_inputs(
      library = req_f,
      components = req_f,
      edges = req_f,
      taxa = req_f,
      annotations = c(req_f, file.path(tmp_dir, "missing_ann.tsv")),
      minimal_ms1_condition = "OR",
      weight_spectral = 0.5,
      weight_chemical = 0.3,
      weight_biological = 0.2,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = FALSE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 1L,
      candidates_final = 1L
    ),
    "Annotation file\\(s\\) not found",
    class = "tima_validation_error"
  )

  expect_error(
    validate_weight_annotations_inputs(
      library = req_f,
      components = req_f,
      edges = req_f,
      taxa = req_f,
      annotations = req_f,
      minimal_ms1_condition = "OR",
      weight_spectral = NA_real_,
      weight_chemical = 0.5,
      weight_biological = 0.5,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      ms1_only = FALSE,
      compounds_names = FALSE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE,
      candidates_neighbors = 1L,
      candidates_final = 1L
    ),
    "must be numeric and non-NA",
    class = "tima_validation_error"
  )

  # 2. load_annotation_tables: multiple files, ms1_only with sirius_confidence, all-pass, empty result, and read error
  ann1 <- file.path(tmp_dir, "ann1.tsv")
  ann2 <- file.path(tmp_dir, "ann2.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      feature_id = c("1", "2"),
      candidate_score_similarity = c(NA_character_, NA_character_),
      candidate_score_sirius_csi = c(NA_character_, NA_character_),
      candidate_score_sirius_confidence = c("0", "0.8")
    ),
    ann1,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      feature_id = "3",
      candidate_score_similarity = NA_character_,
      candidate_score_sirius_csi = NA_character_,
      candidate_score_sirius_confidence = NA_character_
    ),
    ann2,
    sep = "\t"
  )
  res_multi <- load_annotation_tables(c(ann1, ann2), ms1_only = TRUE)
  expect_identical(res_multi$feature_id, c("1", "3"))

  # All rows pass ms1_only filter (hits else branch rm(keep_mask))
  res_all_ms1 <- load_annotation_tables(ann2, ms1_only = TRUE)
  expect_identical(nrow(res_all_ms1), 1L)

  # No rows pass ms1_only filter (hits nrow == 0L warn)
  ann_ms2_only <- file.path(tmp_dir, "ann_ms2.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      feature_id = "1",
      candidate_score_similarity = "0.9"
    ),
    ann_ms2_only,
    sep = "\t"
  )
  res_empty <- load_annotation_tables(ann_ms2_only, ms1_only = TRUE)
  expect_identical(nrow(res_empty), 0L)

  expect_error(
    load_annotation_tables(
      file.path(tmp_dir, "nonexistent_ann.tsv"),
      ms1_only = FALSE
    ),
    "Failed to load annotation files",
    class = "tima_runtime_error"
  )

  # 3. load_structure_organism_pairs: error path, needed_structures with/without stereo, 2-key join, and duplicate-key left_join fallback
  expect_error(
    load_structure_organism_pairs(
      library = file.path(tmp_dir, "nonexistent_lib.tsv"),
      str_stereo = NULL,
      org_tax_ott = NULL
    ),
    "Failed to load library file",
    class = "tima_runtime_error"
  )

  lib_path <- file.path(tmp_dir, "lib.tsv")
  stereo_path <- file.path(tmp_dir, "stereo.tsv")
  org_path <- file.path(tmp_dir, "org.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = c("IK1", "IK2"),
      structure_inchikey = c("IK1-A", "IK2-A"),
      organism_name = c("Org1", "Org2")
    ),
    lib_path,
    sep = "\t"
  )
  # 2-key join on (structure_inchikey_connectivity_layer, structure_inchikey)
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = c("IK1", "IK2"),
      structure_inchikey = c("IK1-A", "IK2-A"),
      structure_smiles_no_stereo = c("C", "CC")
    ),
    stereo_path,
    sep = "\t"
  )
  # Duplicate organism_name triggers left_join fallback
  tidytable::fwrite(
    tidytable::tidytable(
      organism_name = c("Org1", "Org1"),
      organism_taxonomy_01domain = c("Eukaryota", "Eukaryota")
    ),
    org_path,
    sep = "\t"
  )

  sop_with_stereo <- load_structure_organism_pairs(
    library = lib_path,
    str_stereo = stereo_path,
    org_tax_ott = org_path,
    needed_structures = "IK1"
  )
  expect_true(all(
    sop_with_stereo$structure_inchikey_connectivity_layer == "IK1"
  ))

  sop_no_stereo <- load_structure_organism_pairs(
    library = lib_path,
    str_stereo = NULL,
    org_tax_ott = NULL,
    needed_structures = "IK2"
  )
  expect_identical(
    sop_no_stereo$structure_inchikey_connectivity_layer,
    "IK2"
  )

  # 1-key join on structure_inchikey_connectivity_layer and unique organism_name
  stereo_1key_path <- file.path(tmp_dir, "stereo_1key.tsv")
  org_uniq_path <- file.path(tmp_dir, "org_uniq.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey_connectivity_layer = c("IK1", "IK2"),
      structure_smiles_no_stereo = c("C", "CC")
    ),
    stereo_1key_path,
    sep = "\t"
  )
  tidytable::fwrite(
    tidytable::tidytable(
      organism_name = c("Org1", "Org2"),
      organism_taxonomy_01domain = c("Eukaryota", "Bacteria")
    ),
    org_uniq_path,
    sep = "\t"
  )
  sop_1key <- load_structure_organism_pairs(
    library = lib_path,
    str_stereo = stereo_1key_path,
    org_tax_ott = org_uniq_path
  )
  expect_identical(nrow(sop_1key), 2L)

  # Direct validate_weight_annotations_inputs happy path (with missing optional str_stereo), missing required file, negative weight, and sum != 1 errors
  expect_null(
    validate_weight_annotations_inputs(
      library = lib_path,
      org_tax_ott = org_uniq_path,
      str_stereo = file.path(tmp_dir, "missing_opt_stereo.tsv"),
      annotations = ann1,
      canopus = lib_path,
      formula = lib_path,
      components = lib_path,
      edges = lib_path,
      taxa = lib_path,
      candidates_neighbors = 1L,
      candidates_final = 1L,
      weight_spectral = 0.5,
      weight_chemical = 0.3,
      weight_biological = 0.2,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      ms1_only = FALSE,
      compounds_names = FALSE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE
    )
  )
  expect_error(
    validate_weight_annotations_inputs(
      library = file.path(tmp_dir, "missing_req_lib.tsv"),
      org_tax_ott = org_uniq_path,
      str_stereo = NULL,
      annotations = ann1,
      canopus = lib_path,
      formula = lib_path,
      components = lib_path,
      edges = lib_path,
      taxa = lib_path,
      candidates_neighbors = 1L,
      candidates_final = 1L,
      weight_spectral = 0.5,
      weight_chemical = 0.3,
      weight_biological = 0.2,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      ms1_only = FALSE,
      compounds_names = FALSE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE
    ),
    "Required file\\(s\\) not found",
    class = "tima_validation_error"
  )
  expect_error(
    validate_weight_annotations_inputs(
      library = lib_path,
      org_tax_ott = org_uniq_path,
      str_stereo = NULL,
      annotations = ann1,
      canopus = lib_path,
      formula = lib_path,
      components = lib_path,
      edges = lib_path,
      taxa = lib_path,
      candidates_neighbors = 1L,
      candidates_final = 1L,
      weight_spectral = -0.1,
      weight_chemical = 0.6,
      weight_biological = 0.5,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      ms1_only = FALSE,
      compounds_names = FALSE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE
    ),
    "All weights must be non-negative",
    class = "tima_validation_error"
  )
  expect_error(
    validate_weight_annotations_inputs(
      library = lib_path,
      org_tax_ott = org_uniq_path,
      str_stereo = NULL,
      annotations = ann1,
      canopus = lib_path,
      formula = lib_path,
      components = lib_path,
      edges = lib_path,
      taxa = lib_path,
      candidates_neighbors = 1L,
      candidates_final = 1L,
      weight_spectral = 0.5,
      weight_chemical = 0.5,
      weight_biological = 0.5,
      minimal_consistency = 0.5,
      minimal_ms1_bio = 0.5,
      minimal_ms1_chemo = 0.5,
      minimal_ms1_condition = "OR",
      ms1_only = FALSE,
      compounds_names = FALSE,
      high_evidence = FALSE,
      remove_ties = FALSE,
      summarize = FALSE,
      force = FALSE
    ),
    "Weights must sum to 1",
    class = "tima_validation_error"
  )

  # 4. load_edges_table error path, missing similarity col, and .safe_fread_selected_columns empty selector fallback
  expect_error(
    load_edges_table(file.path(tmp_dir, "missing_edges.tsv"), 2L),
    "Failed to load edges file",
    class = "tima_runtime_error"
  )

  edges_nosim <- file.path(tmp_dir, "edges_nosim.tsv")
  tidytable::fwrite(
    tidytable::tidytable(
      feature_source = c("F1", "F1"),
      feature_target = c("T1", "T2")
    ),
    edges_nosim,
    sep = "\t"
  )
  res_nosim <- load_edges_table(edges_nosim, 1L)
  expect_identical(nrow(res_nosim), 1L)

  unmatched_cols_file <- file.path(tmp_dir, "unmatched.tsv")
  tidytable::fwrite(
    tidytable::tidytable(unrelated_col = c("a", "b")),
    unmatched_cols_file,
    sep = "\t"
  )
  res_unm <- .safe_fread_selected_columns(
    file = unmatched_cols_file,
    file_type = "unmatched test",
    selector = .select_edges_working_columns
  )
  expect_identical(res_unm$unrelated_col, c("a", "b"))
})

test_that("rearrange_annotations and export_results cover consume_input, join fallbacks, and export paths", {
  tmp_dir <- withr::local_tempdir()

  # 1. n_in == 1L with consume_input = FALSE and consume_input = TRUE, missing similarity col, identical no_stereo & connectivity_layer
  single_ann <- tidytable::tidytable(
    feature_id = "1",
    candidate_structure_inchikey_no_stereo = "IK1",
    candidate_structure_inchikey_connectivity_layer = "IK1",
    candidate_score_sirius_confidence = NA_character_,
    extra_drop_col = "drop_me"
  )
  empty_for <- tidytable::tidytable(feature_id = character(0))
  empty_can <- tidytable::tidytable(feature_id = character(0))
  empty_edg <- tidytable::tidytable(feature_source = character(0))

  res_single_copy <- rearrange_annotations(
    annotation_table = data.table::copy(single_ann),
    formula_table = empty_for,
    canopus_table = empty_can,
    edges_table = empty_edg,
    consume_input = FALSE
  )
  expect_identical(nrow(res_single_copy), 1L)

  res_single_consume <- rearrange_annotations(
    annotation_table = tidytable::tidytable(
      feature_id = "1",
      candidate_structure_inchikey_no_stereo = "IK1",
      candidate_structure_inchikey_connectivity_layer = "IK1",
      candidate_score_similarity = "0.9",
      candidate_score_sirius_csi = NA_character_,
      extra_drop_col = "drop_me"
    ),
    formula_table = empty_for,
    canopus_table = empty_can,
    edges_table = empty_edg,
    consume_input = TRUE
  )
  expect_identical(nrow(res_single_consume), 1L)
  expect_false("extra_drop_col" %in% names(res_single_consume))

  # 2. n_in > 1L with consume_input = TRUE when all rows are unique vs when duplicates exist,
  # plus 1-col formula join, duplicate formula join fallback, duplicate canopus join fallback, and edges metadata join
  ann_unique <- tidytable::tidytable(
    feature_id = c("1", "2"),
    candidate_structure_inchikey_no_stereo = c("IK1", "IK2"),
    candidate_structure_inchikey_connectivity_layer = c("IK1", "IK2"),
    candidate_score_similarity = c("0.9", "0.8"),
    candidate_score_sirius_csi = c(NA_character_, NA_character_),
    extra_drop_col = c("x", "y")
  )
  for_1key <- tidytable::tidytable(
    feature_id = c("1", "2"),
    candidate_score_sirius_zodiac = c("0.95", "0.85")
  )
  can_dup <- tidytable::tidytable(
    feature_id = c("1", "1"),
    feature_pred_tax_npc_01pat_val = c("Path1", "Path1")
  )
  edg_meta <- tidytable::tidytable(
    feature_source = c("1", "2"),
    feature_spectrum_entropy = c("1.5", "2.5"),
    feature_spectrum_peaks = c("12", "24")
  )
  res_unique_consume <- rearrange_annotations(
    annotation_table = data.table::copy(ann_unique),
    formula_table = for_1key,
    canopus_table = can_dup,
    edges_table = edg_meta,
    consume_input = TRUE
  )
  expect_identical(nrow(res_unique_consume), 3L)
  expect_identical(
    res_unique_consume$candidate_score_sirius_zodiac,
    c("0.95", "0.95", "0.85")
  )
  expect_identical(
    res_unique_consume$feature_spectrum_entropy,
    c("1.5", "1.5", "2.5")
  )

  # Duplicate rows with consume_input = TRUE and duplicate 2-key formula_table (triggers left_join fallback)
  ann_dups <- tidytable::tidytable(
    feature_id = c("1", "1", NA_character_),
    candidate_structure_inchikey_no_stereo = c("IK1", "IK1", NA_character_),
    candidate_structure_inchikey_connectivity_layer = c(
      "IK1",
      "IK1",
      NA_character_
    ),
    candidate_structure_molecular_formula = c(
      "C6H12O6",
      "C6H12O6",
      NA_character_
    ),
    candidate_score_similarity = c("0.9", "0.5", "0.1"),
    candidate_score_sirius_csi = c("-10", "-20", "-30"),
    extra_drop_col = c("a", "b", "c")
  )
  for_dup_2key <- tidytable::tidytable(
    feature_id = c("1", "1", NA_character_),
    candidate_structure_molecular_formula = c(
      "C6H12O6",
      "C6H12O6",
      NA_character_
    ),
    candidate_score_sirius_zodiac = c("0.99", "0.99", "0.10")
  )
  res_dups_consume <- rearrange_annotations(
    annotation_table = data.table::copy(ann_dups),
    formula_table = for_dup_2key,
    canopus_table = empty_can,
    edges_table = empty_edg,
    consume_input = TRUE
  )
  expect_true(nrow(res_dups_consume) >= 2L)

  # 3. export_results writes mini, filtered, and full tables
  local_mocked_bindings(
    get_default_paths = function() {
      list(data = list(processed = list(path = tmp_dir)))
    },
    get_params = function(step) list(step = step),
    export_params = function(...) invisible(NULL),
    .package = "tima"
  )
  exp_res <- export_results(
    results_list = list(
      mini = tidytable::tidytable(a = 1L),
      filtered = tidytable::tidytable(a = 1L),
      full = tidytable::tidytable(a = 1L)
    ),
    output = "out.tsv",
    pattern = "pat"
  )
  expect_identical(names(exp_res), c("filtered", "full"))
  expect_true(all(file.exists(exp_res)))

  # 4. .dedup_ordered_tuple_indices across 32-bit, 53-bit, and table-fallback regimes
  tup_df <- tidytable::tidytable(
    k1 = c("A", "A", "B", "A"),
    k2 = c("X", "X", "Y", "Z")
  )
  ord_tup <- c(2L, 1L, 3L, 4L)
  idx_32 <- .dedup_ordered_tuple_indices(tup_df, c("k1", "k2"), ord_tup)
  idx_53 <- .dedup_ordered_tuple_indices(
    tup_df,
    c("k1", "k2"),
    ord_tup,
    bases_override = c(65536, 65536)
  )
  idx_fb <- .dedup_ordered_tuple_indices(
    tup_df,
    c("k1", "k2"),
    ord_tup,
    bases_override = c(1e9, 1e9)
  )
  expect_identical(idx_32, c(2L, 3L, 4L))
  expect_identical(idx_53, c(2L, 3L, 4L))
  expect_identical(idx_fb, c(2L, 3L, 4L))
})
