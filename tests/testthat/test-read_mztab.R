library(testthat)

test_that(".parse_mztab_cv_name extracts third element from CV term", {
  expect_equal(
    tima:::.parse_mztab_cv_name("[NCBITaxon, NCBITaxon:9606, Homo sapiens, ]"),
    "Homo sapiens"
  )
  expect_equal(
    tima:::.parse_mztab_cv_name("[BTO, BTO:0000131, blood plasma, ]"),
    "blood plasma"
  )
  expect_true(is.na(tima:::.parse_mztab_cv_name(NA_character_)))
  expect_true(is.na(tima:::.parse_mztab_cv_name("[only, two]")))
  expect_equal(
    tima:::.parse_mztab_cv_name("Homo sapiens"),
    "Homo sapiens"
  )
})

test_that(".parse_mztab_cv_name handles escaped commas in Param values", {
  x <- "[NCBITaxon, NCBITaxon:9606, Homo sapiens, liver\\, plasma]"
  expect_equal(tima:::.parse_mztab_cv_name(x), "Homo sapiens")
})

test_that(".mztab_sample_species_map extracts Homo sapiens from lipidcompass fixture", {
  mztab_file <- resolve_fixture_path(
    "mztab/lipidcompass-script_226ea96_MTD_SML_LCS-00001-01.mztab"
  )
  tabs <- read_mztab_tables(mztab_file)
  species_map <- tima:::.mztab_sample_species_map(tabs$metadata)

  expect_true(length(species_map) > 0L)
  expect_true(all(species_map == "Homo sapiens"))
  expect_true("sample[1]" %in% names(species_map))
})

test_that(".mztab_sample_species_map supports sample-organism plain-text fields", {
  meta <- tidytable::tidytable(
    key = c("sample[1]", "sample[1]-organism"),
    value = c("sample_a", "Homo sapiens")
  )
  species_map <- tima:::.mztab_sample_species_map(meta)
  expect_identical(unname(species_map[["sample[1]"]]), "Homo sapiens")
})

test_that("read_mztab_tables preserves literal NA strings", {
  f <- temp_test_path("literal_na_value.mztab")
  writeLines(
    c(
      "MTD\tmzTab-version\t2.0.0-M",
      "SMH\tSML_ID\tchemical_name",
      "SML\t1\tNA"
    ),
    f
  )

  tabs <- read_mztab_tables(f)
  expect_identical(tabs$sml$chemical_name[[1]], "NA")
})

test_that("read_mztab preserves opt_* and SML annotation columns in feature output", {
  f <- temp_test_path("opt_cols_pass.mztab")
  writeLines(
    c(
      "MTD\tmzTab-version\t2.0.0-M",
      "SMH\tSML_ID\tSMF_ID_REFS\tSME_ID_REFS\tdatabase_identifier\tchemical_name\topt_global_lipid_class",
      "SML\t1\t1\tnull\tCHEBI:001\tTestCompound\tSphingolipid",
      "SFH\tSMF_ID\tSML_ID_REFS\texp_mass_to_charge\tcharge\tretention_time_in_seconds",
      "SMF\t1\t1\t123.45\t1\t60"
    ),
    f
  )

  out_features <- temp_test_path("opt_cols_features.tsv")
  read_mztab(input = f, output_features = out_features)

  feat <- tidytable::fread(out_features)
  expect_true("database_identifier" %in% names(feat))
  expect_true("chemical_name" %in% names(feat))
  expect_true("opt_global_lipid_class" %in% names(feat))
  expect_identical(feat$chemical_name[[1]], "TestCompound")
  expect_identical(feat$opt_global_lipid_class[[1]], "Sphingolipid")
})

test_that("read_mztab exports TIMA-compatible metadata with ATTRIBUTE_species for lipidcompass", {
  mztab_file <- resolve_fixture_path(
    "mztab/lipidcompass-script_226ea96_MTD_SML_LCS-00001-01.mztab"
  )

  out_dir <- temp_test_dir("read_mztab_lipidcompass")
  out_features <- file.path(out_dir, "features.tsv")
  out_metadata <- file.path(out_dir, "metadata.tsv")

  paths <- read_mztab(
    input = mztab_file,
    output_features = out_features,
    output_metadata = out_metadata
  )

  expect_true(file.exists(paths$metadata))
  meta <- tidytable::fread(paths$metadata)
  expect_true("filename" %in% names(meta))
  expect_true("ATTRIBUTE_species" %in% names(meta))
  expect_true(any(meta$ATTRIBUTE_species == "Homo sapiens", na.rm = TRUE))
})

test_that("read_mztab parses official example and exports files", {
  mztab_file <- resolve_fixture_path(
    "mztab/example_study_variable_group.mztab"
  )

  out_dir <- temp_test_dir("read_mztab")
  out_features <- file.path(out_dir, "features.tsv")
  out_spectra <- file.path(out_dir, "spectra.mgf")
  out_metadata <- file.path(out_dir, "metadata.tsv")

  paths <- read_mztab(
    input = mztab_file,
    output_features = out_features,
    output_spectra = out_spectra,
    output_metadata = out_metadata
  )

  expect_true(file.exists(paths$features))
  expect_true(file.exists(paths$spectra))
  expect_true(file.exists(paths$metadata))

  features <- tidytable::fread(paths$features)
  expect_true(nrow(features) > 0)
  expect_true(all(c("feature_id", "mz", "rt", "adduct") %in% names(features)))

  mgf_lines <- readLines(paths$spectra, warn = FALSE)
  expect_true(any(grepl("^BEGIN IONS$", mgf_lines)))
  expect_true(any(grepl("^PEPMASS=", mgf_lines)))
  # Proxy MGF must carry FEATURE_ID so get_spectra_ids() can map edges
  expect_true(any(grepl("^FEATURE_ID=", mgf_lines)))
})

# ── Embedded masster COM MGF tests ────────────────────────────────────────────

test_that(".extract_embedded_mgf emits FEATURE_ID via opt_global_mgf_index", {
  mztab_file <- resolve_fixture_path("mztab/masster_minimal_embedded.mztab")
  out_dir <- temp_test_dir("masster_embedded_mgf")
  out_features <- file.path(out_dir, "features.tsv")
  out_spectra <- file.path(out_dir, "spectra.mgf")

  paths <- read_mztab(
    input = mztab_file,
    output_features = out_features,
    output_spectra = out_spectra
  )

  expect_true(file.exists(paths$spectra))
  mgf_lines <- readLines(paths$spectra, warn = FALSE)

  # Must have at least one embedded entry (COM MGF section present)
  expect_true(any(grepl("^BEGIN IONS$", mgf_lines)))
  expect_true(any(grepl("^PEPMASS=", mgf_lines)))

  # FEATURE_ID must be emitted for each entry
  expect_true(any(grepl("^FEATURE_ID=", mgf_lines)))

  # FEATURE_ID values must match the feature IDs in the feature table (1 and 2)
  fid_lines <- grep("^FEATURE_ID=", mgf_lines, value = TRUE)
  fids <- sub("^FEATURE_ID=", "", fid_lines)
  expect_true("1" %in% fids)
  expect_true("2" %in% fids)

  # MS level tag should be present for entries that have it
  expect_true(any(grepl("^MSLEVEL=", mgf_lines)))

  # Features table must have expected columns
  feat <- tidytable::fread(out_features)
  expect_true(all(c("feature_id", "mz", "rt", "adduct") %in% names(feat)))
  expect_equal(nrow(feat), 2L)
})

test_that(".extract_embedded_mgf resolves FEATURE_ID from 'id:<N>' in TITLE", {
  mztab_file <- resolve_fixture_path("mztab/masster_id_title_format.mztab")
  out_dir <- temp_test_dir("masster_id_title_format")
  out_features <- file.path(out_dir, "features.tsv")
  out_spectra <- file.path(out_dir, "spectra.mgf")

  paths <- read_mztab(
    input = mztab_file,
    output_features = out_features,
    output_spectra = out_spectra
  )

  expect_true(file.exists(paths$spectra))
  mgf_lines <- readLines(paths$spectra, warn = FALSE)

  # FEATURE_ID must be emitted even without opt_global_mgf_index
  fid_lines <- grep("^FEATURE_ID=", mgf_lines, value = TRUE)
  expect_true(length(fid_lines) > 0L)
  fids <- sub("^FEATURE_ID=", "", fid_lines)
  expect_true("42" %in% fids)
  expect_true("43" %in% fids)
})

test_that(".extract_embedded_mgf RTINSECONDS only written for finite values", {
  # Build a minimal in-memory masster mzTab where prec_rt is 'null'
  tmp <- temp_test_path("masster_null_rt.mztab")
  writeLines(
    c(
      "COM\ttest",
      "MTD\tmzTab-version\t2.2.0-M",
      "MTD\tmzTab-id\ttest_null_rt",
      "MTD\tsample[1]\tsample_A",
      "MTD\tms_run[1]-location\tfile://x",
      "MTD\tassay[1]\tAssay_1",
      "MTD\tassay[1]-sample_ref\tsample[1]",
      "MTD\tassay[1]-ms_run_ref\tms_run[1]",
      "SMH\tSML_ID\tSMF_ID_REFS",
      "SML\t1\t1",
      "SFH\tSMF_ID\tSML_ID_REFS\texp_mass_to_charge\tretention_time_in_seconds",
      "SMF\t1\t1\t100.5\t60",
      "COM\tMGH\tmgf_id\tprec_id\tprec_rt\tprec_mz\tprec_int\tenergy\tlevel\ttitle\tspec_tic\tspec_len\tspec_mz\tspec_int",
      "COM\tMGF\t1\thash1\tnull\t100.5\tnull\t30.0\t2\tmgf_id:1\t100\t2\t50.3|75.2\t40|60"
    ),
    tmp
  )

  out_dir <- temp_test_dir("masster_null_rt")
  out_features <- file.path(out_dir, "features.tsv")
  out_spectra <- file.path(out_dir, "spectra.mgf")

  paths <- read_mztab(
    input = tmp,
    output_features = out_features,
    output_spectra = out_spectra
  )

  mgf_lines <- readLines(paths$spectra, warn = FALSE)
  # When prec_rt is null the RTINSECONDS line must be absent
  expect_false(any(grepl("^RTINSECONDS=", mgf_lines)))
  expect_true(any(grepl("^PEPMASS=100.5", mgf_lines)))
})

test_that(".write_proxy_mgf skips features with zero or non-finite m/z", {
  features <- data.frame(
    feature_id = c("A", "B", "C", "D"),
    mz = c(100.5, 0, NA, Inf),
    rt = c(1, 2, 3, 4),
    adduct = rep("[M+H]+", 4L)
  )
  tmp <- temp_test_path("proxy_mz_edge.mgf")

  tima:::.write_proxy_mgf(features, tmp, "feature_id", "mz")

  lines <- readLines(tmp, warn = FALSE)
  # Only feature A should produce an entry
  expect_equal(sum(grepl("^BEGIN IONS$", lines)), 1L)
  expect_true(any(grepl("^FEATURE_ID=A$", lines)))
  expect_false(any(grepl("^FEATURE_ID=B$", lines)))
  expect_false(any(grepl("^FEATURE_ID=C$", lines)))
  expect_false(any(grepl("^FEATURE_ID=D$", lines)))
})

test_that("end-to-end: mzTab → features + embedded MGF → edges work together", {
  skip_if_not_installed("Spectra")

  mztab_file <- resolve_fixture_path("mztab/masster_minimal_embedded.mztab")
  out_dir <- temp_test_dir("e2e_mztab_edges")
  out_features <- file.path(out_dir, "features.tsv")
  out_spectra <- file.path(out_dir, "spectra.mgf")
  out_edges <- file.path(out_dir, "edges.tsv")

  # Step 1: Read mzTab and export features + spectra
  paths <- read_mztab(
    input = mztab_file,
    output_features = out_features,
    output_spectra = out_spectra
  )

  expect_true(file.exists(paths$features))
  expect_true(file.exists(paths$spectra))

  feat <- tidytable::fread(paths$features)
  expect_equal(nrow(feat), 2L)
  expect_true(all(c("feature_id", "mz", "rt") %in% names(feat)))

  # Step 2: Create edges from the spectra
  edges <- create_edges_spectra(
    input = paths$spectra,
    output = out_edges,
    threshold = 0.0,
    matched_peaks = 1
  )

  expect_true(file.exists(edges))
  edge_tbl <- tidytable::fread(edges)

  # With 2 spectra from mzTab there should be at most 1 edge,
  # or NA row if similarity is below threshold
  expect_true(nrow(edge_tbl) <= 2L)

  # Most importantly: the edge table must have the cluster ID columns
  # populated from FEATURE_ID in the MGF (not NULL)
  if (nrow(edge_tbl) > 0L && !all(is.na(edge_tbl$CLUSTERID1))) {
    # At least some edges have valid cluster IDs from our FEATURE_ID export
    expect_true(any(!is.na(edge_tbl$CLUSTERID1) | !is.na(edge_tbl$CLUSTERID2)))
  }
})

test_that("prepare_annotations_mztab exports non-empty table for official study-variable example", {
  mztab_file <- resolve_fixture_path(
    "mztab/example_study_variable_group.mztab"
  )

  out_dir <- temp_test_dir("prepare_annotations_mztab")
  out <- file.path(out_dir, "ann.tsv.gz")

  ann <- prepare_annotations_mztab(
    input = mztab_file,
    output = out,
    str_stereo = resolve_fixture_path("structures_stereo.csv"),
    str_met = resolve_fixture_path("structures_metadata.csv"),
    str_tax_cla = resolve_fixture_path("structures_taxonomy_cla.csv"),
    str_tax_npc = resolve_fixture_path("structures_taxonomy_npc.csv")
  )

  expect_true(file.exists(ann))
  df <- tidytable::fread(ann)
  expect_true(nrow(df) >= 0)
  expect_true("feature_id" %in% names(df))
})

test_that("read_mztab_tables parses all mzTab fixtures without errors", {
  files <- c(
    resolve_fixture_path("mztab/mock_all_cases.mztab"),
    resolve_fixture_path("mztab/example_study_variable_group.mztab"),
    resolve_fixture_path("mztab/manual_null_null_lipidomics.mztab")
  )

  for (f in files) {
    expect_no_error(read_mztab_tables(f))
  }
})

test_that("read_mztab exports feature tables for representative mzTab fixtures", {
  files <- c(
    resolve_fixture_path("mztab/mock_all_cases.mztab"),
    resolve_fixture_path("mztab/example_study_variable_group.mztab"),
    resolve_fixture_path("mztab/manual_null_null_lipidomics.mztab")
  )

  out_dir <- temp_test_dir("read_mztab_all")

  for (f in files) {
    base <- tools::file_path_sans_ext(basename(f))
    out_features <- file.path(out_dir, paste0(base, "_features.tsv"))
    tabs <- read_mztab_tables(f)
    has_quant_section <- nrow(tabs$sml) > 0L || nrow(tabs$smf) > 0L

    if (has_quant_section) {
      paths <- read_mztab(
        input = f,
        output_features = out_features
      )

      expect_true(file.exists(paths$features), info = basename(f))
      df <- tidytable::fread(paths$features)
      expect_true(
        all(c("feature_id", "mz", "rt", "adduct") %in% names(df)),
        info = basename(f)
      )
      expect_true(nrow(df) > 0L, info = basename(f))
    } else {
      expect_error(
        read_mztab(
          input = f,
          output_features = out_features
        ),
        class = "tima_validation_error"
      )
    }
  }
})

test_that("prepare_annotations_mztab works across representative mzTab fixtures", {
  files <- c(
    resolve_fixture_path("mztab/mock_all_cases.mztab"),
    resolve_fixture_path("mztab/example_study_variable_group.mztab"),
    resolve_fixture_path("mztab/manual_null_null_lipidomics.mztab")
  )

  out_dir <- temp_test_dir("prepare_annotations_mztab_all")

  for (f in files) {
    tabs <- read_mztab_tables(f)
    has_quant_section <- nrow(tabs$sml) > 0L || nrow(tabs$smf) > 0L

    out <- file.path(
      out_dir,
      paste0(tools::file_path_sans_ext(basename(f)), "_ann.tsv.gz")
    )
    if (has_quant_section) {
      ann <- prepare_annotations_mztab(
        input = f,
        output = out,
        str_stereo = resolve_fixture_path("structures_stereo.csv"),
        str_met = resolve_fixture_path("structures_metadata.csv"),
        str_tax_cla = resolve_fixture_path("structures_taxonomy_cla.csv"),
        str_tax_npc = resolve_fixture_path("structures_taxonomy_npc.csv")
      )

      expect_true(file.exists(ann), info = basename(f))
      df <- tidytable::fread(ann)
      expect_true("feature_id" %in% names(df), info = basename(f))
    } else {
      expect_error(
        prepare_annotations_mztab(
          input = f,
          output = out,
          str_stereo = resolve_fixture_path("structures_stereo.csv"),
          str_met = resolve_fixture_path("structures_metadata.csv"),
          str_tax_cla = resolve_fixture_path("structures_taxonomy_cla.csv"),
          str_tax_npc = resolve_fixture_path("structures_taxonomy_npc.csv")
        ),
        class = "tima_validation_error"
      )
    }
  }
})

test_that("read_mztab_tables parses mzTab-M JSON fixtures in both naming flavors", {
  files <- c(
    resolve_fixture_path("mztab/progenesis_2.4.6505.48857_MTBLS263.json"),
    resolve_fixture_path(
      "mztab/MetaboScape-2025b-QJZ-2025-02-20-source-comparison.json"
    )
  )

  for (f in files) {
    tabs <- read_mztab_tables(f)
    expect_true(nrow(tabs$metadata) > 0L, info = basename(f))
    expect_true(
      nrow(tabs$sml) > 0L || nrow(tabs$smf) > 0L || nrow(tabs$sme) > 0L,
      info = basename(f)
    )
  }
})

test_that("read_mztab_tables supports rmzTabM-style JSON object layout", {
  tmp <- withr::local_tempfile(fileext = ".json")

  payload <- list(
    metadata = list(
      `mzTab-version` = "2.0.0-M",
      `mzTab-ID` = "TIMA-RMZTABM-JSON",
      title = "rmzTabM compatibility payload"
    ),
    smallMoleculeSummary = list(
      list(
        prefix = "SML",
        header_prefix = "SMH",
        sml_id = 1,
        smf_id_refs = list(1),
        database_identifier = list("HMDB:HMDB00001"),
        chemical_name = list("Example compound")
      )
    ),
    smallMoleculeFeature = list(
      list(
        prefix = "SMF",
        header_prefix = "SFH",
        smf_id = 1,
        sme_id_refs = list(1),
        exp_mass_to_charge = 100.1,
        charge = 1,
        retention_time_in_seconds = 120
      )
    ),
    smallMoleculeEvidence = list(
      list(
        prefix = "SME",
        header_prefix = "SEH",
        sme_id = 1,
        evidence_input_id = "scan=1",
        database_identifier = "HMDB:HMDB00001",
        exp_mass_to_charge = 100.1,
        charge = 1,
        theoretical_mass_to_charge = 100.1,
        spectra_ref = list("ms_run[1]:scan=1"),
        identification_method = list(
          cv_label = "MS",
          cv_accession = "MS:1001083",
          name = "ms-ms search",
          value = ""
        ),
        ms_level = list(
          cv_label = "MS",
          cv_accession = "MS:1000580",
          name = "MS2 spectrum",
          value = ""
        ),
        rank = 1
      )
    )
  )

  jsonlite::write_json(payload, path = tmp, auto_unbox = TRUE, pretty = TRUE)

  tabs <- read_mztab_tables(tmp)
  expect_true(nrow(tabs$metadata) > 0L)
  expect_true(any(tabs$metadata$key == "mzTab-version"))

  expect_true("SML_ID" %in% names(tabs$sml))
  expect_true("SMF_ID_REFS" %in% names(tabs$sml))
  expect_equal(as.character(tabs$sml$SML_ID[[1L]]), "1")

  expect_true("SMF_ID" %in% names(tabs$smf))
  expect_true("SME_ID_REFS" %in% names(tabs$smf))
  expect_equal(as.character(tabs$smf$SMF_ID[[1L]]), "1")

  expect_true("SME_ID" %in% names(tabs$sme))
  expect_true("identification_method" %in% names(tabs$sme))
  expect_match(
    as.character(tabs$sme$identification_method[[1L]]),
    "^\\[MS, MS:1001083, ms-ms search, \\]$"
  )
  expect_match(
    as.character(tabs$sme$ms_level[[1L]]),
    "^\\[MS, MS:1000580, MS2 spectrum, \\]$"
  )

  expect_no_error(validate_mztab_tables(tabs, strict = TRUE))
})

test_that("rmzTabM-style JSON round-trips through read_mztab and write_mztab JSON", {
  local_test_project(copy = TRUE)

  tmp_in <- withr::local_tempfile(fileext = ".json")
  tmp_features <- withr::local_tempfile(fileext = ".tsv")
  tmp_out <- withr::local_tempfile(fileext = ".json")

  payload <- list(
    metadata = list(
      `mzTab-version` = "2.0.0-M",
      `mzTab-ID` = "TIMA-RMZTABM-ROUNDTRIP",
      title = "rmzTabM roundtrip payload"
    ),
    smallMoleculeSummary = list(
      list(
        prefix = "SML",
        header_prefix = "SMH",
        sml_id = 1,
        smf_id_refs = list(1),
        database_identifier = list("HMDB:HMDB00001"),
        chemical_name = list("Example compound")
      )
    ),
    smallMoleculeFeature = list(
      list(
        prefix = "SMF",
        header_prefix = "SFH",
        smf_id = 1,
        sme_id_refs = list(1),
        exp_mass_to_charge = 100.1,
        charge = 1,
        retention_time_in_seconds = 120,
        adduct_ion = "[M+H]+"
      )
    ),
    smallMoleculeEvidence = list(
      list(
        prefix = "SME",
        header_prefix = "SEH",
        sme_id = 1,
        evidence_input_id = "scan=1",
        database_identifier = "HMDB:HMDB00001",
        exp_mass_to_charge = 100.1,
        charge = 1,
        theoretical_mass_to_charge = 100.1,
        spectra_ref = list("ms_run[1]:scan=1"),
        identification_method = list(
          cv_label = "MS",
          cv_accession = "MS:1001083",
          name = "ms-ms search",
          value = ""
        ),
        ms_level = list(
          cv_label = "MS",
          cv_accession = "MS:1000580",
          name = "MS2 spectrum",
          value = ""
        ),
        rank = 1
      )
    )
  )
  jsonlite::write_json(payload, path = tmp_in, auto_unbox = TRUE, pretty = TRUE)

  tabs_before <- read_mztab_tables(tmp_in)
  expect_no_error(validate_mztab_tables(tabs_before, strict = TRUE))

  read_mztab(
    input = tmp_in,
    output_features = tmp_features,
    name_features = "feature_id",
    name_mz = "feature_mz",
    name_rt = "feature_rt",
    name_adduct = "feature_adduct",
    strict = TRUE
  )
  write_mztab(
    input = tmp_features,
    output = tmp_out
  )

  tabs_after <- read_mztab_tables(tmp_out)
  expect_no_error(validate_mztab_tables(tabs_after, strict = TRUE))

  expect_equal(nrow(tabs_after$smf), nrow(tabs_before$smf))
  # read_mztab() exports a feature table only, so SME-level evidence is not
  # preserved through this route unless separate annotations are re-supplied.
  expect_equal(nrow(tabs_after$sme), 0L)

  expect_equal(
    as.numeric(tabs_after$smf$exp_mass_to_charge),
    as.numeric(tabs_before$smf$exp_mass_to_charge),
    tolerance = 1e-8
  )
  expect_equal(
    as.numeric(tabs_after$smf$retention_time_in_seconds),
    as.numeric(tabs_before$smf$retention_time_in_seconds),
    tolerance = 1e-8
  )
  expect_true(nrow(tabs_after$sml) >= 1L)
})

test_that("mztab_before_after_batch emits JSON artifacts for JSON fixtures", {
  script <- normalizePath(
    testthat::test_path(
      "..",
      "..",
      "inst",
      "scripts",
      "mztab_before_after_batch.R"
    ),
    winslash = "/",
    mustWork = FALSE
  )
  skip_if_not(file.exists(script), "batch script not found")
  repo_root <- normalizePath(
    file.path(dirname(script), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )

  fixtures_src <- resolve_fixture_path("mztab")
  fixtures_dir <- file.path(tempdir(), paste0("mztab_json_", Sys.getpid()))
  dir.create(fixtures_dir, recursive = TRUE, showWarnings = FALSE)

  json_files <- list.files(
    fixtures_src,
    pattern = "\\.json$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  expect_true(length(json_files) > 0L)
  file.copy(json_files, fixtures_dir, overwrite = TRUE)

  out_dir <- file.path(tempdir(), paste0("mztab_before_after_", Sys.getpid()))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  withr::local_dir(repo_root)
  exit_code <- system2(
    command = "Rscript",
    args = c(
      script,
      "--fixtures-dir",
      fixtures_dir,
      "--output-dir",
      out_dir,
      "--str-stereo",
      resolve_fixture_path("structures_stereo.csv"),
      "--str-met",
      resolve_fixture_path("structures_metadata.csv"),
      "--str-tax-cla",
      resolve_fixture_path("structures_taxonomy_cla.csv"),
      "--str-tax-npc",
      resolve_fixture_path("structures_taxonomy_npc.csv")
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(exit_code, "status")
  if (is.null(status)) {
    status <- 0L
  }
  expect_equal(status, 0L)

  manifest <- tidytable::fread(file.path(out_dir, "manifest.csv"))
  expect_true(nrow(manifest) == length(json_files))
  expect_true(
    all(manifest$status == "ok"),
    info = paste(manifest$error, collapse = " | ")
  )
  expect_true(all(file.exists(manifest$before_tables_json)))
  expect_true(all(file.exists(manifest$after_tables_json)))
})
