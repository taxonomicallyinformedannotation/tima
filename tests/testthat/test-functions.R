library(testthat)

## need to do all in one because of outputs needed in the same temp dir
## use fixtures instead in the future
test_that(desc = "Test functions", code = {
  # Tests
  copy_backbone(cache_dir = ".")

  ## Prepare parameters
  paths <- get_default_paths()
  params <- get_params(step = "prepare_params")
  ## For all steps
  params$organisms$taxon <- ""
  prepare_params(params_small = params)
  params$organisms$taxon <- "Gentiana lutea"
  prepare_params(params_small = params)
  prepare_params(step = "annotate_masses")
  prepare_params(step = "annotate_spectra")
  prepare_params(step = "create_components")
  prepare_params(step = "create_edges_spectra")
  prepare_params(step = "filter_annotations")
  prepare_params(step = "prepare_annotations_gnps")
  prepare_params(step = "prepare_annotations_sirius")
  prepare_params(step = "prepare_annotations_spectra")
  prepare_params(step = "prepare_features_components")
  prepare_params(step = "prepare_features_edges")
  prepare_params(step = "prepare_features_tables")
  prepare_params(step = "prepare_libraries_rt")
  prepare_params(step = "prepare_libraries_sop_closed")
  prepare_params(step = "prepare_libraries_sop_ecmdb")
  prepare_params(step = "prepare_libraries_sop_hmdb")
  prepare_params(step = "prepare_libraries_sop_lotus")
  prepare_params(step = "prepare_libraries_sop_merged")
  prepare_params(step = "prepare_libraries_spectra")
  prepare_params(step = "prepare_taxa")
  prepare_params(step = "weight_annotations")

  ## When previous params exist
  prepare_params()

  ## replace id
  replace_id(
    x = "example/123456_features.tsv",
    user_gnps = "",
    user_filename = "Foo"
  )
  replace_id(
    x = "example/123456_features.tsv",
    user_gnps = "Foo",
    user_filename = "Foo"
  )
  replace_id(
    x = "example/123456_features.tsv",
    user_gnps = get_default_paths()$gnps$example,
    user_filename = "Foo"
  )

  ## Get all files
  get_file(
    url = paths$urls$examples$features,
    export = paths$data$source$features
  )
  get_file(
    url = paths$urls$examples$metadata,
    export = paths$data$source$metadata
  )
  get_file(
    url = paths$urls$examples$metadata |>
      gsub(
        pattern = ".tsv",
        replacement = "_unrecognized.tsv",
        fixed = TRUE
      ),
    export = paths$data$source$metadata |>
      gsub(
        pattern = ".tsv",
        replacement = "_unrecognized.tsv",
        fixed = TRUE
      )
  )
  get_gnps_tables(
    filename = "example",
    path_features = paths$data$source$features,
    path_metadata = paths$data$source$metadata,
    path_spectra = paths$data$source$spectra,
    gnps_job_id = params$gnps$id
  )
  ### Other GNPS job id (without metadata)
  get_gnps_tables(
    filename = "other",
    path_features = paths$data$source$features,
    path_metadata = paths$data$source$metadata,
    path_spectra = paths$data$source$spectra,
    gnps_job_id = paths$gnps$example2
  )
  ### When it is the example
  get_gnps_tables(gnps_job_id = get_default_paths()$gnps$example)
  ### When no GNPS job ID and no metadata are given
  get_gnps_tables(
    filename = "noGNPS",
    path_features = paths$data$source$features,
    path_metadata = list(),
    path_spectra = paths$data$source$spectra,
    gnps_job_id = NULL
  )
  ## When GNPS job ID == ""
  get_gnps_tables(
    filename = "noNoGNPS",
    path_features = paths$data$source$features,
    path_metadata = NULL,
    path_spectra = paths$data$source$spectra,
    gnps_job_id = ""
  )

  ### Spectra
  #### Mini version for tests
  ## Not including it in else statement above on purpose
  unlink(paths$data$source$spectra)
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  ## Checking if file already exists
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  get_file(
    url = paths$urls$examples$spectra_ms1,
    export = paths$data$source$spectra |>
      gsub(
        pattern = "example_spectra",
        replacement = "example_spectra_ms1",
        fixed = TRUE
      )
  )

  ### Spectral library with rt
  get_file(
    url = paths$urls$examples$spectral_lib_mini$with_rt,
    export = paths$data$source$libraries$spectra$exp$with_rt
  )
  ### Temporal library
  get_file(
    url = paths$urls$examples$lib_mini$rt,
    export = paths$data$source$libraries$rt$example_mini
  )

  #### SIRIUS
  ## mini version for tests
  get_example_sirius()

  ### Libraries
  #### ECMDB
  ## try exception because of external resource
  fake_ecmdb(export = "fakeECMDB.json.zip")
  tryCatch(
    expr = {
      get_file(
        url = paths$urls$ecmdb$metabolites,
        export = paths$data$source$libraries$sop$ecmdb
      )
    },
    error = function(e) {
      fake_ecmdb(export = paths$data$source$libraries$sop$ecmdb)
    }
  )

  #### HMDB
  fake_hmdb(export = "fakeHMDB.zip")
  tryCatch(
    expr = {
      get_file(
        url = paths$urls$hmdb$structures,
        export = paths$data$source$libraries$sop$hmdb
      )
    },
    error = function(e) {
      fake_hmdb(export = paths$data$source$libraries$sop$hmdb)
    }
  )

  #### LOTUS
  ## try exception because of external resource
  fake_lotus(export = "fakeLotus.csv.gz")
  tryCatch(
    expr = {
      get_last_version_from_zenodo(
        doi = paths$urls$lotus$doi,
        pattern = paths$urls$lotus$pattern,
        path = paths$data$source$libraries$sop$lotus
      )
    },
    error = function(e) {
      fake_lotus(export = paths$data$source$libraries$sop$lotus)
    }
  )
  ## Check it does not download it a second time
  ## try exception because of external resource
  tryCatch(
    expr = {
      get_last_version_from_zenodo(
        doi = paths$urls$lotus$doi,
        pattern = paths$urls$lotus$pattern,
        path = paths$data$source$libraries$sop$lotus
      )
    },
    error = function(e) {
      fake_lotus(export = paths$data$source$libraries$sop$lotus)
    }
  )

  #### ISDB
  ## smaller version for testing
  # get_file(
  #   url = paths$urls$examples$spectral_lib$pos,
  #   export = paths$data$source$libraries$spectra$is$lotus$pos |>
  #     gsub(
  #       pattern = "isdb_pos.mgf",
  #       replacement = "lotus_pos.rds",
  #       fixed = TRUE
  #     )
  # )
  get_file(
    url = paths$urls$examples$spectral_lib_mini$pos,
    export = paths$data$source$libraries$spectra$is$lotus$pos
  )
  # get_file(
  #   url = paths$urls$examples$spectral_lib$neg,
  #   export = paths$data$source$libraries$spectra$is$lotus$neg |>
  #     gsub(
  #       pattern = "isdb_neg.mgf",
  #       replacement = "lotus_neg.rds",
  #       fixed = TRUE
  #     )
  # )
  get_file(
    url = paths$urls$examples$spectral_lib_mini$neg,
    export = paths$data$source$libraries$spectra$is$lotus$neg
  )

  #### MassBank
  get_massbank_spectra()
  ## test it does not download it a second time
  get_massbank_spectra()

  ## Change small params
  change_params_small()
  change_params_small(
    fil_pat = "myExamplePattern",
    fil_fea_raw = paths$data$source$features,
    fil_met_raw = paths$data$source$metadata,
    fil_sir_raw = paths$data$interim$annotations$example_sirius$v6,
    fil_spe_raw = paths$data$source$spectra,
    ms_pol = "pos",
    org_tax = "Gentiana lutea",
    hig_con = TRUE,
    summarise = FALSE
  )
  ## Prepare libraries
  ### If does not exist
  prepare_libraries_spectra(
    input = "doesNotExists.txt",
    nam_lib = "nope"
  )
  ### If NULL
  prepare_libraries_spectra(
    input = NULL,
    nam_lib = "null"
  )
  ### Classical
  prepare_libraries_spectra()
  #### Check the library already exists warning
  prepare_libraries_spectra()

  ## for msp reading test
  ### Spectrum 1 fails
  import_spectra(dir(
    system.file("extdata", package = "MsBackendMsp"),
    full.names = TRUE,
    pattern = "msp$"
  )[8L])

  ## for feature ID combination in spectra
  data.frame(
    FEATURE_ID = c("FT001", "FT002", "FT003"),
    mz = c(list(123.4567, 234.5678, 345.6789))
  ) |>
    Spectra::Spectra() |>
    sanitize_spectra()

  #### HMDB
  # prepare_isdb_hmdb()

  #### Closed
  # prepare_mona()

  ### Retention time
  prepare_libraries_rt(
    mgf_exp = list(pos = paths$data$source$libraries$spectra$exp$with_rt),
    temp_exp = paths$data$source$libraries$rt$example_mini
  )
  ### Just to check
  prepare_libraries_rt(
    mgf_is = list(pos = paths$data$source$libraries$spectra$exp$with_rt),
    temp_is = paths$data$source$libraries$rt$example_mini
  )
  ## Check wrong SMILES
  tidytable::tidytable(
    "rt" = 0.1,
    "smiles" = "wrongSMILES",
    "inchikey" = NA
  ) |>
    tidytable::fwrite("data/source/libraries/rt/example_bad.tsv")
  prepare_libraries_rt(temp_exp = "data/source/libraries/rt/example_bad.tsv")
  expect_warning(object = prepare_libraries_rt(temp_exp = "data/source/libraries/rt/example_bad.tsv"))

  ### SOP
  #### Closed
  ## To do as if there was an input
  prepare_libraries_sop_closed(input = paths$data$source$libraries$sop$lotus)
  ## When there is no input
  prepare_libraries_sop_closed()

  #### ECMDB
  ## To do as if there was an input
  prepare_libraries_sop_ecmdb(input = "randomNonExistingFile")
  ## When there is no input
  prepare_libraries_sop_ecmdb()

  #### HMDB
  prepare_libraries_sop_hmdb(input = "randomNonExistingFile")
  prepare_libraries_sop_hmdb()

  #### LOTUS
  ## If LOTUS download failed
  prepare_libraries_sop_lotus(input = "randomNonExistingFile")
  prepare_libraries_sop_lotus()

  #### MERGED
  prepare_libraries_sop_merged(
    filter = TRUE,
    level = "family",
    value = "Simaroubaceae|Gentianaceae",
    output_key = "data/interim/libraries/sop/merged/bitter.tsv.gz"
  )
  prepare_libraries_sop_merged()

  ### Features
  #### if no RT
  tidytable::tidytable(
    "row ID" = 1,
    "row m/z" = 123.4567,
    "sample.mzML Peak area" = 98765.43
  ) |>
    tidytable::fwrite("data/source/example_features_no_rt.csv")
  prepare_features_tables(features = "data/source/example_features_no_rt.csv", output = "data/interim/features/example_features_no_rt.tsv.gz")
  #### classical
  prepare_features_tables()

  ## Performing MS1 annotation
  ### Adducts
  parse_adduct("foo bar") # to check for error

  ## TODO check values later on
  calculate_mass_of_m(adduct_string = "[2M1-2H2O+NaCl+H]2+", mz = 123.456)
  calculate_mass_of_m(adduct_string = "[M+Na]+", mz = 123.456)
  calculate_mass_of_m(adduct_string = "[M+H]+", mz = 123.456)
  calculate_mass_of_m(adduct_string = "[M+]+", mz = 123.456)
  calculate_mass_of_m(adduct_string = "[M]+", mz = 123.456)
  calculate_mass_of_m(adduct_string = "[2M1-C6H12O6 (hexose)+NaCl+H]2+", mz = 123.456)
  calculate_mass_of_m(adduct_string = "[M-C6H14O7 (hexose-H2O)+H]+", mz = 123.456)
  calculate_mass_of_m(adduct_string = "[M+CH3COO]-/[M-CH3]-", mz = 123.456)
  calculate_mass_of_m(adduct_string = "[M+K-2H]-", mz = 123.456)
  ### Negative and no RT
  annotate_masses(
    features = "data/interim/features/example_features_no_rt.tsv.gz",
    ## shallow tolerance to speed up tests
    tolerance_ppm = 1,
    tolerance_rt = 0.01,
    ms_mode = "neg"
  )
  ### Positive
  annotate_masses(
    ## shallow tolerance to speed up tests
    tolerance_ppm = 1,
    tolerance_rt = 0.01,
    ms_mode = "pos"
  )
  ### Adducts already attributed (and check for wrong adducts)
  tidytable::tidytable(
    "feature_id" = c(1, 2),
    "mz" = c(123.4567, 141.4678),
    "rt" = c(0.01, 0.02),
    "adduct" = c("[M+XYZ]+", "[M+XYZ-H2O]+")
  ) |>
    tidytable::fwrite("data/source/libraries/rt/example_features_adducts.csv")
  annotate_masses(features = "data/source/libraries/rt/example_features_adducts.csv")

  ## Performing MS2 annotation
  ### Negative
  annotate_spectra(
    library = list(
      neg = paths$data$source$libraries$spectra$exp$with_rt,
      pos = paths$data$source$libraries$spectra$exp$with_rt
    ),
    ## shallow tolerance to speed up tests
    ppm = 1,
    dalton = 0.001,
    polarity = "neg"
  )
  ### Empty
  annotate_spectra(library = list("data/interim/libraries/spectra/exp/nope_pos.rds"))
  ### Approx
  annotate_spectra(
    library = list(pos = paths$data$source$libraries$spectra$exp$with_rt),
    ## shallow tolerance to speed up tests
    ppm = 1,
    dalton = 0.001,
    threshold = 2,
    approx = TRUE
  )

  ## Create MS2 based edges
  create_edges_spectra( ## shallow tolerance to speed up tests
    ppm = 1, dalton = 0.001
  )
  ## if MS1 only
  create_edges_spectra(
    input = "data/source/example_spectra_ms1.mgf",
    ## shallow tolerance to speed up tests
    ppm = 1,
    dalton = 0.001
  )

  ### GNPS results
  prepare_annotations_gnps(input = "fileDoesNotExist")
  prepare_annotations_gnps(input = NULL)
  prepare_annotations_gnps()

  ### SIRIUS results
  prepare_annotations_sirius(input_directory = NULL)
  prepare_annotations_sirius(input_directory = "randomDirThatDoesNotExist")
  prepare_annotations_sirius(input_directory = "randomDirThatDoesNotExist.xyz")
  ## When there is an input
  prepare_annotations_sirius(
    input_directory =
      get_params(step = "prepare_annotations_sirius")$files$annotations$raw$sirius,
  )
  prepare_annotations_sirius(input_directory = "data/interim/annotations/example_sirius.zip", sirius_version = 5)

  ### ISDB results
  prepare_annotations_spectra()

  ### Edges
  prepare_features_edges()

  ## Create components
  create_components()

  ### Features components
  prepare_features_components()

  ### Taxa
  ## Forcing all features to a single source organism
  prepare_taxa(taxon = "Homo sapiens")
  ## When empty
  prepare_taxa(taxon = "")
  ## Without file extension in the column names
  prepare_taxa(extension = FALSE)
  ## Testing for unrecognized taxa
  prepare_taxa(
    metadata = paths$data$source$metadata |>
      gsub(
        pattern = ".tsv",
        replacement = "_unrecognized.tsv",
        fixed = TRUE
      )
  )
  ## Attributing based on intensity (multiple source organisms)
  prepare_taxa()
  ## Tests for ott API
  fake_taxon_df <- data.frame("organism" = "Gentiana lutea")
  wrong_taxon_df <- data.frame("organism" = "Gentiano luteo")
  get_organism_taxonomy_ott(df = fake_taxon_df)
  get_organism_taxonomy_ott(df = wrong_taxon_df)
  get_organism_taxonomy_ott(df = fake_taxon_df, url = "https://api.opentreeoflife.org/v3/taxonomy/fakeDown")
  ## Stupid tests for benchmark
  data.frame(feature_id = 1, organism_name = "Gentiana lutea") |>
    export_output("data/interim/benchmark/bench_test_in.tsv.gz")
  benchmark_taxize_spectra(
    input = "data/interim/benchmark/bench_test_in.tsv.gz",
    keys = paths$data$interim$libraries$sop$merged$keys,
    org_tax_ott =
      get_params(step = "prepare_taxa")$files$libraries$sop$merged$organisms$taxonomies$ott,
    output = "data/interim/benchmark/bench_test_out.tsv.gz"
  )

  ## Filter annotations
  ## case when no RT lib
  filter_annotations(rts = list())
  filter_annotations()

  ## Perform TIMA
  ### Normal
  weight_annotations(candidates_final = 1, minimal_ms1_bio = 0.8)
  ### Only MS1
  weight_annotations(
    ms1_only = TRUE,
    remove_ties = TRUE,
    summarise = TRUE,
    candidates_final = 1,
    minimal_ms1_bio = 0.8,
    minimal_ms1_condition = "AND",
    compounds_names = TRUE,
    high_confidence = FALSE
  )

  ## cleanup
  unlink("data", recursive = TRUE)

  ## in cache version
  copy_backbone()
  get_example_files(
    example = c(
      "features",
      "metadata",
      "sirius",
      "spectra",
      "spectral_lib_with_rt"
    ),
    in_cache = FALSE
  )
  ## for size to match for shinytest2
  unlink(paths$data$source$spectra)
  get_file(
    url = paths$urls$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  ## Install
  install(test = TRUE)
  # get_example_files()
  # tima:::.onLoad()
  # tima_full()

  ## CLI arguments check
  arguments <- character()
  arguments$ann_can_fin <- 0
  arguments$ann_ms1only <- TRUE
  arguments$ann_ms2_app <- TRUE
  arguments$ann_thr_con <- 0
  arguments$ann_thr_ms1_bio <- 0
  arguments$ann_thr_ms1_che <- 0
  arguments$ann_thr_ms1_con <- "x"
  arguments$ann_thr_ms2_sim_ann <- 0
  arguments$ann_thr_ms2_sim_edg <- 0
  arguments$fil_pat <- "x"
  arguments$fil_ann_raw_spe <- "x"
  arguments$fil_ann_raw_spe_gnp <- "x"
  arguments$fil_ann_raw_spe_spe <- "x"
  arguments$fil_ann_raw_sir <- "x"
  arguments$fil_ann_pre_can <- "x"
  arguments$fil_ann_pre_for <- "x"
  arguments$fil_ann_pre_str <- "x"
  arguments$fil_ann_fil <- "x"
  arguments$fil_ann_pro <- "x"
  arguments$fil_fea_raw <- "x"
  arguments$fil_fea_pre <- "x"
  arguments$fil_lib_sop_raw_clo <- "x"
  arguments$fil_lib_sop_raw_ecm <- "x"
  arguments$fil_lib_sop_raw_hmd <- "x"
  arguments$fil_lib_sop_raw_lot <- "x"
  arguments$fil_lib_sop_pre <- "x"
  arguments$fil_lib_spe_neg <- "x"
  arguments$fil_lib_spe_pos <- "x"
  arguments$fil_lib_spe_raw <- "x"
  arguments$fil_lib_tem_exp_csv <- "x"
  arguments$fil_lib_tem_exp_mgf_neg <- "x"
  arguments$fil_lib_tem_exp_mgf_pos <- "x"
  arguments$fil_lib_tem_is_csv <- "x"
  arguments$fil_lib_tem_is_mgf_neg <- "x"
  arguments$fil_lib_tem_is_mgf_pos <- "x"
  arguments$fil_lib_tem_pre <- "x"
  arguments$fil_net_spe_edg_raw <- "x"
  arguments$fil_net_spe_edg_pre <- "x"
  arguments$fil_net_spe_com_raw <- "x"
  arguments$fil_net_spe_com_pre <- "x"
  arguments$fil_met_raw <- "x"
  arguments$fil_met_pre <- "x"
  arguments$fil_spe_raw <- "x"
  arguments$gnps_id <- "x"
  arguments$gnps_workflow <- "x"
  arguments$ms_add_neg <- "x"
  arguments$ms_add_pos <- "x"
  arguments$ms_clu_neg <- "x"
  arguments$ms_clu_pos <- "x"
  arguments$ms_neu <- "x"
  arguments$ms_pol <- "x"
  arguments$ms_thr_ms2_int <- 0
  arguments$ms_tol_mas_ppm_ms1 <- 0
  arguments$ms_tol_mas_ppm_ms2 <- 0
  arguments$ms_tol_mas_dal_ms1 <- 0
  arguments$ms_tol_mas_dal_ms2 <- 0
  arguments$ms_tol_rt_add <- 0
  arguments$ms_tol_rt_lib <- 0
  arguments$names_adduct <- "x"
  arguments$names_extension <- TRUE
  arguments$names_features <- "x"
  arguments$names_filename <- "x"
  arguments$names_inchikey <- "x"
  arguments$names_lib <- "x"
  arguments$names_mgf_ad <- "x"
  arguments$names_mgf_ce <- "x"
  arguments$names_mgf_ci <- "x"
  arguments$names_mgf_em <- "x"
  arguments$names_mgf_in <- "x"
  arguments$names_mgf_io <- "x"
  arguments$names_mgf_ik <- "x"
  arguments$names_mgf_il <- "x"
  arguments$names_mgf_mf <- "x"
  arguments$names_mgf_na <- "x"
  arguments$names_mgf_po <- "x"
  # arguments$names_mgf_pc <- "x"
  # arguments$names_mgf_pm <- "x"
  arguments$names_mgf_sm <- "x"
  arguments$names_mgf_sn <- "x"
  arguments$names_mgf_si <- "x"
  arguments$names_mgf_sp <- "x"
  arguments$names_mgf_sy <- "x"
  arguments$names_mgf_xl <- "x"
  arguments$names_precursor <- "x"
  arguments$names_rt <- "x"
  arguments$names_rt_fea <- "x"
  arguments$names_rt_lib <- "x"
  arguments$names_smiles <- "x"
  arguments$names_source <- "x"
  arguments$names_target <- "x"
  arguments$names_taxon <- "x"
  arguments$org_can <- 0
  arguments$org_fil_mod <- TRUE
  arguments$org_fil_lev <- "x"
  arguments$org_fil_val <- "x"
  arguments$org_tax <- "x"
  arguments$too_met <- "x"
  arguments$too_net_spe_com <- "x"
  arguments$too_net_spe_edg <- "x"
  arguments$too_sir_ver <- 0
  arguments$too_tax_bio <- "x"
  arguments$too_tax_che <- "x"
  arguments$units_rt <- "x"
  arguments$wei_glo_bio <- 0
  arguments$wei_glo_che <- 0
  arguments$wei_glo_spe <- 0
  arguments$wei_bio_01 <- 0
  arguments$wei_bio_02 <- 0
  arguments$wei_bio_03 <- 0
  arguments$wei_bio_04 <- 0
  arguments$wei_bio_05 <- 0
  arguments$wei_bio_06 <- 0
  arguments$wei_bio_07 <- 0
  arguments$wei_bio_08 <- 0
  arguments$wei_bio_09 <- 0
  arguments$wei_bio_10 <- 0
  arguments$wei_bio_11 <- 0
  arguments$wei_bio_12 <- 0
  arguments$wei_bio_13 <- 0
  arguments$wei_bio_14 <- 0
  arguments$wei_bio_15 <- 0
  arguments$wei_che_11 <- 0
  arguments$wei_che_12 <- 0
  arguments$wei_che_13 <- 0
  arguments$wei_che_14 <- 0
  arguments$wei_che_21 <- 0
  arguments$wei_che_22 <- 0
  arguments$wei_che_23 <- 0
  arguments$compounds_names <- TRUE
  arguments$force <- TRUE
  arguments$high_confidence <- TRUE
  arguments$remove_ties <- TRUE
  arguments$summarise <- TRUE

  parse_cli_params(arguments = arguments, parameters = params)

  succeed()
})
