## need to do all in one because of outputs needed in the same temp dir
testthat::test_that("Whole process", {
  ## Prepare parameters
  paths <- parse_yaml_paths()
  vars <- ls(all.names = TRUE)
  for (i in 1:length(vars)) {
    assign(vars[i], get(vars[i]), envir = .GlobalEnv)
  }
  step <- "prepare_params"
  params <- get_params(step = step)
  prepare_params()

  ## Get all files

  ### Benchmark
  get_benchmark(url = "https://raw.githubusercontent.com/matchms/matchms/master/tests/massbank_five_spectra.msp")

  ### Examples

  #### Feature table
  get_example_feature_table()

  #### MGF
  ## mini version for tests
  get_example_mgf(url = paths$url$examples$mgf_mini)
  # get_example_mgf()

  #### SIRIUS
  ## mini version for tests
  get_example_sirius(url = paths$urls$examples$sirius_mini)
  # get_example_sirius()

  #### LOTUS
  get_last_version_from_zenodo(
    doi = paths$url$lotus$doi,
    pattern = paths$urls$lotus$pattern,
    path = paths$data$source$libraries$lotus
  )

  #### HMDB
  # get_hmdb()

  #### ISDB
  ## smaller version for testing
  create_dir(paths$data$source$spectra$lotus$pos)
  utils::download.file(
    url = paths$url$examples$spectral_lib,
    destfile = paths$data$source$spectra$lotus$pos
  )
  utils::download.file(
    url = paths$url$examples$spectral_lib,
    destfile = paths$data$source$spectra$lotus$neg
  )

  ## Prepare libraries
  ### LOTUS
  prepare_lotus()

  ### HMDB
  # prepare_hmdb()

  ### Closed
  # prepare_closed()

  ### Structural library
  step <- "prepare_library"
  params <- get_params(step = step)
  prepare_library(
    filter = TRUE,
    level = "family",
    value = "Simaroubaceae|Gentianaceae"
  )
  prepare_library()

  ## Prepare spectra
  ### LOTUS
  prepare_isdb_lotus()
  prepare_isdb_lotus(export_sqlite = FALSE)

  ### HMDB
  # prepare_isdb_hmdb()

  ### Closed
  # prepare_mona()

  ### Adducts
  step <- "prepare_adducts"
  params <- get_params(step = step)
  prepare_adducts()

  ## Performing MS2 annotation
  ### Normal
  step <- "process_spectra"
  params <- get_params(step = step)
  process_spectra()
  ### Variant
  process_spectra(
    fast = FALSE,
    condition = "AND"
  )

  ### GNPS results
  step <- "prepare_gnps"
  params <- get_params(step = step)
  prepare_gnps()

  ### SIRIUS results
  step <- "prepare_sirius"
  params <- get_params(step = step)
  prepare_sirius()

  ### ISDB results
  step <- "prepare_spectral_matches"
  params <- get_params(step = step)
  prepare_spectral_matches()

  ### Edges
  step <- "prepare_edges"
  params <- get_params(step = step)
  prepare_edges()

  ### Fake edges
  step <- "fake_edges"
  params <- get_params(step = step)
  fake_edges()

  ### Features components
  step <- "prepare_features_components"
  params <- get_params(step = step)
  prepare_features_components()

  ### Fake features components
  step <- "fake_features_components"
  params <- get_params(step = step)
  fake_features_components()

  ### Features classification
  step <- "prepare_features_classification"
  params <- get_params(step = step)
  #### Fake no RT
  fake_no_rt(
    input = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_filled.tsv.gz",
    output = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_filled_no_rt.tsv.gz"
  )
  #### Fake no RT classification
  prepare_features_classification(
    input = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_filled_no_rt.tsv.gz",
    output = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_treated_no_rt.tsv.gz"
  )
  #### Normal
  prepare_features_classification()

  ### Taxa
  step <- "prepare_taxa"
  params <- get_params(step = step)
  prepare_taxa()

  ## Perform TIMA
  step <- "process_annotations"
  params <- get_params(step = step)
  ### Normal
  process_annotations()
  ### No MS1
  process_annotations(
    annotations = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_treated_no_rt.tsv.gz",
    annotate = FALSE
  )
  ### Only MS1
  process_annotations(
    annotations = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_treated_no_rt.tsv.gz",
    ms_mode = "neg",
    ms1_only = TRUE
  )

  ## CLI arguments check
  arguments <<- character()
  arguments$ann_can_ini <<- "x"
  arguments$ann_can_fin <<- "x"
  arguments$ann_ms1only <<- "x"
  arguments$ann_ms1_ann <<- "x"
  arguments$ann_ms1_thr_bio <<- "x"
  arguments$ann_ms1_thr_che <<- "x"
  arguments$ann_ms1_thr_con <<- "x"
  arguments$ann_ms2_app <<- "x"
  arguments$ann_ms2_met <<- "x"
  arguments$ann_ms2_thr_con <<- "x"
  arguments$ann_ms2_thr_pea_abs <<- "x"
  arguments$ann_ms2_thr_pea_rat <<- "x"
  arguments$ann_ms2_thr_sim <<- "x"
  arguments$fil_pat <<- "x"
  arguments$fil_ann_raw_spe <<- "x"
  arguments$fil_ann_raw_sir <<- "x"
  arguments$fil_ann_pre <<- "x"
  arguments$fil_ann_fil <<- "x"
  arguments$fil_ann_tre <<- "x"
  arguments$fil_ann_pro <<- "x"
  arguments$fil_fea_raw <<- "x"
  arguments$fil_lib_add_pro <<- "x"
  arguments$fil_lib_sop_raw_clo <<- "x"
  arguments$fil_lib_sop_raw_lot <<- "x"
  arguments$fil_lib_sop_pro <<- "x"
  arguments$fil_lib_sop_mer <<- "x"
  arguments$fil_lib_spe_neg <<- "x"
  arguments$fil_lib_spe_pos <<- "x"
  arguments$fil_net_spe_edg_raw <<- "x"
  arguments$fil_net_spe_edg_pro <<- "x"
  arguments$fil_net_spe_com_raw <<- "x"
  arguments$fil_tax_raw <<- "x"
  arguments$fil_tax_pro <<- "x"
  arguments$fil_spe_raw <<- "x"
  arguments$gnps_id <<- "x"
  arguments$gnps_workflow <<- "x"
  arguments$ms_add_neg <<- "x"
  arguments$ms_add_pos <<- "x"
  arguments$ms_int_thr_ms1 <<- "x"
  arguments$ms_int_thr_ms2 <<- "x"
  arguments$ms_pol <<- "x"
  arguments$ms_tol_mas_ppm_ms1 <<- "x"
  arguments$ms_tol_mas_ppm_ms2 <<- "x"
  arguments$ms_tol_mas_dal_ms1 <<- "x"
  arguments$ms_tol_mas_dal_ms2 <<- "x"
  arguments$ms_tol_rt_min <<- "x"
  arguments$names_extension <<- "x"
  arguments$names_features <<- "x"
  arguments$names_precursor <<- "x"
  arguments$names_rt <<- "x"
  arguments$names_source <<- "x"
  arguments$names_target <<- "x"
  arguments$names_taxon <<- "x"
  arguments$org_can <<- "x"
  arguments$org_fil_mod <<- "x"
  arguments$org_fil_lev <<- "x"
  arguments$org_fil_val <<- "x"
  arguments$org_tax <<- "x"
  arguments$too_met <<- "x"
  arguments$too_net_spe_com <<- "x"
  arguments$too_net_spe_edg <<- "x"
  arguments$wei_glo_bio <<- "x"
  arguments$wei_glo_che <<- "x"
  arguments$wei_glo_spe <<- "x"
  arguments$wei_bio_01 <<- "x"
  arguments$wei_bio_02 <<- "x"
  arguments$wei_bio_03 <<- "x"
  arguments$wei_bio_04 <<- "x"
  arguments$wei_bio_05 <<- "x"
  arguments$wei_bio_06 <<- "x"
  arguments$wei_bio_07 <<- "x"
  arguments$wei_bio_08 <<- "x"
  arguments$wei_bio_09 <<- "x"
  arguments$wei_bio_10 <<- "x"
  arguments$wei_bio_11 <<- "x"
  arguments$wei_bio_12 <<- "x"
  arguments$wei_bio_13 <<- "x"
  arguments$wei_bio_14 <<- "x"
  arguments$wei_bio_15 <<- "x"
  arguments$wei_che_01 <<- "x"
  arguments$wei_che_02 <<- "x"
  arguments$wei_che_03 <<- "x"
  arguments$fast <<- "x"
  arguments$force <<- "x"
  arguments$parallel <<- "x"

  parse_cli_params()

  succeed()
})
