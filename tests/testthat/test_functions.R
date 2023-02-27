## need to do all in one because of outputs needed in the same temp dir
testthat::test_that("Whole process", {
  ## Prepare parameters
  paths <- parse_yaml_paths()
  vars <- ls(all.names = TRUE)
  for (i in 1:length(vars)) {
    assign(vars[i], get(vars[i]), envir = .GlobalEnv)
  }
  step <- "prepare_config"
  params <- get_params(step = step)
  prepare_config()

  ## Get all files
  ### Features table
  if (!is.null(params$gnps$id)) {
    get_gnps_tables(gnps_job_id = params$gnps$id)
  } else {
    download_file(
      url = paths$urls$examples$features,
      export = paths$data$source$features
    )

    ### Metadata table
    download_file(
      url = paths$urls$examples$metadata,
      export = paths$data$source$metadata
    )
  }

  ### Spectra
  #### Mini version for tests
  ## Not including it in else statement above on purpose
  download_file(
    url = paths$url$examples$spectra_mini,
    export = paths$data$source$spectra
  )
  # download_file(url = paths$url$examples$spectra,
  #               export = paths$data$source$spectra)

  ### Spectral library with rt
  download_file(
    url = paths$url$examples$spectral_lib_mini$with_rt,
    export = paths$data$source$libraries$spectra$with_rt
  )

  #### SIRIUS
  ## mini version for tests
  sirius_mini <- paths$data$interim$annotations$example_sirius |>
    gsub(pattern = ".zip", replacement = "_mini.zip")
  download_file(
    url = paths$urls$examples$sirius_mini,
    export = sirius_mini
  )
  message("Unzipping")
  utils::unzip(
    zipfile = sirius_mini,
    exdir = dirname(sirius_mini)
  )
  # download_file(
  #   url = paths$urls$examples$sirius,
  #   export = paths$data$interim$annotations$example_sirius
  # )
  # message("Unzipping")
  # utils::unzip(
  #   zipfile = paths$data$interim$annotations$example_sirius,
  #   exdir = dirname(paths$data$interim$annotations$example_sirius)
  # )

  #### LOTUS
  get_last_version_from_zenodo(
    doi = paths$url$lotus$doi,
    pattern = paths$urls$lotus$pattern,
    path = paths$data$source$libraries$sop$lotus
  )
  ## Check it does not download it a second time
  get_last_version_from_zenodo(
    doi = paths$url$lotus$doi,
    pattern = paths$urls$lotus$pattern,
    path = paths$data$source$libraries$sop$lotus
  )

  #### HMDB
  # get_hmdb()

  #### ISDB
  ## smaller version for testing
  create_dir(paths$data$source$libraries$spectra$lotus$pos)
  utils::download.file(
    url = paths$url$examples$spectral_lib$pos,
    destfile = paths$data$source$libraries$spectra$lotus$pos
  )
  utils::download.file(
    url = paths$url$examples$spectral_lib$neg,
    destfile = paths$data$source$libraries$spectra$lotus$neg
  )

  ## Prepare libraries
  ### LOTUS
  prepare_lotus()

  ### HMDB
  # prepare_hmdb()

  ### Closed
  step <- "prepare_closed"
  params <- get_params(step = step)
  ## To fake there is an input
  prepare_closed(input = paths$data$source$libraries$sop$lotus)
  ## When there is no input
  prepare_closed()

  ### Structural library
  step <- "prepare_libraries"
  params <- get_params(step = step)
  prepare_libraries(
    filter = TRUE,
    level = "family",
    value = "Simaroubaceae|Gentianaceae"
  )
  prepare_libraries()

  ## Prepare spectra
  step <- "prepare_spectral_libraries"
  params <- get_params(step = step)
  ### LOTUS
  ## Pos
  prepare_spectral_libraries(
    input = paths$data$source$libraries$spectra$lotus$pos,
    output = paths$data$interim$libraries$spectra$lotus$pos,
    col_ce = NULL,
    col_ci = "FILENAME",
    col_em = "EXACTMASS",
    col_in = "INCHI",
    col_ik = "NAME",
    col_mf = "MOLECULAR_FORMULA",
    col_na = NULL,
    col_po = "IONMODE",
    col_sm = "SMILES",
    col_si = NULL,
    col_sp = NULL,
    col_sy = NULL,
    metad = CompoundDb::make_metadata(
      source = "LOTUS",
      url = "https://doi.org/10.5281/zenodo.5607185",
      source_version = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")$doi_url,
      source_date = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")[["metadata"]][["publication_date"]],
      organism = "Life"
    )
  )
  ## Check the library already exists warning
  prepare_spectral_libraries(
    input = paths$data$source$libraries$spectra$lotus$pos,
    output = paths$data$interim$libraries$spectra$lotus$pos
  )
  ## Neg & without metadata
  prepare_spectral_libraries(
    input = paths$data$source$libraries$spectra$lotus$neg,
    output = paths$data$interim$libraries$spectra$lotus$neg,
    col_ce = NULL,
    col_ci = "FILENAME",
    col_em = "EXACTMASS",
    col_in = "INCHI",
    col_ik = "NAME",
    col_mf = "MOLECULAR_FORMULA",
    col_na = NULL,
    col_po = "IONMODE",
    col_sm = "SMILES",
    col_si = NULL,
    col_sp = NULL,
    col_sy = NULL,
    polarity = "neg"
  )
  ## Classical
  prepare_spectral_libraries()
  prepare_spectral_libraries(
    polarity = "neg"
  )

  ### HMDB
  # prepare_isdb_hmdb()

  ### Closed
  # prepare_mona()

  ### Adducts
  step <- "prepare_adducts"
  params <- get_params(step = step)
  prepare_adducts()

  ## Performing MS2 annotation
  step <- "process_spectra"
  params <- get_params(step = step)
  ### Negative
  process_spectra(
    polarity = "neg",
    parallel = FALSE
  )
  ### Slow
  process_spectra(
    fast = FALSE,
    condition = "AND"
  )
  ### Normal
  process_spectra()

  ### GNPS results
  step <- "prepare_gnps"
  params <- get_params(step = step)
  prepare_gnps(input = "fileDoesNotExist")
  prepare_gnps()

  ### SIRIUS results
  step <- "prepare_sirius"
  params <- get_params(step = step)
  ## To fake there is no input
  prepare_sirius(input_directory = "randomDirThatDoesNotExist")
  ## When there is an input
  prepare_sirius(input_directory = params$files$annotations$raw$sirius |>
    gsub(pattern = "sirius", replacement = "sirius_mini"))

  ### ISDB results
  step <- "prepare_spectral_matches"
  params <- get_params(step = step)
  prepare_spectral_matches()

  ### Edges
  step <- "prepare_features_edges"
  params <- get_params(step = step)
  prepare_features_edges()

  ### Fake edges
  step <- "fake_features_edges"
  params <- get_params(step = step)
  fake_features_edges()

  ### Features components
  step <- "prepare_features_components"
  params <- get_params(step = step)
  ## neg
  prepare_features_components(ms_mode = "neg")
  ## pos
  prepare_features_components()

  ### Fake features components
  step <- "fake_features_components"
  params <- get_params(step = step)
  ## neg
  fake_features_components(ms_mode = "neg")
  ## pos
  fake_features_components()

  ### Taxa
  step <- "prepare_taxa"
  params <- get_params(step = step)
  ## Forcing all features to a single source organism
  prepare_taxa(taxon = "Homo sapiens")
  ## Without file extension in the column names
  prepare_taxa(extension = FALSE)
  ## Attributing based on intensity (multiple source organisms)
  prepare_taxa()

  ## Perform TIMA
  step <- "process_annotations"
  params <- get_params(step = step)
  ### Normal
  process_annotations(
    candidates_final = 1,
    minimal_ms1_bio = 0.8
  )
  ### No MS1
  process_annotations(
    annotate = FALSE,
    candidates_final = 1
  )
  ### Only MS1
  process_annotations(
    ms_mode = "neg",
    ms1_only = TRUE,
    candidates_final = 1,
    minimal_ms1_bio = 0.8
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
  arguments$fil_ann_pro <<- "x"
  arguments$fil_fea_raw <<- "x"
  arguments$fil_lib_add_pro <<- "x"
  arguments$fil_lib_sop_raw_clo <<- "x"
  arguments$fil_lib_sop_raw_lot <<- "x"
  arguments$fil_lib_sop_pro <<- "x"
  arguments$fil_lib_sop_mer <<- "x"
  arguments$fil_lib_spe_neg <<- "x"
  arguments$fil_lib_spe_pos <<- "x"
  arguments$fil_lib_spe_raw <<- "x"
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
  arguments$names_mgf_ce <<- "x"
  arguments$names_mgf_ci <<- "x"
  arguments$names_mgf_em <<- "x"
  arguments$names_mgf_in <<- "x"
  arguments$names_mgf_ik <<- "x"
  arguments$names_mgf_mf <<- "x"
  arguments$names_mgf_na <<- "x"
  arguments$names_mgf_po <<- "x"
  arguments$names_mgf_pc <<- "x"
  arguments$names_mgf_pm <<- "x"
  arguments$names_mgf_sm <<- "x"
  arguments$names_mgf_si <<- "x"
  arguments$names_mgf_sp <<- "x"
  arguments$names_mgf_sy <<- "x"
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
  arguments$too_tax_bio <<- "x"
  arguments$too_tax_che <<- "x"
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
