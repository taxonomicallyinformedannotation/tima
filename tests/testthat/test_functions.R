## need to do all in one because of outputs needed in the same temp dir
testthat::test_that("Whole process", {
  setwd("../../")
  cat(list.files())
  system(
    command =
      "
    mkdir inst
    cp -R params inst/params
    cp -R scripts inst/scripts ## because of docopt steps
    cp paths.yaml inst/paths.yaml
    "
  )

  ## Prepare parameters
  paths <<- parse_yaml_paths()
  step <- "prepare_params"
  params <- get_params(step = step)
  ## Prepare config for a single step only
  prepare_params(step = "weight_annotations")
  ## For all steps
  prepare_params()
  ## With empty GNPS ID
  prepare_params(gnps_job_id = "")
  ## When previous params exist
  prepare_params()

  ## Get all files
  ### Features table
  get_gnps_tables(
    filename = "example",
    path_features = paths$data$source$features,
    path_metadata = paths$data$source$metadata,
    path_spectra = paths$data$source$spectra,
    gnps_job_id = params$gnps$id
  )
  ### Metadata table
  get_file(
    url = paths$urls$examples$metadata |>
      gsub(pattern = ".tsv", replacement = "_unrecognized.tsv"),
    export = paths$data$source$metadata |>
      gsub(pattern = ".tsv", replacement = "_unrecognized.tsv")
  )
  ## Other GNPS job id (without metadata)
  get_gnps_tables(
    filename = "other",
    path_features = paths$data$source$features,
    path_metadata = paths$data$source$metadata,
    path_spectra = paths$data$source$spectra,
    gnps_job_id = paths$gnps$example2
  )
  ## When no GNPS job ID and no metadata are given
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
  # get_file(url = paths$urls$examples$spectra,
  #               export = paths$data$source$spectra)

  ### Spectral library with rt
  get_file(
    url = paths$urls$examples$spectral_lib_mini$with_rt,
    export = paths$data$source$libraries$spectra$exp$with_rt
  )

  #### SIRIUS
  ## mini version for tests
  get_example_sirius()

  ### Libraries
  #### ECMDB
  get_file(
    url = paths$urls$ecmdb$metabolites,
    export = paths$data$source$libraries$sop$ecmdb
  )

  #### HMDB
  # get_hmdb()

  #### LOTUS
  get_last_version_from_zenodo(
    doi = paths$urls$lotus$doi,
    pattern = paths$urls$lotus$pattern,
    path = paths$data$source$libraries$sop$lotus
  )
  ## Check it does not download it a second time
  get_last_version_from_zenodo(
    doi = paths$urls$lotus$doi,
    pattern = paths$urls$lotus$pattern,
    path = paths$data$source$libraries$sop$lotus
  )

  #### ISDB
  ## smaller version for testing
  # get_file(
  #   url = paths$urls$examples$spectral_lib$pos,
  #   export = paths$data$source$libraries$spectra$is$lotus$pos |>
  #     gsub(pattern = "isdb_pos.mgf", replacement = "lotus_pos.rds")
  # )
  get_file(
    url = paths$urls$examples$spectral_lib_mini$pos,
    export = paths$data$source$libraries$spectra$is$lotus$pos
  )
  # get_file(
  #   url = paths$urls$examples$spectral_lib$neg,
  #   export = paths$data$source$libraries$spectra$is$lotus$neg |>
  #     gsub(pattern = "isdb_neg.mgf", replacement = "lotus_neg.rds")
  # )
  get_file(
    url = paths$urls$examples$spectral_lib_mini$neg,
    export = paths$data$source$libraries$spectra$is$lotus$neg
  )

  ## Prepare libraries
  ### Closed
  step <- "prepare_libraries_sop_closed"
  params <- get_params(step = step)
  ## To do as if there was an input
  prepare_libraries_sop_closed(input = paths$data$source$libraries$sop$lotus)
  ## When there is no input
  prepare_libraries_sop_closed()

  ### ECMDB
  step <- "prepare_libraries_sop_ecmdb"
  params <- get_params(step = step)
  ## To do as if there was an input
  prepare_libraries_sop_ecmdb(input = "randomNonExistingFile")
  ## When there is no input
  prepare_libraries_sop_ecmdb()

  ### HMDB
  # step <- "prepare_libraries_sop_hmdb"
  # params <- get_params(step = step)
  # prepare_libraries_sop_hmdb()

  ### LOTUS
  step <- "prepare_libraries_sop_lotus"
  params <- get_params(step = step)
  prepare_libraries_sop_lotus()

  ### SOP library
  step <- "prepare_libraries_sop_merged"
  params <- get_params(step = step)
  prepare_libraries_sop_merged(
    filter = TRUE,
    level = "family",
    value = "Simaroubaceae|Gentianaceae"
  )
  prepare_libraries_sop_merged()

  ## Prepare spectra
  step <- "prepare_libraries_spectra"
  params <- get_params(step = step)
  ### LOTUS
  col_args <- list(
    col_ce = NULL,
    col_em = "EXACTMASS",
    col_in = NULL,
    col_io = "INCHI",
    col_ik = NULL,
    col_il = "NAME",
    col_mf = "MOLECULAR_FORMULA",
    col_na = NULL,
    col_po = "IONMODE",
    col_sm = NULL,
    col_sn = "SMILES",
    col_si = NULL,
    col_sp = NULL,
    col_sy = NULL,
    col_xl = NULL
  )
  ## Cannot pass it as such to do.call
  meta_args <- CompoundDb::make_metadata(
    source = "LOTUS",
    url = "https://doi.org/10.5281/zenodo.5607185",
    source_version = jsonlite::fromJSON(
      txt = "https://zenodo.org/api/records/5607185"
    )$doi_url,
    source_date = jsonlite::fromJSON(
      txt = "https://zenodo.org/api/records/5607185"
    )[["metadata"]][["publication_date"]],
    organism = "Life"
  )
  ## Pos
  do.call(
    what = prepare_libraries_spectra,
    args = c(
      col_args,
      col_ci = "FILENAME",
      input = params$files$libraries$spectral$is$raw[[2]] |>
        gsub(pattern = "lotus_pos.rds", replacement = "isdb_pos.mgf"),
      output = params$files$libraries$spectral$is$pos
    )
  )
  ## Check the library already exists warning
  prepare_libraries_spectra(
    input = params$files$libraries$spectral$is$raw[[2]] |>
      gsub(pattern = "lotus_pos.rds", replacement = "isdb_pos.mgf"),
    output = params$files$libraries$spectral$is$pos,
    metad = meta_args
  )
  ## Neg & without metadata
  do.call(
    what = prepare_libraries_spectra,
    args = c(
      col_args,
      col_ci = "FILENAME",
      input = params$files$libraries$spectral$is$raw[[1]] |>
        gsub(pattern = "lotus_neg.rds", replacement = "isdb_neg.mgf"),
      output = params$files$libraries$spectral$is$neg,
      polarity = "neg"
    )
  )
  ## Classical
  prepare_libraries_spectra()
  prepare_libraries_spectra(polarity = "neg")
  prepare_libraries_spectra(
    polarity = "neg",
    output = params$files$libraries$spectral$exp |>
      gsub(pattern = ".sqlite", replacement = ".mgf")
  )

  ### HMDB
  # prepare_isdb_hmdb()

  ### Closed
  # prepare_mona()

  ### Adducts
  step <- "prepare_libraries_adducts"
  params <- get_params(step = step)
  prepare_libraries_adducts()

  ### Features
  step <- "prepare_features_tables"
  params <- get_params(step = step)
  prepare_features_tables()

  ## Performing MS1 annotation
  step <- "annotate_masses"
  params <- get_params(step = step)
  ### Negative
  annotate_masses(ms_mode = "neg")
  ### Positive
  annotate_masses(ms_mode = "pos")

  ## Performing MS2 annotation
  step <- "annotate_spectra"
  params <- get_params(step = step)
  ### Negative
  annotate_spectra(
    polarity = "neg",
    parallel = FALSE
  )
  ### Slow
  annotate_spectra(
    fast = FALSE,
    condition = "AND"
  )
  ### Normal
  annotate_spectra()

  ## Create MS2 based edges
  step <- "create_edges_spectra"
  params <- get_params(step = step)
  ### Slow
  create_edges_spectra(
    parallel = FALSE,
    fast = FALSE,
    condition = "AND"
  )
  ### Normal
  create_edges_spectra(condition = "OR")

  ## additional test not covered by lapply
  spectra <- params$files$spectral$raw |>
    import_spectra()
  spectra <- spectra |>
    sanitize_spectra(cutoff = params$ms$intensity$thresholds$ms2)
  single_pair <- spectra@backend@peaksData[1:2]
  # single_pair[[1]] <- single_pair[[1]] |>
  #   normalize_peaks()
  # single_pair[[2]] <- single_pair[[2]] |>
  #   normalize_peaks()
  precursors <- spectra$precursorMz[1:2]
  nspe <- length(single_pair)
  create_edges_parallel(1,
    frags = single_pair,
    precs = precursors,
    nspecs = nspe
  )
  ##

  ### GNPS results
  step <- "prepare_annotations_gnps"
  params <- get_params(step = step)
  prepare_annotations_gnps(input = "fileDoesNotExist")
  prepare_annotations_gnps(input = NULL)
  prepare_annotations_gnps()

  ### SIRIUS results
  step <- "prepare_annotations_sirius"
  params <- get_params(step = step)
  ## To do as if there was no input
  prepare_annotations_sirius(input_directory = "randomDirThatDoesNotExist")
  ## When there is an input
  prepare_annotations_sirius(
    input_directory =
      params$files$annotations$raw$sirius
  )

  ### ISDB results
  step <- "prepare_annotations_spectra"
  params <- get_params(step = step)
  prepare_annotations_spectra()

  ### Edges
  step <- "prepare_features_edges"
  params <- get_params(step = step)
  prepare_features_edges()

  ## Create components
  step <- "create_components"
  params <- get_params(step = step)
  create_components()

  ### Features components
  step <- "prepare_features_components"
  params <- get_params(step = step)
  prepare_features_components()

  ### Taxa
  step <- "prepare_taxa"
  params <- get_params(step = step)
  ## Forcing all features to a single source organism
  prepare_taxa(taxon = "Homo sapiens")
  ## When empty
  prepare_taxa(taxon = "")
  ## Without file extension in the column names
  prepare_taxa(extension = FALSE)
  ## Testing for unrecognized taxa
  prepare_taxa(
    metadata = paths$data$source$metadata |>
      gsub(pattern = ".tsv", replacement = "_unrecognized.tsv")
  )
  ## Attributing based on intensity (multiple source organisms)
  prepare_taxa()
  ## Tests for ott API
  fake_taxon_df <- data.frame("organism" = "Gentiana lutea")
  get_organism_taxonomy_ott(df = fake_taxon_df)
  get_organism_taxonomy_ott(
    df = fake_taxon_df,
    url = "https://api.opentreeoflife.org/v3/taxonomy/fakeDown"
  )

  ## Perform TIMA
  step <- "weight_annotations"
  params <- get_params(step = step)
  ### Normal
  weight_annotations(
    candidates_final = 1,
    minimal_ms1_bio = 0.8
  )
  ### Only MS1
  weight_annotations(
    ms1_only = TRUE,
    summarise = FALSE,
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
  arguments$fil_fea_pre <<- "x"
  arguments$fil_lib_add_pro <<- "x"
  arguments$fil_lib_sop_raw_clo <<- "x"
  arguments$fil_lib_sop_raw_ecm <<- "x"
  arguments$fil_lib_sop_raw_lot <<- "x"
  arguments$fil_lib_sop_pro <<- "x"
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
  arguments$names_mgf_io <<- "x"
  arguments$names_mgf_ik <<- "x"
  arguments$names_mgf_il <<- "x"
  arguments$names_mgf_mf <<- "x"
  arguments$names_mgf_na <<- "x"
  arguments$names_mgf_po <<- "x"
  arguments$names_mgf_pc <<- "x"
  arguments$names_mgf_pm <<- "x"
  arguments$names_mgf_sm <<- "x"
  arguments$names_mgf_sn <<- "x"
  arguments$names_mgf_si <<- "x"
  arguments$names_mgf_sp <<- "x"
  arguments$names_mgf_sy <<- "x"
  arguments$names_mgf_xl <<- "x"
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
  arguments$wei_che_11 <<- "x"
  arguments$wei_che_12 <<- "x"
  arguments$wei_che_13 <<- "x"
  arguments$wei_che_14 <<- "x"
  arguments$wei_che_21 <<- "x"
  arguments$wei_che_22 <<- "x"
  arguments$wei_che_23 <<- "x"
  arguments$fast <<- "x"
  arguments$force <<- "x"
  arguments$parallel <<- "x"
  arguments$summarise <<- "x"

  parse_cli_params()

  succeed()
})
