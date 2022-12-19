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
  get_example_mgf(url = paths$url$example_mgf_mini)
  # get_example_mgf()

  #### SIRIUS
  ## mini version for tests
  get_example_sirius(url = paths$urls$example_sirius_mini)
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
    url = paths$url$example_spectral_lib,
    destfile = paths$data$source$spectra$lotus$pos
  )
  utils::download.file(
    url = paths$url$example_spectral_lib,
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
  ### Long
  process_spectra(
    condition = "AND",
    quickmode = FALSE
  )

  ### ISDB results
  step <- "prepare_spectral_matches"
  params <- get_params(step = step)
  prepare_spectral_matches()

  ### GNPS results
  step <- "prepare_gnps"
  params <- get_params(step = step)
  prepare_gnps()

  ### SIRIUS results
  step <- "prepare_sirius"
  params <- get_params(step = step)
  prepare_sirius()

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
    input = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_filled.tsv.gz",
    output = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_filled_no_rt.tsv.gz"
  )
  #### Fake no RT classification
  prepare_features_classification(
    input = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_filled_no_rt.tsv.gz",
    output = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_treated_no_rt.tsv.gz"
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
    isdb = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_treated_no_rt.tsv.gz",
    annotate = FALSE
  )
  ### Only MS1
  process_annotations(
    isdb = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_treated_no_rt.tsv.gz",
    ms_mode = "neg",
    ms1_only = TRUE
  )

  ## CLI arguments check
  arguments <<- character()
  arguments$biological <<- "x"
  arguments$column.name <<- "x"
  arguments$complement <<- "x"
  arguments$components <<- "x"
  arguments$condition <<- "x"
  arguments$dalton <<- "x"
  arguments$directory <<- "x"
  arguments$edges <<- "x"
  arguments$extension <<- "x"
  arguments$features <<- "x"
  arguments$filter <<- "x"
  arguments$gnps <<- "x"
  arguments$input <<- "x"
  arguments$isdb <<- "x"
  arguments$j.top <<- "x"
  arguments$k.top <<- "x"
  arguments$level <<- "x"
  arguments$library <<- "x"
  arguments$method <<- "x"
  arguments$mode <<- "x"
  arguments$ms <<- "x"
  arguments$name <<- "x"
  arguments$nap <<- "x"
  arguments$npc <<- "x"
  arguments$npeaks <<- "x"
  arguments$output <<- "x"
  arguments$parallel <<- "x"
  arguments$ppm <<- "x"
  arguments$precursor <<- "x"
  arguments$qemical <<- "x"
  arguments$quickmode <<- "x"
  arguments$rpeaks <<- "x"
  arguments$rt <<- "x"
  arguments$similarity <<- "x"
  arguments$source <<- "x"
  arguments$spectral <<- "x"
  arguments$target <<- "x"
  arguments$taxon <<- "x"
  arguments$threshold <<- "x"
  arguments$tool <<- "x"
  arguments$value <<- "x"
  arguments$workflow <<- "x"
  arguments$xbim <<- "x"
  arguments$zirius <<- "x"
  arguments$force <<- "x"

  parse_cli_params()

  succeed()
})
