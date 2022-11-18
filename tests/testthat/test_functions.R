#' need to do all in one because of outputs needed in the same temp dir
testthat::test_that("Whole process", {
  ## 0.1
  paths <- parse_yaml_paths()
  vars <- ls(all.names = TRUE)
  for (i in 1:length(vars)) {
    assign(vars[i], get(vars[i]), envir = .GlobalEnv)
  }
  step <- "prepare_params"
  params <- get_params(step = step)
  prepare_params()

  ## 1.1
  get_last_version_from_zenodo(
    doi = paths$url$lotus$doi,
    pattern = paths$urls$lotus$pattern,
    path = paths$data$source$libraries$lotus
  )

  ## 1.1.b
  get_benchmark(url = "https://raw.githubusercontent.com/matchms/matchms/master/tests/massbank_five_spectra.msp")

  ## 1.2
  prepare_lotus()

  ## 1.2.1
  # source(file = "inst/scripts/prepare_closed.R")

  ## 1.3
  step <- "prepare_library"
  params <- get_params(step = step)
  prepare_library(
    filter = TRUE,
    level = "family",
    value = "Simaroubaceae|Gentianaceae"
  )
  prepare_library()

  ## 1.4
  step <- "prepare_adducts"
  params <- get_params(step = step)
  prepare_adducts()

  ## 2.a
  get_example_isdb()

  ## 2.b
  get_example_sirius()

  ## 2.c
  get_example_feature_table()

  ## 3.a
  step <- "prepare_isdb"
  params <- get_params(step = step)
  prepare_isdb()

  ## 3.b
  step <- "prepare_sirius"
  params <- get_params(step = step)
  prepare_sirius()

  # 3.c
  step <- "prepare_gnps"
  params <- get_params(step = step)
  prepare_gnps()

  ## 4.1.a
  step <- "prepare_edges"
  params <- get_params(step = step)
  prepare_edges()

  ## 4.1.b
  step <- "fake_edges"
  params <- get_params(step = step)
  fake_edges()

  ## 4.2.a
  step <- "prepare_features_components"
  params <- get_params(step = step)
  prepare_features_components()

  ## 4.2.b
  step <- "fake_features_components"
  params <- get_params(step = step)
  fake_features_components()

  ## 4.3
  step <- "prepare_features_classification"
  params <- get_params(step = step)
  fake_no_rt(
    input = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_filled.tsv.gz",
    output = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_filled_no_rt.tsv.gz"
  )
  prepare_features_classification(
    input = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_filled_no_rt.tsv.gz",
    output = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_treated_no_rt.tsv.gz"
  )
  prepare_features_classification()

  ## 4.4
  step <- "prepare_taxa"
  params <- get_params(step = step)
  prepare_taxa()

  ## 4.5
  step <- "process_annotations"
  params <- get_params(step = step)
  process_annotations()
  process_annotations(
    isdb = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_treated_no_rt.tsv.gz",
    annotate = FALSE
  )
  process_annotations(
    isdb = "data/interim/annotations/96fa7c88200e4a03bee4644e581e3fb0_isdb_treated_no_rt.tsv.gz",
    ms_mode = "neg",
    ms1_only = TRUE
  )

  ## 5
  ## CLI
  arguments <<- character()
  arguments$biological <<- "x"
  arguments$column.name <<- "x"
  arguments$complement <<- "x"
  arguments$components <<- "x"
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
  arguments$mode <<- "x"
  arguments$ms <<- "x"
  arguments$name <<- "x"
  arguments$nap <<- "x"
  arguments$npc <<- "x"
  arguments$output <<- "x"
  arguments$ppm <<- "x"
  arguments$precursor <<- "x"
  arguments$qemical <<- "x"
  arguments$quickmode <<- "x"
  arguments$rt <<- "x"
  arguments$source <<- "x"
  arguments$spectral <<- "x"
  arguments$target <<- "x"
  arguments$taxon <<- "x"
  arguments$tool <<- "x"
  arguments$value <<- "x"
  arguments$workflow <<- "x"
  arguments$xbim <<- "x"
  arguments$zirius <<- "x"
  arguments$force <<- "x"

  parse_cli_params()

  succeed()
})
