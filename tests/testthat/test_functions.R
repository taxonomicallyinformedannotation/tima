# test log_debug

testthat::test_that("parsing YAML path", {
  paths <- parse_yaml_paths()
  vars <- ls(all.names = TRUE)
  for (i in 1:length(vars)) {
    assign(vars[i], get(vars[i]), envir = .GlobalEnv)
  }
  testthat::succeed()
})

## need to do all in one because of outputs needed in the same temp dir
testthat::test_that("Whole process", {
  ## 1.1
  get_lotus()

  ## 1.1.a
 adding test get_isdb()

  ## 1.2
  prepare_lotus()

  ## 1.3
  step <- "prepare_closed"
  params <- get_params(step = step)
  prepare_closed()

  ## 1.4
  step <- "prepare_library"

  ## 1.4.a
  params <- get_params(step = step)
  params$filter$mode <- TRUE
  params$filter$level <- "family"
  params$filter$level <- "Simaroubaceae|Gentianaceae"
  prepare_library()

  ## 1.4.b
  params <- get_params(step = step)
  prepare_library()

  ## 1.5
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
  prepare_features_classification()

  ## 4.4
  step <- "prepare_taxa"
  params <- get_params(step = step)
  prepare_taxa()

  ## 4.5
  step <- "process_annotations"
  params <- get_params(step = step)
  process_annotations(annotate = TRUE)
  process_annotations(annotate = FALSE)
  succeed()
})

# ...
