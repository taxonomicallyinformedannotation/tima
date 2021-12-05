rootdir <- "../.."

# test log_debug

testthat::test_that("parsing YAML path", {
  with_dir(dir = rootdir, expr = {
    paths <- parse_yaml_paths()
    vars <- ls(all.names = TRUE)
    for (i in 1:length(vars)) {
      assign(vars[i], get(vars[i]), envir = .GlobalEnv)
    }
  })
  succeed()
})

testthat::test_that("downloading LOTUS", {
  with_dir(dir = rootdir, expr = {
    get_lotus()
  })
  succeed()
})

testthat::test_that("downloading ISDB example", {
  with_dir(dir = rootdir, expr = {
    get_example_isdb()
  })
  succeed()
})

testthat::test_that("preparing LOTUS", {
  with_dir(dir = rootdir, expr = {
    prepare_lotus()
  })
  succeed()
})

testthat::test_that("getting parameters", {
  with_dir(dir = rootdir, expr = {
    params <-
      get_params(step = "prepare_closed")
  }) ## TODO other examples
  succeed()
})

step <- "prepare_closed"
testthat::test_that("preparing closed", {
  with_dir(dir = rootdir, expr = {
    params <- get_params(step = step)
    prepare_closed()
  })
  succeed()
})

step <- "prepare_library"
testthat::test_that("preparing library", {
  with_dir(dir = rootdir, expr = {
    params <- get_params(step = step)
    prepare_library()
  })
  succeed()
})

step <- "prepare_adducts"
testthat::test_that("preparing adducts", {
  with_dir(dir = rootdir, expr = {
    params <- get_params(step = step)
    prepare_adducts()
  })
  succeed()
})

step <- "prepare_isdb"
testthat::test_that("preparing ISDB results", {
  with_dir(dir = rootdir, expr = {
    params <- get_params(step = step)
    prepare_isdb()
  })
  succeed()
})

step <- "prepare_gnps"
testthat::test_that("preparing GNPS results", {
  with_dir(dir = rootdir, expr = {
    params <- get_params(step = step)
    prepare_gnps()
  })
  succeed()
})

# step <- "prepare_sirius"
# testthat::test_that("preparing SIRIUS results", {
#   with_dir(dir = rootdir, expr = {
#     params <- get_params(step = step)
#     prepare_sirius()
#   })
#   succeed()
# })

step <- "prepare_edges"
testthat::test_that("preparing edges", {
  with_dir(dir = rootdir, expr = {
    params <- get_params(step = step)
    prepare_edges()
  })
  succeed()
})

step <- "prepare_features_components"
testthat::test_that("preparing features components", {
  with_dir(dir = rootdir, expr = {
    params <- get_params(step = step)
    prepare_features_components()
  })
  succeed()
})

step <- "prepare_features_classification"
testthat::test_that("preparing features classification", {
  with_dir(dir = rootdir, expr = {
    params <- get_params(step = step)
    prepare_features_classification()
  })
  succeed()
})

step <- "prepare_taxa"
testthat::test_that("preparing taxa", {
  with_dir(dir = rootdir, expr = {
    params <- get_params(step = step)
    prepare_taxa()
  })
  succeed()
})

step <- "process_annotations"
testthat::test_that("processing annotations", {
  with_dir(dir = rootdir, expr = {
    params <- get_params(step = step)
    process_annotations()
  })
  succeed()
})
