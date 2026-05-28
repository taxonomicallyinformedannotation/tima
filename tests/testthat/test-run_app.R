# Test Suite: run_app ----

library(testthat)

test_that("run_app validates parameters", {
  expect_error(run_app(host = ""), "host")
  expect_error(run_app(port = 0), "port")
  expect_error(run_app(port = 70000), "port")
  expect_error(run_app(browser = "yes"), "browser")
})

test_that("app_path_exists reports file existence", {
  tmp <- tempfile(fileext = ".R")
  writeLines("# app stub", tmp)

  expect_true(app_path_exists(tmp))
  expect_false(app_path_exists(paste0(tmp, ".missing")))
})

test_that("is_docker_env returns a scalar logical", {
  out <- is_docker_env()
  expect_type(out, "logical")
  expect_length(out, 1)
})

test_that("get_app_path returns a character scalar", {
  out <- get_app_path()
  expect_type(out, "character")
  expect_length(out, 1)
})

test_that("run_app launches shiny app in standard mode", {
  calls <- new.env(parent = emptyenv())
  calls$run <- NULL
  calls$installed <- 0L

  with_mocked_bindings(
    is_docker_env = function() FALSE,
    log_info = function(...) invisible(NULL),
    install_tima = function() {
      calls$installed <- calls$installed + 1L
      invisible(NULL)
    },
    get_app_path = function() "dummy/app.R",
    app_path_exists = function(path) identical(path, "dummy/app.R"),
    build_shiny_app_dir = function(app_path) paste0("APPDIR::", app_path),
    run_shiny_app = function(appDir, port, host, launch.browser) {
      calls$run <- list(
        appDir = appDir,
        port = port,
        host = host,
        launch.browser = launch.browser
      )
      invisible(NULL)
    },
    {
      expect_null(run_app(host = "127.0.0.1", port = 3838, browser = TRUE, reinstall = FALSE))
    }
  )

  expect_identical(calls$installed, 0L)
  expect_identical(calls$run$appDir, "APPDIR::dummy/app.R")
  expect_identical(calls$run$host, "127.0.0.1")
  expect_identical(calls$run$port, 3838L)
  expect_true(calls$run$launch.browser)
})

test_that("run_app enforces Docker host/browser overrides and reinstall", {
  calls <- new.env(parent = emptyenv())
  calls$installed <- 0L
  calls$run <- NULL

  with_mocked_bindings(
    is_docker_env = function() TRUE,
    log_info = function(...) invisible(NULL),
    install_tima = function() {
      calls$installed <- calls$installed + 1L
      invisible(NULL)
    },
    get_app_path = function() "dummy/app.R",
    app_path_exists = function(path) identical(path, "dummy/app.R"),
    build_shiny_app_dir = function(app_path) app_path,
    run_shiny_app = function(appDir, port, host, launch.browser) {
      calls$run <- list(appDir = appDir, port = port, host = host, launch.browser = launch.browser)
      invisible(NULL)
    },
    {
      expect_null(run_app(host = "127.0.0.1", port = 9999, browser = TRUE, reinstall = TRUE))
    }
  )

  expect_identical(calls$installed, 1L)
  expect_identical(calls$run$host, "0.0.0.0")
  expect_false(calls$run$launch.browser)
  expect_identical(calls$run$port, 9999L)
})

test_that("run_app aborts when app file is missing", {
  with_mocked_bindings(
    is_docker_env = function() FALSE,
    log_info = function(...) invisible(NULL),
    get_app_path = function() "",
    app_path_exists = function(path) FALSE,
    {
      expect_error(
        run_app(reinstall = FALSE),
        class = "tima_runtime_error"
      )
    }
  )
})

