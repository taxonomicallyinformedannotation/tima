# Test Suite: run_app ----

library(testthat)

test_that("run_app validates parameters", {
  expect_error(run_app(host = ""), "host")
  expect_error(run_app(port = 0), "port")
  expect_error(run_app(port = 70000), "port")
  expect_error(run_app(browser = "yes"), "browser")
})

test_that("run_app aborts clearly when app file is missing", {
  local_mocked_bindings(
    install_tima = function() invisible(NULL),
    is_docker_env = function() FALSE,
    get_app_path = function() "",
    run_shiny_app = function(appDir, port, host, launch.browser) {
      force(appDir)
      force(port)
      force(host)
      force(launch.browser)
      invisible(NULL)
    }
  )

  expect_error(
    run_app(),
    regexp = "app file not found",
    class = "tima_runtime_error"
  )
})

test_that("run_app enforces Docker host and browser settings", {
  seen <- new.env(parent = emptyenv())

  local_mocked_bindings(
    install_tima = function() invisible(NULL),
    is_docker_env = function() TRUE,
    get_app_path = function() "/tmp/app.R",
    app_path_exists = function(app_path) identical(app_path, "/tmp/app.R"),
    build_shiny_app_dir = function(app_path) {
      expect_identical(app_path, "/tmp/app.R")
      "mock-app-dir"
    },
    run_shiny_app = function(appDir, port, host, launch.browser) {
      seen$appDir <- appDir
      seen$port <- port
      seen$host <- host
      seen$launch.browser <- launch.browser
      invisible(NULL)
    }
  )

  expect_no_error(run_app(host = "127.0.0.1", port = 8888, browser = TRUE))
  expect_identical(seen$appDir, "mock-app-dir")
  expect_identical(seen$port, 8888L)
  expect_identical(seen$host, "0.0.0.0")
  expect_false(seen$launch.browser)
})

test_that("run_app keeps provided settings outside Docker", {
  seen <- new.env(parent = emptyenv())

  local_mocked_bindings(
    install_tima = function() invisible(NULL),
    is_docker_env = function() FALSE,
    get_app_path = function() "/tmp/app.R",
    app_path_exists = function(app_path) identical(app_path, "/tmp/app.R"),
    build_shiny_app_dir = function(app_path) {
      force(app_path)
      "mock-app-dir"
    },
    run_shiny_app = function(appDir, port, host, launch.browser) {
      seen$appDir <- appDir
      seen$port <- port
      seen$host <- host
      seen$launch.browser <- launch.browser
      invisible(NULL)
    }
  )

  expect_no_error(run_app(host = "127.0.0.1", port = 3839, browser = FALSE))
  expect_identical(seen$appDir, "mock-app-dir")
  expect_identical(seen$port, 3839L)
  expect_identical(seen$host, "127.0.0.1")
  expect_false(seen$launch.browser)
})
