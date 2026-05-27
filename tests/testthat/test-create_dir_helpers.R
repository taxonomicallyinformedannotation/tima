library(testthat)

extract_directory_path <- extract_directory_path
ensure_directory_exists <- ensure_directory_exists
verify_directory_writable <- verify_directory_writable


test_that("extract_directory_path handles file and directory paths", {
  expect_equal(extract_directory_path("a/b/file.tsv"), "a/b")
  expect_equal(extract_directory_path("file.tsv"), ".")
  expect_equal(extract_directory_path("a/b/dir"), "a/b/dir")
  expect_equal(extract_directory_path("a/b/dir/"), "a/b/dir/")
})

test_that("extract_directory_path does not treat extensionless names as files", {
  expect_equal(extract_directory_path("results"), "results")
  expect_equal(extract_directory_path("path/to/results"), "path/to/results")
})

test_that("ensure_directory_exists creates nested directory", {
  d <- file.path(tempdir(), paste0("tima-create-", Sys.getpid()), "x", "y")
  on.exit(unlink(dirname(dirname(d)), recursive = TRUE), add = TRUE)

  expect_false(dir.exists(d))
  expect_invisible(ensure_directory_exists(d))
  expect_true(dir.exists(d))
})

test_that("ensure_directory_exists is no-op for existing directory", {
  d <- tempfile(pattern = "tima-existing-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  expect_invisible(ensure_directory_exists(d))
  expect_true(dir.exists(d))
})

test_that("verify_directory_writable returns FALSE for missing directory", {
  d <- tempfile(pattern = "tima-missing-")
  expect_false(verify_directory_writable(d))
})

test_that("verify_directory_writable returns TRUE for writable directory", {
  d <- tempfile(pattern = "tima-writable-")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  expect_true(verify_directory_writable(d))
})

test_that("verify_directory_writable handles unreadable directories when enforced", {
  d <- tempfile(pattern = "tima-unwritable-")
  dir.create(d)
  on.exit(
    {
      try(Sys.chmod(d, mode = "700"), silent = TRUE)
      unlink(d, recursive = TRUE)
    },
    add = TRUE
  )

  Sys.chmod(d, mode = "500")
  ok <- verify_directory_writable(d)

  if (isTRUE(ok)) {
    skip(
      "Filesystem permissions in tempdir do not enforce unwritable directory"
    )
  } else {
    expect_false(ok)
  }
})
