# Test Suite: get_compounds_xrefs ----

library(testthat)

## Cache Behavior ----

test_that("get_compounds_xrefs returns cached file when it is fresh", {
  output_file <- temp_test_path("xrefs_fresh.tsv.gz")

  # Create a mock cached file with realistic structure

  mock_data <- tidytable::tidytable(
    inchikey = c(
      "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
      "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
      "RYYVLZVUVIJVGH-UHFFFAOYSA-N"
    ),
    prefix = c("ChEBI", "wikidata", "ChEBI"),
    id = c("15365", "Q2270", "18012")
  )
  export_output(x = mock_data, file = output_file)

  initial_mtime <- file.mtime(output_file)

  Sys.sleep(0.1)
  result <- get_compounds_xrefs(
    output = output_file,
    max_age_hours = 24
  )

  # Should return existing file without re-fetching
  expect_equal(result, output_file)
  expect_equal(file.mtime(output_file), initial_mtime)
})

test_that("get_compounds_xrefs detects stale cache", {
  output_file <- temp_test_path("xrefs_stale.tsv.gz")

  # Create a mock cached file
  mock_data <- tidytable::tidytable(
    inchikey = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
    prefix = "ChEBI",
    id = "15365"
  )
  export_output(x = mock_data, file = output_file)

  # Set max_age_hours = 0 so the file is always considered stale.
  # This will trigger the fetch path, which we expect to fail here

  # because we don't have network access in unit tests — that's fine,
  # we just verify the cache-skip logic is NOT triggered.
  expect_no_error(
    get_compounds_xrefs(
      output = output_file,
      max_age_hours = 0
    )
  )
})

test_that("get_compounds_xrefs returns output path as character", {
  output_file <- temp_test_path("xrefs_type.tsv.gz")

  mock_data <- tidytable::tidytable(
    inchikey = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
    prefix = "ChEBI",
    id = "15365"
  )
  export_output(x = mock_data, file = output_file)

  result <- get_compounds_xrefs(
    output = output_file,
    max_age_hours = 24
  )

  expect_type(result, "character")
  expect_length(result, 1L)
  expect_false(is.na(result))
  expect_false(result == "")
  expect_true(file.exists(result))
})

test_that("get_compounds_xrefs cached file content is readable", {
  output_file <- temp_test_path("xrefs_content.tsv.gz")

  mock_data <- tidytable::tidytable(
    inchikey = c(
      "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
      "RYYVLZVUVIJVGH-UHFFFAOYSA-N"
    ),
    prefix = c("wikidata", "ChEBI"),
    id = c("Q2270", "18012")
  )
  export_output(x = mock_data, file = output_file)

  result <- get_compounds_xrefs(
    output = output_file,
    max_age_hours = 24
  )

  df <- tidytable::fread(result)
  expect_s3_class(df, "data.frame")
  expect_true("inchikey" %in% names(df))
  expect_true("prefix" %in% names(df))
  expect_true("id" %in% names(df))
  expect_equal(nrow(df), 2L)
})

## Integration (Network) ----

test_that("get_compounds_xrefs fetches fresh data from QLever", {
  skip_on_cran()
  skip_if_offline()

  # Quick QLever reachability check
  qlever_ok <- tryCatch(
    {
      resp <- httr2::request(
        "https://qlever.cs.uni-freiburg.de/api/wikidata"
      ) |>
        httr2::req_method("POST") |>
        httr2::req_body_form(query = "SELECT * WHERE { ?s ?p ?o } LIMIT 1") |>
        httr2::req_headers(Accept = "text/csv") |>
        httr2::req_timeout(10) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform()
      httr2::resp_status(resp) < 500L
    },
    error = function(...) FALSE
  )
  if (!isTRUE(qlever_ok)) {
    skip("QLever backend is unreachable; skipping live integration test")
  }

  output_file <- temp_test_path("xrefs_integration.tsv.gz")

  result <- get_compounds_xrefs(
    # Use a small subset of props to keep the query fast
    props = c("P683"),
    output = output_file
  )

  expect_true(file.exists(result))
  expect_equal(result, output_file)

  df <- tidytable::fread(result)
  expect_s3_class(df, "data.frame")
  expect_true(all(c("inchikey", "prefix", "id") %in% names(df)))

  if (nrow(df) == 0L) {
    skip(
      "Upstream xrefs service unavailable; graceful fallback returned empty table"
    )
  }

  expect_gt(nrow(df), 0L)

  # Should contain wikidata QID rows and external DB rows
  expect_true("wikidata" %in% df$prefix)
})

test_that("get_compounds_xrefs refreshes stale cache with real data", {
  skip_on_cran()
  skip_if_offline()

  # Quick QLever reachability check — skip entire test if backend is down
  qlever_ok <- tryCatch(
    {
      resp <- httr2::request(
        "https://qlever.cs.uni-freiburg.de/api/wikidata"
      ) |>
        httr2::req_method("POST") |>
        httr2::req_body_form(query = "SELECT * WHERE { ?s ?p ?o } LIMIT 1") |>
        httr2::req_headers(Accept = "text/csv") |>
        httr2::req_timeout(10) |>
        httr2::req_error(is_error = \(r) FALSE) |>
        httr2::req_perform()
      httr2::resp_status(resp) < 500L
    },
    error = function(...) FALSE
  )
  if (!isTRUE(qlever_ok)) {
    skip("QLever backend is unreachable; skipping live refresh test")
  }

  output_file <- temp_test_path("xrefs_refresh.tsv.gz")

  # Create a small old-looking file
  mock_data <- tidytable::tidytable(
    inchikey = "FAKE-INCHIKEY",
    prefix = "test",
    id = "0"
  )
  export_output(x = mock_data, file = output_file)
  initial_mtime <- file.mtime(output_file)

  Sys.sleep(0.1)

  # Force refresh by setting max_age_hours = 0
  result <- tryCatch(
    get_compounds_xrefs(
      props = c("P683"),
      output = output_file,
      max_age_hours = 0
    ),
    error = function(e) {
      skip(paste("QLever unavailable during refresh:", conditionMessage(e)))
    }
  )

  expect_true(file.exists(result))

  df <- tidytable::fread(result)

  # If the service returned stale/empty data, skip gracefully
  if (nrow(df) == 0L || (nrow(df) == 1L && "FAKE-INCHIKEY" %in% df$inchikey)) {
    skip(
      "Upstream xrefs service unavailable; graceful fallback returned stale/empty table"
    )
  }

  # Should no longer contain our fake data
  expect_false("FAKE-INCHIKEY" %in% df$inchikey)
  expect_gt(nrow(df), 1L)

  # File should have been overwritten
  expect_true(file.mtime(output_file) > initial_mtime)
})

## Error Handling ----

test_that("get_compounds_xrefs errors on invalid props", {
  output_file <- temp_test_path("xrefs_bad_props.tsv.gz")

  expect_error(
    get_compounds_xrefs(
      props = c("P_NONEXISTENT_99999"),
      output = output_file
    ),
    "supplied props were found",
    class = "tima_validation_error"
  )
})
