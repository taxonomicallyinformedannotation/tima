# Test Suite: benchmark_taxize_spectra ----

library(testthat)

## Validation ----

test_that("test-benchmark_taxize_spectra validates input file exists", {
  expect_error(
    benchmark_taxize_spectra(
      input = file.path("missing.tsv"),
      keys = file.path("keys.tsv"),
      org_tax_ott = file.path("tax.tsv"),
      output = file.path("out.tsv")
    ),
    "Input features file not found"
  )
})

test_that("test-benchmark_taxize_spectra validates keys file exists", {
  input <- file.path("input.tsv")
  writeLines("feature_id", input)

  expect_error(
    benchmark_taxize_spectra(
      input = input,
      keys = file.path("missing.tsv"),
      org_tax_ott = file.path("tax.tsv"),
      output = file.path("out.tsv")
    ),
    "Keys file not found"
  )
})

test_that("test-benchmark_taxize_spectra validates taxonomy file exists", {
  input <- file.path("input.tsv")
  keys <- file.path("keys.tsv")
  writeLines("feature_id", input)
  writeLines("structure_inchikey", keys)

  expect_error(
    benchmark_taxize_spectra(
      input = input,
      keys = keys,
      org_tax_ott = file.path("missing.tsv"),
      output = file.path("out.tsv")
    ),
    "Taxonomy file not found"
  )
})

## Behavior ----

test_that("test-benchmark_taxize_spectra processes minimal valid input", {
  # Create minimal features file
  features <- tidytable::tidytable(
    feature_id = c("F1", "F2"),
    inchikey_connectivity_layer = c("AAAAAAAAAAAAAA", "BBBBBBBBBBBBBB")
  )
  input <- file.path("features.tsv")
  tidytable::fwrite(features, input, sep = "\t")

  # Create minimal SOP keys
  sop <- tidytable::tidytable(
    structure_inchikey = c("AAAAAAAAAAAAAA-XXXXXXXX-X"),
    organism_name = c("Test organism")
  )
  keys <- file.path("keys.tsv")
  tidytable::fwrite(sop, keys, sep = "\t")

  # Create minimal taxonomy with all required columns
  tax <- tidytable::tidytable(
    organism_name = c("Test organism"),
    organism_taxonomy_01domain = c("Eukaryota"),
    organism_taxonomy_02kingdom = c("Plantae"),
    organism_taxonomy_03phylum = c("TestPhylum"),
    organism_taxonomy_04class = c("TestClass"),
    organism_taxonomy_05order = c("TestOrder"),
    organism_taxonomy_06family = c("TestFamily"),
    organism_taxonomy_07tribe = c("TestTribe"),
    organism_taxonomy_08genus = c("TestGenus"),
    organism_taxonomy_09species = c("Test organism"),
    organism_taxonomy_10varietas = c(NA_character_)
  )
  org_tax_ott <- file.path("tax.tsv")
  tidytable::fwrite(tax, org_tax_ott, sep = "\t")

  output <- file.path("output.tsv")

  res <- benchmark_taxize_spectra(
    input = input,
    keys = keys,
    org_tax_ott = org_tax_ott,
    output = output
  )

  expect_equal(res, output)
  expect_true(file.exists(output))
  result <- tidytable::fread(output)
  expect_true("feature_id" %in% names(result))
})

test_that("test-benchmark_taxize_spectra handles features without organisms", {
  features <- tidytable::tidytable(
    feature_id = c("F1"),
    inchikey_connectivity_layer = c("ZZZZZZZZZZZZZZ")
  )
  input <- file.path("features.tsv")
  tidytable::fwrite(features, input, sep = "\t")

  # Empty keys
  sop <- tidytable::tidytable(
    structure_inchikey = character(),
    organism_name = character()
  )
  keys <- file.path("keys.tsv")
  tidytable::fwrite(sop, keys, sep = "\t")

  tax <- tidytable::tidytable(
    organism_name = character(),
    organism_taxonomy_01domain = character(),
    organism_taxonomy_02kingdom = character(),
    organism_taxonomy_03phylum = character(),
    organism_taxonomy_04class = character(),
    organism_taxonomy_05order = character(),
    organism_taxonomy_06family = character(),
    organism_taxonomy_07tribe = character(),
    organism_taxonomy_08genus = character(),
    organism_taxonomy_09species = character(),
    organism_taxonomy_10varietas = character()
  )
  org_tax_ott <- file.path("tax.tsv")
  tidytable::fwrite(tax, org_tax_ott, sep = "\t")

  output <- file.path("output.tsv")

  res <- benchmark_taxize_spectra(
    input = input,
    keys = keys,
    org_tax_ott = org_tax_ott,
    output = output
  )

  expect_true(file.exists(output))
  result <- tidytable::fread(output)
  expect_equal(nrow(result), 1)
})

## Fixture-based behavior tests ----

test_that("test-benchmark_taxize_spectra basic successful run with helpers", {
  fx <- create_bench_full_set(
    dir = tmp,
    feature_ids = c("F1", "F2"),
    inchikey_layers = c("AAAAAAAAAAAAAA", "BBBBBBBBBBBBBB"),
    structure_inchikeys = c("AAAAAAAAAAAAAA-XXXX-XX", "CCCCCCCCCCCCCC-YYYY-YY"),
    organisms = c("OrgA", "OrgB")
  )
  res <- benchmark_taxize_spectra(
    input = fx$features,
    keys = fx$keys,
    org_tax_ott = fx$taxonomy,
    output = fx$output
  )
  expect_equal(res, fx$output)
  out <- read_bench_output(res)
  expect_true("feature_id" %in% names(out))
  expect_equal(nrow(out), 2)
})

test_that("test-benchmark_taxize_spectra handles missing taxonomy levels gracefully", {
  fx <- create_bench_full_set(
    dir = tmp,
    feature_ids = c("F1"),
    inchikey_layers = c("AAAAAAAAAAAAAA"),
    structure_inchikeys = c("AAAAAAAAAAAAAA-ZZZ-ZZ"),
    organisms = c("OrgA"),
    with_missing_tax = TRUE
  )
  res <- benchmark_taxize_spectra(
    input = fx$features,
    keys = fx$keys,
    org_tax_ott = fx$taxonomy,
    output = fx$output
  )
  out <- read_bench_output(res)
  expect_true("sample_organism_01_domain" %in% names(out))
  expect_true(any(is.na(out$sample_organism_01_domain)))
})

test_that("test-benchmark_taxize_spectra samples one organism per feature when multiple available", {
  # Same feature mapped to two organisms via duplicated inchikey connectivity layer
  feature_ids <- c("F1", "F2")
  inchikey_layers <- c("AAAAAAAAAAAAAA", "CCCCCCCCCCCCCC")
  features_path <- create_bench_features(tmp, feature_ids, inchikey_layers)
  keys_path <- create_bench_keys(
    tmp,
    structure_inchikeys = c("AAAAAAAAAAAAAA-AAAA-AA", "AAAAAAAAAAAAAA-BBBB-BB"),
    organisms = c("OrgA", "OrgB")
  )
  tax_path <- create_bench_taxonomy(tmp, organisms = c("OrgA", "OrgB"))
  output <- file.path("out.tsv")
  res <- benchmark_taxize_spectra(
    input = features_path,
    keys = keys_path,
    org_tax_ott = tax_path,
    output = output
  )
  out <- read_bench_output(res)
  # Only one row for F1 should be present (sampled) plus F2 with no organism
  expect_true(nrow(out) <= length(feature_ids))
})

test_that("test-benchmark_taxize_spectra retains features without organism match", {
  fx <- create_bench_full_set(
    dir = tmp,
    feature_ids = c("F1", "F2"),
    inchikey_layers = c("AAAAAAAAAAAAAA", "BBBBBBBBBBBBBB"),
    structure_inchikeys = c("AAAAAAAAAAAAAA-ZZZ-ZZ"),
    organisms = c("OrgA")
  )
  # Modify keys to only match first feature
  tidytable::fwrite(
    tidytable::tidytable(
      structure_inchikey = "AAAAAAAAAAAAAA-ZZZ-ZZ",
      organism_name = "OrgA"
    ),
    fx$keys,
    sep = "\t"
  )
  res <- benchmark_taxize_spectra(
    input = fx$features,
    keys = fx$keys,
    org_tax_ott = fx$taxonomy,
    output = fx$output
  )
  out <- read_bench_output(res)
  expect_equal(length(unique(out$feature_id)), 2)
})

test_that("test-benchmark_taxize_spectra supports repeated taxonomy entries", {
  fx <- create_bench_full_set(
    dir = tmp,
    feature_ids = c("F1"),
    inchikey_layers = c("AAAAAAAAAAAAAA"),
    structure_inchikeys = c("AAAAAAAAAAAAAA-AAAA-AA", "AAAAAAAAAAAAAA-BBBB-BB"),
    organisms = c("OrgA", "OrgA")
  )
  res <- benchmark_taxize_spectra(
    input = fx$features,
    keys = fx$keys,
    org_tax_ott = fx$taxonomy,
    output = fx$output
  )
  out <- read_bench_output(res)
  expect_equal(length(unique(out$feature_id)), 1)
  expect_equal(nrow(out), 1)
})
