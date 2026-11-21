library(testthat)

# Helper: standard output paths
std_outputs <- function() {
  list(
    output_key = tempfile(fileext = ".tsv"),
    output_org_tax_ott = tempfile(fileext = ".tsv"),
    output_str_can = tempfile(fileext = ".tsv"),
    output_str_stereo = tempfile(fileext = ".tsv"),
    output_str_met = tempfile(fileext = ".tsv"),
    output_str_tax_cla = tempfile(fileext = ".tsv"),
    output_str_tax_npc = tempfile(fileext = ".tsv")
  )
}

call_validate_sop_merged <- function(
  files = c("a.tsv", "b.tsv"),
  filter = FALSE,
  level = "genus",
  value = "Homo",
  ...
) {
  out <- std_outputs()
  args <- c(
    list(files = files, filter = filter, level = level, value = value),
    out
  )
  overrides <- list(...)
  if (length(overrides) > 0L) {
    args[names(overrides)] <- overrides
  }
  do.call(validate_sop_merged_inputs, args)
}

# ── validate_sop_merged_inputs ─────────────────────────────────────────────────

test_that("validate_sop_merged_inputs passes with valid minimal inputs", {
  expect_invisible(call_validate_sop_merged())
})

test_that("validate_sop_merged_inputs errors on empty files vector", {
  expect_error(
    call_validate_sop_merged(files = character(0)),
    class = "tima_validation_error"
  )
})

test_that("validate_sop_merged_inputs errors on non-logical filter", {
  expect_error(
    call_validate_sop_merged(files = "a.tsv", filter = "yes"),
    class = "tima_validation_error"
  )
})

test_that("validate_sop_merged_inputs errors on invalid taxonomic level", {
  expect_error(
    call_validate_sop_merged(
      files = "a.tsv",
      filter = TRUE,
      level = "not_a_rank"
    ),
    class = "tima_validation_error"
  )
})

test_that("validate_sop_merged_inputs errors when value not a single string", {
  expect_error(
    call_validate_sop_merged(
      files = "a.tsv",
      filter = TRUE,
      value = c("Homo", "Pan")
    ),
    class = "tima_validation_error"
  )
})

test_that("validate_sop_merged_inputs errors on invalid output paths", {
  expect_error(
    call_validate_sop_merged(files = "a.tsv", output_key = c("a.tsv", "b.tsv")),
    class = "tima_validation_error"
  )
})

# ── apply_taxonomic_filter ─────────────────────────────────────────────────────

test_that("apply_taxonomic_filter filters correctly by genus column", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("IK1", "IK2", "IK3"),
    structure_smiles_no_stereo = c("C1", "C2", "C3"),
    organism_name = c("Homo sapiens", "Mus musculus", "Homo erectus"),
    reference_doi = c("10.1/a", "10.1/b", "10.1/c")
  )
  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Homo sapiens", "Mus musculus", "Homo erectus"),
    organism_taxonomy_08genus = c("Homo", "Mus", "Homo")
  )

  result <- apply_taxonomic_filter(
    table_keys = table_keys,
    table_org_tax_ott = table_org_tax_ott,
    level = "genus",
    value = "Homo"
  )

  expect_equal(nrow(result), 2L)
  expect_true(all(grepl("IK1|IK3", result$structure_inchikey)))
})

test_that("apply_taxonomic_filter errors on missing level column", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = "IK1",
    structure_smiles_no_stereo = "C1",
    organism_name = "Homo sapiens",
    reference_doi = "10.1/a"
  )
  table_org_tax_ott <- tidytable::tidytable(
    organism_name = "Homo sapiens"
    # no genus column
  )

  expect_error(
    apply_taxonomic_filter(table_keys, table_org_tax_ott, "genus", "Homo"),
    class = "tima_validation_error"
  )
})

test_that("apply_taxonomic_filter errors when no rows match value", {
  table_keys <- tidytable::tidytable(
    structure_inchikey = c("IK1", "IK2"),
    structure_smiles_no_stereo = c("C1", "C2"),
    organism_name = c("Homo sapiens", "Mus musculus"),
    reference_doi = c("10.1/a", "10.1/b")
  )
  table_org_tax_ott <- tidytable::tidytable(
    organism_name = c("Homo sapiens", "Mus musculus"),
    organism_taxonomy_08genus = c("Homo", "Mus")
  )

  expect_error(
    apply_taxonomic_filter(table_keys, table_org_tax_ott, "genus", "Pan"),
    class = "tima_validation_error"
  )
})

test_that(".apply_col_mapping renames the first matching source column", {
  df <- data.frame(old_a = 1, old_b = 2, keep = 3)
  apply_col_mapping <- getFromNamespace(".apply_col_mapping", "tima")
  mapped <- apply_col_mapping(
    df,
    list(target = c("missing", "old_b", "old_a"))
  )

  expect_true("target" %in% names(mapped))
  expect_false("old_b" %in% names(mapped))
  expect_true("old_a" %in% names(mapped))
  expect_true("keep" %in% names(mapped))
})

test_that(".normalize_chemontid_in_tables normalizes both cache and taxonomy tables", {
  cache_data <- data.frame(
    structure_tax_cla_chemontid = c("CHEMONT:0002011", "2011"),
    stringsAsFactors = FALSE
  )
  taxonomy_table <- data.frame(
    structure_tax_cla_chemontid = c("CHEMONTID:0002011", NA_character_),
    stringsAsFactors = FALSE
  )

  normalize_chemontid_in_tables <- getFromNamespace(
    ".normalize_chemontid_in_tables",
    "tima"
  )
  result <- normalize_chemontid_in_tables(cache_data, taxonomy_table)

  expect_identical(
    result$cache_data$structure_tax_cla_chemontid,
    c("CHEMONTID:0002011", "CHEMONTID:0002011")
  )
  expect_identical(
    result$taxonomy_table$structure_tax_cla_chemontid,
    c("CHEMONTID:0002011", NA_character_)
  )
})

test_that("enrich_taxonomy_from_cache returns early when cache file is absent", {
  taxonomy_table <- data.frame(
    structure_inchikey = "IK1",
    stringsAsFactors = FALSE
  )

  result <- enrich_taxonomy_from_cache(
    taxonomy_table = taxonomy_table,
    cache_path = tempfile(pattern = "missing-cache-", fileext = ".tsv"),
    key_col = "structure_inchikey",
    all_keys = c("IK1", "IK2")
  )

  expect_identical(result, taxonomy_table)
})
