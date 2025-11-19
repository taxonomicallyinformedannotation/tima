# Test: Taxonomy Functions

test_that("get_organism_taxonomy_ott works with valid organism", {
  fake_taxon_df <- data.frame("organism" = "Gentiana lutea")

  result <- get_organism_taxonomy_ott(df = fake_taxon_df)
  expect_s3_class(result, "data.frame")
})

test_that("get_organism_taxonomy_ott handles invalid organism", {
  wrong_taxon_df <- data.frame("organism" = "Gentiano luteo")

  result <- get_organism_taxonomy_ott(df = wrong_taxon_df)
  expect_s3_class(result, "data.frame")
})

test_that("get_organism_taxonomy_ott handles API failures", {
  fake_taxon_df <- data.frame("organism" = "Gentiana lutea")

  result <- get_organism_taxonomy_ott(
    df = fake_taxon_df,
    url = "https://api.opentreeoflife.org/v3/taxonomy/fakeDown"
  )
  expect_s3_class(result$matches, "data.frame")
  expect_s3_class(result$taxonomy, "data.frame")
})

test_that(
  skip("Not implemented")
)
# test_that("prepare_taxa works with forced organism", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   get_file(
#     url = paths$urls$examples$metadata,
#     export = paths$data$source$metadata
#   )
#   prepare_features_tables()
#
#   expect_no_error(
#     prepare_taxa(taxon = "Homo sapiens")
#   )
#
#
# })

test_that(
  skip("Not implemented")
)
# test_that("prepare_taxa handles empty taxon", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   get_file(
#     url = paths$urls$examples$metadata,
#     export = paths$data$source$metadata
#   )
#   prepare_features_tables()
#
#   expect_no_error(
#     prepare_taxa(taxon = "")
#   )
#
#
# })

test_that(
  skip("Not implemented")
)
# test_that("prepare_taxa works without file extension", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   get_file(
#     url = paths$urls$examples$metadata,
#     export = paths$data$source$metadata
#   )
#   prepare_features_tables()
#
#   expect_no_error(
#     prepare_taxa(extension = FALSE)
#   )
#
#
# })

test_that(
  skip("Not implemented")
)
# test_that("prepare_taxa handles unrecognized taxa", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   get_file(
#     url = paths$urls$examples$metadata |>
#       gsub(
#         pattern = ".tsv",
#         replacement = "_unrecognized.tsv",
#         fixed = TRUE
#       ),
#     export = paths$data$source$metadata |>
#       gsub(
#         pattern = ".tsv",
#         replacement = "_unrecognized.tsv",
#         fixed = TRUE
#       )
#   )
#   prepare_features_tables()
#
#   expect_no_error(
#     prepare_taxa(
#       metadata = paths$data$source$metadata |>
#         gsub(
#           pattern = ".tsv",
#           replacement = "_unrecognized.tsv",
#           fixed = TRUE
#         )
#     )
#   )
#
#
# })

test_that(
  skip("Not implemented")
)
# test_that("prepare_taxa works with intensity-based attribution", {
#   skip_on_cran()
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   get_file(
#     url = paths$urls$examples$features,
#     export = paths$data$source$features
#   )
#   get_file(
#     url = paths$urls$examples$metadata,
#     export = paths$data$source$metadata
#   )
#   prepare_features_tables()
#
#   expect_no_error(prepare_taxa())
#
#
# })

test_that(
  skip("Not implemented")
)
# test_that("benchmark_taxize_spectra works", {
#   local_test_project(copy = TRUE)
#   paths <- get_default_paths()
#
#   data.frame(feature_id = 1, organism_name = "Gentiana lutea") |>
#     export_output("data/interim/benchmark/bench_test_in.tsv.gz")
#
#   fake_lotus(export = paths$data$source$libraries$sop$lotus)
#   prepare_libraries_sop_lotus()
#   prepare_libraries_sop_merged()
#
#   expect_no_error(
#     benchmark_taxize_spectra(
#       input = "data/interim/benchmark/bench_test_in.tsv.gz",
#       keys = paths$data$interim$libraries$sop$merged$keys,
#       org_tax_ott = get_params(
#         step = "prepare_taxa"
#       )$files$libraries$sop$merged$organisms$taxonomies$ott,
#       output = "data/interim/benchmark/bench_test_out.tsv.gz"
#     )
#   )
#
#
# })
