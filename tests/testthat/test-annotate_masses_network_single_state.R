library(testthat)

test_that("annotate_masses keeps precise single-state identities in modifier chains", {
  temp_dir <- tempfile("annotate_masses_network_")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink("data", recursive = TRUE, force = TRUE), add = TRUE)

  neutral_mass <- 300
  adduct_pos <- c("[M-H4O2+H]+", "[M-H2O+H]+", "[M+H]+", "[M+Na]+")
  feature_adduct_map <- tidytable::tidytable(
    feature_id = c("f1", "f2", "f3", "f4"),
    expected_adduct = adduct_pos
  )

  features <- feature_adduct_map |>
    tidytable::mutate(
      mz = vapply(
        expected_adduct,
        function(x) {
          calculate_mz_from_mass(neutral_mass = neutral_mass, adduct_string = x)
        },
        numeric(1L)
      ),
      rt = 5,
      sample = "s1"
    ) |>
    tidytable::select(feature_id, mz, rt, sample)

  features_file <- file.path(temp_dir, "features.tsv")
  tidytable::fwrite(features, features_file, sep = "\t")

  library_file <- copy_fixture_to(
    "library_minimal.csv",
    file.path(temp_dir, "library.tsv")
  )
  str_stereo <- copy_fixture_to(
    "structures_stereo.csv",
    file.path(temp_dir, "structures_stereo.tsv")
  )
  str_met <- copy_fixture_to(
    "structures_metadata_minimal.csv",
    file.path(temp_dir, "structures_metadata.tsv")
  )
  str_tax_cla <- copy_fixture_to(
    "structures_taxonomy_cla_minimal.csv",
    file.path(temp_dir, "structures_taxonomy_cla.tsv")
  )
  str_tax_npc <- copy_fixture_to(
    "structures_taxonomy_npc_minimal.csv",
    file.path(temp_dir, "structures_taxonomy_npc.tsv")
  )

  output_annotations <- file.path(temp_dir, "annotations.tsv")
  output_edges <- file.path(temp_dir, "edges.tsv")

  result <- annotate_masses(
    features = features_file,
    output_annotations = output_annotations,
    output_edges = output_edges,
    name_source = "src",
    name_target = "dst",
    library = library_file,
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc,
    adducts_list = list(pos = adduct_pos, neg = c("[M-H]-")),
    clusters_list = list(pos = character(), neg = character()),
    solvents_list = list(pos = character(), neg = character()),
    neutral_losses_list = c("H2O"),
    ms_mode = "pos",
    tolerance_ppm = 10,
    tolerance_rt = 0.02
  )

  expect_type(result, "character")
  expect_true(file.exists(output_annotations))
  expect_true(file.exists(output_edges))

  annotations <- tidytable::fread(output_annotations)
  edges <- tidytable::fread(output_edges)

  observed <- annotations |>
    tidytable::distinct(feature_id, candidate_adduct)

  expect_equal(nrow(observed), 4L)
  expect_true(all(observed$feature_id %in% feature_adduct_map$feature_id))

  observed_keys <- stats::setNames(
    vapply(observed$candidate_adduct, adduct_to_state_key, character(1L)),
    observed$feature_id
  )
  expected_keys <- stats::setNames(
    vapply(
      feature_adduct_map$expected_adduct,
      adduct_to_state_key,
      character(1L)
    ),
    feature_adduct_map$feature_id
  )
  expect_equal(
    unname(observed_keys[names(expected_keys)]),
    unname(expected_keys)
  )

  edge_f1_f2 <- edges |>
    tidytable::filter((src == "f1" & dst == "f2") | (src == "f2" & dst == "f1"))

  expect_gt(nrow(edge_f1_f2), 0L)
  expect_true(all(grepl(" _ ", edge_f1_f2$label, fixed = TRUE)))
  expect_false(any(grepl("loss", edge_f1_f2$label, ignore.case = TRUE)))
  expect_false(any(grepl("cluster", edge_f1_f2$label, ignore.case = TRUE)))
})
