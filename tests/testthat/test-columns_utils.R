# Test Suite: columns_utils ----
# Tests columns_model(), clean_collapse(), fake_annotations_columns(),
# fake_sop_columns() — all previously untested.

library(testthat)

# ---- columns_model -----------------------------------------------------------

test_that("columns_model returns a named list with expected keys", {
  model <- columns_model()
  expect_type(model, "list")
  expected_keys <- c(
    "features_columns",
    "features_calculated_columns",
    "candidates_calculated_columns",
    "candidates_sirius_for_columns",
    "candidates_sirius_str_columns",
    "candidates_spectra_columns",
    "candidates_structures_columns",
    "components_columns",
    "rank_columns",
    "score_columns"
  )
  expect_named(model, expected_keys, ignore.order = TRUE)
})

test_that("columns_model feature columns contain core identifiers", {
  m <- columns_model()
  expect_true("feature_id" %in% m$features_columns)
  expect_true("feature_mz" %in% m$features_columns)
  expect_true("feature_rt" %in% m$features_columns)
})

test_that("columns_model tag column is present in candidates_structures_columns", {
  m <- columns_model()
  expect_true(
    "candidate_structure_tag" %in%
      m$candidates_structures_columns
  )
})

test_that("columns_model score columns include all primary scores", {
  m <- columns_model()
  expect_true(all(
    c("score_biological", "score_chemical", "score_final") %in% m$score_columns
  ))
})

# ---- clean_collapse ----------------------------------------------------------

test_that("clean_collapse collapses unique values per group with separator", {
  df <- tidytable::tidytable(
    group = c("A", "A", "B"),
    val = c("x", "y", "x")
  ) |>
    tidytable::group_by(group)

  out <- clean_collapse(df, cols = "val", separator = " | ")
  expect_equal(nrow(out), 2L)
  a_row <- out[out$group == "A", ]$val
  expect_true(grepl("x", a_row) && grepl("y", a_row))
})

test_that("clean_collapse drops NA values before collapsing", {
  df <- tidytable::tidytable(
    group = c("A", "A"),
    val = c("x", NA_character_)
  ) |>
    tidytable::group_by(group)

  out <- clean_collapse(df, cols = "val")
  expect_equal(trimws(out$val), "x")
})

test_that("clean_collapse deduplicates repeated values", {
  df <- tidytable::tidytable(
    group = c("A", "A", "A"),
    val = c("x", "x", "x")
  ) |>
    tidytable::group_by(group)

  out <- clean_collapse(df, cols = "val", separator = " $ ")
  # No separator should appear since only one unique value
  expect_false(grepl("$", out$val, fixed = TRUE))
  expect_equal(out$val, "x")
  expect_null(attr(out$group, ".match.hash"))
})

test_that("clean_collapse fast-paths singleton groups and trims whitespace", {
  df_single <- tidytable::tidytable(
    group = c("G1", "G2", "G3"),
    val1 = c("  a  ", NA_character_, "c"),
    val2 = c("1", " 2 ", "3")
  ) |>
    tidytable::group_by(group)

  out_single <- clean_collapse(df_single, cols = c("val1", "val2"))
  expect_equal(nrow(out_single), 3L)
  expect_equal(out_single$val1, c("a", NA_character_, "c"))
  expect_equal(out_single$val2, c("1", "2", "3"))
  expect_null(attr(out_single$group, ".match.hash"))
})

# ---- fake_annotations_columns ------------------------------------------------

test_that("fake_annotations_columns returns a 1-row NA data frame", {
  out <- fake_annotations_columns()
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_true("feature_id" %in% names(out))
  expect_true("candidate_structure_tag" %in% names(out))
  expect_true(all(is.na(out)))
})

# ---- fake_sop_columns --------------------------------------------------------

test_that("fake_sop_columns returns a 1-row NA tidytable with tag column", {
  out <- fake_sop_columns()
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_true("structure_tag" %in% names(out))
  expect_true("organism_name" %in% names(out))
  expect_true("structure_inchikey" %in% names(out))
  expect_true(is.na(out$structure_tag))
})

# ---- normalize_chemontid ----------------------------------------------------

test_that("normalize_chemontid pads bare numbers to CHEMONTID:NNNNNNN", {
  expect_equal(
    normalize_chemontid(c("2011", "42")),
    c("CHEMONTID:0002011", "CHEMONTID:0000042")
  )
})

test_that("normalize_chemontid normalizes zero-padded numbers without prefix", {
  expect_equal(
    normalize_chemontid(c("0002011", "0003443")),
    c("CHEMONTID:0002011", "CHEMONTID:0003443")
  )
})

test_that("normalize_chemontid preserves already canonical values", {
  expect_equal(
    normalize_chemontid("CHEMONTID:0002011"),
    "CHEMONTID:0002011"
  )
})

test_that("normalize_chemontid handles CHEMONT: prefix variant", {
  expect_equal(
    normalize_chemontid("CHEMONT:001"),
    "CHEMONTID:0000001"
  )
})

test_that("normalize_chemontid preserves NA", {
  expect_equal(normalize_chemontid(NA), NA_character_)
})

test_that("normalize_chemontid handles mixed input", {
  input <- c("CHEMONTID:0002011", "2011", "0002011", "CHEMONT:001", NA)
  expected <- c(
    "CHEMONTID:0002011",
    "CHEMONTID:0002011",
    "CHEMONTID:0002011",
    "CHEMONTID:0000001",
    NA
  )
  expect_equal(normalize_chemontid(input), expected)
})

test_that("normalize_chemontid deduplicates equivalent chemontids", {
  # The main use case: 2011 and 0002011 should become identical
  normed <- normalize_chemontid(c("2011", "0002011"))
  expect_null(attr(normed, ".match.hash"))
  result <- unique(normed)
  expect_length(result, 1L)
  expect_equal(result, "CHEMONTID:0002011")
  expect_equal(normalize_chemontid(character(0)), character(0))
  expect_equal(
    normalize_chemontid(c(NA_character_, NA_character_)),
    c(NA_character_, NA_character_)
  )
  expect_equal(
    normalize_chemontid(c("CHEMONTID:0002011", NA_character_)),
    c("CHEMONTID:0002011", NA_character_)
  )
  expect_equal(
    normalize_chemontid(c("non_numeric", NA_character_)),
    c("non_numeric", NA_character_)
  )
})

# ---- determine_columns_to_process & helper functions -------------------------

test_that("determine_columns_to_process handles NULL, valid cols, and missing cols", {
  df <- tidytable::tidytable(a = 1:2, b = c("x", "y"), c = c(TRUE, FALSE))

  # NULL returns all column names
  expect_identical(determine_columns_to_process(df, NULL), c("a", "b", "c"))

  # Valid subset (including factor/non-character coerced to character)
  expect_identical(
    determine_columns_to_process(df, factor(c("b", "a"))),
    c("b", "a")
  )

  # Missing column aborts with tima_validation_error listing missing names
  expect_error(
    determine_columns_to_process(df, c("a", "missing_1", "missing_2")),
    regexp = "missing_1, missing_2",
    class = "tima_validation_error"
  )
})

test_that("create_collapse_function, convert_lists_to_characters, and clean_character_columns work accurately", {
  fn <- create_collapse_function(" ; ")
  expect_identical(fn(c("a", NA, "b", "a")), list("a ; b"))
  expect_identical(fn(c(NA_character_, NA_character_)), list(""))

  df_list <- tidytable::tidytable(
    id = 1:2,
    lst = list("  alpha  ", "   "),
    chr = c("  beta ", "")
  )
  conv <- convert_lists_to_characters(df_list)
  expect_type(conv$lst, "character")
  expect_identical(conv$lst, c("  alpha  ", "   "))
  expect_identical(conv$id, 1:2)

  cleaned <- clean_character_columns(conv)
  expect_identical(cleaned$lst, c("alpha", NA_character_))
  expect_identical(cleaned$chr, c("beta", NA_character_))
  expect_identical(cleaned$id, 1:2)
})

# ---- clean_collapse edge cases, validation, and branches ---------------------

test_that("clean_collapse validates inputs and handles multi-group, ungrouped, zero-row, NA groups, and interleaved groups", {
  # 1. Input validation: non-data.frame and invalid separator
  expect_error(
    clean_collapse("not_a_df"),
    regexp = "grouped_df must be a data frame",
    class = "tima_validation_error"
  )
  df_base <- tidytable::tidytable(g = c("A", "B"), v = c("x", "y")) |>
    tidytable::group_by(g)
  expect_error(
    clean_collapse(df_base, separator = ""),
    regexp = "separator cannot be an empty string",
    class = "tima_error"
  )
  expect_error(
    clean_collapse(df_base, cols = "nonexistent_col"),
    regexp = "nonexistent_col",
    class = "tima_validation_error"
  )

  # 2. Fallback path: multi-grouped data frame (length(grp_vars) == 2L)
  df_multi_grp <- tidytable::tidytable(
    g1 = c("A", "A", "A"),
    g2 = c("X", "X", "Y"),
    v = c(" p ", "q", "   ")
  ) |>
    tidytable::group_by(g1, g2)
  out_multi <- clean_collapse(df_multi_grp, cols = "v", separator = "|")
  expect_identical(out_multi$g1, c("A", "A"))
  expect_identical(out_multi$g2, c("X", "Y"))
  expect_identical(out_multi$v, c("p |q", NA_character_))

  # 3. Fallback path: zero-row grouped data frame (nrow == 0L)
  df_zero <- tidytable::tidytable(
    g = character(0),
    v = character(0)
  ) |>
    tidytable::group_by(g)
  out_zero <- clean_collapse(df_zero, cols = "v")
  expect_identical(nrow(out_zero), 0L)
  expect_identical(names(out_zero), c("g", "v"))

  # 4. Single-group path with cols = NULL (includes group column in cols),
  #    NA in group column, interleaved (unsorted) groups, numeric col, and all-NA col
  df_interleaved <- tidytable::tidytable(
    g = c(" B ", "A", NA_character_, " B ", "A", "   "),
    v_chr = c("b1", "a1", "na1", "b2", "a1", "   "),
    v_num = c(10L, 20L, 30L, 40L, 20L, 50L),
    v_na = rep(NA_character_, 6L)
  ) |>
    tidytable::group_by(g)

  out_null_cols <- clean_collapse(df_interleaved, cols = NULL, separator = "|")
  # Unique groups in order of appearance: " B " -> "B", "A" -> "A", NA -> NA, "   " -> NA
  expect_identical(out_null_cols$g, c("B", "A", NA_character_, NA_character_))
  expect_identical(out_null_cols$v_chr, c("b1|b2", "a1", "na1", NA_character_))
  expect_identical(out_null_cols$v_num, c("10|40", "20", "30", "50"))
  expect_identical(out_null_cols$v_na, rep(NA_character_, 4L))

  # 5. Single-group path with integer group column and cols = group column only
  df_int_grp <- tidytable::tidytable(
    g_id = c(2L, 1L, 2L),
    v = c("x", "y", "z")
  ) |>
    tidytable::group_by(g_id)
  out_int_grp <- clean_collapse(df_int_grp, cols = "v", separator = "+")
  expect_identical(out_int_grp$g_id, c(2L, 1L))
  expect_identical(out_int_grp$v, c("x+z", "y"))

  out_only_grp <- clean_collapse(df_int_grp, cols = "g_id")
  expect_identical(names(out_only_grp), "g_id")
  expect_identical(out_only_grp$g_id, c("2", "1"))
})

# ---- select_* column helpers -------------------------------------------------

test_that("select_* column helpers filter extraneous columns and preserve matching tidytables", {
  # 1. select_structure_organism_pair_columns
  df_sop <- tidytable::tidytable(
    structure_inchikey = "IK1",
    organism_name = "Taxon1",
    reference_doi = "10.1000/1",
    stereo_name = "S1",
    extra_col = "drop_me"
  )
  res_sop <- select_structure_organism_pair_columns(df_sop)
  expect_identical(
    names(res_sop),
    c("structure_inchikey", "organism_name", "reference_doi", "stereo_name")
  )
  # Fast-path when already matching tidytable
  expect_identical(select_structure_organism_pair_columns(res_sop), res_sop)

  # 2. select_structure_organism_pair_loading_columns
  df_load <- tidytable::tidytable(
    inchikey = "IK1",
    structure_smiles = "C",
    organism_taxonomy_ottid = "1",
    unwanted = 99L
  )
  res_load <- select_structure_organism_pair_loading_columns(df_load)
  expect_identical(
    names(res_load),
    c("inchikey", "structure_smiles", "organism_taxonomy_ottid")
  )
  expect_identical(
    select_structure_organism_pair_loading_columns(res_load),
    res_load
  )

  # 3. select_clean_chemo_consensus_working_columns
  df_cc <- tidytable::tidytable(
    .row_id = 1L,
    feature_id = "F1",
    rank_final = 1L,
    extra = "x"
  )
  res_cc <- select_clean_chemo_consensus_working_columns(df_cc)
  expect_identical(names(res_cc), c(".row_id", "feature_id", "rank_final"))
  expect_identical(
    select_clean_chemo_consensus_working_columns(res_cc),
    res_cc
  )

  # 4. select_weight_annotations_working_columns
  df_wa <- tidytable::tidytable(
    feature_id = "F1",
    mz = 100.1,
    score_biological = 0.8,
    extra = "x"
  )
  res_wa <- select_weight_annotations_working_columns(df_wa)
  expect_identical(names(res_wa), c("feature_id", "mz", "score_biological"))
  expect_identical(select_weight_annotations_working_columns(res_wa), res_wa)

  # 5. select_annotations_complement_working_columns
  df_ac <- tidytable::tidytable(
    feature_id = "F1",
    candidate_structure_smiles_no_stereo = "CCO",
    extra = "x"
  )
  res_ac <- select_annotations_complement_working_columns(df_ac)
  expect_identical(
    names(res_ac),
    c("feature_id", "candidate_structure_smiles_no_stereo")
  )
  expect_identical(
    select_annotations_complement_working_columns(res_ac),
    res_ac
  )
})
