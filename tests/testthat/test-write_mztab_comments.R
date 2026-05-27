library(testthat)

build_export_comments <- .mztab_build_export_comments
build_ambiguity_comments <- .mztab_build_ambiguity_comments
build_edges_comments <- .mztab_build_edges_comments
library_to_method <- .mztab_library_to_identification_method
mztab_escape <- .mztab_escape
charge_from_adduct <- .mztab_charge_from_adduct
pick_best_uri <- .mztab_pick_best_uri
pick_best_db_id <- .mztab_pick_best_database_identifier
connectivity_key <- .mztab_connectivity_layer_key

# .mztab_build_export_comments ----
test_that(".mztab_build_export_comments returns empty for no non-classical cols", {
  df <- data.frame(
    feature_id = "1",
    feature_mz = 100.0,
    rank_final = 1,
    score_final = 0.9
  )
  result <- build_export_comments(df)
  expect_equal(result, character(0))
})

test_that(".mztab_build_export_comments returns empty for non-data-frame", {
  expect_equal(build_export_comments(NULL), character(0))
  expect_equal(build_export_comments(list()), character(0))
})

test_that(".mztab_build_export_comments returns COM lines for extra columns", {
  df <- data.frame(
    feature_id = "1",
    extra_col_1 = "x",
    extra_col_2 = "y",
    stringsAsFactors = FALSE
  )
  result <- build_export_comments(df)
  expect_true(any(grepl("COM", result)))
  expect_true(any(grepl("extra_col_1", result)))
})

test_that(".mztab_build_export_comments chunks extra columns", {
  # Create a data frame with >16 non-canonical columns
  extra_cols <- setNames(
    as.list(seq_len(20)),
    paste0("extra_", seq_len(20))
  )
  df <- as.data.frame(extra_cols)
  result <- build_export_comments(df, chunk_size = 5L)
  # Should produce header line + ceiling(20/5) = 4 chunk lines
  expect_true(length(result) >= 5L)
})

# .mztab_build_ambiguity_comments ----
test_that(".mztab_build_ambiguity_comments returns empty for empty df", {
  expect_equal(build_ambiguity_comments(data.frame()), character(0))
})

test_that(".mztab_build_ambiguity_comments returns empty when no feature_id col", {
  df <- data.frame(x = 1:3)
  result <- build_ambiguity_comments(df)
  expect_equal(result, character(0))
})

test_that(".mztab_build_ambiguity_comments produces summary COM lines", {
  df <- data.frame(
    feature_id = c("F1", "F1", "F2"),
    database_identifier = c("CHEBI:1", "CHEBI:2", "CHEBI:3"),
    chemical_formula = c("C6H12O6", "C6H12O6", "C2H4O2"),
    smiles = c("OCC", "OCC", "CC(=O)O"),
    inchi = c("InChI=1/A", "InChI=1/B", "InChI=1/C"),
    chemical_name = c("Glucose", "Fructose", "AcOH"),
    uri = c("http://a", "http://b", "http://c"),
    adduct_ions = c("[M+H]+", "[M+H]+", "[M+H]+"),
    stringsAsFactors = FALSE
  )
  result <- build_ambiguity_comments(df)
  expect_true(length(result) >= 1L)
  expect_true(any(grepl("TIMA ambiguity", result)))
})

# .mztab_library_to_identification_method ----
test_that(".mztab_library_to_identification_method maps SIRIUS correctly", {
  result <- library_to_method("sirius_csi")
  expect_true(grepl("SIRIUS", result))
})

test_that(".mztab_library_to_identification_method maps GNPS correctly", {
  result <- library_to_method("GNPS")
  expect_true(grepl("GNPS", result))
})

test_that(".mztab_library_to_identification_method maps spectral library", {
  result <- library_to_method("spectral_library")
  expect_true(grepl("spectral library", result))
})

test_that(".mztab_library_to_identification_method maps MS1", {
  result <- library_to_method("ms1_mass")
  expect_true(grepl("exact mass", result))
})

test_that(".mztab_library_to_identification_method handles NA", {
  result <- library_to_method(NA_character_)
  expect_true(grepl("unknown", result))
})

test_that(".mztab_library_to_identification_method passes through Param format", {
  param <- "[, , my method, value]"
  result <- library_to_method(param)
  expect_equal(result, param)
})

test_that(".mztab_library_to_identification_method wraps unknown library", {
  result <- library_to_method("my_custom_lib")
  expect_true(grepl("my_custom_lib", result))
})

# .mztab_escape ----
test_that(".mztab_escape replaces tab characters with spaces", {
  result <- mztab_escape("hello\tworld")
  expect_equal(result, "hello world")
})

test_that(".mztab_escape leaves strings without tabs unchanged", {
  result <- mztab_escape("hello world")
  expect_equal(result, "hello world")
})

test_that(".mztab_escape handles empty string", {
  expect_equal(mztab_escape(""), "")
})

# .mztab_charge_from_adduct ----
test_that(".mztab_charge_from_adduct handles [M+H]+ as +1", {
  result <- unname(charge_from_adduct("[M+H]+"))
  expect_equal(result, "1")
})

test_that(".mztab_charge_from_adduct handles [M-H]- as -1", {
  result <- unname(charge_from_adduct("[M-H]-"))
  expect_equal(result, "-1")
})

test_that(".mztab_charge_from_adduct handles [M+2H]2+ as +2", {
  result <- unname(charge_from_adduct("[M+2H]2+"))
  expect_equal(result, "2")
})

test_that(".mztab_charge_from_adduct handles NA as 'null'", {
  result <- unname(charge_from_adduct(NA_character_))
  expect_equal(result, "null")
})

test_that(".mztab_charge_from_adduct handles 'null' as 'null'", {
  result <- unname(charge_from_adduct("null"))
  expect_equal(result, "null")
})

test_that(".mztab_charge_from_adduct handles empty string as 'null'", {
  result <- unname(charge_from_adduct(""))
  expect_equal(result, "null")
})

test_that(".mztab_charge_from_adduct is vectorized", {
  result <- unname(charge_from_adduct(c("[M+H]+", "[M-H]-", "[M+2H]2+")))
  expect_equal(result, c("1", "-1", "2"))
})

# .mztab_pick_best_uri ----
test_that(".mztab_pick_best_uri prefers wikidata", {
  rows <- data.frame(
    prefix = c("chebi", "wikidata"),
    id = c("12345", "Q123"),
    stringsAsFactors = FALSE
  )
  result <- pick_best_uri(rows)
  expect_true(grepl("wikidata", result))
})

test_that(".mztab_pick_best_uri returns 'null' for empty rows", {
  result <- pick_best_uri(data.frame(prefix = character(), id = character()))
  expect_equal(result, "null")
})

# .mztab_pick_best_database_identifier ----
test_that(".mztab_pick_best_database_identifier returns fallback when no rows", {
  result <- pick_best_db_id(NULL, fallback = "HMDB:12345")
  expect_equal(result, "HMDB:12345")
})

test_that(".mztab_pick_best_database_identifier returns 'null' for NA fallback", {
  result <- pick_best_db_id(NULL, fallback = NA_character_)
  expect_equal(result, "null")
})

test_that(".mztab_pick_best_database_identifier picks chebi over hmdb", {
  rows <- data.frame(
    prefix = c("hmdb", "chebi"),
    id = c("HMDB0001234", "12345"),
    stringsAsFactors = FALSE
  )
  result <- pick_best_db_id(rows)
  expect_true(grepl("CHEBI", result))
})

# .mztab_connectivity_layer_key ----
test_that(".mztab_connectivity_layer_key extracts 14-character prefix", {
  ik <- "WQZGKKKJIJFFOK-GASJEMHNSA-N"
  result <- connectivity_key(ik)
  expect_equal(result, "WQZGKKKJIJFFOK")
})

test_that(".mztab_connectivity_layer_key handles NA", {
  result <- connectivity_key(NA_character_)
  expect_true(is.na(result))
})

test_that(".mztab_connectivity_layer_key handles empty string", {
  result <- connectivity_key("")
  expect_true(is.na(result))
})
