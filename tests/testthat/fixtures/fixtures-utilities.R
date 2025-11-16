#' Fixture Utilities
#'
#' @description
#' Utilities for loading CSV fixtures and creating minimal test data structures.
#' This file provides both CSV loaders and programmatic fixture generators.

# ==============================================================================
# CSV Fixture Loaders
# ==============================================================================

#' Load fixture CSV file
#'
#' @param filename Name of CSV file in fixtures/ directory
#' @return tidytable loaded from CSV
#' @keywords internal
load_fixture_csv <- function(filename) {
  path <- testthat::test_path("fixtures", filename)
  if (!file.exists(path)) {
    stop("Fixture file not found: ", filename)
  }
  tidytable::fread(path, na.strings = c("", "NA"))
}

#' Load structure metadata fixture
#'
#' @return tidytable with structure metadata
#' @export
#' @keywords internal
load_fixture_metadata <- function() {
  load_fixture_csv("metadata.csv")
}

#' Load structure stereochemistry fixture
#'
#' @return tidytable with stereo data
#' @export
#' @keywords internal
load_fixture_stereo <- function() {
  load_fixture_csv("stereo.csv")
}

#' Load structure names fixture
#'
#' @return tidytable with structure names
#' @export
#' @keywords internal
load_fixture_names <- function() {
  load_fixture_csv("names.csv")
}

#' Load Classyfire taxonomy fixture
#'
#' @return tidytable with Classyfire taxonomy
#' @export
#' @keywords internal
load_fixture_taxonomy_classyfire <- function() {
  load_fixture_csv("taxonomy_classyfire.csv")
}

#' Load NPClassifier taxonomy fixture
#'
#' @return tidytable with NPC taxonomy
#' @export
#' @keywords internal
load_fixture_taxonomy_npc <- function() {
  load_fixture_csv("taxonomy_npc.csv")
}

#' Load features fixture
#'
#' @return tidytable with features data
#' @export
#' @keywords internal
load_fixture_features <- function() {
  load_fixture_csv("features.csv")
}

#' Load edges fixture
#'
#' @return tidytable with network edges
#' @export
#' @keywords internal
load_fixture_edges <- function() {
  load_fixture_csv("edges.csv")
}

#' Load components fixture
#'
#' @return tidytable with network components
#' @export
#' @keywords internal
load_fixture_components <- function() {
  load_fixture_csv("components.csv")
}

#' Load annotations fixture
#'
#' @return tidytable with annotation data
#' @export
#' @keywords internal
load_fixture_annotations <- function() {
  load_fixture_csv("annotations.csv")
}

#' Load structure-organism pairs fixture
#'
#' @return tidytable with structure-organism pairs
#' @export
#' @keywords internal
load_fixture_sop <- function() {
  load_fixture_csv("structure_organism_pairs.csv")
}

# ==============================================================================
# Empty Structure Generators (for validation tests)
# ==============================================================================

#' Create empty table with required columns (for validation tests)
#'
#' @description
#' Creates a 0-row table with the correct column structure.
#' Use this for input validation tests where you need the structure but no data.
#'
#' @param template Character name of template ("annotation", "features", "sop", etc.)
#' @return Empty tidytable with correct columns
#' @export
#' @keywords internal
create_empty_table <- function(template = "annotation") {
  templates <- list(
    annotation = tidytable::tidytable(
      candidate_structure_inchikey_connectivity_layer = character(0),
      sample_organism_name = character(0),
      feature_id = character(0),
      candidate_structure_smiles_no_stereo = character(0)
    ),

    features = tidytable::tidytable(
      feature_id = character(0),
      mz = numeric(0),
      rt = numeric(0)
    ),

    sop = tidytable::tidytable(
      structure_inchikey_connectivity_layer = character(0),
      organism_name = character(0),
      organism_taxonomy_ottid = character(0),
      organism_taxonomy_01domain = character(0),
      organism_taxonomy_02kingdom = character(0),
      organism_taxonomy_03phylum = character(0),
      organism_taxonomy_04class = character(0),
      organism_taxonomy_05order = character(0),
      organism_taxonomy_06family = character(0),
      organism_taxonomy_07tribe = character(0),
      organism_taxonomy_08genus = character(0),
      organism_taxonomy_09species = character(0),
      organism_taxonomy_10varietas = character(0)
    ),

    library_keys = tidytable::tidytable(
      structure_inchikey_connectivity_layer = character(0),
      structure_smiles = character(0),
      structure_exact_mass = numeric(0),
      organism_name = character(0)
    ),

    components = tidytable::tidytable(
      feature_id = character(0),
      component_id = integer(0)
    ),

    edges = tidytable::tidytable(
      feature_source = character(0),
      feature_target = character(0),
      candidate_score_similarity = numeric(0)
    ),

    metadata = tidytable::tidytable(
      sample_organism_name = character(0),
      organism_taxonomy_ottid = character(0)
    )
  )

  if (!template %in% names(templates)) {
    stop(
      "Unknown template: ",
      template,
      ". Available: ",
      paste(names(templates), collapse = ", ")
    )
  }

  templates[[template]]
}

# ==============================================================================
# Minimal Data Generators (for simple tests)
# ==============================================================================

#' Create minimal sample data for testing
#'
#' @description
#' Extends empty templates with 2-3 rows of sample data.
#' Use this for simple logic tests where you need minimal realistic data.
#'
#' @param template Character name of template
#' @param n_rows Number of rows (default: 2)
#' @return tidytable with sample data
#' @export
#' @keywords internal
create_minimal_data <- function(template = "annotation", n_rows = 2) {
  empty <- create_empty_table(template)

  if (template == "annotation") {
    return(tidytable::tidytable(
      candidate_structure_inchikey_connectivity_layer = replicate(
        n_rows,
        paste(sample(LETTERS, 14, replace = TRUE), collapse = "")
      ),
      sample_organism_name = rep("Gentiana lutea", n_rows),
      feature_id = paste0("FT", sprintf("%04d", seq_len(n_rows))),
      candidate_structure_smiles_no_stereo = rep("CCCC", n_rows)
    ))
  }

  if (template == "features") {
    return(tidytable::tidytable(
      feature_id = paste0("FT", sprintf("%04d", seq_len(n_rows))),
      mz = runif(n_rows, 100, 500),
      rt = runif(n_rows, 0.1, 10.0)
    ))
  }

  if (template == "sop") {
    return(create_structure_organism_pairs(n_rows))
  }

  # For other templates, return empty structure
  # (can be extended as needed)
  empty
}

# ==============================================================================
# Extended Fixtures (programmatic generation)
# ==============================================================================

#' Create structure-organism pairs with full taxonomy
#'
#' @description
#' Programmatically creates structure-organism pairs.
#' Use this when you need specific test scenarios with taxonomy.
#'
#' @param n_rows Number of rows
#' @return tidytable with complete SOP structure
#' @export
#' @keywords internal
create_structure_organism_pairs <- function(n_rows = 0) {
  result <- create_empty_table("sop")

  if (n_rows == 0) {
    return(result)
  }

  # Populate with sample data
  result <- tidytable::tidytable(
    structure_inchikey_connectivity_layer = replicate(
      n_rows,
      paste(sample(LETTERS, 14, replace = TRUE), collapse = "")
    ),
    organism_name = sample(
      c("Gentiana lutea", "Arabidopsis thaliana", "Solanum tuberosum"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_ottid = as.character(sample(100000:999999, n_rows)),
    organism_taxonomy_01domain = rep("Eukaryota", n_rows),
    organism_taxonomy_02kingdom = rep("Plantae", n_rows),
    organism_taxonomy_03phylum = sample(
      c("Tracheophyta", "Chlorophyta"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_04class = rep("Magnoliopsida", n_rows),
    organism_taxonomy_05order = sample(
      c("Gentianales", "Brassicales", "Solanales"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_06family = sample(
      c("Gentianaceae", "Brassicaceae", "Solanaceae"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_07tribe = sample(
      c("Gentianeae", "Arabideae", "Solaneae", NA_character_),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_08genus = sample(
      c("Gentiana", "Arabidopsis", "Solanum"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_09species = sample(
      c("lutea", "thaliana", "tuberosum"),
      n_rows,
      replace = TRUE
    ),
    organism_taxonomy_10varietas = rep(NA_character_, n_rows)
  )

  result
}

#' Create annotation table with taxonomy (for weight_bio tests)
#'
#' @param n_rows Number of rows
#' @return tidytable with annotation and taxonomy
#' @export
#' @keywords internal
create_annotation_table_taxed <- function(n_rows = 0) {
  base <- create_minimal_data("annotation", n_rows)

  if (n_rows == 0) {
    # Return empty structure with taxonomy columns
    taxonomy_cols <- c(
      "sample_organism_01_domain",
      "sample_organism_02_kingdom",
      "sample_organism_03_phylum",
      "sample_organism_04_class",
      "sample_organism_05_order",
      "sample_organism_06_family",
      "sample_organism_07_tribe",
      "sample_organism_08_genus",
      "sample_organism_09_species",
      "sample_organism_10_varietas"
    )
    for (col in taxonomy_cols) {
      base[[col]] <- character(0)
    }
    return(base)
  }

  # Add taxonomy for sample data
  base$sample_organism_01_domain <- "Eukaryota"
  base$sample_organism_02_kingdom <- "Plantae"
  base$sample_organism_03_phylum <- "Tracheophyta"
  base$sample_organism_04_class <- "Magnoliopsida"
  base$sample_organism_05_order <- "Gentianales"
  base$sample_organism_06_family <- "Gentianaceae"
  base$sample_organism_07_tribe <- "Gentianeae"
  base$sample_organism_08_genus <- "Gentiana"
  base$sample_organism_09_species <- "lutea"
  base$sample_organism_10_varietas <- NA_character_

  base
}

# ==============================================================================
# Usage Guide
# ==============================================================================

# Use CSV fixtures when:
# - You need realistic reference data
# - Multiple tests use the same data
# - Data is static and doesn't need to vary
# Example: metadata <- load_fixture_metadata()

# Use create_empty_table() when:
# - Testing input validation
# - Need proper column structure but no data
# Example: empty_annot <- create_empty_table("annotation")

# Use create_minimal_data() when:
# - Need 2-3 rows for simple logic tests
# - Testing basic functionality
# Example: features <- create_minimal_data("features", n_rows = 3)

# Use create_*() functions when:
# - Need programmatically generated data
# - Testing with varying sizes/scenarios
# - Need specific test conditions
# Example: sop <- create_structure_organism_pairs(n_rows = 10)
