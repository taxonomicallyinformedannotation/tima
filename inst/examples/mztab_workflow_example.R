#!/usr/bin/env Rscript

#' Example: mzTab-M Workflow with TIMA
#'
#' This script demonstrates a complete workflow:
#' 1. Import mzTab-M file
#' 2. Run TIMA annotation
#' 3. Export results back to mzTab-M
#'
#' @author TIMA Team
#' @date 2026-01-05

# Load TIMA ----
library(tima)

# Setup paths ----
mztab_input <- "data/source/experiment.mztab"
mztab_output <- "data/output/experiment_annotated.mztab"

features_csv <- "data/interim/features/features.csv"
spectra_mgf <- "data/source/spectra.mgf"
metadata_csv <- "data/source/metadata.csv"

# Step 1: Import mzTab-M Data ----
cat("Step 1: Importing mzTab-M file...\n")

paths <- read_mztab(
  input = mztab_input,
  output_features = features_csv,
  output_spectra = spectra_mgf,
  output_metadata = metadata_csv
)

cat("  ✓ Features:", paths$features, "\n")
cat("  ✓ Spectra:", ifelse(is.null(paths$spectra), "N/A", paths$spectra), "\n")
cat(
  "  ✓ Metadata:",
  ifelse(is.null(paths$metadata), "N/A", paths$metadata),
  "\n\n"
)

# Step 2: Prepare TIMA Inputs ----
cat("Step 2: Preparing features and annotations...\n")

# Prepare feature table
prepare_features_tables(
  features = features_csv,
  output = "data/interim/features/features_prepared.csv"
)

# Import and sanitize spectra (if available)
if (!is.null(paths$spectra) && file.exists(paths$spectra)) {
  spectra <- import_spectra(
    file = paths$spectra,
    polarity = "pos", # Adjust based on your data
    sanitize = TRUE
  )

  # Export sanitized spectra
  export_spectra_rds(
    spectra = spectra,
    output = "data/interim/spectra/spectra_processed.rds"
  )
}

# Prepare mzTab annotations
prepare_annotations_mztab(
  input = mztab_input,
  output = "data/interim/annotations/mztab_prepared.tsv"
)

cat("  ✓ Features prepared\n")
cat("  ✓ Annotations prepared\n\n")

# Step 3: Run TIMA Annotation ----
cat("Step 3: Running TIMA annotation pipeline...\n")

# This is where you would run the full TIMA pipeline:
# - annotate_masses()
# - annotate_spectra()
# - prepare_taxa()
# - weight_annotations()
# - filter_annotations()

# For this example, assume we have annotated results
# In practice, you would run:
#
# annotations <- weight_annotations(
#   input_features = "data/interim/features/features_prepared.csv",
#   input_spectral = c(
#     "data/interim/annotations/mztab_prepared.tsv",
#     "data/interim/annotations/spectral_matches.tsv"
#   ),
#   input_metadata = metadata_csv,
#   output = "data/output/tima_annotations.tsv"
# )

cat("  ✓ TIMA annotation completed\n\n")

# Step 4: Export to mzTab-M ----
cat("Step 4: Writing annotated mzTab-M file...\n")

write_mztab(
  annotations = "data/output/tima_annotations.tsv",
  original_mztab = mztab_input,
  output = mztab_output,
  top_n = 3, # Keep top 3 candidates per feature
  score_threshold = 0.5 # Minimum TIMA score
)

cat("  ✓ Annotated mzTab-M:", mztab_output, "\n\n")

# Summary ----
cat("═══════════════════════════════════════════════════════════\n")
cat("WORKFLOW COMPLETE\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("\nInput:  ", mztab_input, "\n")
cat("Output: ", mztab_output, "\n")
cat("\nThe annotated mzTab-M file contains:\n")
cat("  • Original features and metadata\n")
cat("  • TIMA structural annotations\n")
cat("  • TIMA confidence scores\n")
cat("  • Taxonomic classifications\n")
cat("\nYou can now:\n")
cat("  1. Share the annotated mzTab-M with collaborators\n")
cat("  2. Import into other metabolomics tools\n")
cat("  3. Submit to metabolomics repositories\n")
cat("═══════════════════════════════════════════════════════════\n")
