#' Validate Input Data
#'
#' @description Standalone command to validate all input data before starting
#'     the TIMA pipeline. This helps catch issues early and avoid wasting time
#'     on library downloads and processing.
#'
#' @param features Character path to features CSV/TSV file
#' @param spectra Character path to MGF spectra file
#' @param metadata Character path to metadata file
#' @param sirius Character path to SIRIUS output directory or ZIP file
#' @param filename_col Character name of filename column (default: "filename")
#' @param organism_col Character name of organism column (default: "organism")
#' @param feature_col Character name of feature ID column (default: "feature_id")
#'
#' @return Invisible TRUE if all checks pass, stops with error otherwise
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Validate all inputs before starting pipeline
#' validate_inputs(
#'   features = "data/features.csv",
#'   spectra = "data/spectra.mgf",
#'   sirius = "data/sirius_output"
#' )
#'
#' # Validate with metadata consistency check
#' validate_inputs(
#'   features = "data/features.csv",
#'   metadata = "data/metadata.tsv"
#' )
#' }
validate_inputs <- function(
  features = NULL,
  spectra = NULL,
  metadata = NULL,
  sirius = NULL,
  filename_col = "filename",
  organism_col = "organism",
  feature_col = "feature_id"
) {
  sanitize_all_inputs(
    features_file = features,
    mgf_file = spectra,
    metadata_file = metadata,
    sirius_dir = sirius,
    filename_col = filename_col,
    organism_col = organism_col,
    feature_col = feature_col
  )
}
