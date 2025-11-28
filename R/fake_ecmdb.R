#' @title Fake ECMDB
#'
#' @description This function creates a minimal fake ECMDB JSON file when the
#'     real download fails. Used as a fallback to prevent pipeline failures
#'     during testing or when external resources are unavailable.
#'
#' @param export Character string path where the fake ECMDB zip file should be saved
#'
#' @return Character string path to the created fake file
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' fake_ecmdb(export = "data/source/ecmdb.json.zip")
#' }
fake_ecmdb <- function(export) {
  # Validate input
  if (missing(export) || !is.character(export) || length(export) != 1L) {
    stop("export path must be a single character string")
  }

  log_warn("ECMDB download failed. Creating empty placeholder file.")

  # Extract filename without path and .zip extension
  fake_export <- export |>
    gsub(
      pattern = ".*/",
      replacement = "",
      perl = TRUE
    ) |>
    gsub(
      pattern = ".zip",
      replacement = "",
      fixed = TRUE
    )

  # Create minimal valid JSON structure
  fake_json <- paste0(
    "[{",
    "\"name\":null,",
    "\"moldb_inchikey\":null,",
    "\"moldb_smiles\":null,",
    "\"moldb_formula\":null,",
    "\"moldb_mono_mass\":null,",
    "\"moldb_logp\":null",
    "}]"
  )

  # Write JSON to temporary file
  writeLines(fake_json, fake_export)

  # Create zip archive
  zip_result <- system2(
    "zip",
    args = c(basename(export), fake_export),
    stdout = FALSE,
    stderr = FALSE
  )

  if (zip_result != 0) {
    log_warn("Failed to create zip file, trying alternative method")
    utils::zip(zipfile = basename(export), files = fake_export)
  }

  # Move to final location
  if (file.exists(basename(export))) {
    file.rename(basename(export), export)
  }

  # Clean up temporary file
  unlink(fake_export)

  log_debug("Created fake ECMDB file at: ", export)
  return(export)
}
