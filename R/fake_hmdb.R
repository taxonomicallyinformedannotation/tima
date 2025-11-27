#' @title Fake HMDB
#'
#' @description This function creates a minimal fake HMDB SDF file when the
#'     real download fails. Used as a fallback to prevent pipeline failures
#'     during testing or when external resources are unavailable.
#'
#' @param export Character string path where the fake HMDB zip file should be saved
#'
#' @return Character string path to the created fake file
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' fake_hmdb(export = "data/source/hmdb.sdf.zip")
#' }
fake_hmdb <- function(export) {
  # Validate input
  if (missing(export) || !is.character(export) || length(export) != 1L) {
    stop("export path must be a single character string")
  }

  logger::log_warn(
    "HMDB download failed. Creating minimal placeholder SDF file."
  )

  # Extract filename and convert .zip to .sdf
  fake_export <- export |>
    gsub(
      pattern = ".*/",
      replacement = "",
      perl = TRUE
    ) |>
    gsub(
      pattern = ".zip",
      replacement = ".sdf",
      fixed = TRUE
    )

  # Create minimal valid SDF structure with one compound (1-Methylhistidine)
  fake_sdf <- paste0(
    "",
    "> <DATABASE_ID>",
    "HMDB0000001",
    "> <SMILES>",
    "[H]OC(=O)[C@@]([H])(N([H])[H])C([H])([H])C1=C([H])N(C([H])=N1)C([H])([H])[H]",
    "> <INCHI_KEY>",
    "BRMWTNUJHUMWMS-LURJTMIESA-N",
    "> <FORMULA>",
    "C7H11N3O2",
    "> <GENERIC_NAME>",
    "1-Methylhistidine",
    ""
  )

  # Write SDF to temporary file
  writeLines(fake_sdf, fake_export)

  # Create zip archive
  zip_result <- system2(
    "zip",
    args = c(basename(export), fake_export),
    stdout = FALSE,
    stderr = FALSE
  )

  if (zip_result != 0) {
    logger::log_warn("Failed to create zip file, trying alternative method")
    utils::zip(zipfile = basename(export), files = fake_export)
  }

  # Move to final location
  if (file.exists(basename(export))) {
    file.rename(basename(export), export)
  }

  # Clean up temporary file
  unlink(fake_export)

  logger::log_debug("Created fake HMDB file at: {export}")
  return(export)
}
