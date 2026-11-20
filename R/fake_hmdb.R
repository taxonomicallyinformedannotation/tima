.fake_hmdb_zip_cmd <- function(export, fake_export) {
  system2(
    "zip",
    args = c(basename(export), fake_export),
    stdout = FALSE,
    stderr = FALSE
  )
}

.fake_hmdb_zip_fallback <- function(export, fake_export) {
  utils::zip(zipfile = basename(export), files = fake_export)
}

.fake_hmdb_archive_exists <- function(export) {
  file.exists(basename(export))
}

#' @title Fake HMDB
#'
#' @description This function creates a minimal fake HMDB SDF file when the
#'     real download fails. Used as a fallback to prevent pipeline failures
#'     during testing or when external resources are unavailable.
#'
#' @include logs_utils.R
#' @include move_file_safely.R
#' @param export Character string path where the fake HMDB zip file should be
#'     saved
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
  .assert_single_char(export, "export path")

  log_warn(
    "HMDB download failed. Creating minimal placeholder SDF file."
  )

  # Extract filename and convert .zip to .sdf
  fake_export <- basename(export) |>
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
    "[H]OC(=O)[C@@]([H])(N([H])[H])C([H])([H])C1=C([H])N(C([H]) =
      N1)C([H])([H])[H]",
    "> <INCHI_KEY>",
    "BRMWTNUJHUMWMS-LURJTMIESA-N",
    "> <FORMULA>",
    "C7H11N3O2",
    "> <GENERIC_NAME>",
    "1-Methylhistidine",
    ""
  )

  .write_fake_zip_archive(
    export = export,
    fake_export = fake_export,
    content = fake_sdf,
    zip_cmd = .fake_hmdb_zip_cmd,
    zip_fallback = .fake_hmdb_zip_fallback,
    archive_exists = .fake_hmdb_archive_exists,
    label = "HMDB"
  )
}
