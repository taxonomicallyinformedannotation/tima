.fake_zip_cmd <- function(export, fake_export) {
  system2(
    "zip",
    args = c(basename(export), fake_export),
    stdout = FALSE,
    stderr = FALSE
  )
}

.fake_zip_fallback <- function(export, fake_export) {
  utils::zip(zipfile = basename(export), files = fake_export)
}

.fake_archive_exists <- function(export) {
  file.exists(basename(export))
}

#' Write fake payload to temp file, zip it, move to destination, and clean up
#' @noRd
.write_fake_zip_archive <- function(
  export,
  fake_export,
  content,
  zip_cmd,
  zip_fallback,
  archive_exists,
  label
) {
  writeLines(content, fake_export)

  zip_result <- zip_cmd(export = export, fake_export = fake_export)

  if (zip_result != 0) {
    log_warn("Failed to create zip file, trying alternative method")
    zip_fallback(export = export, fake_export = fake_export)
  }

  # Move to final location (handles cross-device moves in Docker)
  if (archive_exists(export)) {
    moved <- move_file_safely(basename(export), export)
    if (!isTRUE(moved)) {
      cli::cli_abort(
        c(
          "failed to move temporary archive",
          "x" = export
        ),
        class = c("tima_runtime_error", "tima_error"),
        call = NULL
      )
    }
  }

  unlink(fake_export)

  log_debug("Created fake %s file at: %s", label, export)
  export
}

#' @title Fake ECMDB
#'
#' @description This function creates a minimal fake ECMDB JSON file when the
#'     real download fails. Used as a fallback to prevent pipeline failures
#'     during testing or when external resources are unavailable.
#'
#' @include logs_utils.R
#' @include move_file_safely.R
#' @param export Character string path where the fake ECMDB zip file should be
#'     saved
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
  .assert_single_char(export, "export path")

  log_warn("ECMDB download failed. Creating empty placeholder file.")

  # Extract filename without path and .zip extension
  fake_export <- basename(export) |>
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

  .write_fake_zip_archive(
    export = export,
    fake_export = fake_export,
    content = fake_json,
    zip_cmd = .fake_zip_cmd,
    zip_fallback = .fake_zip_fallback,
    archive_exists = .fake_archive_exists,
    label = "ECMDB"
  )
}
