#' @title Fake ECMDB
#'
#' @description This function fakes ECMDB in case the download failed
#'
#' @noRd
#'
#' @param export Path to save the file to
#'
#' @return NULL
#'
#' @examples NULL
fake_ecmdb <- function(export) {
  log_debug("External failure. Returning empty file instead.")
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
  paste0(
    "[{",
    "\"name\":null,",
    "\"moldb_inchikey\":null,",
    "\"moldb_smiles\":null,",
    "\"moldb_formula\":null,",
    "\"moldb_mono_mass\":null,",
    "\"moldb_logp\":null",
    "}]"
  ) |>
    writeLines(fake_export)
  system(paste("zip", export, fake_export))
  unlink(fake_export)
  return(export)
}
