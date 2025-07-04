#' @title Fake ECMDB
#'
#' @description This function fakes ECMDB in case the download failed
#'
#' @param export Path to save the file to
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
fake_ecmdb <- function(export) {
  logger::log_error("External failure. Returning empty file instead.")
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
  system(paste("zip", basename(export), fake_export))
  system(paste("mv", basename(export), export))
  unlink(fake_export)
  return(export)
}
