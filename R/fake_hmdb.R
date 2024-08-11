#' @title Fake HMDB
#'
#' @description This function fakes HMDB in case the download failed
#'
#' @noRd
#'
#' @param export Path to save the file to
#'
#' @return NULL
#'
#' @examples NULL
fake_hmdb <- function(export) {
  log_debug("External failure. Returning empty file instead.")
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
  paste0(
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
  ) |>
    writeLines(fake_export)
  system(paste("zip", export, fake_export))
  unlink(fake_export)
  return(export)
}
