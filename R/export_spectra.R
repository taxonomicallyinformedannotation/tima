#' @title Export spectra
#'
#' @description This function prepares spectra for export
#'
#' @param file File where spectra will be exported. Can be '.mgf' or '.sqlite'
#' @param spectra The spectra object where spectra are stored
#' @param dir Directory for export
#' @param cmps Optional. Compounds data frame as in 'CompoundDb' package
#' @param metad Optional. Metadata as in 'CompoundDb' package
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom CompoundDb createCompDb
#' @importFrom MsBackendMgf MsBackendMgf
#' @importFrom Spectra export Spectra
#'
#' @examples NULL
export_spectra <- function(file,
                           spectra,
                           dir = ".",
                           cmps = NULL,
                           metad = NULL) {
  switch(
    EXPR = gsub(
      pattern = ".*\\.",
      replacement = "",
      x = file
    ),
    "mgf" = {
      Spectra::Spectra(object = spectra) |>
        Spectra::export(
          backend = MsBackendMgf::MsBackendMgf(),
          file = file
        )
    },
    "sqlite" = {
      if (is.null(cmps)) {
        cmps <-
          data.frame(
            compound_id = spectra$compound_id,
            name = spectra$name,
            inchi = spectra$inchi,
            inchikey = spectra$inchikey,
            exactmass = spectra$exactmass,
            formula = spectra$formula,
            synonyms = spectra$synonyms
          )
      }
      if (is.null(metad)) {
        metad <-
          data.frame(
            name = c(
              "source",
              "url",
              "source_version",
              "source_date",
              "organism"
            ),
            value = c(
              NA,
              NA,
              0,
              Sys.time() |>
                as.character(),
              NA
            )
          )
      }

      CompoundDb::createCompDb(
        x = cmps,
        metadata = metad,
        msms_spectra = spectra,
        path = dir,
        dbFile = file
      )
    }
  )
}

#' @title Export spectra 2
#'
#' @description This function export spectra.
#'
#' @param file File where spectra will be exported. Can be '.mgf' or '.sqlite'
#' @param spectra The spectra object where spectra are stored
#' @param meta Optional. Metadata as in 'CompoundDb' package
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr filter
#'
#' @examples NULL
export_spectra_2 <- function(file,
                             spectra,
                             meta) {
  if (nrow(spectra |>
    dplyr::filter(!is.na(compound_id))) != 0) {
    log_debug("Exporting")
    create_dir(export = file)
    spectra |>
      export_spectra(
        file = file,
        metad = meta
      )
  }
}
