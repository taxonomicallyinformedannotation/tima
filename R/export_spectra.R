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
#' @examples NULL
export_spectra <- function(file,
                           spectra,
                           dir = ".",
                           cmps = NULL,
                           metad = NULL) {
  spectra |>
    data.frame() |>
    saveRDS(file = file)
  # switch(
  #   EXPR = gsub(
  #     pattern = ".*\\.",
  #     replacement = "",
  #     x = file,
  #     perl = TRUE
  #   ),
  #   "mgf" = {
  #     Spectra(object = spectra) |>
  #       export(
  #         backend = MsBackendMgf(),
  #         file = file
  #       )
  #   },
  #   "sqlite" = {
  #     if (is.null(cmps)) {
  #       cmps <-
  #         data.frame(
  #           compound_id = spectra$compound_id,
  #           name = spectra$name,
  #           inchi = spectra$inchi,
  #           inchikey = spectra$inchikey,
  #           exactmass = spectra$exactmass,
  #           formula = spectra$formula,
  #           synonyms = spectra$synonyms
  #         )
  #     }
  #     if (is.null(metad)) {
  #       metad <-
  #         data.frame(
  #           name = c(
  #             "source",
  #             "url",
  #             "source_version",
  #             "source_date",
  #             "organism"
  #           ),
  #           value = c(
  #             NA,
  #             NA,
  #             0,
  #             Sys.time() |>
  #               as.character(),
  #             NA
  #           )
  #         )
  #     }
  #     createCompDb(
  #       x = cmps,
  #       metadata = metad,
  #       msms_spectra = spectra,
  #       path = dir,
  #       dbFile = file
  #     )
  #   }
  # )
}
