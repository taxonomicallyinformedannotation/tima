source(file = here::here("R", "log_debug.R"))

#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
preclean_gnverifier <- function(file) {
  log_debug("Loading GNVerifier results")
  verified <-
    jsonlite::stream_in(con = file(file))

  verified_df <- verified |>
    dplyr::select(
      -curation,
      -matchType
    ) |>
    tidyr::unnest(preferredResults, names_repair = "minimal") |>
    dplyr::select(
      organismValue = input,
      organismCleaned = currentCanonicalFull,
      organismDbTaxo = dataSourceTitleShort,
      taxonId = currentRecordId,
      organismCleanedCurrent = currentName,
      taxonomy = classificationPath,
      rank = classificationRanks
    )
  log_debug("Formatting GNVerifier results")
  dataOrganismVerified <- dplyr::left_join(organism_table,
    verified_df,
    by = c("organism" = "organismValue")
  ) %>%
    dplyr::select(
      organism,
      organismCleaned,
      organismDbTaxo,
      taxonId,
      organismCleanedCurrent,
      organismCleaned_dbTaxoTaxonomy = taxonomy,
      organismCleaned_dbTaxoTaxonRanks = rank
    )

  return(dataOrganismVerified)
}
