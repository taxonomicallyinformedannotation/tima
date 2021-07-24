#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
preclean_gnverifier <- function(file) {
  verified <-
    jsonlite::stream_in(con = file(file))

  verified_df <- verified |>
    dplyr::select(
      -curation,
      -matchType
    ) |>
    tidyr::unnest(preferredResults, names_repair = "minimal") |>
    dplyr::filter(dataSourceTitleShort != "IRMNG (old)" &
      dataSourceTitleShort != "IPNI") |>
    # filter(!matchedName %in% wrongVerifiedDictionary$wrongOrganismsVerified) |>
    dplyr::mutate(organismType = "clean") %>%
    dplyr::select(
      organismType,
      organismValue = input,
      organismCleaned = currentCanonicalFull,
      organismDbTaxo = dataSourceTitleShort,
      taxonId = currentRecordId,
      organismCleanedCurrent = currentName,
      taxonomy = classificationPath,
      rank = classificationRanks
    )

  ## example ID 165 empty, maybe fill later on
  verified_df$organismDbTaxo <-
    y_as_na(verified_df$organismDbTaxo, "")

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
