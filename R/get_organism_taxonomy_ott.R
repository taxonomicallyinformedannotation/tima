#' @title Get organism taxonomy (Open Tree of Life Taxonomy)
#'
#' @description This function retrieves taxonomy from the Open Tree of Life taxonomy
#'
#' @param df Dataframe containing your organism(s) name(s)
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
get_organism_taxonomy_ott <- function(df) {
  organism_table <- df |>
    dplyr::mutate(organism = stringr::str_remove(
      string = organism,
      pattern = stringr::fixed(pattern = " x ")
    )) |>
    dplyr::distinct() |>
    dplyr::mutate(search_string = tolower(organism)) |>
    dplyr::distinct(
      organism,
      search_string
    ) |>
    dplyr::select(
      canonical_name = organism,
      search_string
    ) |>
    dplyr::filter(!is.na(canonical_name)) |>
    data.frame()

  organisms <- organism_table$canonical_name

  new_matched_otl_exact <- rotl::tnrs_match_names(
    names = organisms,
    do_approximate_matching = FALSE,
    include_suppressed = FALSE
  )

  new_ott_id <- new_matched_otl_exact |>
    dplyr::filter(!is.na(ott_id)) |>
    dplyr::distinct(ott_id)

  otts <- new_ott_id$ott_id

  taxon_info <- rotl::taxonomy_taxon_info(
    ott_ids = otts,
    include_lineage = TRUE,
    include_terminal_descendants = TRUE
  )

  taxon_lineage <- taxon_info |>
    rotl::tax_lineage()

  list_df <- list()

  for (i in seq_along(1:length(taxon_lineage))) {
    list_df[[i]] <- dplyr::bind_rows(
      data.frame(
        id = otts[i],
        rank = taxon_info[[i]]$rank,
        name = taxon_info[[i]]$name,
        unique_name = taxon_info[[i]]$unique_name,
        ott_id = as.character(taxon_info[[i]]$ott_id)
      ),
      data.frame(id = otts[i], taxon_lineage[[i]])
    )
  }

  otl <- dplyr::bind_rows(list_df) |>
    dplyr::mutate(ott_id = as.integer(ott_id))

  biological_metadata <-
    dplyr::left_join(organism_table, new_matched_otl_exact) |>
    dplyr::left_join(otl, by = c("ott_id" = "id")) |>
    dplyr::filter(
      rank %in% c(
        "domain",
        "kingdom",
        "phylum",
        "class",
        "order",
        "infraorder",
        "family",
        "subfamily",
        "tribe",
        "subtribe",
        "genus",
        "subgenus",
        "species",
        "subspecies",
        "varietas"
      )
    ) |>
    dplyr::distinct() |>
    dplyr::arrange(dplyr::desc(dplyr::row_number())) |>
    ## feeling it is better that way
    dplyr::distinct(canonical_name, ott_id, rank, .keep_all = TRUE) |>
    ## canonical_name important for synonyms
    tidyr::pivot_wider(
      names_from = "rank",
      values_from = c("name", "unique_name.y", "ott_id.y")
    ) |>
    dplyr::select(
      organism_name = canonical_name,
      organism_taxonomy_ottid = ott_id,
      organism_taxonomy_01domain = dplyr::matches("name_domain"),
      organism_taxonomy_02kingdom = dplyr::matches("name_kingdom"),
      organism_taxonomy_03phylum = dplyr::matches("name_phylum"),
      organism_taxonomy_04class = dplyr::matches("name_class"),
      organism_taxonomy_05order = dplyr::matches("name_order"),
      organism_taxonomy_06family = dplyr::matches("name_family"),
      organism_taxonomy_07tribe = dplyr::matches("name_tribe"),
      organism_taxonomy_08genus = dplyr::matches("name_genus"),
      organism_taxonomy_09species = dplyr::matches("name_species"),
      organism_taxonomy_10varietas = dplyr::matches("name_varietas")
    ) |>
    dplyr::arrange(dplyr::desc(dplyr::row_number())) |>
    dplyr::coalesce()

  if (nrow(biological_metadata) != 0) {
    biological_metadata[dplyr::setdiff(
      x = c(
        "organism_name",
        "organism_taxonomy_ottid",
        "organism_taxonomy_01domain",
        "organism_taxonomy_02kingdom",
        "organism_taxonomy_03phylum",
        "organism_taxonomy_04class",
        "organism_taxonomy_05order",
        "organism_taxonomy_06family",
        "organism_taxonomy_07tribe",
        "organism_taxonomy_08genus",
        "organism_taxonomy_09species",
        "organism_taxonomy_10varietas"
      ),
      y = names(biological_metadata)
    )] <- NA
  }

  return(biological_metadata)
}
