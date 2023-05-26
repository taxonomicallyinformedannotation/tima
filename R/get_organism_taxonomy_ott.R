utils::globalVariables(c(
  "canonical_name",
  "organism",
  "ott_id",
  "search_string"
))

#' @title Get organism taxonomy (Open Tree of Life Taxonomy)
#'
#' @description This function retrieves taxonomy from the Open Tree of Life taxonomy
#'
#' @param df Dataframe containing your organism(s) name(s)
#' @param url url of the ott api (for testing purposes)
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
get_organism_taxonomy_ott <- function(df,
                                      url = "https://api.opentreeoflife.org/v3/taxonomy/about") {
  organism_table <- df |>
    dplyr::mutate(
      organism = organism |>
        trimws()
    ) |>
    dplyr::mutate(
      organism = organism |>
        stringi::stri_replace_all_fixed(
          pattern = " x ",
          replacement = "",
          vectorize = FALSE
        )
    ) |>
    dplyr::mutate(
      organism = organism |>
        stringi::stri_replace_all_fixed(
          pattern = "× ",
          replacement = "",
          vectorize = FALSE
        )
    ) |>
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

  log_debug("Testing if Open Tree of Life API is up")
  res <- httr::POST(url = url)
  status <- res |>
    httr::http_status()
  if (status$category != "Success") {
    log_debug("Sorry, Open Tree of Life API is down")
    log_debug("Failing gracefuly and returning empty results")
    new_matched_otl_exact <-
      data.frame(
        "search_string" = NA_character_,
        "ott_id" = NA_integer_,
        "unique_name" = NA_character_
      )
    otl <-
      data.frame(
        "id" = NA_integer_,
        "rank" = NA_character_,
        "name" = NA_character_,
        "unique_name" = NA_character_,
        "ott_id" = NA_integer_
      )
  } else {
    log_debug("Success! Submitting request...")
    ## cutting in smaller requests
    cut <- 100
    organisms_split <- lapply(seq(1, length(organisms), cut), function(i) {
      organisms[i:(i + cut - 1)][!is.na(organisms[i:(i + cut - 1)])]
    })
    new_matched_otl_exact_list <- organisms_split |>
      lapply(
        FUN = function(x) {
          rotl::tnrs_match_names(
            names = x,
            do_approximate_matching = FALSE,
            include_suppressed = FALSE
          )
        }
      )

    new_ott_id <- new_matched_otl_exact_list |>
      dplyr::bind_rows() |>
      dplyr::filter(!is.na(ott_id)) |>
      dplyr::distinct(ott_id)

    if (nrow(new_matched_otl_exact) != nrow(new_ott_id)) {
      ## keep obtained results
      pretable <- new_matched_otl_exact |>
        dplyr::filter(!is.na(ott_id))

      new_ott_id_1 <- pretable |>
        dplyr::distinct(ott_id)

      organism_table_2 <- organism_table |>
        dplyr::filter(!organism_table$search_string %in% pretable$search_string)

      organism_table_2$search_string <-
        stringi::stri_replace_all_regex(
          str = organism_table_2$search_string,
          pattern = " .*",
          replacement = "",
          vectorize = FALSE
        )
      organisms <- unique(organism_table_2$search_string)
      organisms_new <-
        stringi::stri_replace_all_regex(
          str = organisms,
          pattern = " .*",
          replacement = "",
          vectorize = FALSE
        )
      ## TODO make it cleaner
      cut <- 100
      organisms_new_split <- lapply(seq(1, length(organisms_new), cut), function(i) {
        organisms_new[i:(i + cut - 1)][!is.na(organisms_new[i:(i + cut - 1)])]
      })
      log_debug("Retrying with", organisms_new)
      new_matched_otl_exact_list_2 <- organisms_new_split |>
        lapply(
          FUN = function(x) {
            rotl::tnrs_match_names(
              names = x,
              do_approximate_matching = FALSE,
              include_suppressed = FALSE
            )
          }
        )

      new_ott_id_2 <- new_matched_otl_exact_list_2 |>
        dplyr::bind_rows() |>
        dplyr::filter(!is.na(ott_id)) |>
        dplyr::distinct(ott_id)

      new_ott_id <- dplyr::bind_rows(new_ott_id_1, new_ott_id_2)
    }

    if (nrow(new_ott_id) != 0) {
      otts <- new_ott_id$ott_id

      log_debug("Getting taxonomy...")
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
    } else {
      otl <-
        data.frame(
          id = NA_integer_,
          rank = NA_character_,
          name = NA_character_,
          unique_name = NA_character_,
          ott_id = NA_integer_
        )
    }

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
      dplyr::distinct(canonical_name, ott_id, rank, .keep_all = TRUE)

    if (nrow(biological_metadata) != 0) {
      biological_metadata <- biological_metadata |>
        ## canonical_name important for synonyms
        tidytable::pivot_wider(
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
}
