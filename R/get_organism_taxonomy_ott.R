import::from(httr2, req_error, .into = environment())
import::from(httr2, req_method, .into = environment())
import::from(httr2, req_perform, .into = environment())
import::from(httr2, request, .into = environment())
import::from(httr2, resp_status_desc, .into = environment())
import::from(rotl, tax_lineage, .into = environment())
import::from(rotl, taxonomy_taxon_info, .into = environment())
import::from(rotl, tnrs_match_names, .into = environment())
import::from(stringi, stri_replace_all_fixed, .into = environment())
import::from(stringi, stri_replace_all_regex, .into = environment())
import::from(tidyfst, set, .into = environment())
import::from(tidyfst, setDT, .into = environment())
import::from(tidytable, across, .into = environment())
import::from(tidytable, arrange, .into = environment())
import::from(tidytable, as_tidytable, .into = environment())
import::from(tidytable, bind_rows, .into = environment())
import::from(tidytable, desc, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, filter, .into = environment())
import::from(tidytable, left_join, .into = environment())
import::from(tidytable, matches, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, pivot_wider, .into = environment())
import::from(tidytable, rename, .into = environment())
import::from(tidytable, row_number, .into = environment())
import::from(tidytable, select, .into = environment())
import::from(tidytable, tidytable, .into = environment())
import::from(tidytable, where, .into = environment())

#' @title Get organism taxonomy (Open Tree of Life Taxonomy)
#'
#' @description This function retrieves taxonomy
#'    from the Open Tree of Life taxonomy
#'
#' @importFrom httr2 req_error
#' @importFrom httr2 req_method
#' @importFrom httr2 req_perform
#' @importFrom httr2 request
#' @importFrom httr2 resp_status_desc
#' @importFrom rotl tax_lineage
#' @importFrom rotl taxonomy_taxon_info
#' @importFrom rotl tnrs_match_names
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom stringi stri_replace_all_regex
#' @importFrom tidyfst set
#' @importFrom tidyfst setDT
#' @importFrom tidytable across
#' @importFrom tidytable arrange
#' @importFrom tidytable as_tidytable
#' @importFrom tidytable bind_rows
#' @importFrom tidytable desc
#' @importFrom tidytable distinct
#' @importFrom tidytable filter
#' @importFrom tidytable left_join
#' @importFrom tidytable matches
#' @importFrom tidytable mutate
#' @importFrom tidytable pivot_wider
#' @importFrom tidytable rename
#' @importFrom tidytable row_number
#' @importFrom tidytable select
#' @importFrom tidytable tidytable
#' @importFrom tidytable where
#'
#' @param df Dataframe containing your organism(s) name(s)
#' @param url url of the ott api (for testing purposes)
#' @param retry Boolean. Retry with generic epithet
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
get_organism_taxonomy_ott <- function(df, url = "https://api.opentreeoflife.org/v3/taxonomy/about", retry = TRUE) {
  organism_table <- df |>
    as_tidytable() |>
    mutate(organism = organism |>
      trimws()) |>
    mutate(
      organism = organism |>
        stri_replace_all_fixed(
          pattern = " x ",
          replacement = "",
          vectorize_all = FALSE
        )
    ) |>
    mutate(
      organism = organism |>
        stri_replace_all_fixed(
          pattern = "\u00D7 ",
          replacement = "",
          vectorize_all = FALSE
        )
    ) |>
    distinct() |>
    mutate(search_string = tolower(organism)) |>
    distinct(organism, search_string) |>
    select(canonical_name = organism, search_string) |>
    filter(!is.na(canonical_name))

  organisms <- organism_table$canonical_name

  log_debug("Testing if Open Tree of Life API is up")
  if (url |>
    request() |>
    req_method("POST") |>
    ## weird hack to avoid error
    req_error(
      is_error = function(resp) {
        return(FALSE)
      }
    ) |>
    req_perform() |>
    resp_status_desc() != "OK") {
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
    organisms_split <-
      lapply(
        X = seq(1, length(organisms), cut),
        FUN = function(i) {
          organisms[i:(i + cut - 1)][!is.na(organisms[i:(i + cut - 1)])]
        }
      )
    new_matched_otl_exact_list <- organisms_split |>
      lapply(
        FUN = function(x) {
          tnrs_match_names(
            names = x,
            do_approximate_matching = FALSE,
            include_suppressed = FALSE
          )
        }
      )
    log_debug("Request finished!")

    new_matched_otl_exact <- new_matched_otl_exact_list |>
      bind_rows() |>
      mutate(across(.cols = where(is.logical), .fns = as.character))
    new_ott_id <- new_matched_otl_exact |>
      filter(!is.na(ott_id)) |>
      distinct(ott_id)

    if (nrow(new_matched_otl_exact) != nrow(new_ott_id) &&
      retry == TRUE) {
      ## keep obtained results
      pretable <- new_matched_otl_exact |>
        filter(!is.na(ott_id))

      new_ott_id_1 <- pretable |>
        distinct(ott_id)

      organism_table_2 <- organism_table |>
        as_tidytable() |>
        filter(!organism_table$search_string %in% pretable$search_string)

      organism_table_2$search_string <-
        stri_replace_all_regex(
          str = organism_table_2$search_string,
          pattern = " .*",
          replacement = "",
          vectorize_all = FALSE
        )
      organisms <- unique(organism_table_2$search_string)
      organisms_new <-
        stri_replace_all_regex(
          str = organisms,
          pattern = " .*",
          replacement = "",
          vectorize_all = FALSE
        )
      ## TODO make it cleaner
      cut <- 100
      organisms_new_split <-
        lapply(
          X = seq(1, length(organisms_new), cut),
          FUN = function(i) {
            organisms_new[i:(i + cut - 1)][!is.na(organisms_new[i:(i + cut - 1)])]
          }
        )
      log_debug("Retrying with", organisms_new)
      new_matched_otl_exact_list_2 <- organisms_new_split |>
        lapply(
          FUN = function(x) {
            tnrs_match_names(
              names = x,
              do_approximate_matching = FALSE,
              include_suppressed = FALSE
            )
          }
        )

      new_matched_otl_exact_2 <- new_matched_otl_exact_list_2 |>
        bind_rows() |>
        filter(!is.na(ott_id)) |>
        mutate(across(.cols = where(is.logical), .fns = as.character))
      new_ott_id_2 <- new_matched_otl_exact_2 |>
        distinct(ott_id)

      new_ott_id <- bind_rows(new_ott_id_1, new_ott_id_2)
      new_matched_otl_exact <-
        bind_rows(new_matched_otl_exact, new_matched_otl_exact_2)
    }

    if (nrow(new_ott_id) != 0) {
      otts <- new_ott_id$ott_id

      log_debug("Getting taxonomy...")
      taxon_info <- taxonomy_taxon_info(
        ott_ids = otts,
        include_lineage = TRUE,
        include_terminal_descendants = TRUE
      )
      log_debug("Taxonomy retrieved!")

      taxon_lineage <- taxon_info |>
        tax_lineage()

      list_df <- seq_along(taxon_lineage) |>
        lapply(
          FUN = function(x) {
            bind_rows(
              data.frame(
                id = otts[x],
                rank = taxon_info[[x]]$rank,
                name = taxon_info[[x]]$name,
                unique_name = taxon_info[[x]]$unique_name,
                ott_id = as.character(taxon_info[[x]]$ott_id)
              ),
              data.frame(id = otts[x], taxon_lineage[[x]])
            )
          }
        )

      otl <- bind_rows(list_df) |>
        mutate(ott_id = as.integer(ott_id)) |>
        ## feeling it is better that way
        mutate(n = row_number()) |>
        arrange(desc(n)) |>
        select(-n)
    } else {
      log_debug("Nothing found, returning empty dataframe")
      otl <-
        tidytable(
          id = NA_integer_,
          rank = NA_character_,
          name = NA_character_,
          unique_name = NA_character_,
          ott_id = NA_integer_
        )
    }

    biological_metadata <-
      left_join(organism_table, new_matched_otl_exact) |>
      left_join(
        otl |>
          rename(unique_name.y = unique_name, ott_id.y = ott_id),
        by = c("ott_id" = "id")
      ) |>
      filter(
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
      distinct() |>
      distinct(canonical_name, ott_id, rank, .keep_all = TRUE)

    if (nrow(biological_metadata) != 0) {
      biological_metadata <- biological_metadata |>
        ## canonical_name important for synonyms
        pivot_wider(
          names_from = "rank",
          values_from = c("name", "unique_name.y", "ott_id.y")
        ) |>
        select(
          organism_name = canonical_name,
          organism_taxonomy_ottid = ott_id,
          organism_taxonomy_01domain = matches("name_domain"),
          organism_taxonomy_02kingdom = matches("name_kingdom"),
          organism_taxonomy_03phylum = matches("name_phylum"),
          organism_taxonomy_04class = matches("name_class"),
          organism_taxonomy_05order = matches("name_order"),
          organism_taxonomy_06family = matches("name_family"),
          organism_taxonomy_07tribe = matches("name_tribe"),
          organism_taxonomy_08genus = matches("name_genus"),
          organism_taxonomy_09species = matches("name_species"),
          organism_taxonomy_10varietas = matches("name_varietas")
        )

      cols_to_set <- c(
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
      )

      new_cols <- setdiff(cols_to_set, colnames(biological_metadata))
      # Add new columns if they don't exist
      if (length(new_cols) > 0) {
        setDT(biological_metadata)
        set(biological_metadata, NULL, new_cols, NA_character_)
        biological_metadata |>
          as_tidytable()
      }
    }

    log_debug("Got OTTaxonomy!")
    return(biological_metadata)
  }
}
