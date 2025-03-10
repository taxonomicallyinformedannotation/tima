#' @title Get organism taxonomy (Open Tree of Life Taxonomy)
#'
#' @description This function retrieves taxonomy
#'    from the Open Tree of Life taxonomy
#'
#' @param df Dataframe containing your organism(s) name(s)
#' @param url url of the ott api (for testing purposes)
#' @param retry Boolean. Retry with generic epithet
#'
#' @return The path to the obtained OTT taxonomy
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame("organism" = "Homo sapiens")
#' get_organism_taxonomy_ott(df)
#' }
get_organism_taxonomy_ott <- function(df, url = "https://api.opentreeoflife.org/v3/taxonomy/about", retry = TRUE) {
  organism_table <- df |>
    tidytable::as_tidytable() |>
    tidytable::mutate(organism = organism |>
      trimws()) |>
    tidytable::mutate(
      organism = organism |>
        stringi::stri_replace_all_fixed(
          pattern = " x ",
          replacement = "",
          vectorize_all = FALSE
        )
    ) |>
    tidytable::mutate(
      organism = organism |>
        stringi::stri_replace_all_fixed(
          pattern = "\u00D7 ",
          replacement = "",
          vectorize_all = FALSE
        )
    ) |>
    tidytable::distinct() |>
    tidytable::mutate(search_string = tolower(organism)) |>
    tidytable::distinct(organism, search_string) |>
    tidytable::select(canonical_name = organism, search_string) |>
    tidytable::filter(!is.na(canonical_name))

  organisms <- organism_table$canonical_name

  log_debug("Testing if Open Tree of Life API is up")
  if (url |>
    httr2::request() |>
    # See https://github.com/ropensci/rotl/issues/147
    httr2::req_options(ssl_verifypeer = FALSE) |>
    httr2::req_method("POST") |>
    ## weird hack to avoid error
    httr2::req_error(
      is_error = function(resp) {
        return(FALSE)
      }
    ) |>
    httr2::req_perform() |>
    httr2::resp_status_desc() != "OK") {
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
      purrr::map(
        .x = seq(1, length(organisms), cut),
        .f = function(i) {
          organisms[i:(i + cut - 1)][!is.na(organisms[i:(i + cut - 1)])]
        }
      )
    new_matched_otl_exact_list <- organisms_split |>
      purrr::map(
        .f = function(x) {
          # See https://github.com/ropensci/rotl/issues/147
          httr::with_config(
            httr::config(ssl_verifypeer = FALSE),
            rotl::tnrs_match_names(
              names = x,
              do_approximate_matching = FALSE,
              include_suppressed = FALSE
            )
          )
        }
      )
    log_debug("Request finished!")

    new_matched_otl_exact <- new_matched_otl_exact_list |>
      tidytable::bind_rows() |>
      tidytable::mutate(tidytable::across(.cols = tidyselect::where(is.logical), .fns = as.character))
    new_ott_id <- new_matched_otl_exact |>
      tidytable::filter(!is.na(ott_id)) |>
      tidytable::distinct(ott_id)

    if (nrow(new_matched_otl_exact) != nrow(new_ott_id) &&
      retry == TRUE) {
      ## keep obtained results
      pretable <- new_matched_otl_exact |>
        tidytable::filter(!is.na(ott_id))

      new_ott_id_1 <- pretable |>
        tidytable::distinct(ott_id)

      organism_table_2 <- organism_table |>
        tidytable::as_tidytable() |>
        tidytable::filter(!organism_table$search_string %in% pretable$search_string)

      organism_table_2$search_string <-
        stringi::stri_replace_all_regex(
          str = organism_table_2$search_string,
          pattern = " .*",
          replacement = "",
          vectorize_all = FALSE
        )
      organisms <- unique(organism_table_2$search_string)
      organisms_new <-
        stringi::stri_replace_all_regex(
          str = organisms,
          pattern = " .*",
          replacement = "",
          vectorize_all = FALSE
        )
      cut <- 100
      organisms_new_split <-
        purrr::map(
          .x = seq(1, length(organisms_new), cut),
          .f = function(i) {
            organisms_new[i:(i + cut - 1)][!is.na(organisms_new[i:(i + cut - 1)])]
          }
        )
      log_debug("Retrying with", organisms_new)
      new_matched_otl_exact_list_2 <- organisms_new_split |>
        purrr::map(
          .f = function(x) {
            # See https://github.com/ropensci/rotl/issues/147
            httr::with_config(
              httr::config(ssl_verifypeer = FALSE),
              rotl::tnrs_match_names(
                names = x,
                do_approximate_matching = FALSE,
                include_suppressed = FALSE
              )
            )
          }
        )

      new_matched_otl_exact_2 <- new_matched_otl_exact_list_2 |>
        tidytable::bind_rows() |>
        tidytable::filter(!is.na(ott_id)) |>
        tidytable::mutate(tidytable::across(.cols = tidyselect::where(is.logical), .fns = as.character))
      new_ott_id_2 <- new_matched_otl_exact_2 |>
        tidytable::distinct(ott_id)

      new_ott_id <- tidytable::bind_rows(new_ott_id_1, new_ott_id_2)
      new_matched_otl_exact <-
        tidytable::bind_rows(new_matched_otl_exact, new_matched_otl_exact_2)
    }

    if (nrow(new_ott_id) != 0) {
      otts <- new_ott_id$ott_id

      log_debug("Getting taxonomy...")
      # See https://github.com/ropensci/rotl/issues/147
      taxon_info <-
        httr::with_config(
          httr::config(ssl_verifypeer = FALSE),
          rotl::taxonomy_taxon_info(
            ott_ids = otts,
            include_lineage = TRUE,
            include_terminal_descendants = TRUE
          )
        )
      log_debug("Taxonomy retrieved!")

      taxon_lineage <- taxon_info |>
        rotl::tax_lineage()

      list_df <- seq_along(taxon_lineage) |>
        purrr::map(
          .f = function(x) {
            tidytable::bind_rows(
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

      otl <- tidytable::bind_rows(list_df) |>
        tidytable::mutate(ott_id = as.integer(ott_id)) |>
        ## feeling it is better that way
        tidytable::mutate(n = tidytable::row_number()) |>
        tidytable::arrange(tidytable::desc(n)) |>
        tidytable::select(-n)
    } else {
      log_debug("Nothing found, returning empty dataframe")
      otl <-
        tidytable::tidytable(
          id = NA_integer_,
          rank = NA_character_,
          name = NA_character_,
          unique_name = NA_character_,
          ott_id = NA_integer_
        )
    }

    biological_metadata <-
      tidytable::left_join(organism_table, new_matched_otl_exact) |>
      tidytable::left_join(
        otl |>
          tidytable::rename(unique_name.y = unique_name, ott_id.y = ott_id),
        by = c("ott_id" = "id")
      ) |>
      tidytable::filter(
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
      tidytable::distinct() |>
      tidytable::distinct(canonical_name, ott_id, rank, .keep_all = TRUE)

    if (nrow(biological_metadata) != 0) {
      biological_metadata <- biological_metadata |>
        ## canonical_name important for synonyms
        tidytable::pivot_wider(
          names_from = "rank",
          values_from = c("name", "unique_name.y", "ott_id.y")
        ) |>
        tidytable::select(
          organism_name = canonical_name,
          organism_taxonomy_ottid = ott_id,
          organism_taxonomy_01domain = tidyselect::matches("name_domain"),
          organism_taxonomy_02kingdom = tidyselect::matches("name_kingdom"),
          organism_taxonomy_03phylum = tidyselect::matches("name_phylum"),
          organism_taxonomy_04class = tidyselect::matches("name_class"),
          organism_taxonomy_05order = tidyselect::matches("name_order"),
          organism_taxonomy_06family = tidyselect::matches("name_family"),
          organism_taxonomy_07tribe = tidyselect::matches("name_tribe"),
          organism_taxonomy_08genus = tidyselect::matches("name_genus"),
          organism_taxonomy_09species = tidyselect::matches("name_species"),
          organism_taxonomy_10varietas = tidyselect::matches("name_varietas")
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
        tidyfst::setDT(biological_metadata)
        tidyfst::set(biological_metadata, NULL, new_cols, NA_character_)
        biological_metadata |>
          tidytable::as_tidytable()
      }
    }

    log_debug("Got OTTaxonomy!")
    return(biological_metadata)
  }
}
