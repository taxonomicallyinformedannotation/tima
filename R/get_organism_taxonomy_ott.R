# Internal Helper Functions ----

#' Create batches from a vector
#' @description Splits a vector into batches of specified size
#' @param items Character vector to batch
#' @param batch_size Integer size of each batch
#' @return List of character vectors (batches)
#' @keywords internal
.create_batches <- function(items, batch_size) {
  purrr::map(
    .x = seq(1L, length(items), batch_size),
    # TODO
    .f = function(i) {
      end_idx <- min(i + batch_size - 1L, length(items))
      items[i:end_idx][!is.na(items[i:end_idx])]
    }
  )
}

#' Query OTT API for a single batch
#' @description Queries the Open Tree of Life API for taxonomic names
#' @param batch Character vector of organism names
#' @return Data frame with taxonomy matches
#' @keywords internal
.query_ott_batch <- function(batch) {
  # SSL verification disabled due to rotl package issue #147
  # See: https://github.com/ropensci/rotl/issues/147
  httr::with_config(
    httr::config(ssl_verifypeer = FALSE),
    rotl::tnrs_match_names(
      names = batch,
      do_approximate_matching = FALSE,
      include_suppressed = FALSE
    )
  )
}

#' Extract genus name from organism name
#' @description Extracts the first word (genus) from binomial/trinomial names
#' @param organism_names Character vector of organism names
#' @return Character vector of genus names
#' @keywords internal
.extract_genus_names <- function(organism_names) {
  stringi::stri_replace_all_regex(
    str = organism_names,
    pattern = " .*",
    replacement = "",
    vectorize_all = FALSE
  )
}

#' Build taxonomy lineage data frame
#' @description Constructs taxonomy lineage from OTT taxon info
#' @param taxon_info List of taxonomy information from rotl
#' @param taxon_lineage List of lineage information from rotl
#' @param ott_ids Integer vector of OTT IDs
#' @return Data frame with complete taxonomy lineage
#' @keywords internal
.build_taxonomy_lineage <- function(taxon_info, taxon_lineage, ott_ids) {
  list_df <- seq_along(taxon_lineage) |>
    purrr::map(
      # TODO
      .f = function(x) {
        tidytable::bind_rows(
          data.frame(
            id = ott_ids[x],
            rank = taxon_info[[x]]$rank,
            name = taxon_info[[x]]$name,
            unique_name = taxon_info[[x]]$unique_name,
            ott_id = as.character(taxon_info[[x]]$ott_id)
          ),
          data.frame(id = ott_ids[x], taxon_lineage[[x]])
        )
      }
    )

  tidytable::bind_rows(list_df) |>
    tidytable::mutate(ott_id = as.integer(ott_id)) |>
    # Sort by reverse row number for proper hierarchy
    tidytable::mutate(n = tidytable::row_number()) |>
    tidytable::arrange(tidytable::desc(x = n)) |>
    tidytable::select(-n)
}

#' Create empty taxonomy template
#' @description Returns empty data frames for when API is unavailable
#' @return Named list with empty matches and taxonomy data frames
#' @keywords internal
.create_empty_taxonomy_template <- function() {
  empty_matches <- data.frame(
    search_string = NA_character_,
    ott_id = NA_integer_,
    unique_name = NA_character_,
    stringsAsFactors = FALSE
  )

  empty_taxonomy <- data.frame(
    id = NA_integer_,
    rank = NA_character_,
    name = NA_character_,
    unique_name = NA_character_,
    ott_id = NA_integer_,
    stringsAsFactors = FALSE
  )

  list(matches = empty_matches, taxonomy = empty_taxonomy)
}

# Main Function ----

#' @title Get organism taxonomy (Open Tree of Life Taxonomy)
#'
#' @description This function retrieves taxonomic information from the Open Tree
#'     of Life (OTT) taxonomy service. It cleans organism names, queries the OTT
#'     API, and returns structured taxonomic data including OTT IDs and hierarchical
#'     classifications.
#'
#' @param df Data frame containing organism names in a column named "organism"
#' @param url Character string URL of the OTT API endpoint (default: production API,
#'     can be changed for testing)
#' @param retry Logical indicating whether to retry failed queries using only the
#'     generic epithet (genus name) when full species names fail (default: TRUE)
#'
#' @return Data frame with taxonomic information including OTT IDs, ranks, and
#'     taxonomic hierarchy. Returns empty template if API is unavailable.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Single organism
#' df <- data.frame(organism = "Homo sapiens")
#' taxonomy <- get_organism_taxonomy_ott(df)
#'
#' # Multiple organisms
#' df <- data.frame(organism = c("Homo sapiens", "Arabidopsis thaliana"))
#' taxonomy <- get_organism_taxonomy_ott(df)
#' }
get_organism_taxonomy_ott <- function(
  df,
  url = "https://api.opentreeoflife.org/v3/taxonomy/about",
  retry = TRUE
) {
  # Validate inputs
  if (missing(df) || !is.data.frame(df)) {
    stop("Input 'df' must be a data frame")
  }

  if (!"organism" %in% names(df)) {
    stop("Data frame must contain an 'organism' column")
  }

  n_organisms <- nrow(df)
  logger::log_info(
    "Processing {n_organisms} organism name(s) for OTT taxonomy lookup"
  )

  # Clean and prepare organism names
  organism_table <- df |>
    tidytable::as_tidytable() |>
    tidytable::mutate(organism = trimws(organism)) |>
    # Remove hybrid indicators (x and ×)
    tidytable::mutate(
      organism = stringi::stri_replace_all_fixed(
        str = organism,
        pattern = " x ",
        replacement = " ",
        vectorize_all = FALSE
      )
    ) |>
    tidytable::mutate(
      organism = stringi::stri_replace_all_fixed(
        str = organism,
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
  n_unique <- length(organisms)

  logger::log_debug("Cleaned to {n_unique} unique organism name(s)")

  # Test OTT API availability
  logger::log_debug("Testing Open Tree of Life API availability")

  api_status <- tryCatch(
    {
      url |>
        httr2::request() |>
        # See https://github.com/ropensci/rotl/issues/147
        httr2::req_options(ssl_verifypeer = FALSE) |>
        httr2::req_method(method = "POST") |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform() |>
        httr2::resp_status_desc()
    },
    error = function(e) {
      logger::log_error("Failed to connect to OTT API: {conditionMessage(e)}")
      "ERROR"
    }
  )

  # Handle API unavailability
  if (api_status != "OK") {
    logger::log_error(
      "Open Tree of Life API is unavailable (status: {api_status})"
    )
    logger::log_warn(
      "Returning empty taxonomy template due to API unavailability"
    )
    return(.create_empty_taxonomy_template())
  }

  logger::log_debug("OTT API is available, proceeding with taxonomy queries")

  # Split into smaller batches to avoid API limits
  batch_size <- 100L
  organism_batches <- .create_batches(organisms, batch_size)

  logger::log_info("Querying OTT API in {length(organism_batches)} batches")

  # Query OTT API for each batch
  taxonomy_matches <- organism_batches |>
    purrr::map(.f = .query_ott_batch)

  logger::log_debug("Initial taxonomy queries completed")

  # Combine batch results
  new_matched_otl_exact <- taxonomy_matches |>
    tidytable::bind_rows() |>
    tidytable::mutate(tidytable::across(
      .cols = tidyselect::where(fn = is.logical),
      .fns = as.character
    ))

  new_ott_id <- new_matched_otl_exact |>
    tidytable::filter(!is.na(ott_id)) |>
    tidytable::distinct(ott_id)

  # Retry failed queries with genus-only names if requested
  if (nrow(new_matched_otl_exact) != nrow(new_ott_id) && retry == TRUE) {
    logger::log_info("Retrying failed queries using genus names only")

    # Keep successfully matched results
    successful_matches <- new_matched_otl_exact |>
      tidytable::filter(!is.na(ott_id))

    successful_ott_ids <- successful_matches |>
      tidytable::distinct(ott_id)

    # Identify failed organism names
    failed_organisms <- organism_table |>
      tidytable::filter(
        !search_string %in% successful_matches$search_string
      ) |>
      tidytable::mutate(
        search_string = .extract_genus_names(search_string)
      )

    # Extract unique genus names from failures
    genus_names <- unique(failed_organisms$search_string)

    logger::log_info(
      "Retrying with {length(genus_names)} genus names: ",
      "{paste(head(genus_names, 5), collapse = ', ')}",
      "{ifelse(length(genus_names) > 5, '...', '')}"
    )

    # Query OTT API with genus names only
    batch_size <- 100L
    genus_batches <- .create_batches(genus_names, batch_size)

    retry_matches <- genus_batches |>
      purrr::map(.f = .query_ott_batch) |>
      tidytable::bind_rows() |>
      tidytable::filter(!is.na(ott_id)) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(fn = is.logical),
        .fns = as.character
      ))

    retry_ott_ids <- retry_matches |>
      tidytable::distinct(ott_id)

    # Combine original and retry results
    new_ott_id <- tidytable::bind_rows(successful_ott_ids, retry_ott_ids)
    new_matched_otl_exact <- tidytable::bind_rows(
      new_matched_otl_exact,
      retry_matches
    )

    logger::log_debug(
      "Retry complete: {nrow(retry_ott_ids)} additional matches found"
    )
  }

  # Retrieve detailed taxonomy information
  if (nrow(new_ott_id) != 0) {
    ott_ids <- new_ott_id$ott_id

    logger::log_info(
      "Retrieving detailed taxonomy for {length(ott_ids)} unique OTT IDs"
    )

    # Query OTT API for detailed taxonomic information
    # SSL verification disabled due to rotl package issue #147
    taxon_info <- httr::with_config(
      httr::config(ssl_verifypeer = FALSE),
      rotl::taxonomy_taxon_info(
        ott_ids = ott_ids,
        include_lineage = TRUE,
        include_terminal_descendants = TRUE
      )
    )

    logger::log_debug("Taxonomy information retrieved successfully")

    # Extract lineage information
    taxon_lineage <- taxon_info |>
      rotl::tax_lineage()

    # Build complete taxonomy lineage data frame
    otl <- .build_taxonomy_lineage(taxon_info, taxon_lineage, ott_ids)
  } else {
    logger::log_warn("No OTT IDs found, returning empty dataframe")
    empty_template <- .create_empty_taxonomy_template()
    otl <- empty_template$taxonomy
  }

  # Join organism names with taxonomy data
  biological_metadata <- tidytable::left_join(
    organism_table,
    new_matched_otl_exact
  ) |>
    tidytable::left_join(
      otl |>
        tidytable::rename(unique_name.y = unique_name, ott_id.y = ott_id),
      by = c("ott_id" = "id")
    ) |>
    tidytable::filter(
      rank %in%
        c(
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
        organism_taxonomy_01domain = tidyselect::matches(match = "name_domain"),
        organism_taxonomy_02kingdom = tidyselect::matches(
          match = "name_kingdom"
        ),
        organism_taxonomy_03phylum = tidyselect::matches(match = "name_phylum"),
        organism_taxonomy_04class = tidyselect::matches(match = "name_class"),
        organism_taxonomy_05order = tidyselect::matches(match = "name_order"),
        organism_taxonomy_06family = tidyselect::matches(match = "name_family"),
        organism_taxonomy_07tribe = tidyselect::matches(match = "name_tribe"),
        organism_taxonomy_08genus = tidyselect::matches(match = "name_genus"),
        organism_taxonomy_09species = tidyselect::matches(
          match = "name_species"
        ),
        organism_taxonomy_10varietas = tidyselect::matches(
          match = "name_varietas"
        )
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
      tidyfst::setDT(x = biological_metadata)
      tidyfst::set(
        x = biological_metadata,
        i = NULL,
        j = new_cols,
        value = NA_character_
      )
      biological_metadata |>
        tidytable::as_tidytable()
    }
  }

  logger::log_info("Got OTTaxonomy!")
  return(biological_metadata)
}
