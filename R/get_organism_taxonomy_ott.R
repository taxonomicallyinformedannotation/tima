#' Create batches from a vector
#' @description Splits a vector into batches of specified size
#' @include logs_utils.R
#' @param items [character] Vector to batch
#' @param batch_size [integer] Size of each batch
#' @return List of character vectors (batches)
#' @keywords internal
.create_batches <- function(items, batch_size) {
  if (length(items) == 0L) {
    return(list())
  }

  lapply(
    X = seq(1L, length(items), batch_size),
    # Split items into batches, removing NA values
    FUN = function(i) {
      end_idx <- min(i + batch_size - 1L, length(items))
      chunk <- items[i:end_idx]
      chunk[!is.na(chunk)]
    }
  )
}

#' Query OTT API for a single batch
#' @description Queries the Open Tree of Life API for taxonomic names.
#' @param batch [character] Vector of organism names
#' @return Data frame with taxonomy matches
#' @keywords internal
.query_ott_batch <- function(batch) {
  rotl::tnrs_match_names(
    names = batch,
    do_approximate_matching = FALSE,
    include_suppressed = FALSE
  )
}

#' Extract genus name from organism name
#' @description Extracts the first word (genus) from binomial/trinomial names
#' @param organism_names [character] Vector of organism names
#' @return Character vector of genus names
#' @keywords internal
.extract_genus_names <- function(organism_names) {
  stri_replace_all_regex(
    str = organism_names,
    pattern = " .*",
    replacement = "",
    vectorize_all = FALSE
  )
}

#' Build taxonomy lineage data frame
#' @description Constructs taxonomy lineage from OTT taxon info
#' @param taxon_info [list] List of taxonomy information from rotl
#' @param taxon_lineage [list] List of lineage information from rotl
#' @param ott_ids [integer] Vector of OTT IDs
#' @return Data frame with complete taxonomy lineage
#' @keywords internal
.build_taxonomy_lineage <- function(taxon_info, taxon_lineage, ott_ids) {
  n_taxa <- length(taxon_lineage)
  if (n_taxa == 0L) {
    return(tidytable::tidytable(
      id = integer(),
      rank = character(),
      name = character(),
      unique_name = character(),
      ott_id = integer()
    ))
  }

  std_cols <- c("rank", "name", "unique_name", "ott_id")
  all_std <- all(vapply(
    taxon_lineage,
    function(tl) is.data.frame(tl) && identical(names(tl), std_cols),
    logical(1L)
  ))
  if (all_std) {
    lin_n <- vapply(taxon_lineage, nrow, integer(1L))
    tot_per_taxon <- 1L + lin_n
    total_rows <- sum(tot_per_taxon)

    id_col <- rep.int(ott_ids[seq_len(n_taxa)], tot_per_taxon)
    rank_parts <- vector("list", n_taxa * 2L)
    name_parts <- vector("list", n_taxa * 2L)
    uname_parts <- vector("list", n_taxa * 2L)
    ott_parts <- vector("list", n_taxa * 2L)

    p1 <- 2L * seq_len(n_taxa) - 1L
    p2 <- 2L * seq_len(n_taxa)
    rank_parts[p1] <- lapply(taxon_info, function(ti) as.character(ti$rank))
    name_parts[p1] <- lapply(taxon_info, function(ti) as.character(ti$name))
    uname_parts[p1] <- lapply(
      taxon_info,
      function(ti) as.character(ti$unique_name)
    )
    ott_parts[p1] <- lapply(taxon_info, function(ti) as.integer(ti$ott_id))

    rank_parts[p2] <- lapply(taxon_lineage, function(tl) as.character(tl$rank))
    name_parts[p2] <- lapply(taxon_lineage, function(tl) as.character(tl$name))
    uname_parts[p2] <- lapply(
      taxon_lineage,
      function(tl) as.character(tl$unique_name)
    )
    ott_parts[p2] <- lapply(taxon_lineage, function(tl) as.integer(tl$ott_id))

    rev_idx <- total_rows:1L
    return(tidytable::tidytable(
      id = id_col[rev_idx],
      rank = unlist(rank_parts, use.names = FALSE)[rev_idx],
      name = unlist(name_parts, use.names = FALSE)[rev_idx],
      unique_name = unlist(uname_parts, use.names = FALSE)[rev_idx],
      ott_id = unlist(ott_parts, use.names = FALSE)[rev_idx]
    ))
  }

  # Fallback when lineage data frames have non-standard columns
  .combine_single_taxon <- function(idx, taxon_info, taxon_lineage, ott_ids) {
    tidytable::bind_rows(
      tidytable::tidytable(
        id = ott_ids[idx],
        rank = taxon_info[[idx]]$rank,
        name = taxon_info[[idx]]$name,
        unique_name = taxon_info[[idx]]$unique_name,
        ott_id = as.character(taxon_info[[idx]]$ott_id)
      ),
      tidytable::as_tidytable(
        data.frame(id = ott_ids[idx], taxon_lineage[[idx]])
      )
    )
  }

  list_df <- lapply(
    X = seq_along(taxon_lineage),
    FUN = .combine_single_taxon,
    taxon_info = taxon_info,
    taxon_lineage = taxon_lineage,
    ott_ids = ott_ids
  )

  out <- tidytable::bind_rows(list_df)
  out$ott_id <- as.integer(out$ott_id)
  if (nrow(out) > 1L) {
    out <- out[rev(seq_len(nrow(out))), ]
  }
  out
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

#' Empty organism taxonomy table
#' @description Same columns as a successful get_organism_taxonomy_ott()
#'     result, with no rows. Returned when the API is unavailable so that
#'     callers can bind it like any other result.
#' @return A tidytable with 0 rows
#' @keywords internal
.empty_organism_taxonomy_table <- function() {
  cols <- c(
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
  tidytable::as_tidytable(
    stats::setNames(rep(list(character()), length(cols)), cols)
  )
}


#' @title Get organism taxonomy (Open Tree of Life Taxonomy)
#'
#' @description This function retrieves taxonomic information from the Open Tree
#'     of Life (OTT) taxonomy service. It cleans organism names, queries the OTT
#' API, and returns structured taxonomic data including OTT IDs and hierarchical
#'     classifications.
#'
#' @param df [data.frame] Data frame containing organism names in a column named
#'     "organism"
#' @param url [character] URL of the OTT API endpoint (default:
#'     production API,
#'     can be changed for testing)
#' @param retry [logical] Whether to retry failed queries
#'     using only the
#'     generic epithet (genus name) when full species names fail (default: TRUE)
#'
#' @return Data frame with taxonomic information including OTT IDs, ranks, and
#'     taxonomic hierarchy. If the API is unavailable, warns
#'     (`tima_ott_unavailable_warning`) and returns the same columns with no
#'     rows.
#'
#' @family data-retrieval
#' @concept EDAM:operation_3460 Taxonomic classification
#' @concept EDAM:data_3028 Taxonomy
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
  if (missing(df) || !is.data.frame(df)) {
    cli::cli_abort(
      "input 'df' must be a data frame",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  if (!"organism" %in% names(df)) {
    cli::cli_abort(
      "data frame must contain an 'organism' column",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  n_organisms <- nrow(df)
  log_info(
    "Processing %d organism name(s) for OTT taxonomy lookup",
    n_organisms
  )

  # Clean and prepare organism names on unique values
  u_org <- unique(df$organism)
  u_org <- u_org[!is.na(u_org)]
  if (length(u_org) == 0L) {
    return(.empty_organism_taxonomy_table())
  }
  u_org <- trimws(as.character(u_org))
  # Remove hybrid indicators (x and ×)
  u_org <- stri_replace_all_fixed(
    str = u_org,
    pattern = " x ",
    replacement = " ",
    vectorize_all = FALSE
  )
  u_org <- stri_replace_all_fixed(
    str = u_org,
    pattern = "\u00D7 ",
    replacement = "",
    vectorize_all = FALSE
  )
  u_org <- unique(u_org)
  search_str <- tolower(u_org)
  organism_table <- tidytable::tidytable(
    canonical_name = u_org,
    search_string = search_str
  )

  organisms <- organism_table$canonical_name
  n_unique <- length(organisms)

  log_debug("Cleaned to %d unique organism name(s)", n_unique)

  # Test OTT API availability
  log_debug("Testing Open Tree of Life API availability")

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
      log_error("Failed to connect to OTT API: %s", conditionMessage(e))
      "ERROR"
    }
  )

  # Handle API unavailability
  if (api_status != "OK") {
    log_error(
      "Open Tree of Life API is unavailable (status: %s)",
      api_status
    )
    cli::cli_warn(
      c(
        "Open Tree of Life API is unavailable",
        "i" = "{length(organism_table$canonical_name)} organism(s) left without taxonomy"
      ),
      class = c("tima_ott_unavailable_warning", "tima_warning")
    )
    return(.empty_organism_taxonomy_table())
  }

  log_debug("OTT API is available, proceeding with taxonomy queries")

  # Split into smaller batches to avoid API limits
  batch_size <- 100L
  organism_batches <- .create_batches(organisms, batch_size)

  log_info("Querying OTT API in %d batches", length(organism_batches))

  # Query OTT API for each batch
  taxonomy_matches <- lapply(organism_batches, .query_ott_batch)

  log_debug("Initial taxonomy queries completed")

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

  # Retry failed queries with genus-only names if requested. Only names
  # without an OTT id failed; several names may share one id (synonyms)
  failed_names <- new_matched_otl_exact$search_string[
    is.na(new_matched_otl_exact$ott_id)
  ]
  if (retry && length(failed_names) > 0L) {
    log_info("Retrying failed queries using genus names only")

    # Keep successfully matched results
    successful_matches <- new_matched_otl_exact |>
      tidytable::filter(!is.na(ott_id))

    successful_ott_ids <- successful_matches |>
      tidytable::distinct(ott_id)

    # Identify failed organism names, keeping their full search string so
    # genus matches can be linked back to them
    failed_organisms <- organism_table |>
      tidytable::filter(
        !search_string %in% successful_matches$search_string
      ) |>
      tidytable::mutate(
        genus_string = .extract_genus_names(search_string)
      )

    genus_names <- unique(failed_organisms$genus_string)

    log_info(
      "Retrying with %d genus names: %s %s",
      length(genus_names),
      paste(utils::head(x = genus_names, n = 5L), collapse = ", "),
      ifelse(test = length(genus_names) > 5, yes = "...", no = "")
    )

    # Query OTT API with genus names only
    batch_size <- 100L
    genus_batches <- .create_batches(genus_names, batch_size)

    retry_matches <- lapply(genus_batches, .query_ott_batch) |>
      tidytable::bind_rows() |>
      tidytable::filter(!is.na(ott_id)) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(fn = is.logical),
        .fns = as.character
      ))

    retry_ott_ids <- retry_matches |>
      tidytable::distinct(ott_id)

    # Attach each genus match to the organisms it was retried for
    retry_linked <- failed_organisms |>
      tidytable::select(tidyselect::all_of(c(
        "search_string",
        "genus_string"
      ))) |>
      tidytable::inner_join(
        retry_matches |>
          tidytable::rename(genus_string = search_string),
        by = "genus_string"
      ) |>
      tidytable::select(-tidyselect::all_of("genus_string"))

    # Combine original and retry results
    new_ott_id <- tidytable::bind_rows(successful_ott_ids, retry_ott_ids) |>
      tidytable::distinct()
    new_matched_otl_exact <- tidytable::bind_rows(
      successful_matches,
      retry_linked
    )

    log_debug(
      "Retry complete: %d additional matches found",
      nrow(retry_ott_ids)
    )
  }

  # Retrieve detailed taxonomy information
  if (nrow(new_ott_id) != 0) {
    ott_ids <- new_ott_id$ott_id

    log_info(
      "Retrieving detailed taxonomy for %d unique OTT IDs",
      length(ott_ids)
    )

    # Query OTT API for detailed taxonomic information
    # SSL verification disabled due to rotl package issue #147
    # httr is an indirect dependency via rotl, so always available here
    taxon_info <- rotl::taxonomy_taxon_info(
      ott_ids = ott_ids,
      include_lineage = TRUE,
      include_terminal_descendants = TRUE
    )

    log_debug("Taxonomy information retrieved successfully")

    taxon_lineage <- taxon_info |>
      rotl::tax_lineage()

    otl <- .build_taxonomy_lineage(taxon_info, taxon_lineage, ott_ids)
  } else {
    log_warn("No OTT IDs found, returning empty dataframe")
    empty_template <- .create_empty_taxonomy_template()
    otl <- empty_template$taxonomy
  }

  # Join organism names with taxonomy data
  valid_ranks <- c(
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
  keep_otl <- !is.na(otl$rank) &
    fastmatch::fmatch(otl$rank, valid_ranks, nomatch = 0L) > 0L &
    !is.na(otl$id)
  if (!any(keep_otl)) {
    return(.empty_organism_taxonomy_table())
  }
  otl_sub_id <- otl$id[keep_otl]
  otl_sub_rank <- otl$rank[keep_otl]
  otl_sub_name <- as.character(otl$name[keep_otl])

  valid_ott_ids <- unique(otl_sub_id)
  keep_m <- !is.na(new_matched_otl_exact$ott_id) &
    fastmatch::fmatch(
      new_matched_otl_exact$ott_id,
      valid_ott_ids,
      nomatch = 0L
    ) >
      0L
  m_ss <- new_matched_otl_exact$search_string[keep_m]
  m_ott <- new_matched_otl_exact$ott_id[keep_m]

  joined_orgs <- tidytable::inner_join(
    organism_table,
    tidytable::tidytable(search_string = m_ss, ott_id = m_ott),
    by = "search_string"
  )
  if (nrow(joined_orgs) == 0L) {
    return(.empty_organism_taxonomy_table())
  }
  cn_vec <- joined_orgs$canonical_name
  ott_vec <- joined_orgs$ott_id

  ord_org <- order(cn_vec, ott_vec)
  cn_vec <- cn_vec[ord_org]
  ott_vec <- ott_vec[ord_org]
  if (length(cn_vec) > 1L) {
    not_dup <- c(
      TRUE,
      cn_vec[-1L] != cn_vec[-length(cn_vec)] |
        ott_vec[-1L] != ott_vec[-length(ott_vec)]
    )
    if (!all(not_dup)) {
      cn_vec <- cn_vec[not_dup]
      ott_vec <- ott_vec[not_dup]
    }
  }

  target_ranks <- c(
    "organism_taxonomy_01domain" = "domain",
    "organism_taxonomy_02kingdom" = "kingdom",
    "organism_taxonomy_03phylum" = "phylum",
    "organism_taxonomy_04class" = "class",
    "organism_taxonomy_05order" = "order",
    "organism_taxonomy_06family" = "family",
    "organism_taxonomy_07tribe" = "tribe",
    "organism_taxonomy_08genus" = "genus",
    "organism_taxonomy_09species" = "species",
    "organism_taxonomy_10varietas" = "varietas"
  )

  n_out <- length(cn_vec)
  out_list <- vector("list", 12L)
  names(out_list) <- c(
    "organism_name",
    "organism_taxonomy_ottid",
    names(target_ranks)
  )
  out_list[["organism_name"]] <- cn_vec
  out_list[["organism_taxonomy_ottid"]] <- ott_vec

  for (col_nm in names(target_ranks)) {
    rk <- target_ranks[[col_nm]]
    rk_idx <- which(otl_sub_rank == rk)
    if (length(rk_idx) == 0L) {
      out_list[[col_nm]] <- rep.int(NA_character_, n_out)
    } else {
      rk_ids <- otl_sub_id[rk_idx]
      rk_names <- otl_sub_name[rk_idx]
      first_rk <- !duplicated(rk_ids)
      m_id <- fastmatch::fmatch(ott_vec, rk_ids[first_rk])
      out_list[[col_nm]] <- rk_names[first_rk][m_id]
    }
  }
  biological_metadata <- tidytable::as_tidytable(out_list)

  log_info("Got OTTaxonomy!")
  biological_metadata
}
