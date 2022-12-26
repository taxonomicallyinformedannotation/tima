#' @title Prepare taxa
#'
#' @description TODO
#'
#' @param input TODO
#' @param tool TODO
#' @param extension TODO
#' @param colname TODO
#' @param gnps_job_id TODO
#' @param metadata TODO
#' @param top_k TODO
#' @param output TODO
#' @param taxon TODO
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr across all_of arrange bind_rows coalesce distinct
#' @importFrom dplyr everything filter group_by group_cols left_join matches
#' @importFrom dplyr mutate mutate_all mutate_at na_if row_number select
#' @importFrom dplyr setdiff summarise_all ungroup vars
#' @importFrom purrr map_df
#' @importFrom readr read_delim write_delim
#' @importFrom rotl tax_lineage taxonomy_taxon_info tnrs_match_names
#' @importFrom stringr fixed str_length str_remove
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom tidyr gather pivot_wider separate_rows
#'
#' @examples NULL
prepare_taxa <-
  function(input = params$input,
           tool = params$tool,
           extension = params$extension,
           colname = params$column_name,
           gnps_job_id = params$gnps,
           metadata = params$metadata,
           top_k = params$top_k,
           output = params$output,
           taxon = params$taxon) {
    stopifnot("Your tool must be 'gnps', 'manual' or 'ready'" = tool %in% c("gnps", "manual", "ready"))
    if (tool == "gnps") {
      stopifnot("Your GNPS job ID is invalid" = stringr::str_length(string = gnps_job_id) == 32)
    } else {
      if (is.null(taxon)) {
        if (tool == "manual") {
          stopifnot("Your metadata file does not exist" = file.exists(metadata))
        }
        stopifnot("Your input file does not exist" = file.exists(input))
      }
    }

    stopifnot("Your top k organisms parameter should be lower or equal to 5" = top_k <=
      5)

    log_debug(x = "Loading taxa ranks dictionary")
    taxa_ranks_dictionary <-
      readr::read_delim(file = paths$data$source$dictionaries$ranks)

    if (tool == "gnps") {
      log_debug(x = "Loading feature table")
      feature_table <- read_features(id = gnps_job_id)
      if (is.null(taxon)) {
        log_debug(x = "Loading metadata table")
        metadata_table <- read_metadata(id = gnps_job_id)
      }
    }

    if (tool == "manual") {
      log_debug(x = "Loading feature table")
      feature_table <- readr::read_delim(file = input)
      log_debug(x = "Loading metadata table")
      metadata_table <- readr::read_delim(file = metadata)
    }

    if (tool != "ready") {
      log_debug(x = "Formatting feature table ...")
      log_debug(x = "... WARNING: requires 'Peak area' in columns (MZmine format)")
      feature_table <- feature_table |>
        dplyr::select(
          `row ID`,
          dplyr::matches(" Peak area")
        ) |>
        tibble::column_to_rownames(var = "row ID")
      colnames(feature_table) <-
        stringr::str_remove(
          string = colnames(feature_table),
          pattern = stringr::fixed(pattern = " Peak area")
        )
      log_debug(x = "... filtering top K intensities per feature")
      top_n <- feature_table |>
        tibble::rownames_to_column() |>
        tidyr::gather(column, value, -rowname) |>
        dplyr::filter(value != 0) |>
        dplyr::group_by(rowname) |>
        dplyr::mutate(rank = rank(-value)) |>
        dplyr::ungroup() |>
        dplyr::filter(rank <= top_k) |>
        dplyr::arrange(rowname, rank)
    } else {
      metadata_table <-
        readr::read_delim(file = input)
    }

    if (!is.null(taxon)) {
      log_debug(x = "Forcing all features to given organism")
      metadata_table <- data.frame(taxon)
      colnames(metadata_table) <- colname
    }

    log_debug(x = "Keeping list of organisms to submit to OTL")
    organism_table <- metadata_table |>
      dplyr::filter(!is.na(dplyr::all_of(colname))) |>
      dplyr::distinct(dplyr::across(dplyr::all_of(colname))) |>
      dplyr::select(organism = dplyr::all_of(colname)) |>
      tidyr::separate_rows(organism,
        sep = "\\|",
      ) |>
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
      purrr::map_df(rev) |>
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
      purrr::map_df(rev) |>
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

    if (is.null(taxon)) {
      if (extension == FALSE) {
        log_debug("Removing filename extensions")
        metadata_table <- metadata_table |>
          dplyr::mutate(filename = stringr::str_remove(
            string = filename,
            pattern = stringr::fixed(pattern = ".mzML")
          )) |>
          dplyr::mutate(filename = stringr::str_remove(
            string = filename,
            pattern = stringr::fixed(pattern = ".mzxML")
          ))
      }
    }
    log_debug(x = "Joining top K with metadata table")
    if (tool != "ready") {
      if (!is.null(taxon)) {
        metadata_table_joined <- cbind(
          feature_table |> dplyr::mutate(feature_id = dplyr::row_number()),
          biological_metadata |>
            dplyr::select(organismOriginal = organism_name)
        )
      } else {
        metadata_table_joined <-
          dplyr::left_join(top_n, metadata_table, by = c("column" = "filename")) |>
          dplyr::select(
            feature_id := rowname,
            organismOriginal = dplyr::all_of(colname),
            dplyr::everything()
          )
      }
    } else {
      metadata_table_joined <- metadata_table |>
        dplyr::select(feature_id,
          organismOriginal = dplyr::all_of(colname),
          dplyr::everything()
        )
    }

    log_debug(x = "Joining with cleaned taxonomy table")
    metadata_table_joined_summarized <-
      dplyr::left_join(
        metadata_table_joined,
        biological_metadata,
        by = c("organismOriginal" = "organism_name")
      ) |>
      dplyr::distinct() |>
      dplyr::mutate_at(dplyr::vars(-dplyr::group_cols()), as.character) |>
      dplyr::select(
        feature_id,
        sample_organism_01_domain = dplyr::matches("organism_taxonomy_01domain"),
        sample_organism_02_kingdom = dplyr::matches("organism_taxonomy_02kingdom"),
        sample_organism_03_phylum = dplyr::matches("organism_taxonomy_03phylum"),
        sample_organism_04_class = dplyr::matches("organism_taxonomy_04class"),
        sample_organism_05_order = dplyr::matches("organism_taxonomy_05order"),
        sample_organism_06_family = dplyr::matches("organism_taxonomy_06family"),
        sample_organism_07_tribe = dplyr::matches("organism_taxonomy_07tribe"),
        sample_organism_08_genus = dplyr::matches("organism_taxonomy_08genus"),
        sample_organism_09_species = dplyr::matches("organism_taxonomy_09species"),
        sample_organism_10_varietas = dplyr::matches("organism_taxonomy_10varietas")
      ) |>
      dplyr::group_by(feature_id) |>
      dplyr::summarise_all(function(x) {
        x <- list(paste(unique(x[!is.na(x)]), collapse = "|"))
      }) |>
      dplyr::ungroup() |>
      dplyr::mutate_all(as.character) |>
      dplyr::mutate_all(dplyr::na_if, "")

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_taxa")
    export_output(x = metadata_table_joined_summarized, file = output)
  }
