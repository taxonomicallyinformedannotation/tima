utils::globalVariables(
  c(
    "column",
    "feature_id",
    "filename",
    "organism",
    "organism_name",
    "organism_taxonomy_ottid",
    "rowname",
    "value"
  )
)

#' @title Prepare taxa
#'
#' @description This function performs taxon name preparation to match the Open Tree of Life taxonomy
#'
#' @details Depending if the features are aligned between samples originating from various organisms or not,
#'    It can either attribute all features to a single organism, or attribute them to multiple ones,
#'    according to their relative intensities among the samples.
#'
#' @param input File containing your features intensities
#' @param extension Does your column names contain the file extension? (MZmine mainly)
#' @param name_features Name of the features column in the features data
#' @param colname Name of the column containing biological source information
#' @param metadata File containing your metadata including biological source
#' @param top_k Number of organisms to be retained per feature top intensities
#' @param org_tax_ott File containing Open Tree of Life Taxonomy
#' @param output Output file
#' @param taxon If you want to enforce all features to a given taxon, put its name here.
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_taxa <-
  function(input = params$files$features$raw,
           extension = params$names$extension,
           name_features = params$names$features,
           colname = params$names$taxon,
           metadata = params$files$taxa$raw,
           top_k = params$organisms$candidates,
           org_tax_ott = params$files$libraries$sop$merged$organisms$taxonomies$ott,
           output = params$files$taxa$prepared,
           taxon = params$organisms$taxon,
           parameters = params) {
    if (!is.null(taxon)) {
      if (taxon == "") {
        taxon <- NULL
      }
    }
    if (is.null(taxon)) {
      stopifnot("Your metadata file does not exist" = file.exists(metadata))
      stopifnot("Your input file does not exist" = file.exists(input))
    }

    stopifnot("Your top k organisms parameter should be lower or equal to 5" = top_k <=
      5)

    params <<- parameters

    log_debug(x = "Loading feature table")
    feature_table <- readr::read_delim(file = input)
    log_debug(x = "Loading metadata table")
    metadata_table <- readr::read_delim(file = metadata)

    log_debug(x = "Formatting feature table ...")
    log_debug(x = "... WARNING: requires 'Peak area' in columns (MZmine format)")
    log_debug(x = "... WARNING: or 'quant_' in columns (SLAW format)")
    feature_table <- feature_table |>
      dplyr::select(
        dplyr::all_of(name_features),
        dplyr::matches(" Peak area"),
        dplyr::matches("quant_"),
      ) |>
      dplyr::select(-dplyr::matches("quant_peaktable")) |>
      tibble::column_to_rownames(var = name_features)
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

    if (!is.null(taxon)) {
      log_debug(x = "Forcing all features to given organism")
      metadata_table <- data.frame(taxon)
      colnames(metadata_table) <- colname
    }

    log_debug(x = "Preparing organisms names")
    organism_table <- metadata_table |>
      dplyr::filter(!is.na(!!as.name(colname))) |>
      dplyr::distinct(!!as.name(colname)) |>
      dplyr::select(organism = !!as.name(colname)) |>
      tidyr::separate_rows(organism,
        sep = "\\|",
      )

    log_debug(x = "Retrieving already computed Open Tree of Life Taxonomy")
    organism_table_filled <- organism_table |>
      dplyr::left_join(readr::read_delim(org_tax_ott),
        by = c("organism" = "organism_name")
      )

    log_debug(x = "Submitting the rest to OTL")
    organism_table_missing <- organism_table_filled |>
      dplyr::filter(is.na(organism_taxonomy_ottid))

    if (nrow(organism_table_missing) != 0) {
      biological_metadata_1 <- organism_table_missing |>
        get_organism_taxonomy_ott()

      log_debug(x = "Joining all results")
      biological_metadata <- organism_table_filled |>
        dplyr::filter(!is.na(organism_taxonomy_ottid)) |>
        dplyr::rename(organism_name = organism) |>
        dplyr::bind_rows(biological_metadata_1)
    } else {
      biological_metadata <- organism_table_filled |>
        dplyr::filter(!is.na(organism_taxonomy_ottid)) |>
        dplyr::rename(organism_name = organism)
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
    if (!is.null(taxon)) {
      metadata_table_joined <- cbind(
        feature_table |>
          dplyr::mutate(feature_id = dplyr::row_number()),
        biological_metadata |>
          dplyr::select(organismOriginal = organism_name)
      )
    } else {
      metadata_table_joined <-
        dplyr::left_join(top_n, metadata_table, by = c("column" = "filename")) |>
        dplyr::select(feature_id := rowname,
          organismOriginal = dplyr::all_of(colname),
          dplyr::everything()
        )
    }

    log_debug(x = "Joining with cleaned taxonomy table")
    taxed_features_table <-
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
        x <- list(paste(unique(x[!is.na(x)]), collapse = " $ "))
      }) |>
      dplyr::ungroup() |>
      dplyr::mutate_all(as.character) |>
      dplyr::mutate_all(dplyr::na_if, "")

    taxed_features_table[is.na(taxed_features_table)] <- "ND"

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_taxa")
    export_output(x = taxed_features_table, file = output)

    return(output)
  }
