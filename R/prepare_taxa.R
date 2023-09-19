#' @title Prepare taxa
#'
#' @description This function performs taxon name preparation
#'    to match the Open Tree of Life taxonomy
#'
#' @details Depending if the features are aligned between samples
#'    originating from various organisms or not,
#'    It can either attribute all features to a single organism,
#'    or attribute them to multiple ones,
#'    according to their relative intensities among the samples.
#'
#' @include clean_collapse.R
#' @include get_organism_taxonomy_ott.R
#'
#' @param input File containing your features intensities
#' @param extension Does your column names contain the file extension?
#'    (MZmine mainly)
#' @param name_features Name of the features column in the features file
#' @param name_filename Name of the file name column in the metadata file
#' @param colname Name of the column containing biological source information
#' @param metadata File containing your metadata including biological source
#' @param top_k Number of organisms to be retained per feature top intensities
#' @param org_tax_ott File containing Open Tree of Life Taxonomy
#' @param output Output file
#' @param taxon If you want to enforce all features to a given taxon,
#'    put its name here.
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_taxa <-
  function(input = get_params(step = "prepare_taxa")$files$features$raw,
           extension = get_params(step = "prepare_taxa")$names$extension,
           name_features = get_params(step = "prepare_taxa")$names$features,
           name_filename = get_params(step = "prepare_taxa")$names$filename,
           colname = get_params(step = "prepare_taxa")$names$taxon,
           metadata = get_params(step = "prepare_taxa")$files$taxa$raw,
           top_k = get_params(step = "prepare_taxa")$organisms$candidates,
           org_tax_ott = get_params(step = "prepare_taxa")$files$libraries$sop$merged$organisms$taxonomies$ott,
           output = get_params(step = "prepare_taxa")$files$taxa$prepared,
           taxon = get_params(step = "prepare_taxa")$organisms$taxon) {
    if (!is.null(taxon)) {
      if (taxon == "") {
        taxon <- NULL
      }
    }
    if (is.null(taxon)) {
      stopifnot("Your metadata file does not exist" = file.exists(metadata))
      stopifnot("Your input file does not exist" = file.exists(input))
    }

    stopifnot(
      "Your top k organisms parameter should be lower or equal to 5" =
        top_k <= 5
    )

    log_debug(x = "Loading feature table")
    feature_table_0 <- tidytable::fread(
      file = input,
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    if (is.null(taxon)) {
      log_debug(x = "Loading metadata table")
      metadata_table <- tidytable::fread(
        file = metadata,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    }

    log_debug(x = "Formatting feature table ...")
    log_debug(x = "... WARNING: requires 'Peak area'
              in columns (MZmine format)")
    log_debug(x = "... WARNING: or 'quant_' in columns (SLAW format)")
    feature_table <- feature_table_0 |>
      tidytable::select(
        tidytable::all_of(c(name_features)),
        tidytable::matches(" Peak area"),
        tidytable::matches("quant_"),
      ) |>
      tidytable::select(-tidytable::matches("quant_peaktable")) |>
      tidyfst::col_rn(var = name_features)
    colnames(feature_table) <- colnames(feature_table) |>
      stringi::stri_replace_all_fixed(
        pattern = " Peak area",
        replacement = "",
        vectorize_all = FALSE
      )
    colnames(feature_table) <- colnames(feature_table) |>
      stringi::stri_replace_all_fixed(
        pattern = "quant_",
        replacement = "",
        vectorize_all = FALSE
      )
    log_debug(x = "... filtering top K intensities per feature")
    top_n <- feature_table |>
      tidyfst::rn_col() |>
      tidytable::pivot_longer(cols = seq_len(ncol(feature_table)) + 1) |>
      tidytable::filter(value != 0) |>
      tidytable::mutate(rank = rank(-as.numeric(value)), .by = c(rowname)) |>
      tidytable::filter(rank <= top_k) |>
      tidytable::arrange(rowname, rank)
    rm(feature_table)

    if (!is.null(taxon)) {
      log_debug(x = "Forcing all features to given organism")
      metadata_table <- data.frame(taxon)
      colnames(metadata_table) <- colname
    }

    log_debug(x = "Preparing organisms names")
    organism_table <- metadata_table |>
      tidytable::filter(!is.na(!!as.name(colname))) |>
      tidytable::distinct(!!as.name(colname)) |>
      tidytable::select(organism = !!as.name(colname)) |>
      tidytable::separate_rows(organism,
        sep = "\\|",
      )

    log_debug(x = "Retrieving already computed Open Tree of Life Taxonomy")
    organism_table_filled <- organism_table |>
      tidytable::left_join(
        tidytable::fread(org_tax_ott,
          na.strings = c("", "NA"),
          colClasses = "character"
        ),
        by = c("organism" = "organism_name")
      )
    rm(organism_table)

    log_debug(x = "Submitting the rest to OTL")
    organism_table_missing <- organism_table_filled |>
      tidytable::filter(is.na(organism_taxonomy_ottid))

    if (nrow(organism_table_missing) != 0) {
      biological_metadata_1 <- organism_table_missing |>
        get_organism_taxonomy_ott()

      log_debug(x = "Joining all results")
      biological_metadata <- organism_table_filled |>
        tidytable::filter(!is.na(organism_taxonomy_ottid)) |>
        tidytable::rename(organism_name = organism) |>
        tidytable::bind_rows(biological_metadata_1)
      rm(biological_metadata_1)
    } else {
      biological_metadata <- organism_table_filled |>
        tidytable::filter(!is.na(organism_taxonomy_ottid)) |>
        tidytable::rename(organism_name = organism)
    }
    rm(organism_table_filled, organism_table_missing)

    if (is.null(taxon)) {
      if (extension == FALSE) {
        log_debug("Removing filename extensions")
        metadata_table <- metadata_table |>
          tidytable::mutate(
            filename = stringi::stri_replace_all_fixed(
              str = filename,
              pattern = ".mzML",
              replacement = "",
              vectorize_all = FALSE
            )
          ) |>
          tidytable::mutate(
            filename = stringi::stri_replace_all_fixed(
              str = filename,
              pattern = ".mzxML",
              replacement = "",
              vectorize_all = FALSE
            )
          )
      }
    }
    log_debug(x = "Joining top K with metadata table")
    if (!is.null(taxon)) {
      metadata_table_joined <- cbind(
        feature_table_0 |>
          tidytable::mutate(feature_id = !!as.name(name_features)),
        biological_metadata |>
          tidytable::select(organismOriginal = organism_name)
      )
    } else {
      metadata_table_joined <-
        tidytable::left_join(top_n,
          metadata_table,
          by = c("name" = name_filename)
        ) |>
        tidytable::select(
          feature_id := rowname,
          organismOriginal = tidytable::all_of(colname),
          tidytable::everything()
        )
    }
    rm(feature_table_0, metadata_table)

    log_debug(x = "Joining with cleaned taxonomy table")
    taxed_features_table <-
      tidytable::left_join(
        metadata_table_joined,
        biological_metadata,
        by = c("organismOriginal" = "organism_name")
      ) |>
      tidytable::distinct() |>
      tidytable::select(
        feature_id,
        sample_organism_01_domain = organism_taxonomy_01domain,
        sample_organism_02_kingdom = organism_taxonomy_02kingdom,
        sample_organism_03_phylum = organism_taxonomy_03phylum,
        sample_organism_04_class = organism_taxonomy_04class,
        sample_organism_05_order = organism_taxonomy_05order,
        sample_organism_06_family = organism_taxonomy_06family,
        sample_organism_07_tribe = organism_taxonomy_07tribe,
        sample_organism_08_genus = organism_taxonomy_08genus,
        sample_organism_09_species = organism_taxonomy_09species,
        sample_organism_10_varietas = organism_taxonomy_10varietas
      ) |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::everything(),
          .fns = as.character
        )
      ) |>
      tidytable::group_by(feature_id) |>
      clean_collapse(
        cols = c(
          "sample_organism_01_domain",
          "sample_organism_02_kingdom",
          "sample_organism_03_phylum",
          "sample_organism_04_class",
          "sample_organism_05_order",
          "sample_organism_06_family",
          "sample_organism_07_tribe",
          "sample_organism_08_genus",
          "sample_organism_09_species",
          "sample_organism_10_varietas"
        )
      )
    rm(biological_metadata, metadata_table_joined)

    ## TODO change to mutate
    taxed_features_table[is.na(taxed_features_table)] <- "ND"

    log_debug(x = "Exporting ...")
    export_params(parameters = get_params(step = "prepare_taxa"), step = "prepare_taxa")
    export_output(x = taxed_features_table, file = output)
    rm(taxed_features_table)
    return(output)
  }
