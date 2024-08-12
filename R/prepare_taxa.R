import::from(stringi, stri_replace_all_fixed, .into = environment())
import::from(tidyfst, col_rn, .into = environment())
import::from(tidyfst, rn_col, .into = environment())
import::from(tidytable, across, .into = environment())
import::from(tidytable, all_of, .into = environment())
import::from(tidytable, arrange, .into = environment())
import::from(tidytable, bind_rows, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, everything, .into = environment())
import::from(tidytable, filter, .into = environment())
import::from(tidytable, fread, .into = environment())
import::from(tidytable, group_by, .into = environment())
import::from(tidytable, left_join, .into = environment())
import::from(tidytable, matches, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, pivot_longer, .into = environment())
import::from(tidytable, rename, .into = environment())
import::from(tidytable, replace_na, .into = environment())
import::from(tidytable, select, .into = environment())
import::from(tidytable, separate_rows, .into = environment())
import::from(tidytable, where, .into = environment())

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
#' @importFrom stringi stri_replace_all_fixed
#' @importFrom tidyfst col_rn
#' @importFrom tidyfst rn_col
#' @importFrom tidytable across
#' @importFrom tidytable all_of
#' @importFrom tidytable arrange
#' @importFrom tidytable bind_rows
#' @importFrom tidytable distinct
#' @importFrom tidytable everything
#' @importFrom tidytable filter
#' @importFrom tidytable fread
#' @importFrom tidytable group_by
#' @importFrom tidytable left_join
#' @importFrom tidytable matches
#' @importFrom tidytable mutate
#' @importFrom tidytable pivot_longer
#' @importFrom tidytable rename
#' @importFrom tidytable replace_na
#' @importFrom tidytable select
#' @importFrom tidytable separate_rows
#' @importFrom tidytable where
#'
#' @include clean_collapse.R
#' @include get_params.R
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
#' @return The path to the prepared taxa
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
           metadata = get_params(step = "prepare_taxa")$files$metadata$raw,
           top_k = get_params(step = "prepare_taxa")$organisms$candidates,
           org_tax_ott = get_params(step = "prepare_taxa")$files$libraries$sop$merged$organisms$taxonomies$ott,
           output = get_params(step = "prepare_taxa")$files$metadata$prepared,
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
    feature_table_0 <- fread(
      file = input,
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    if (is.null(taxon)) {
      log_debug(x = "Loading metadata table")
      metadata_table <- fread(
        file = metadata,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    }

    log_debug(x = "Formatting feature table ...")
    log_debug(x = "... requires 'Peak area'
              in columns (MZmine format)")
    log_debug(x = "... or 'quant_' in columns (SLAW format)")
    feature_table <- feature_table_0 |>
      select(all_of(c(name_features)), matches(" Peak area"), matches("quant_"), ) |>
      select(-matches("quant_peaktable")) |>
      col_rn(var = name_features)
    colnames(feature_table) <- colnames(feature_table) |>
      stri_replace_all_fixed(
        pattern = " Peak area",
        replacement = "",
        vectorize_all = FALSE
      )
    colnames(feature_table) <- colnames(feature_table) |>
      stri_replace_all_fixed(
        pattern = "quant_",
        replacement = "",
        vectorize_all = FALSE
      )
    log_debug(x = "... filtering top K intensities per feature")
    top_n <- feature_table |>
      rn_col() |>
      pivot_longer(cols = seq_len(ncol(feature_table)) + 1) |>
      filter(value != 0) |>
      mutate(rank = rank(-as.numeric(value)), .by = c(rowname)) |>
      filter(rank <= top_k) |>
      arrange(rowname, rank)
    rm(feature_table)

    if (!is.null(taxon)) {
      log_debug(x = "Forcing all features to given organism")
      metadata_table <- data.frame(taxon)
      colnames(metadata_table) <- colname
    }

    log_debug(x = "Preparing organisms names")
    organism_table <- metadata_table |>
      filter(!is.na(!!as.name(colname))) |>
      distinct(!!as.name(colname)) |>
      select(organism = !!as.name(colname)) |>
      separate_rows(organism, sep = "\\|", )

    log_debug(x = "Retrieving already computed Open Tree of Life Taxonomy")
    organism_table_filled <- organism_table |>
      left_join(
        fread(
          org_tax_ott,
          na.strings = c("", "NA"),
          colClasses = "character"
        ),
        by = c("organism" = "organism_name")
      )
    rm(organism_table)

    log_debug(x = "Submitting the rest to OTL")
    organism_table_missing <- organism_table_filled |>
      filter(is.na(organism_taxonomy_ottid))

    if (nrow(organism_table_missing) != 0) {
      biological_metadata_1 <- organism_table_missing |>
        get_organism_taxonomy_ott()

      log_debug(x = "Joining all results")
      biological_metadata <- organism_table_filled |>
        filter(!is.na(organism_taxonomy_ottid)) |>
        rename(organism_name = organism) |>
        bind_rows(biological_metadata_1)
      rm(biological_metadata_1)
    } else {
      biological_metadata <- organism_table_filled |>
        filter(!is.na(organism_taxonomy_ottid)) |>
        rename(organism_name = organism)
    }
    rm(organism_table_filled, organism_table_missing)

    if (is.null(taxon)) {
      if (extension == FALSE) {
        log_debug("Removing filename extensions")
        metadata_table <- metadata_table |>
          mutate(
            filename = stri_replace_all_fixed(
              str = filename,
              pattern = ".mzML",
              replacement = "",
              vectorize_all = FALSE
            )
          ) |>
          mutate(
            filename = stri_replace_all_fixed(
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
          mutate(feature_id = !!as.name(name_features)),
        biological_metadata |>
          select(organismOriginal = organism_name)
      )
    } else {
      metadata_table_joined <-
        left_join(top_n, metadata_table, by = c("name" = name_filename)) |>
        select(feature_id := rowname,
          organismOriginal = all_of(colname),
          everything()
        )
    }
    rm(feature_table_0, metadata_table)

    log_debug(x = "Joining with cleaned taxonomy table")
    taxed_features_table <-
      left_join(
        metadata_table_joined,
        biological_metadata,
        by = c("organismOriginal" = "organism_name")
      ) |>
      distinct() |>
      select(
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
      mutate(across(.cols = everything(), .fns = as.character)) |>
      group_by(feature_id) |>
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
      ) |>
      mutate(across(
        .cols = where(is.character),
        .fns = function(x) {
          replace_na(x, "ND")
        }
      ))
    rm(biological_metadata, metadata_table_joined)

    tryCatch(expr = {
      export_params(
        parameters = get_params(step = "prepare_taxa"),
        step = "prepare_taxa"
      )
    }, error = function(e) {})
    export_output(x = taxed_features_table, file = output)
    rm(taxed_features_table)
    return(output)
  }
