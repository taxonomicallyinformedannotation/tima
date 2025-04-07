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
#' @include get_params.R
#' @include get_organism_taxonomy_ott.R
#'
#' @param input File containing your features intensities
#' @param extension Does your column names contain the file extension?
#'    (mzmine mainly)
#' @param name_filename Name of the file name column in the metadata file
#' @param colname Name of the column containing biological source information
#' @param metadata File containing your metadata including biological source
#' @param org_tax_ott File containing Open Tree of Life Taxonomy
#' @param output Output file
#' @param taxon If you want to enforce all features to a given taxon,
#'    put its name here.
#'
#' @return The path to the prepared taxa
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' dir <- paste0(github, repo)
#' org_tax_ott <- paste0(
#'   "data/interim/libraries/",
#'   "sop/merged/organisms/taxonomies/ott.tsv"
#' )
#' get_file(url = paste0(dir, org_tax_ott), export = org_tax_ott)
#' get_file(
#'   url = paste0(dir, "data/interim/features/example_features.tsv),
#'   export = get_params(step = "prepare_taxa")$files$features$prepared
#' )
#' prepare_taxa(
#'   taxon = "Homo sapiens",
#'   org_tax_ott = org_tax_ott
#' )
#' unlink("data", recursive = TRUE)
#' }
prepare_taxa <-
  function(
    input = get_params(step = "prepare_taxa")$files$features$prepared,
    extension = get_params(step = "prepare_taxa")$names$extension,
    name_filename = get_params(step = "prepare_taxa")$names$filename,
    colname = get_params(step = "prepare_taxa")$names$taxon,
    metadata = get_params(step = "prepare_taxa")$files$metadata$raw,
    org_tax_ott = get_params(
      step = "prepare_taxa"
    )$files$libraries$sop$merged$organisms$taxonomies$ott,
    output = get_params(step = "prepare_taxa")$files$metadata$prepared,
    taxon = get_params(step = "prepare_taxa")$organisms$taxon
  ) {
    if (!is.null(taxon)) {
      if (taxon == "") {
        taxon <- NULL
      }
    }
    if (is.null(taxon)) {
      stopifnot("Your metadata file does not exist" = file.exists(metadata))
      stopifnot("Your input file does not exist" = file.exists(input))
    }

    logger::log_trace("Loading feature table")
    feature_table_0 <- tidytable::fread(
      file = input,
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    if (is.null(taxon)) {
      logger::log_trace("Loading metadata table")
      metadata_table <- tidytable::fread(
        file = metadata,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    }

    if (!is.null(taxon)) {
      logger::log_trace("Forcing all features to given organism")
      metadata_table <- data.frame(taxon)
      colnames(metadata_table) <- colname
    }

    logger::log_trace("Preparing organisms names")
    organism_table <- metadata_table |>
      tidytable::filter(!is.na(!!as.name(colname))) |>
      tidytable::distinct(!!as.name(colname)) |>
      tidytable::select(organism = !!as.name(colname)) |>
      tidytable::separate_rows(organism, sep = "\\|", )

    logger::log_trace(
      "Retrieving already computed Open Tree of Life Taxonomy"
    )
    organism_table_filled <- organism_table |>
      tidytable::left_join(
        tidytable::fread(
          org_tax_ott,
          na.strings = c("", "NA"),
          colClasses = "character"
        ),
        by = c("organism" = "organism_name")
      )
    rm(organism_table)

    logger::log_trace("Submitting the rest to OTL")
    organism_table_missing <- organism_table_filled |>
      tidytable::filter(is.na(organism_taxonomy_ottid))

    if (nrow(organism_table_missing) != 0) {
      biological_metadata_1 <- organism_table_missing |>
        get_organism_taxonomy_ott()

      logger::log_trace("Joining all results")
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
        logger::log_trace("Removing filename extensions")
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
    logger::log_trace("Joining with metadata table")
    if (!is.null(taxon)) {
      metadata_table_joined <- tidytable::inner_join(
        feature_table_0 |>
          tidytable::mutate(join = "x"),
        biological_metadata |>
          tidytable::select(organismOriginal = organism_name) |>
          tidytable::mutate(join = "x")
      ) |>
        tidytable::select(-join)
    } else {
      metadata_table_joined <-
        tidytable::left_join(
          feature_table_0,
          metadata_table,
          by = c("sample" = name_filename)
        ) |>
        tidytable::select(
          feature_id,
          organismOriginal = tidyselect::all_of(colname),
          tidyselect::everything()
        )
    }
    rm(feature_table_0, metadata_table)

    logger::log_trace("Joining with cleaned taxonomy table")
    taxed_features_table <-
      tidytable::left_join(
        metadata_table_joined,
        biological_metadata,
        by = c("organismOriginal" = "organism_name")
      ) |>
      tidytable::distinct() |>
      tidytable::select(
        feature_id,
        sample_organism_name = organismOriginal,
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
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::everything(),
        .fns = as.character
      )) |>
      tidytable::group_by(feature_id) |>
      clean_collapse(
        cols = c(
          "sample_organism_name",
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
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(is.character),
        .fns = function(x) {
          tidytable::replace_na(x, "ND")
        }
      ))
    rm(biological_metadata, metadata_table_joined)

    export_params(
      parameters = get_params(step = "prepare_taxa"),
      step = "prepare_taxa"
    )
    export_output(x = taxed_features_table, file = output)
    rm(taxed_features_table)
    return(output)
  }
