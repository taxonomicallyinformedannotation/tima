#' @title Prepare taxa
#'
#' @description This function prepares taxonomic information for features by
#'     matching organism names to Open Tree of Life taxonomy. Can attribute
#'     all features to a single organism or distribute them across multiple
#'     organisms based on relative intensities in samples.
#'
#' @details Depending on whether features are aligned between samples from
#'     various organisms, this function either:
#'     - Attributes all features to a single organism (if taxon specified), or
#'     - Attributes features to multiple organisms based on their relative
#'       intensities across samples (using metadata)
#'
#' @include columns_utils.R
#' @include get_params.R
#' @include get_organism_taxonomy_ott.R
#' @include safe_fread.R
#' @include validations_utils.R
#'
#' @param input Character string path to features file with intensities
#' @param extension Logical whether column names contain file extensions
#' @param name_filename Character string name of filename column in metadata
#' @param colname Character string name of column with biological source info
#' @param metadata Character string path to metadata file with organism info
#' @param org_tax_ott Character string path to Open Tree of Life taxonomy file
#' @param output Character string path for output file
#' @param taxon Character string organism name to enforce for all features
#'     (e.g., "Homo sapiens"). If provided, overrides metadata-based assignment.
#'
#' @return Character string path to the prepared taxa file
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
#'   url = paste0(dir, "data/interim/features/example_features.tsv"),
#'   export = get_params(step = "prepare_taxa")$files$features$prepared
#' )
#' prepare_taxa(
#'   taxon = "Homo sapiens",
#'   org_tax_ott = org_tax_ott
#' )
#' unlink("data", recursive = TRUE)
#' }
prepare_taxa <- function(
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
  ctx <- log_operation("prepare_taxa", taxon = taxon)

  # Validate file paths
  validate_character(input, param_name = "input", allow_empty = FALSE)
  validate_file_exists(
    path = input,
    file_type = "features file",
    param_name = "input"
  )

  validate_character(
    org_tax_ott,
    param_name = "org_tax_ott",
    allow_empty = FALSE
  )
  validate_file_exists(
    path = org_tax_ott,
    file_type = "OTT taxonomy file",
    param_name = "org_tax_ott"
  )

  if (!is.character(output) || length(output) != 1L) {
    stop("output must be a single character string")
  }

  # Validate logical parameter
  if (!is.logical(extension) || length(extension) != 1L) {
    stop("extension must be a single logical value (TRUE/FALSE)")
  }

  # Check if using single taxon or metadata-based assignment
  if (!is.null(taxon) && !is.na(taxon)) {
    log_debug("Assigning all features to single organism: %s", taxon)
  } else {
    validate_character(metadata, param_name = "metadata", allow_empty = FALSE)
    validate_file_exists(
      path = metadata,
      file_type = "metadata file",
      param_name = "metadata"
    )
  }

  # Handle empty taxon string as NULL
  if (!is.null(taxon) && taxon == "") {
    taxon <- NULL
  }

  if (is.null(taxon)) {
    log_debug("Using metadata for organism assignments")
  }

  # log_trace("Loading feature table")
  feature_table_0 <- safe_fread(
    file = input,
    file_type = "features table",
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  if (is.null(taxon)) {
    # log_trace("Loading metadata table")
    metadata_table <- safe_fread(
      file = metadata,
      file_type = "metadata table",
      na.strings = c("", "NA"),
      colClasses = "character"
    )
  }

  if (!is.null(taxon)) {
    # log_trace("Forcing all features to given organism")
    metadata_table <- data.frame(taxon)
    colnames(metadata_table) <- colname
  }

  # log_trace("Preparing organisms names")
  organism_table <- metadata_table |>
    tidytable::filter(!is.na(!!as.name(colname))) |>
    tidytable::distinct(!!as.name(colname)) |>
    tidytable::select(organism = !!as.name(colname)) |>
    tidytable::separate_rows(organism, sep = "\\|") |>
    tidytable::mutate(organism = trimws(organism))

  # log_trace(
  #  "Retrieving already computed Open Tree of Life Taxonomy"
  # )
  organism_table_filled <- organism_table |>
    tidytable::left_join(
      y = tidytable::fread(
        org_tax_ott,
        na.strings = c("", "NA"),
        colClasses = "character"
      ),
      by = c("organism" = "organism_name")
    )
  rm(organism_table)

  # log_trace("Submitting the rest to OTL")
  organism_table_missing <- organism_table_filled |>
    tidytable::filter(is.na(organism_taxonomy_ottid))

  if (nrow(organism_table_missing) != 0) {
    biological_metadata_1 <- organism_table_missing |>
      get_organism_taxonomy_ott()

    # log_trace("Joining all results")
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
      # log_trace("Removing filename extensions")
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
  # log_trace("Joining with metadata table")
  if (!is.null(taxon)) {
    metadata_table_joined <- tidytable::inner_join(
      x = feature_table_0 |>
        tidytable::mutate(join = "x"),
      y = biological_metadata |>
        tidytable::select(organismOriginal = organism_name) |>
        tidytable::mutate(join = "x")
    ) |>
      tidytable::select(-join)
  } else {
    metadata_table_joined <-
      tidytable::left_join(
        x = feature_table_0,
        y = metadata_table,
        by = c("sample" = name_filename)
      ) |>
      tidytable::select(
        feature_id,
        organismOriginal = tidyselect::all_of(x = colname),
        tidyselect::everything()
      ) |>
      tidytable::separate_rows(organismOriginal, sep = "\\|")
  }
  rm(feature_table_0, metadata_table)

  # log_trace("Joining with cleaned taxonomy table")
  # Split organismOriginal to match the split organism_name in biological_metadata
  taxed_features_table <-
    metadata_table_joined |>
    tidytable::mutate(organismOriginal = trimws(organismOriginal)) |>
    tidytable::left_join(
      y = biological_metadata,
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
      .cols = tidyselect::where(fn = is.character),
      .fns = ~ tidytable::replace_na(.x = .x, replace = "ND")
    ))
  rm(biological_metadata, metadata_table_joined)

  log_complete(ctx, n_features = nrow(taxed_features_table))

  export_params(
    parameters = get_params(step = "prepare_taxa"),
    step = "prepare_taxa"
  )
  export_output(x = taxed_features_table, file = output)
  rm(taxed_features_table)
  return(output)
}
