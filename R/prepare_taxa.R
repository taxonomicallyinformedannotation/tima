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
#' @param input [character] Character string path to features file with
#'     intensities
#' @param extension [logical] Logical whether column names contain file
#'     extensions
#' @param name_filename [character] Character string name of filename column in
#'     metadata
#' @param colname [character] Character string name of column with biological
#'     source info
#' @param metadata [character] Character string path to metadata file with
#'     organism info
#' @param org_tax_ott [character] Character string path to Open Tree of Life
#'     taxonomy file
#' @param output [character] Character string path for output file
#' @param taxon [character] Character string organism name to enforce for all
#'     features
#'     (e.g., "Homo sapiens"). If provided, overrides metadata-based assignment.
#'
#' @return Character string path to the prepared taxa file
#'
#' @family preparation
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

  .make_nd_taxa_table <- function(feature_ids) {
    feature_ids <- unique(as.character(feature_ids))
    tidytable::tidytable(
      feature_id = feature_ids,
      sample_organism_name = "ND",
      sample_organism_01_domain = "ND",
      sample_organism_02_kingdom = "ND",
      sample_organism_03_phylum = "ND",
      sample_organism_04_class = "ND",
      sample_organism_05_order = "ND",
      sample_organism_06_family = "ND",
      sample_organism_07_tribe = "ND",
      sample_organism_08_genus = "ND",
      sample_organism_09_species = "ND",
      sample_organism_10_varietas = "ND"
    )
  }

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
    cli::cli_abort(
      "output must be a single character string",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  # Validate logical parameter
  if (!is.logical(extension) || length(extension) != 1L) {
    cli::cli_abort(
      "extension must be a single logical value (TRUE/FALSE)",
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
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

  feature_table_0 <- safe_fread(
    file = input,
    file_type = "features table",
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  if (is.null(taxon)) {
    metadata_table <- safe_fread(
      file = metadata,
      file_type = "metadata table",
      na.strings = c("", "NA"),
      colClasses = "character"
    )
  }

  if (!is.null(taxon)) {
    metadata_table <- data.frame(taxon)
    colnames(metadata_table) <- colname
  } else {
    # Resolve taxon column name with compatibility fallback.
    if (!colname %in% names(metadata_table)) {
      fallback_taxon <- setdiff(c("ATTRIBUTE_species", "organism"), colname)
      fallback_taxon <- fallback_taxon[
        fallback_taxon %in% names(metadata_table)
      ]
      if (length(fallback_taxon) > 0L) {
        colname <- fallback_taxon[[1L]]
        log_warn(
          "Metadata column '%s' not found; using '%s' instead",
          get_params(step = "prepare_taxa")$names$taxon,
          colname
        )
      } else {
        log_warn(
          "No organism column found in metadata; exporting ND taxonomy for all features"
        )
        taxed_features_table <- .make_nd_taxa_table(feature_table_0$feature_id)
        log_complete(ctx, n_features = nrow(taxed_features_table))
        export_params(
          parameters = get_params(step = "prepare_taxa"),
          step = "prepare_taxa"
        )
        export_output(x = taxed_features_table, file = output)
        return(output)
      }
    }

    has_organism <- any(
      !is.na(metadata_table[[colname]]) &
        nzchar(trimws(metadata_table[[colname]]))
    )
    if (!has_organism) {
      log_warn(
        "Metadata column '%s' has no organism values; exporting ND taxonomy for all features",
        colname
      )
      taxed_features_table <- .make_nd_taxa_table(feature_table_0$feature_id)
      log_complete(ctx, n_features = nrow(taxed_features_table))
      export_params(
        parameters = get_params(step = "prepare_taxa"),
        step = "prepare_taxa"
      )
      export_output(x = taxed_features_table, file = output)
      return(output)
    }
  }

  organism_table <- metadata_table |>
    tidytable::filter(!is.na(!!as.name(colname))) |>
    tidytable::distinct(!!as.name(colname)) |>
    tidytable::select(organism = !!as.name(colname)) |>
    tidytable::separate_rows(organism, sep = "\\|") |>
    tidytable::mutate(organism = trimws(organism))

  #  "Retrieving already computed Open Tree of Life Taxonomy"
  # )
  ott_table <- safe_fread(
    file = org_tax_ott,
    file_type = "organism taxonomy (OTT)",
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  # Validate OTT table has required columns
  required_ott_cols <- c(
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

  missing_cols <- setdiff(required_ott_cols, names(ott_table))
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      c(
        "OTT (Open Tree of Life) taxonomy file is missing required columns",
        "x" = sprintf(
          "Missing columns: %s",
          paste(missing_cols, collapse = ", ")
        ),
        "i" = sprintf(
          "File has columns: %s",
          paste(names(ott_table), collapse = ", ")
        ),
        "!" = "Please ensure your OTT file has the standard TIMA taxonomy columns"
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  organism_table_filled <- organism_table |>
    tidytable::left_join(
      y = ott_table,
      by = c("organism" = "organism_name")
    )
  rm(organism_table, ott_table)

  organism_table_missing <- organism_table_filled |>
    tidytable::filter(is.na(organism_taxonomy_ottid))

  if (nrow(organism_table_missing) != 0) {
    biological_metadata_1 <- organism_table_missing |>
      get_organism_taxonomy_ott()

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
    if (!extension) {
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
    # Validate join keys exist in their respective tables
    if (!"sample" %in% names(feature_table_0)) {
      cli::cli_abort(
        c(
          "Features file does not contain the 'sample' column",
          "i" = sprintf(
            "Found columns: %s",
            paste(names(feature_table_0), collapse = ", ")
          ),
          "x" = "Features file must have a 'sample' column for metadata mapping"
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

    if (!name_filename %in% names(metadata_table)) {
      cli::cli_abort(
        c(
          "Metadata file does not contain the filename column",
          "i" = sprintf(
            "Expected column '%s' as specified in 'names$filename' parameter",
            name_filename
          ),
          "x" = sprintf(
            "Found columns: %s",
            paste(names(metadata_table), collapse = ", ")
          ),
          "!" = "Please ensure the 'names$filename' parameter matches a column in your metadata file"
        ),
        class = c("tima_validation_error", "tima_error"),
        call = NULL
      )
    }

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

    if (nrow(metadata_table_joined) == 0L) {
      log_warn(
        "No metadata rows matched features by sample name; exporting ND taxonomy for all features"
      )
      taxed_features_table <- .make_nd_taxa_table(feature_table_0$feature_id)
      log_complete(ctx, n_features = nrow(taxed_features_table))
      export_params(
        parameters = get_params(step = "prepare_taxa"),
        step = "prepare_taxa"
      )
      export_output(x = taxed_features_table, file = output)
      return(output)
    }
  }
  rm(feature_table_0, metadata_table)

  # Split organismOriginal to match the split organism_name in
  # biological_metadata
  joined_data <- metadata_table_joined |>
    tidytable::mutate(organismOriginal = trimws(organismOriginal)) |>
    tidytable::left_join(
      y = biological_metadata,
      by = c("organismOriginal" = "organism_name")
    ) |>
    tidytable::distinct()

  # Validate that we have the required taxonomy columns for selection
  expected_cols <- c(
    "feature_id",
    "organismOriginal",
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
  missing_select_cols <- setdiff(expected_cols, names(joined_data))
  if (length(missing_select_cols) > 0L) {
    cli::cli_abort(
      c(
        "Failed to prepare taxa: missing expected columns after joining with OTT taxonomy",
        "x" = sprintf(
          "Missing columns: %s",
          paste(missing_select_cols, collapse = ", ")
        ),
        "i" = sprintf(
          "Available columns: %s",
          paste(names(joined_data), collapse = ", ")
        ),
        "!" = "This may indicate: (1) empty organism list, (2) mismatched organism names, or (3) corrupted OTT file"
      ),
      class = c("tima_validation_error", "tima_error"),
      call = NULL
    )
  }

  taxed_features_table <- joined_data |>
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
  output
}
