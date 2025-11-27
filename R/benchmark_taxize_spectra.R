#' @title Add taxonomy to benchmark spectra
#'
#' @description Adds taxonomic information to benchmark features by linking
#'     them to structure-organism pairs and organism taxonomy data. For
#'     features with multiple organism associations, one is randomly selected.
#'
#' @details Benchmark data often lacks clean taxonomic assignments. This
#'     function performs a controlled joining process with random sampling
#'     for ambiguous cases.
#'
#' @include validations_utils.R
#'
#' @param input Path to the initial features file
#' @param keys Path to the structure-organism pair (SOP) keys file
#' @param org_tax_ott Path to the organism taxonomy (OTT) file
#' @param output Path for the taxed benchmark output file
#'
#' @return Path to the taxed benchmark file
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Add taxonomy to benchmark data
#' taxed_file <- benchmark_taxize_spectra(
#'   input = "data/benchmark/features.tsv",
#'   keys = "data/sop/keys.tsv",
#'   org_tax_ott = "data/taxonomy/org_tax_ott.tsv",
#'   output = "data/benchmark/features_taxed.tsv"
#' )
#' }
benchmark_taxize_spectra <- function(input, keys, org_tax_ott, output) {
  # Input Validation ----
  validate_file_existence(list(
    input = input,
    keys = keys,
    org_tax_ott = org_tax_ott
  ))

  logger::log_info("Loading benchmark features from: ", input)
  features <- tidytable::fread(
    input,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  logger::log_debug("Loading structure-organism pairs from: {keys}")
  sop <- tidytable::fread(
    keys,
    na.strings = c("", "NA"),
    colClasses = "character"
  ) |>
    tidytable::mutate(
      # Extract 2D InChIKey (connectivity layer)
      inchikey_connectivity_layer = stringi::stri_sub(
        str = structure_inchikey,
        from = 1L,
        to = 14L
      )
    )

  logger::log_debug("Loading organism taxonomy from: {org_tax_ott}")
  taxo <- tidytable::fread(
    org_tax_ott,
    na.strings = c("", "NA"),
    colClasses = "character"
  )

  # Join features with organism names from SOP
  features_with_organisms <- features |>
    tidytable::left_join(
      y = sop |>
        tidytable::distinct(organism_name, inchikey_connectivity_layer)
    )

  logger::log_debug("Sampling features with multiple organism associations")
  set.seed(42L) # Reproducible sampling

  # For features with organism data: randomly select one per feature
  # For features without organism data: keep as-is
  features_sampled <- features_with_organisms |>
    tidytable::filter(!is.na(organism_name)) |>
    tidytable::slice_sample(n = 1L, .by = feature_id) |>
    tidytable::bind_rows(
      features_with_organisms |>
        tidytable::filter(is.na(organism_name))
    )

  logger::log_debug("Adding full taxonomic hierarchy")
  features_taxed <- features_sampled |>
    tidytable::left_join(
      y = taxo |>
        tidytable::distinct(organism_name, .keep_all = TRUE)
    ) |>
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
    )

  logger::log_info(
    "Added taxonomy to ",
    nrow(features_taxed),
    " benchmark features"
  )

  export_output(x = features_taxed, file = output)

  return(output)
}
