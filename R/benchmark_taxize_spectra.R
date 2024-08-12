import::from(stringi, stri_sub, .into = environment())
import::from(tidytable, bind_rows, .into = environment())
import::from(tidytable, distinct, .into = environment())
import::from(tidytable, filter, .into = environment())
import::from(tidytable, fread, .into = environment())
import::from(tidytable, left_join, .into = environment())
import::from(tidytable, mutate, .into = environment())
import::from(tidytable, select, .into = environment())
import::from(tidytable, slice_sample, .into = environment())

#' @title Taxize spectra benchmark
#'
#' @description This function adds taxa to the benchmark
#'
#' @details Because they are still quite dirty
#'
#' @export
#'
#' @importFrom stringi stri_sub
#' @importFrom tidytable bind_rows
#' @importFrom tidytable distinct
#' @importFrom tidytable filter
#' @importFrom tidytable fread
#' @importFrom tidytable left_join
#' @importFrom tidytable mutate
#' @importFrom tidytable select
#' @importFrom tidytable slice_sample
#'
#' @noRd
#'
#' @param input Initial features
#' @param keys SOP keys
#' @param org_tax_ott Taxonomy
#' @param output Prepared features
#'
#' @return The path to the taxed benchmark
#'
#' @examples NULL
benchmark_taxize_spectra <-
  function(input, keys, org_tax_ott, output) {
    features <- input |>
      fread(na.strings = c("", "NA"), colClasses = "character")
    sop <- keys |>
      fread(na.strings = c("", "NA"), colClasses = "character") |>
      mutate(inchikey_no_stereo = stri_sub(str = structure_inchikey, from = 1, to = 14))
    taxo <- org_tax_ott |>
      fread(na.strings = c("", "NA"), colClasses = "character")

    features_pretaxed <- features |>
      left_join(sop |>
        distinct(organism_name, inchikey_no_stereo))
    rm(features, sop)
    set.seed(42)
    features_sampled <- features_pretaxed |>
      filter(!is.na(organism_name)) |>
      slice_sample(n = 1, .by = feature_id) |>
      bind_rows(features_pretaxed |> filter(is.na(organism_name)))
    rm(features_pretaxed)

    features_taxed <- features_sampled |>
      left_join(taxo |>
        distinct(organism_name, .keep_all = TRUE)) |>
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
      )
    rm(features_sampled, taxo)

    export_output(x = features_taxed, file = output)
    rm(features_taxed)

    return(c(output))
  }
