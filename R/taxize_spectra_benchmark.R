#' @title Taxize spectra benchmark
#'
#' @description This function adds taxa to the benchmark
#'
#' @details Because they are still quite dirty
#'
#' @param input Initial features
#' @param keys SOP keys
#' @param org_tax_ott Taxonomy
#' @param output Prepared features
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
taxize_spectra_benchmark <-
  function(input,
           keys,
           org_tax_ott,
           output) {
    features <- input |>
      tidytable::fread(na.strings = c("", "NA"))
    sop <- keys |>
      tidytable::fread(na.strings = c("", "NA")) |>
      tidyft::mutate(inchikey_2D = stringi::stri_sub(
        str = structure_inchikey,
        from = 1,
        to = 14
      ))
    taxo <- org_tax_ott |>
      tidytable::fread(na.strings = c("", "NA"))

    features_pretaxed <- features |>
      tidytable::left_join(sop |>
        tidytable::distinct(
          organism_name,
          inchikey_2D
        ))
    set.seed(42)
    features_sampled <- features_pretaxed |>
      dplyr::filter(!is.na(organism_name)) |>
      dplyr::group_by(feature_id) |>
      dplyr::sample_n(1) |>
      dplyr::ungroup() |>
      dplyr::bind_rows(features_pretaxed |>
        dplyr::filter(is.na(organism_name)))

    features_taxed <- features_sampled |>
      tidytable::left_join(taxo |>
        tidytable::distinct(organism_name,
          .keep_all = TRUE
        )) |>
      tidytable::select(
        feature_id,
        contains("organism_taxonomy_"),
        -contains("ott")
      )

    export_output(x = features_taxed, file = output)

    return(c(output))
  }
