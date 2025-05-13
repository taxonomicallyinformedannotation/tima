#' @title Prepare features table
#'
#' @description This function prepares features
#'
#' @include get_params.R
#'
#' @param features Path to the file containing the features data
#' @param output Path to the file to export the merged data to
#' @param candidates Number of samples to be retained per feature top intensities
#' @param name_adduct Name of the adduct column in the features data
#' @param name_features Name of the features column in the features data
#' @param name_rt Name of the retention time column in the features data
#' @param name_mz Name of the m/z column in the features data
#'
#' @return The path to the prepared feature table
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' get_file(
#'   url = get_default_paths()$urls$examples$features,
#'   export = get_params(step = "prepare_features_tables")$files$features$raw
#' )
#' prepare_features_tables()
#' unlink("data", recursive = TRUE)
#' }
prepare_features_tables <-
  function(
    features = get_params(step = "prepare_features_tables")$files$features$raw,
    output = get_params(
      step = "prepare_features_tables"
    )$files$features$prepared,
    candidates = get_params(
      step = "prepare_features_tables"
    )$annotations$canidates$samples,
    name_adduct = get_params(step = "prepare_features_tables")$names$adduct,
    name_features = get_params(step = "prepare_features_tables")$names$features,
    name_rt = get_params(step = "prepare_features_tables")$names$rt$features,
    name_mz = get_params(step = "prepare_features_tables")$names$precursor
  ) {
    stopifnot("Your features file does not exist" = file.exists(features))
    stopifnot(
      "Your candidates samples parameter should be lower or equal to 5" = candidates <=
        5L
    )

    logger::log_trace("Preparing features table")
    features_table_0 <- features |>
      tidytable::fread(na.strings = c("", "NA"), colClasses = "character")

    logger::log_trace("Formatting feature table")
    logger::log_trace(
      "... requires 'Peak area' or ':area' in columns (mzmine format)"
    )
    logger::log_trace("... or 'quant_' in columns (SLAW format)")
    logger::log_trace("... or 'Peak height' in columns (SIRIUS format)")

    features_table <- features_table_0 |>
      tidytable::select(
        tidyselect::any_of(c(name_features, name_rt, name_mz, name_adduct)),
        tidyselect::matches(" Peak area"),
        tidyselect::matches(":area"),
        tidyselect::matches("quant_"),
        tidyselect::matches(" Peak height")
      ) |>
      tidytable::select(-tidyselect::matches("quant_peaktable"))
    rm(features_table_0)
    colnames(features_table) <- colnames(features_table) |>
      stringi::stri_replace_all_fixed(
        pattern = " Peak area",
        replacement = "",
        vectorize_all = FALSE
      )
    colnames(features_table) <- colnames(features_table) |>
      stringi::stri_replace_all_fixed(
        pattern = ":area",
        replacement = "",
        vectorize_all = FALSE
      ) |>
      stringi::stri_replace_all_fixed(
        pattern = "datafile:",
        replacement = "",
        vectorize_all = FALSE
      )
    colnames(features_table) <- colnames(features_table) |>
      stringi::stri_replace_all_fixed(
        pattern = "quant_",
        replacement = "",
        vectorize_all = FALSE
      )
    colnames(features_table) <- colnames(features_table) |>
      stringi::stri_replace_all_fixed(
        pattern = " Peak height",
        replacement = "",
        vectorize_all = FALSE
      )
    logger::log_trace("Filtering top intensities per feature")
    features_prepared <- features_table |>
      tidytable::pivot_longer(
        cols = !tidyselect::any_of(c(
          name_features,
          name_rt,
          name_mz,
          name_adduct
        )),
        names_to = "sample"
      ) |>
      tidytable::filter(value != 0) |>
      tidytable::mutate(
        rank = rank(-as.numeric(value)),
        .by = tidyselect::all_of(
          c(name_features)
        )
      ) |>
      tidytable::filter(rank <= candidates) |>
      tidytable::select(tidyselect::any_of(
        c(
          feature_id = name_features,
          rt = name_rt,
          mz = name_mz,
          adduct = name_adduct,
          "sample"
        )
      )) |>
      tidytable::arrange(
        feature_id |>
          as.numeric()
      )
    rm(features_table)

    export_params(
      parameters = get_params(step = "prepare_features_tables"),
      step = "prepare_features_tables"
    )
    export_output(x = features_prepared, file = output)
    rm(features_prepared)

    return(output)
  }
