import::from(tidytable, any_of, .into = environment())
import::from(tidytable, fread, .into = environment())
import::from(tidytable, select, .into = environment())

#' @title Prepare features table
#'
#' @description This function prepares features
#'
#' @importFrom tidytable any_of
#' @importFrom tidytable fread
#' @importFrom tidytable select
#'
#' @include get_params.R
#'
#' @param features Path to the file containing the features data
#' @param output Path to the file to export the merged data to
#' @param name_adduct Name of the adduct column in the features data
#' @param name_features Name of the features column in the features data
#' @param name_rt Name of the retention time column in the features data
#' @param name_mz Name of the m/z column in the features data
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_features_tables <-
  function(features = get_params(step = "prepare_features_tables")$files$features$raw,
           output = get_params(step = "prepare_features_tables")$files$features$prepared,
           name_adduct = get_params(step = "prepare_features_tables")$names$adduct,
           name_features = get_params(step = "prepare_features_tables")$names$features,
           name_rt = get_params(step = "prepare_features_tables")$names$rt$features,
           name_mz = get_params(step = "prepare_features_tables")$names$precursor) {
    stopifnot("Your features file does not exist" = file.exists(features))

    log_debug("Preparing features table")
    features_prepared <- features |>
      fread(na.strings = c("", "NA"), colClasses = "character") |>
      select(any_of(
        c(
          feature_id = name_features,
          rt = name_rt,
          mz = name_mz,
          adduct = name_adduct
        )
      ))

    log_debug(x = "Exporting ...")
    export_params(
      parameters = get_params(step = "prepare_features_tables"),
      step = "prepare_features_tables"
    )
    export_output(x = features_prepared, file = output)
    rm(features_prepared)

    return(output)
  }
