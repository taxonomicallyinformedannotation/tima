#' @title Prepare features table
#'
#' @description This function prepares features
#'
#' @param features Path to the file containing the features data
#' @param output Path to the file to export the merged data to
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
           name_features = get_params(step = "prepare_features_tables")$names$features,
           name_rt = get_params(step = "prepare_features_tables")$names$rt$features,
           name_mz = get_params(step = "prepare_features_tables")$names$precursor) {
    stopifnot("Your features file does not exist" = file.exists(features))

    log_debug("Preparing features table")
    features_prepared <- features |>
      tidytable::fread(
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::select(tidytable::any_of(c(
        feature_id = name_features,
        rt = name_rt,
        mz = name_mz
      )))

    log_debug(x = "Exporting ...")
    export_params(parameters = get_params(step = "prepare_features_tables"), step = "prepare_features_tables")
    export_output(x = features_prepared, file = output)
    rm(features_prepared)

    return(output)
  }
