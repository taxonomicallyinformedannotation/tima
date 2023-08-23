utils::globalVariables(
  c(
    "params"
  )
)
#' @title Prepare features table
#'
#' @description This function prepares features
#'
#' @include export_output.R
#' @include export_params.R
#'
#' @param features Path to the file containing the features data
#' @param output Path to the file to export the merged data to
#' @param name_features Name of the features column in the features data
#' @param name_rt Name of the retention time column in the features data
#' @param name_mz Name of the m/z column in the features data
#' @param parameters params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
prepare_features_tables <-
  function(features = params$files$features$raw,
           output = params$files$features$prepared,
           name_features = params$names$features,
           name_rt = params$names$rt,
           name_mz = params$names$precursor,
           parameters = params) {
    params <<- parameters
    stopifnot("Your features file does not exist" = file.exists(features))

    log_debug("Preparing features table")
    features_prepared <- features |>
      tidytable::fread(
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
      tidytable::select(tidytable::all_of(c(
        feature_id = name_features,
        rt = name_rt,
        mz = name_mz
      )))

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_features_tables")
    export_output(x = features_prepared, file = output)

    return(output)
  }
