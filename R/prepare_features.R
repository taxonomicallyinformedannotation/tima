#' @title Prepare features
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
#' @importFrom dplyr arrange desc distinct left_join mutate select
#' @importFrom readr cols read_delim write_delim
#'
#' @examples NULL
prepare_features <-
  function(features = params$files$features$raw,
           output = params$files$annotations$filled,
           name_features = params$names$features,
           name_rt = params$names$rt,
           name_mz = params$names$precursor) {
    stopifnot("Your features file does not exist" = file.exists(features))

    log_debug("Preparing features table")
    features_prepared <- features |>
      readr::read_delim(
        col_select =
          dplyr::all_of(c(
            feature_id = name_features,
            rt = name_rt,
            mz = name_mz
          ))
      )

    log_debug(x = "Exporting ...")
    export_params(step = "prepare_features")
    export_output(x = features_prepared, file = output)

    return(output)
  }
