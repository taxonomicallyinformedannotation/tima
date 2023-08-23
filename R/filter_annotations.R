utils::globalVariables(
  c(
    "params",
    "rt_target",
    "type"
  )
)

#' @title Filter annotations
#'
#' @description This function filters initial annotations.
#'
#' @param annotations Prepared annotations file
#' @param features Prepared features file
#' @param rts Prepared retention time library
#' @param output Output file
#' @param tolerance_rt Tolerance to filter retention time
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
filter_annotations <-
  function(annotations = params$files$annotations$prepared,
           features = params$files$features$prepared,
           rts = params$files$libraries$temporal$prepared,
           output = params$files$annotations$filtered,
           tolerance_rt = params$ms$tolerances$rt$minutes,
           parameters = params) {
    stopifnot(
      "Annotations file(s) do(es) not exist" =
        rep(TRUE, length(annotations)) ==
          lapply(X = annotations, file.exists)
    )
    stopifnot(
      "Retention time file(s) do(es) not exist" =
        rep(TRUE, length(rts)) ==
          lapply(X = rts, file.exists)
    )
    stopifnot("Your features file does not exist." = file.exists(features))

    paths <<- parse_yaml_paths()
    params <<- parameters

    log_debug(x = "... features")
    features_table <- tidytable::fread(
      file = features,
      colClasses = "character",
      na.strings = c("", "NA")
    )
    log_debug(x = "... annotations")
    annotation_table <- lapply(
      X = annotations,
      FUN = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    ) |>
      tidytable::bind_rows()
    log_debug(x = "... retention times")
    rt_table <- lapply(
      X = rts,
      FUN = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    ) |>
      tidytable::bind_rows() |>
      tidytable::rename(rt_target = rt)

    log_debug(
      "Filtering annotations outside of",
      tolerance_rt * 3,
      "minutes tolerance"
    )
    features_annotated_table <- features_table |>
      tidytable::left_join(annotation_table) |>
      tidytable::left_join(rt_table) |>
      tidytable::mutate(
        error_rt = as.numeric(rt) -
          as.numeric(rt_target)
      ) |>
      tidytable::arrange(abs(error_rt)) |>
      tidytable::distinct(-error_rt, -rt_target,
        .keep_all = TRUE
      ) |>
      ## TODO adapt for types and improve the * 3
      tidytable::filter(abs(error_rt) <= abs(tolerance_rt * 3) |
        is.na(error_rt)) |>
      tidytable::select(-rt_target, -type)

    ## in case some features had a single filtered annotation
    final_table <- features_table |>
      tidytable::left_join(features_annotated_table)

    export_params(step = "filter_annotations")
    export_output(
      x = final_table,
      file = output[[1]]
    )

    return(output[[1]])
  }
