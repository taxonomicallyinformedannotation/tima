#' @title Filter annotations
#'
#' @description This function filters initial annotations.
#'
#' @include filter_nitrogen_rule.R
#'
#' @param annotations Prepared annotations file
#' @param features Prepared features file
#' @param filter_nitro Filter according to Nitrogen rule. Boolean
#' @param rts Prepared retention time library
#' @param output Output file
#' @param tolerance_rt Tolerance to filter retention time
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
filter_annotations <-
  function(annotations = get_params(step = "filter_annotations")$files$annotations$prepared$structural,
           features = get_params(step = "filter_annotations")$files$features$prepared,
           filter_nitro = get_params(step = "filter_annotations")$options$nitrogen_rule,
           rts = get_params(step = "filter_annotations")$files$libraries$temporal$prepared,
           output = get_params(step = "filter_annotations")$files$annotations$filtered,
           tolerance_rt = get_params(step = "filter_annotations")$ms$tolerances$rt$minutes) {
    stopifnot(
      "Annotations file(s) do(es) not exist" = all(lapply(X = annotations, FUN = file.exists) |> unlist())
    )
    stopifnot(
      "Retention time file(s) do(es) not exist" = all(lapply(X = rts, FUN = file.exists) |> unlist())
    )
    stopifnot("Your features file does not exist." = file.exists(features))

    if (length(rts) == 0) {
      rts <- NULL
    }

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
    if (!is.null(rts)) {
      rt_table <- lapply(
        X = rts,
        FUN = tidytable::fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
        tidytable::bind_rows() |>
        tidytable::rename(rt_target = rt)
    }

    features_annotated_table_1 <- features_table |>
      tidytable::left_join(annotation_table)

    if (!is.null(rts)) {
      log_debug(
        "Filtering annotations outside of",
        tolerance_rt * 3,
        "minutes tolerance"
      )
      features_annotated_table_2 <- features_annotated_table_1 |>
        tidytable::left_join(rt_table) |>
        tidytable::mutate(
          candidate_structure_error_rt = as.numeric(rt) -
            as.numeric(rt_target)
        ) |>
        tidytable::arrange(abs(candidate_structure_error_rt)) |>
        tidytable::distinct(-candidate_structure_error_rt, -rt_target,
          .keep_all = TRUE
        ) |>
        ## TODO adapt for types and improve the * 3
        tidytable::filter(abs(candidate_structure_error_rt) <= abs(tolerance_rt * 3) | is.na(candidate_structure_error_rt)) |>
        tidytable::select(-rt_target, -type)
    } else {
      log_debug("No RT library provided, not filtering anything")
      features_annotated_table_2 <- features_annotated_table_1 |>
        tidytable::mutate(candidate_structure_error_rt = NA)
    }

    log_debug(
      crayon::green(
        nrow(features_annotated_table_1) - nrow(features_annotated_table_2)
      ),
      "Candidates were removed based on retention time."
    )

    features_annotated_table_3 <- features_annotated_table_2 |>
      filter_nitrogen_rule(features_table = features_table, filter_nitro = filter_nitro)

    ## in case some features had a single filtered annotation
    final_table <- features_table |>
      tidytable::left_join(features_annotated_table_3)

    rm(
      features_table,
      annotation_table,
      features_annotated_table_1,
      features_annotated_table_2
    )

    export_params(
      parameters = get_params(step = "filter_annotations"),
      step = "filter_annotations"
    )
    export_output(
      x = final_table,
      file = output[[1]]
    )

    rm(final_table)
    return(output[[1]])
  }
