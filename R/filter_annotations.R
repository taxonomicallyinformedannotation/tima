#' @title Filter annotations
#'
#' @description This function filters initial annotations.
#'
#' @importFrom crayon green
#' @importFrom tidytable arrange bind_rows distinct filter fread left_join mutate rename select
#'
#' @include get_params.R
#'
#' @param annotations Prepared annotations file
#' @param features Prepared features file
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
    features_table <- fread(
      file = features,
      colClasses = "character",
      na.strings = c("", "NA")
    )
    log_debug(x = "... annotations")
    annotation_table <- lapply(
      X = annotations,
      FUN = fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    ) |>
      bind_rows()
    log_debug(x = "... retention times")
    if (!is.null(rts)) {
      rt_table <- lapply(
        X = rts,
        FUN = fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
        bind_rows() |>
        rename(rt_target = rt)
    }

    features_annotated_table_1 <- features_table |>
      left_join(annotation_table)

    if (!is.null(rts)) {
      log_debug(
        "Filtering annotations outside of",
        tolerance_rt * 3,
        "minutes tolerance"
      )
      features_annotated_table_2 <- features_annotated_table_1 |>
        left_join(rt_table) |>
        mutate(
          candidate_structure_error_rt = as.numeric(rt) -
            as.numeric(rt_target)
        ) |>
        arrange(abs(candidate_structure_error_rt)) |>
        distinct(-candidate_structure_error_rt, -rt_target,
          .keep_all = TRUE
        ) |>
        ## ISSUE see #149 adapt for types and improve the * 3
        filter(abs(candidate_structure_error_rt) <= abs(tolerance_rt * 3) | is.na(candidate_structure_error_rt)) |>
        select(-rt_target, -type)
    } else {
      log_debug("No RT library provided, not filtering anything")
      features_annotated_table_2 <- features_annotated_table_1 |>
        mutate(candidate_structure_error_rt = NA)
    }

    log_debug(
      green(
        nrow(features_annotated_table_1) - nrow(features_annotated_table_2)
      ),
      "Candidates were removed based on retention time."
    )

    ## in case some features had a single filtered annotation
    final_table <- features_table |>
      left_join(features_annotated_table_2)

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
