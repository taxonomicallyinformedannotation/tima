#' @title Filter annotations
#'
#' @description This function filters initial annotations.
#'
#' @include get_params.R
#'
#' @param annotations Prepared annotations file
#' @param features Prepared features file
#' @param rts Prepared retention time library
#' @param output Output file
#' @param tolerance_rt Tolerance to filter retention time
#'
#' @return The path to the filtered annotations
#'
#' @export
#'
#' @examples
#' tima:::copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' dir <- paste0(github, repo)
#' annotations <- get_params(step = "filter_annotations")$files$annotations$prepared$structural[[2]] |>
#'   gsub(
#'     pattern = ".gz",
#'     replacement = "",
#'     fixed = TRUE
#'   )
#' features <- get_params(step = "filter_annotations")$files$features$prepared |>
#'   gsub(
#'     pattern = ".gz",
#'     replacement = "",
#'     fixed = TRUE
#'   )
#' rts <- get_params(step = "filter_annotations")$files$libraries$temporal$prepared |>
#'   gsub(
#'     pattern = ".gz",
#'     replacement = "",
#'     fixed = TRUE
#'   )
#' get_file(url = paste0(dir, annotations), export = annotations)
#' get_file(url = paste0(dir, features), export = features)
#' get_file(url = paste0(dir, rts), export = rts)
#' filter_annotations(
#'   annotations = annotations,
#'   features = features,
#'   rts = rts
#' )
#' unlink("data", recursive = TRUE)
filter_annotations <-
  function(annotations = get_params(step = "filter_annotations")$files$annotations$prepared$structural,
           features = get_params(step = "filter_annotations")$files$features$prepared,
           rts = get_params(step = "filter_annotations")$files$libraries$temporal$prepared,
           output = get_params(step = "filter_annotations")$files$annotations$filtered,
           tolerance_rt = get_params(step = "filter_annotations")$ms$tolerances$rt$library) {
    stopifnot("Annotations file(s) do(es) not exist" = all(lapply(X = annotations, FUN = file.exists) |> unlist()))
    stopifnot("Retention time file(s) do(es) not exist" = all(lapply(X = rts, FUN = file.exists) |> unlist()))
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
        tolerance_rt,
        "minutes tolerance"
      )
      features_annotated_table_2 <- features_annotated_table_1 |>
        tidytable::left_join(rt_table) |>
        tidytable::mutate(candidate_structure_error_rt = as.numeric(rt) -
          as.numeric(rt_target)) |>
        tidytable::arrange(abs(candidate_structure_error_rt)) |>
        tidytable::distinct(-candidate_structure_error_rt, -rt_target, .keep_all = TRUE) |>
        tidytable::filter(
          abs(candidate_structure_error_rt) <= abs(tolerance_rt) |
            is.na(candidate_structure_error_rt)
        ) |>
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

    ## in case some features had a single filtered annotation
    final_table <- features_table |>
      tidytable::left_join(features_annotated_table_2)

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
    export_output(x = final_table, file = output[[1]])

    rm(final_table)
    return(output[[1]])
  }
