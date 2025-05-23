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
#' \dontrun{
#' copy_backbone()
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
#' }
filter_annotations <-
  function(
    annotations = get_params(
      step = "filter_annotations"
    )$files$annotations$prepared$structural,
    features = get_params(step = "filter_annotations")$files$features$prepared,
    rts = get_params(
      step = "filter_annotations"
    )$files$libraries$temporal$prepared,
    output = get_params(step = "filter_annotations")$files$annotations$filtered,
    tolerance_rt = get_params(
      step = "filter_annotations"
    )$ms$tolerances$rt$library
  ) {
    stopifnot(
      "Annotations file(s) do(es) not exist" = all(
        purrr::map(.x = annotations, .f = file.exists) |>
          unlist()
      )
    )
    stopifnot(
      "Retention time file(s) do(es) not exist" = all(
        purrr::map(.x = rts, .f = file.exists) |>
          unlist()
      )
    )
    stopifnot("Your features file does not exist." = file.exists(features))

    if (length(rts) == 0) {
      rts <- NULL
    }

    logger::log_trace("... features")
    features_table <- tidytable::fread(
      file = features,
      colClasses = "character",
      na.strings = c("", "NA")
    ) |>
      tidytable::distinct(
        feature_id,
        rt,
        mz
      )
    logger::log_trace("... annotations")
    annotation_tables_list <- purrr::map(
      .x = annotations,
      .f = tidytable::fread,
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    if ("ms1" %in% names(annotation_tables_list)) {
      logger::log_trace(
        "Removing MS1 annotations for which we have spectral hits"
      )
      annotations_tables_spectral <- annotation_tables_list[names(
        annotation_tables_list
      )[names(annotation_tables_list) != "ms1"]] |>
        tidytable::bind_rows()

      spectral_keys <- annotations_tables_spectral |>
        tidytable::distinct(
          feature_id,
          ## Comment not taking into account because of inconsistencies among tools
          # candidate_adduct,
          candidate_structure_inchikey_connectivity_layer
        )
      annotation_table <- annotation_tables_list[["ms1"]] |>
        tidytable::anti_join(spectral_keys) |>
        tidytable::bind_rows(annotations_tables_spectral)
    } else {
      annotation_table <- annotation_tables_list |>
        tidytable::bind_rows()
    }
    rm(
      annotation_tables_list,
      annotations_tables_spectral,
      spectral_keys
    )

    logger::log_trace("... retention times")
    if (!is.null(rts)) {
      rt_table <- purrr::map(
        .x = rts,
        .f = tidytable::fread,
        na.strings = c("", "NA"),
        colClasses = "character"
      ) |>
        tidytable::bind_rows() |>
        tidytable::rename(rt_target = rt)
    }

    features_annotated_table_1 <- features_table |>
      tidytable::left_join(annotation_table)
    rm(annotation_table)

    if (!is.null(rts)) {
      logger::log_info(
        "Filtering annotations outside of {tolerance_rt} minutes tolerance"
      )
      features_annotated_table_2 <- features_annotated_table_1 |>
        tidytable::left_join(rt_table) |>
        tidytable::mutate(
          candidate_structure_error_rt = as.numeric(rt) -
            as.numeric(rt_target)
        ) |>
        tidytable::arrange(abs(candidate_structure_error_rt)) |>
        tidytable::distinct(
          -candidate_structure_error_rt,
          -rt_target,
          .keep_all = TRUE
        ) |>
        tidytable::filter(
          abs(candidate_structure_error_rt) <= abs(tolerance_rt) |
            is.na(candidate_structure_error_rt)
        ) |>
        tidytable::select(-rt_target, -type)
    } else {
      logger::log_info("No RT library provided, not filtering anything")
      features_annotated_table_2 <- features_annotated_table_1 |>
        tidytable::mutate(candidate_structure_error_rt = NA)
    }

    logger::log_info(
      "{nrow(features_annotated_table_1) - nrow(features_annotated_table_2)} candidates were removed based on retention time."
    )
    rm(features_annotated_table_1)

    ## in case some features had a single filtered annotation
    final_table <- features_table |>
      tidytable::left_join(features_annotated_table_2)

    rm(
      features_table,
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
