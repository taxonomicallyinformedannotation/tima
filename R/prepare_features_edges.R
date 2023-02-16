#' @title Prepare edges
#'
#' @description This function prepares edges for further use
#'
#' @param tool Tool used to generate the edges
#' @param gnps_job_id GNPS job ID, if the tool was 'gnps'
#' @param input Input file if 'manual'
#' @param output Output file
#' @param name_source Name of the source features column
#' @param name_target Name of the target features column
#' @param parameters Params
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom dplyr filter select
#' @importFrom readr read_delim write_delim
#' @importFrom stringr str_length
#'
#' @examples NULL
prepare_features_edges <-
  function(tool = params$tools$networks$spectral$edges,
           gnps_job_id = params$gnps$id,
           input = params$files$networks$spectral$edges$raw,
           output = params$files$networks$spectral$edges$processed,
           name_source = params$names$source,
           name_target = params$names$target,
           parameters = params) {
    ## Check that tool is valid
    stopifnot("Your tool must be 'manual' or 'gnps" = tool %in% c("gnps", "manual"))

    ## Check that input is valid
    if (tool == "gnps") {
      stopifnot("Your GNPS job ID is invalid" = stringr::str_length(string = gnps_job_id) == 32)
    } else {
      stopifnot("Your input file does not exist" = file.exists(input))
    }
    params <<- parameters
    ## Load edges table
    log_debug(x = "Loading edge table")
    if (tool == "gnps") {
      edges_table <- read_edges(id = gnps_job_id)
    } else {
      edges_table <- readr::read_delim(file = input)
    }

    ## Format edges table
    log_debug(x = "Formatting edge table")
    edges_table_treated <- edges_table |>
      dplyr::select(
        feature_source = !!as.name(name_source),
        feature_target = !!as.name(name_target)
      ) |>
      dplyr::filter(feature_source != feature_target)

    ## Export edges table
    log_debug(x = "Exporting ...")
    export_params(step = "prepare_features_edges")
    export_output(x = edges_table_treated, file = output)

    return(output)
  }
