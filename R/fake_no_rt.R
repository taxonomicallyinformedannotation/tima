#' Title
#'
#' @param input TODO
#' @param output TODO
#'
#' @return TODO
#' @export
#'
#' @importFrom readr read_delim write_delim
#'
#' @examples
fake_no_rt <-
  function(input = paths$data$interim$annotations$example_feature_table,
           output = paths$data$interim$annotations$example_feature_table_no_rt) {
    paths <- parse_yaml_paths()
    stopifnot("Your input file does not exist" = file.exists(input))

    feature_table <- readr::read_delim(
      file = input,
      col_select = c(-rt)
    )

    log_debug(x = "Exporting ...")
    ifelse(
      test = !dir.exists(paths$data$path),
      yes = dir.create(paths$data$path),
      no = paste(paths$data$path, "exists")
    )
    ifelse(
      test = !dir.exists(paths$data$interim$path),
      yes = dir.create(paths$data$interim$path),
      no = paste(paths$data$interim$path, "exists")
    )
    ifelse(
      test = !dir.exists(paths$data$interim$edges$path),
      yes = dir.create(paths$data$interim$edges$path),
      no = paste(paths$data$interim$edges$path, "exists")
    )
    ifelse(
      test = !dir.exists(paths$data$interim$config$path),
      yes = dir.create(paths$data$interim$config$path),
      no = paste(paths$data$interim$config$path, "exists")
    )
    ifelse(
      test = !dir.exists(dirname(output)),
      yes = dir.create(dirname(output)),
      no = paste(dirname(output), "exists")
    )

    log_debug(
      x = "... path to export is",
      output
    )
    readr::write_delim(
      x = feature_table,
      file = output,
      delim = "\t"
    )
  }
