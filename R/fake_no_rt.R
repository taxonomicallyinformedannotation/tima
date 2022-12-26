#' @title Fake no retention time
#'
#' @param input the file path of the input feature table
#' @param output the file path of the output feature table with no retention time column
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom readr read_delim write_delim
#'
#' @examples TODO
fake_no_rt <- function(input = paths$data$interim$annotations$example_feature_table,
                       output = paths$data$interim$annotations$example_feature_table_no_rt) {
  # Check if input file exists
  stopifnot("Your input file does not exist" = file.exists(input))

  # Read input file and select all columns except "rt"
  feature_table <- readr::read_delim(file = input, col_select = c(-rt))

  # Export the modified data
  log_debug(x = "Exporting ...")
  export_output(x = feature_table, file = output)
}
