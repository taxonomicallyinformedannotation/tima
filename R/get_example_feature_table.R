#' @title Get example feature table
#'
#' @description TODO
#'
#' @param url URL of the example feature table file
#' @param export File path to where the example feature table should be saved
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom readr read_tsv write_tsv
#'
#' @examples NULL
get_example_feature_table <-
  function(url = paths$urls$example_feature_table,
           export = paths$data$interim$annotations$example_feature_table) {
    # Create the export directory if it does not exist
    create_dir(export = export)

    # Read the tsv file from the given url
    # Select only the columns "cluster index" as "feature_id", "RTMean" as "rt" and "precursor mass" as "mz"
    # Write the resulting dataframe to the export path in tsv format
    readr::read_tsv(file = url, col_select = c(feature_id = `cluster index`, rt = RTMean, mz = `precursor mass`)) |>
      readr::write_tsv(file = export)
  }
