#' @title Get benchmark
#'
#' @description This function gets the spectra used for the benchmark
#'
#' @param url Character string containing the URL of the benchmarking set
#' @param export Character string containing the file path where the benchmarking set should be exported
#'
#' @return NULL
#'
#' @export
#'
#' @importFrom curl curl_download
#' @importFrom readr read_lines write_lines
#'
#' @examples get_benchmark(url = "https://raw.githubusercontent.com/matchms/matchms/master/tests/massbank_five_spectra.msp", export = "data/source/benchmark/set.csv")
get_benchmark <- function(url = paths$urls$benchmarking_set,
                          export = paths$data$source$benchmark$set) {
  ## Create the directory where the benchmarking set will be exported
  create_dir(export = export)

  ## Download the benchmarking set and write it to the specified export path
  curl::curl_download(url = url, destfile = tempfile()) |>
    readr::read_lines() |>
    readr::write_lines(file = export)
}
