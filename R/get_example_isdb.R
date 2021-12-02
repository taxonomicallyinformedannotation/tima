#' Title
#'
#' @param link 
#' @param export 
#'
#' @return TODO
#' @export
#'
#' @examples TODO
get_example_isdb <-
  function(link = paths$links$example_isdb,
           export = paths$data$interim$annotations$example_isdb) {
    paths <- parse_yaml_paths()
    
    ifelse(
      test = !dir.exists(dirname(dirname(export))),
      yes = dir.create(dirname(dirname(export))),
      no = paste(dirname(dirname(export)),
                 "exists")
    )
    ifelse(
      test = !dir.exists(dirname(export)),
      yes = dir.create(dirname(export)),
      no = paste(dirname(export),
                 "exists")
    )
    
    readr::read_tsv(file = link) |>
      readr::write_tsv(file = export)
  }
