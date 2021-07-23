#' Title
#'
#' @param step
#'
#' @return
#' @export
#'
#' @examples
get_params <- function(step) {
  doc_path <<- file.path(paths$src$docopt, paste0(step, ".txt"))
  default_path <<-
    file.path(paths$config$default$path, paste0(step, ".yaml"))
  params_path <<-
    file.path(paths$config$params$path, paste0(step, ".yaml"))
  
  doc <<- readChar(con = doc_path,
                   nchars = file.info(doc_path)$size)
  
  arguments <<- docopt(doc)
  
  params <<- parse_yaml_params()
  
  params <<- parse_cli_params()
  
  return(params)
}