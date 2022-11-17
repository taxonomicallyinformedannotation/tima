#' Credit goes to partially to https://inbo.github.io/inborutils/

#' @title Get last version from Zenodo
#'
#' @param doi TODO
#' @param pattern TODO
#' @param path TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom curl curl_download curl_fetch_memory
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_match str_remove
#'
#' @examples TODO
get_last_version_from_zenodo <-
  function(doi, pattern, path) {
    record <-
      stringr::str_remove(
        string = doi,
        pattern = stringr::fixed("10.5281/zenodo.")
      )

    # Retrieve file name by records call
    base_url <- "https://zenodo.org/api/records/"
    req <- curl::curl_fetch_memory(url = paste0(base_url, record))
    content <- jsonlite::fromJSON(txt = rawToChar(req$content))

    # extract individual file names and urls
    fileurls <- content$files$links$self
    filenames <- stringr::str_match(
      string = fileurls,
      pattern = ".+/([^/]+)"
    )[, 2]

    fileurl <- fileurls[grepl(pattern = pattern, x = fileurls)]
    filename <- filenames[grepl(pattern = pattern, x = filenames)]

    # check size and not md5 as we rename the file
    # a bit hacky
    zenodo_size <-
      content$files$size[grepl(pattern = pattern, x = filenames)]

    local_size <- file.size(path)

    if (is.na(local_size) ||
      zenodo_size != local_size) {
      # download files
      message(
        "Downloading ",
        filename,
        " from https://doi.org/",
        doi,
        " (",
        content$metadata$title,
        "; version: ",
        ifelse(
          !is.null(content$metadata$version),
          content$metadata$version,
          content$metadata$relations$version[1, 1]
        ),
        ")\n"
      )
      curl::curl_download(url = fileurl, destfile = path)
    } else {
      message("A file with the same size is already present. Skipping")
    }
  }
