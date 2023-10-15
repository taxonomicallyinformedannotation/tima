#' @title Get last version from Zenodo
#'
#' @description This function gets the last version
#'    of a file from a Zenodo record
#'
#' @details Credit goes to partially to
#'    https://inbo.github.io/inborutils/
#'
#' @param doi DOI of the Zenodo record
#' @param pattern Pattern to identify the file to download
#' @param path Path to save the file to
#'
#' @return NULL
#'
#' @export
#'
#' @examples NULL
get_last_version_from_zenodo <-
  function(doi, pattern, path) {
    ## Remove the prefix from the DOI
    record <-
      stringi::stri_replace_all_fixed(
        str = doi,
        pattern = "10.5281/zenodo.",
        replacement = "",
        case_insensitive = TRUE
      )

    ## Retrieve file name by records call
    base_url <- "https://zenodo.org/records/"
    ## Fix with new Zenodo since the conceptdoi does not work anymore
    base_url_api <- "https://zenodo.org/api/records/"
    record_new <-
      httr2::request(base_url = paste0(base_url, record, "/latest")) |>
      httr2::req_perform()
    content <- jsonlite::fromJSON(txt = paste0(
      base_url_api,
      gsub(
        pattern = ".*/",
        replacement = "",
        x = record_new$url
      )
    ))

    ## Extract individual file names and urls
    fileurls <- content$files$links$self
    filenames <- content$files$filename

    ## Select the file URL and name matching the given pattern
    indices <- grepl(pattern = pattern, x = filenames)
    fileurl <- fileurls[indices]
    filename <- filenames[indices]

    ## Check size and not md5 as we rename the file
    ## a bit hacky
    zenodo_size <- content$files$filesize[indices]
    local_size <- file.size(path)

    ## If the local file does not exist or the sizes are different,
    ## download the file from Zenodo
    if (is.na(local_size) || zenodo_size != local_size) {
      message(
        "Downloading ",
        filename,
        " from https://doi.org/",
        doi,
        " (",
        content$metadata$title,
        "; unique identifier: ",
        content$doi_url,
        ")\n"
      )
      create_dir(export = path)
      get_file(url = fileurl, export = path)
    } else {
      message("A file with the same size is already present. Skipping")
    }
    return(path)
  }
