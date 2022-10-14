#' @title Get ISDB
#'
#' @param url_pos TODO
#' @param url_neg TODO
#' @param export_pos TODO
#' @param export_neg TODO
#'
#' @return TODO
#'
#' @export
#'
#' @importFrom curl curl_download
#' @importFrom readr read_file write_file
#'
#' @examples TODO
get_isdb <-
  function(url_pos = paths$urls$isdb$pos,
           url_neg = paths$urls$isdb$neg,
           export_pos = paths$data$source$libraries$isdb$pos,
           export_neg = paths$data$source$libraries$isdb$neg) {
    paths <- parse_yaml_paths()

    if (!is.null(url_pos)) {
      log_debug("Downloading positive mode ISDB ...")

      ifelse(
        test = !dir.exists(dirname(dirname(
          dirname(export_pos)
        ))),
        yes = dir.create(dirname(dirname(
          dirname(export_pos)
        ))),
        no = paste(
          dirname(dirname(
            dirname(export_pos)
          )),
          "exists"
        )
      )
      ifelse(
        test = !dir.exists(dirname(dirname(export_pos))),
        yes = dir.create(dirname(dirname(export_pos))),
        no = paste(
          dirname(dirname(export_pos)),
          "exists"
        )
      )
      ifelse(
        test = !dir.exists(dirname(export_pos)),
        yes = dir.create(dirname(export_pos)),
        no = paste(
          dirname(export_pos),
          "exists"
        )
      )

      readr::read_file(file = curl::curl_download(url = url_pos, destfile = tempfile())) |>
        readr::write_file(file = export_pos)
    }
    if (!is.null(url_neg)) {
      log_debug("Downloading negative mode ISDB ...")

      ifelse(
        test = !dir.exists(dirname(dirname(
          dirname(export_neg)
        ))),
        yes = dir.create(dirname(dirname(
          dirname(export_neg)
        ))),
        no = paste(
          dirname(dirname(
            dirname(export_neg)
          )),
          "exists"
        )
      )
      ifelse(
        test = !dir.exists(dirname(dirname(export_neg))),
        yes = dir.create(dirname(dirname(export_neg))),
        no = paste(
          dirname(dirname(export_neg)),
          "exists"
        )
      )
      ifelse(
        test = !dir.exists(dirname(export_neg)),
        yes = dir.create(dirname(export_neg)),
        no = paste(
          dirname(export_neg),
          "exists"
        )
      )

      readr::read_file(file = curl::curl_download(url = url_neg, destfile = tempfile())) |>
        readr::write_file(file = export_neg)
    }
  }
