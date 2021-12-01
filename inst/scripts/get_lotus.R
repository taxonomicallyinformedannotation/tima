start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

cat("This script downloads LOTUS referenced structure-organism pairs. \n")
cat("Authors: AR")
cat("Contributors: ...")

paths <- parse_yaml_paths()

ifelse(
  test = !dir.exists(dirname(paths$data$source$libraries$lotus)),
  yes = dir.create(dirname(paths$data$source$libraries$lotus)),
  no = paste(dirname(paths$data$source$libraries$lotus), "exists")
)

readr::read_csv(file = curl::curl_download(paths$links$lotus, tempfile())) |>
  readr::write_csv(file = paths$data$source$libraries$lotus)
