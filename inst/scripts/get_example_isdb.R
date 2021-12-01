start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

cat("This script downloads an example of spectral_lib_matcher output. \n")
cat("Authors: AR")
cat("Contributors: ...")

paths <- parse_yaml_paths()

ifelse(
  test = !dir.exists(dirname(
    dirname(paths$data$interim$annotations$example_isdb)
  )),
  yes = dir.create(dirname(
    dirname(paths$data$interim$annotations$example_isdb)
  )),
  no = paste(
    dirname(
      dirname(paths$data$interim$annotations$example_isdb)
    ),
    "exists"
  )
)
ifelse(
  test = !dir.exists(dirname(
    paths$data$interim$annotations$example_isdb
  )),
  yes = dir.create(dirname(
    paths$data$interim$annotations$example_isdb
  )),
  no = paste(
    dirname(paths$data$interim$annotations$example_isdb),
    "exists"
  )
)

readr::read_tsv(file = paths$links$example_isdb) |>
  readr::write_tsv(file = paths$data$interim$annotations$example_isdb)
