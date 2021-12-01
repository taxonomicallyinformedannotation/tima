start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

log_debug("This script creates adducts")
log_debug("Authors: AR")
log_debug("Contributors: ...")

step <- "prepare_adducts"
paths <- parse_yaml_paths()
params <- get_params(step = step)

log_debug("Loading files ...")
log_debug("... exact masses")
masses <- readr::read_delim(
  file = params$input,
  col_select = "structure_exact_mass"
) |>
  dplyr::select(exact_mass = structure_exact_mass) |>
  dplyr::distinct()

log_debug("... adducts")
adducts_table <-
  readr::read_delim(file = paths$data$source$adducts)

log_debug("Treating adducts table")
adducts_t <- t(adducts_table) |>
  data.frame()

colnames(adducts_t) <- adducts_t[1, ]

adducts_t <- adducts_t[2, ] |>
  dplyr::mutate_all(as.numeric)

masses_adducts <- cbind(masses, adducts_t)

log_debug("Adding adducts to exact masses ...")
log_debug("... positive")
adducts_pos <-
  form_adducts_pos(massesTable = masses_adducts, adductsTable = adducts_t)

log_debug("... negative")
adducts_neg <-
  form_adducts_neg(massesTable = masses_adducts, adductsTable = adducts_t)

log_debug("... pure adducts masses ...")
mass_null <-
  cbind(data.frame(exact_mass = 0), adducts_t)

log_debug("... positive")
pure_pos <-
  form_adducts_pos(massesTable = mass_null, adductsTable = adducts_t) |>
  dplyr::filter(grepl(pattern = "pos_1", x = adduct, fixed = TRUE)) |>
  dplyr::select(-exact_mass)

log_debug("... negative")
pure_neg <-
  form_adducts_neg(massesTable = mass_null, adductsTable = adducts_t) |>
  dplyr::filter(grepl(pattern = "neg_1", x = adduct, fixed = TRUE)) |>
  dplyr::select(-exact_mass)

log_debug("Exporting ...")
ifelse(
  test = !dir.exists(paths$data$interim$adducts$path),
  yes = dir.create(paths$data$interim$adducts$path),
  no = paste(paths$data$interim$adducts$path, "exists")
)
ifelse(
  test = !dir.exists(paths$data$interim$config$path),
  yes = dir.create(paths$data$interim$config$path),
  no = paste(paths$data$interim$config$path, "exists")
)

log_debug("... structure adducts positive")
readr::write_delim(
  x = adducts_pos,
  file = file.path(
    paths$data$interim$adducts$path,
    paste0(params$output, "_pos.tsv.gz")
  ),
  delim = "\t"
)

log_debug("... structure adducts negative")
readr::write_delim(
  x = adducts_neg,
  file = file.path(
    paths$data$interim$adducts$path,
    paste0(params$output, "_neg.tsv.gz")
  ),
  delim = "\t"
)

log_debug("... adducts masses positive")
readr::write_delim(
  x = pure_pos,
  file = paths$data$interim$adducts$pos,
  delim = "\t"
)

log_debug("... adducts masses negative")
readr::write_delim(
  x = pure_neg,
  file = paths$data$interim$adducts$neg,
  delim = "\t"
)

export_params(
  parameters = params,
  directory = paths$data$interim$config$path,
  step = step
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
