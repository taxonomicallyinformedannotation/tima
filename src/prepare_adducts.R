start <- Sys.time()

source(file = "R/helpers.R")
source(file = "R/form_adduct_pos.R")
source(file = "R/form_adduct_neg.R")

log_debug("This script creates adducts")
log_debug("Authors: AR")
log_debug("Contributors: ...")

library(dplyr)
library(docopt)
library(purrr)
library(readr)
library(yaml)

paths <- parse_yaml_paths()

params <- get_params(step = "prepare_adducts")

log_debug(x = "loading files")

log_debug(x = "... exact masses")
masses <- readr::read_delim(
  file = params$input,
  col_select = "structure_exact_mass"
) |>
  dplyr::select(exact_mass = structure_exact_mass) |>
  dplyr::distinct()

log_debug(x = "... adducts")
adducts_table <- readr::read_delim(file = paths$data$source$adducts)

log_debug(x = "preparing adducts")
adducts_t <- t(adducts_table) |>
  data.frame()

colnames(adducts_t) <- adducts_t[1, ]

adducts_t <- adducts_t[2, ] |>
  mutate_all(as.numeric)

masses_adducts <- cbind(masses, adducts_t)

log_debug(x = "calculating adducts ...")
log_debug(x = "... pos")
adducts_pos <-
  form_adducts_pos(massesTable = masses_adducts, adductsTable = adducts_t)

log_debug(x = "... neg")
adducts_neg <-
  form_adducts_neg(massesTable = masses_adducts, adductsTable = adducts_t)

log_debug(x = "... pure adducts table ...")
mass_null <-
  cbind(data.frame(exact_mass = 0), adducts_t)

log_debug(x = "... pos")
pure_pos <-
  form_adducts_pos(massesTable = mass_null, adductsTable = adducts_t) |>
  dplyr::filter(grepl(pattern = "pos_1", x = adduct, fixed = TRUE)) |>
  dplyr::select(-exact_mass)

log_debug(x = "... neg")
pure_neg <-
  form_adducts_neg(massesTable = mass_null, adductsTable = adducts_t) |>
  dplyr::filter(grepl(pattern = "neg_1", x = adduct, fixed = TRUE)) |>
  dplyr::select(-exact_mass)

log_debug(x = "exporting ...")
log_debug(x = "... pos")
readr::write_delim(
  x = adducts_pos,
  file = file.path(
    paths$data$interim$adducts$path,
    paste0(params$output, "_pos.tsv.gz")
  )
)

log_debug(x = "... neg")
readr::write_delim(
  x = adducts_neg,
  file = file.path(data_processed, paste0(params$output, "_neg.tsv.gz"))
)

log_debug(x = "... pure pos")
readr::write_delim(
  x = pure_pos,
  file = paths$data$interim$adducts$pos,
)

log_debug(x = "... pure neg")
readr::write_delim(
  x = pure_neg,
  file = paths$data$interim$adducts$neg,
)

log_debug(x = "... parameters used are saved in", paths$data$interim$config$path)
yaml::write_yaml(
  x = params,
  file = file.path(
    paths$data$interim$config$path,
    paste(
      format(Sys.time(), "%y%m%d_%H%M%OS"),
      "prepare_adducts.yaml",
      sep = "_"
    )
  )
)

end <- Sys.time()

log_debug("Script finished in", format(end - start))
