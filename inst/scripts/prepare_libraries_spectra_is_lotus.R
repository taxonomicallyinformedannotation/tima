start <- Sys.time()

require(
  package = "timaR",
  quietly = TRUE
)

# paths <- parse_yaml_paths()

log_debug(
  "This script",
  crayon::green("Prepares the predicted spectra from LOTUS \n")
)
log_debug("Authors: ", crayon::green("AR"), "\n")
log_debug("Contributors: ...")

# prepare_libraries_spectra(
#   metad = CompoundDb::make_metadata(
#     source = "LOTUS",
#     url = "https://doi.org/10.5281/zenodo.5607185",
#     source_version = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")$doi_url,
#     source_date = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")[["metadata"]][["publication_date"]],
#     organism = "Life"
#   )
# )
targets::tar_make(names = starts_with("library_spectra_is_lotus_prepared"))

end <- Sys.time()

log_debug("Script finished in", crayon::green(format(end - start)))
