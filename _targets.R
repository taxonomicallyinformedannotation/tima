# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(format = "rds")

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
list(
  tar_target(name = parse_yaml_paths,
             command = {
               parse_yaml_paths()
             }),
  tar_target(name = prepare_params,
             command = {
               source(file = "inst/scripts/prepare_params.R")
             }),
  tar_target(name = get_lotus,
             command = {
               source(file = "inst/scripts/get_lotus.R")
             }),
  tar_target(name = prepare_lotus,
             command = {
               source(file = "inst/scripts/prepare_lotus.R")
             }),
  tar_target(name = prepare_library,
             command = {
               source(file = "inst/scripts/prepare_library.R")
             }),
  tar_target(name = prepare_adducts,
             command = {
               source(file = "inst/scripts/prepare_adducts.R")
             }),
  tar_target(name = get_isdb_lotus,
             command = {
               source(file = "inst/scripts/get_isdb_lotus.R")
             }),
  tar_target(name = prepare_isdb_lotus,
             command = {
               source(file = "inst/scripts/prepare_isdb_lotus.R")
             }),
  tar_target(name = process_spectra,
             command = {
               source(file = "inst/scripts/process_spectra.R")
             }),
  tar_target(name = prepare_spectral_matches,
             command = {
               source(file = "inst/scripts/prepare_spectral_matches.R")
             }),
  tar_target(name = prepare_edges,
             command = {
               source(file = "inst/scripts/prepare_edges.R")
             }),
  tar_target(name = prepare_features_components,
             command = {
               source(file = "inst/scripts/prepare_features_components.R")
             }),
  tar_target(name = prepare_features_classification,
             command = {
               source(file = "inst/scripts/prepare_features_classification.R")
             }),
  tar_target(name = prepare_taxa,
             command = {
               source(file = "inst/scripts/prepare_taxa.R")
             }),
  tar_target(name = process_annotations,
             command = {
               source(file = "inst/scripts/process_annotations.R")
             })
)
