# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(format = "rds")

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
## Generic paths and params
target_paths <- tar_target(name = paths,
                           command = {
                             parse_yaml_paths()
                           })
target_params <- tar_target(name = params,
                            command = {
                              source(file = "inst/scripts/prepare_params.R")
                            })

target_params_fake_edges <-
  tar_target(name = params_fake_edges,
             command = {
               get_params(step = "fake_edges")
             })
target_params_fake_features_components <-
  tar_target(name = params_fake_features_components,
             command = {
               get_params(step = "fake_features_components")
             })
target_params_prepare_adducts <-
  tar_target(name = params_prepare_adducts,
             command = {
               get_params(step = "prepare_adducts")
             })
target_params_prepare_closed <-
  tar_target(name = params_prepare_closed,
             command = {
               get_params(step = "prepare_closed")
             })
target_params_prepare_edges <-
  tar_target(name = params_prepare_edges,
             command = {
               get_params(step = "prepare_edges")
             })
target_params_prepare_features_classification <-
  tar_target(name = params_prepare_features_classification,
             command = {
               get_params(step = "prepare_features_classification")
             })
target_params_prepare_features_components <-
  tar_target(name = params_prepare_features_components,
             command = {
               get_params(step = "prepare_features_components")
             })
target_params_prepare_gnps <-
  tar_target(name = params_prepare_gnps,
             command = {
               get_params(step = "prepare_gnps")
             })
target_params_prepare_library <-
  tar_target(name = params_prepare_library,
             command = {
               get_params(step = "prepare_library")
             })
target_params_prepare_sirius <-
  tar_target(name = params_prepare_sirius,
             command = {
               get_params(step = "prepare_sirius")
             })
target_params_prepare_spectral_matches <-
  tar_target(name = params_prepare_spectral_matches,
             command = {
               get_params(step = "prepare_spectral_matches")
             })
target_params_prepare_taxa <-
  tar_target(name = params_prepare_taxa,
             command = {
               get_params(step = "prepare_taxa")
             })
target_params_process_annotations <-
  tar_target(name = params_process_annotations,
             command = {
               get_params(step = "process_annotations")
             })
target_params_process_spectra <-
  tar_target(name = params_process_spectra,
             command = {
               get_params(step = "process_spectra")
             })

## 1st level targets
# target_fake_edges <- tar_target(name = fake_edges,
#                                 command = {
#                                   source(file = "inst/scripts/fake_edges.R")
#                                 })
# target_fake_features_components <-
#   tar_target(name = fake_features_components,
#              command = {
#                source(file = "inst/scripts/fake_features_components.R")
#              })
# target_get_benchmark <- tar_target(name = get_benchmark,
#                                    command = {
#                                      source(file = "inst/scripts/get_benchmark.R")
#                                    })
# target_get_example_features <-
#   tar_target(name = get_example_features,
#              command = {
#                source(file = "inst/scripts/get_example_features.R")
#              })
# target_get_example_sirius <-
#   tar_target(name = get_example_sirius,
#              command = {
#                source(file = "inst/scripts/get_example_sirius.R")
#              })
# target_get_example_spectra <-
#   tar_target(name = get_example_spectra,
#              command = {
#                source(file = "inst/scripts/get_example_spectra.R")
#              })
# target_get_hmdb <- tar_target(name = get_hmdb,
#                               command = {
#                                 source(file = "inst/scripts/get_hmdb.R")
#                               })
# target_get_isdb_hmdb <- tar_target(name = get_isdb_hmdb,
#                                    command = {
#                                      source(file = "inst/scripts/get_isdb_hmdb.R")
#                                    })
target_get_isdb_lotus <- tar_target(name = get_isdb_lotus,
                                    command = {
                                      source(file = "inst/scripts/get_isdb_lotus.R")
                                    })
target_get_lotus <- tar_target(name = get_lotus,
                               command = {
                                 source(file = "inst/scripts/get_lotus.R")
                               })
target_prepare_adducts <- tar_target(name = prepare_adducts,
                                     command = {
                                       source(file = "inst/scripts/prepare_adducts.R")
                                     })
target_prepare_closed <- tar_target(name = prepare_closed,
                                    command = {
                                      source(file = "inst/scripts/prepare_closed.R")
                                    })
target_prepare_edges <- tar_target(name = prepare_edges,
                                   command = {
                                     source(file = "inst/scripts/prepare_edges.R")
                                   })
target_prepare_features_classification <-
  tar_target(name = prepare_features_classification,
             command = {
               source(file = "inst/scripts/prepare_features_classification.R")
             })
target_prepare_features_components <-
  tar_target(name = prepare_features_components,
             command = {
               source(file = "inst/scripts/prepare_features_components.R")
             })
# target_prepare_gnps <- tar_target(name = prepare_gnps,
#                                   command = {
#                                     source(file = "inst/scripts/prepare_gnps.R")
#                                   })
# target_prepare_hmdb <- tar_target(name = prepare_hmdb,
#                                   command = {
#                                     source(file = "inst/scripts/prepare_hmdb.R")
#                                   })
# target_prepare_isdb_hmdb <- tar_target(name = prepare_isdb_hmdb,
#                                        command = {
#                                          source(file = "inst/scripts/prepare_isdb_hmdb.R")
#                                        })
target_prepare_isdb_lotus <- tar_target(name = prepare_isdb_lotus,
                                        command = {
                                          source(file = "inst/scripts/prepare_isdb_lotus.R")
                                        })
target_prepare_library <- tar_target(name = prepare_library,
                                     command = {
                                       source(file = "inst/scripts/prepare_library.R")
                                     })
target_prepare_lotus <- tar_target(name = prepare_lotus,
                                   command = {
                                     source(file = "inst/scripts/prepare_lotus.R")
                                   })
# target_prepare_mona <- tar_target(name = prepare_mona,
#                                   command = {
#                                     source(file = "inst/scripts/prepare_mona.R")
#                                   })
# target_prepare_sirius <- tar_target(name = prepare_sirius,
#                                   command = {
#                                     source(file = "inst/scripts/prepare_sirius.R")
#                                   })
target_prepare_spectral_matches <-
  tar_target(name = prepare_spectral_matches,
             command = {
               source(file = "inst/scripts/prepare_spectral_matches.R")
             })
target_prepare_taxa <- tar_target(name = prepare_taxa,
                                  command = {
                                    source(file = "inst/scripts/prepare_taxa.R")
                                  })
target_process_annotations <- tar_target(name = process_annotations,
                                         command = {
                                           source(file = "inst/scripts/process_annotations.R")
                                         })
target_process_spectra <- tar_target(name = process_spectra,
                                     command = {
                                       source(file = "inst/scripts/process_spectra.R")
                                     })

## 2nd level targets
# target_fake_edges_2 <-
#   tar_combine(target_fake_edges, target_params_fake_edges, target_paths)
# target_fake_features_components_2 <-
#   tar_combine(
#     target_fake_features_components,
#     target_params_fake_features_components,
#     target_paths
#   )
target_get_benchmark_2 <-
  tar_combine(target_get_benchmark,
              target_paths)
target_get_example_features_2 <-
  tar_combine(target_get_example_features,
              target_paths)
target_get_example_sirius_2 <-
  tar_combine(target_get_example_sirius,
              target_paths)
target_get_example_spectra_2 <-
  tar_combine(target_get_example_spectra,
              target_paths)
target_get_hmdb_2 <-
  tar_combine(target_get_hmdb,
              target_paths)
target_get_isdb_hmdb_2 <-
  tar_combine(target_get_isdb_hmdb,
              target_paths)
target_get_isdb_lotus_2 <-
  tar_combine(target_get_isdb_lotus,
              target_paths)
target_get_lotus_2 <-
  tar_combine(target_get_lotus,
              target_paths)
target_prepare_closed_2 <-
  tar_combine(target_prepare_closed,
              target_params_prepare_closed,
              target_paths)
target_prepare_edges_2 <-
  tar_combine(target_prepare_edges,
              target_params_prepare_edges,
              target_paths)
# target_prepare_gnps_2 <-
#   tar_combine(
#     target_prepare_gnps,
#     target_params_prepare_gnps,
#     target_paths
#   )
target_prepare_sirius_2 <-
  tar_combine(target_prepare_sirius,
              target_params_prepare_sirius,
              target_paths)
target_prepare_taxa_2 <-
  tar_combine(target_prepare_taxa,
              target_params_prepare_taxa,
              target_paths)

## 3rd level targets
target_prepare_library_3 <-
  tar_combine(
    target_prepare_library,
    target_prepare_closed_2,
    target_prepare_lotus,
    target_params_prepare_library,
    target_paths
  )

## 4th level targets
target_prepare_adducts_4 <-
  tar_combine(
    target_prepare_adducts,
    target_params_prepare_adducts,
    target_prepare_library_3,
    target_paths
  )
target_process_spectra_4 <-
  tar_combine(
    target_process_spectra,
    target_params_process_spectra,
    target_prepare_isdb_lotus,
    target_paths
  )

## 5th level targets
target_prepare_spectral_matches_5 <-
  tar_combine(
    target_prepare_spectral_matches,
    target_params_prepare_spectral_matches,
    target_process_spectra_4,
    # mona,
    # isdb,
    target_paths
  )

## 6th level targets
target_prepare_features_components_6 <-
  tar_combine(
    target_prepare_features_components,
    target_params_prepare_features_components,
    target_prepare_spectral_matches_5,
    # sirius,
    # gnps,
    target_paths
  )

## 7th level targets
target_prepare_features_classification_7 <-
  tar_combine(
    target_prepare_features_classification,
    target_params_prepare_features_classification,
    target_prepare_features_components_6,
    target_prepare_library_3,
    target_paths
  )

## 8th level targets
target_process_annotations_8 <-
  tar_combine(
    target_process_annotations,
    target_params_process_annotations,
    target_prepare_features_classification_7,
    target_prepare_adducts_4,
    target_prepare_library_3,
    target_prepare_edges_2,
    target_prepare_taxa_2,
    target_paths
  )

## Final targets
list(
  target_paths,
  target_params,
  target_params_fake_edges,
  target_params_fake_features_components,
  target_params_prepare_adducts,
  target_params_prepare_closed,
  target_params_prepare_edges,
  target_params_prepare_features_classification,
  target_params_prepare_features_components,
  target_params_prepare_gnps,
  target_params_prepare_library,
  target_params_prepare_sirius,
  target_params_prepare_spectral_matches,
  target_params_prepare_taxa,
  target_params_process_annotations,
  target_params_process_spectra,
  # target_fake_edges,
  # target_fake_features_components,
  # target_get_benchmark,
  # target_get_example_features,
  # target_get_example_sirius,
  # target_get_example_spectra,
  # target_get_hmdb,
  # target_get_isdb_hmdb,
  target_get_isdb_lotus,
  target_get_lotus,
  target_prepare_adducts,
  target_prepare_closed,
  target_prepare_edges,
  target_prepare_features_classification,
  target_prepare_features_components,
  # target_prepare_gnps,
  # target_prepare_hmdb,
  # target_prepare_isdb_hmdb,
  target_prepare_isdb_lotus,
  target_prepare_library,
  target_prepare_lotus,
  # target_prepare_mona,
  # target_prepare_sirius,
  target_prepare_spectral_matches,
  target_prepare_taxa,
  target_process_annotations,
  target_process_spectra,
  # target_fake_edges_2,
  # target_fake_features_components_2,
  # target_get_benchmark_2,
  # target_get_example_features_2,
  # target_get_example_sirius_2,
  # target_get_example_spectra_2,
  # target_get_hmdb_2,
  # target_get_isdb_hmdb_2,
  target_get_isdb_lotus_2,
  target_get_lotus_2,
  target_prepare_closed_2,
  target_prepare_edges_2,
  # target_prepare_gnps_2,
  # target_prepare_sirius_2,
  target_prepare_taxa_2,
  target_prepare_library_3,
  target_prepare_adducts_4,
  target_process_spectra_4,
  target_prepare_spectral_matches_5,
  target_prepare_features_components_6,
  target_prepare_features_classification_7,
  target_process_annotations_8
)
