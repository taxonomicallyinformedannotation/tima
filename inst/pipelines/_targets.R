# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = "timaR",
  memory = "transient",
  garbage_collection = TRUE,
  resources = tar_resources(
    qs = tar_resources_qs(preset = "fast")
  )
)

# tar_make_clustermq() configuration (okay to leave alone):

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}}
# to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
list(
  ## Architecture
  list( ## Paths
    list(
      tar_target(
        name = yaml_paths,
        format = "file",
        command = {
          yaml_paths <- system.file("extdata", "paths.yaml", package = "timaR")
        }
      ),
      tar_target(
        name = paths,
        command = {
          paths <- parse_yaml_paths(file = yaml_paths)
        }
      ),
      # tar_target(
      #   name = paths_gnps_example_id,
      #   command = {
      #     paths_gnps_example_id <- paths$gnps$example
      #   }
      # ),
      tar_target(
        name = paths_source,
        command = {
          paths_source <- paths$data$source$path
        }
      ),
      tar_target(
        name = paths_interim_a,
        command = {
          paths_interim_a <- paths$data$interim$annotations$path
        }
      ),
      tar_target(
        name = paths_interim_f,
        command = {
          paths_interim_f <- paths$data$interim$features$path
        }
      ),
      tar_target(
        name = paths_test_mode,
        command = {
          paths_test_mode <- paths$tests$mode
        }
      ),
      tar_target(
        name = paths_urls_massbank_file,
        command = {
          paths_urls_massbank_file <- paths$urls$massbank$file
        }
      ),
      tar_target(
        name = paths_urls_massbank_url,
        command = {
          paths_urls_massbank_url <- paths$urls$massbank$url
        }
      ),
      tar_target(
        name = paths_urls_massbank_version,
        command = {
          paths_urls_massbank_version <- paths$urls$massbank$version
        }
      ),
      tar_target(
        name = paths_urls_examples_spectra_mini,
        command = {
          paths_urls_examples_spectra_mini <- paths$urls$examples$spectra_mini
        }
      ),
      tar_target(
        name = paths_data_source_spectra,
        command = {
          paths_data_source_spectra <- paths$data$source$spectra
        }
      ),
      tar_target(
        name = paths_urls_ecmdb_metabolites,
        command = {
          paths_urls_ecmdb_metabolites <- paths$urls$ecmdb$metabolites
        }
      ),
      tar_target(
        name = paths_data_source_libraries_sop_ecmdb,
        command = {
          paths_data_source_libraries_sop_ecmdb <-
            paths$data$source$libraries$sop$ecmdb
        }
      ),
      tar_target(
        name = paths_urls_hmdb_structures,
        command = {
          paths_urls_hmdb_structures <- paths$urls$hmdb$structures
        }
      ),
      tar_target(
        name = paths_data_source_libraries_sop_hmdb,
        command = {
          paths_data_source_libraries_sop_hmdb <-
            paths$data$source$libraries$sop$hmdb
        }
      ),
      tar_target(
        name = paths_urls_lotus_doi,
        command = {
          paths_urls_lotus_doi <- paths$urls$lotus$doi
        }
      ),
      tar_target(
        name = paths_urls_lotus_pattern,
        command = {
          paths_urls_lotus_pattern <- paths$urls$lotus$pattern
        }
      ),
      tar_target(
        name = paths_data_source_libraries_sop_lotus,
        command = {
          paths_data_source_libraries_sop_lotus <-
            paths$data$source$libraries$sop$lotus
        }
      ),
      tar_target(
        name = paths_urls_examples_spectral_lib_pos,
        command = {
          paths_urls_examples_spectral_lib_pos <-
            paths$urls$examples$spectral_lib$pos
        }
      ),
      tar_target(
        name = paths_data_source_libraries_spectra_is_lotus_pos,
        command = {
          paths_data_source_libraries_spectra_is_lotus_pos <-
            paths$data$source$libraries$spectra$is$lotus$pos
        }
      ),
      tar_target(
        name = paths_urls_examples_spectral_lib_neg,
        command = {
          paths_urls_examples_spectral_lib_neg <-
            paths$urls$examples$spectral_lib$neg
        }
      ),
      tar_target(
        name = paths_data_source_libraries_spectra_is_lotus_neg,
        command = {
          paths_data_source_libraries_spectra_is_lotus_neg <-
            paths$data$source$libraries$spectra$is$lotus$neg
        }
      ),
      tar_target(
        name = paths_data_interim_libraries_adducts_path,
        command = {
          paths_data_interim_libraries_adducts_path <-
            paths$data$interim$libraries$adducts$path
        }
      ),
      tar_target(
        name = paths_urls_benchmarking_set,
        command = {
          paths_urls_benchmarking_set <-
            paths$urls$benchmarking_set
        }
      ),
      tar_target(
        name = paths_data_source_benchmark_zip,
        command = {
          paths_data_source_benchmark_zip <-
            paths$data$source$benchmark$zip
        }
      ),
      tar_target(
        name = paths_data_source_benchmark_cleaned,
        command = {
          paths_data_source_benchmark_cleaned <-
            paths$data$source$benchmark$cleaned
        }
      ),
      tar_target(
        name = paths_data_source_benchmark_mgf_neg,
        command = {
          paths_data_source_benchmark_mgf_neg <-
            paths$data$source$benchmark$mgf$neg
        }
      ),
      tar_target(
        name = paths_data_source_benchmark_mgf_pos,
        command = {
          paths_data_source_benchmark_mgf_pos <-
            paths$data$source$benchmark$mgf$pos
        }
      )
    )
  ),
  ## Params
  list(
    ## Default
    list(
      tar_target(
        name = par_def_ann_mas,
        format = "file",
        command = {
          par_def_ann_mas <- paths$params$default$annotate$masses
        }
      ),
      tar_target(
        name = par_def_ann_spe,
        format = "file",
        command = {
          par_def_ann_spe <- paths$params$default$annotate$spectra
        }
      ),
      tar_target(
        name = par_def_cre_com,
        format = "file",
        command = {
          par_def_cre_com <- paths$params$default$create$components
        }
      ),
      tar_target(
        name = par_def_cre_edg_spe,
        format = "file",
        command = {
          par_def_cre_edg_spe <- paths$params$default$create$edges$spectra
        }
      ),
      tar_target(
        name = par_def_fil_ann,
        format = "file",
        command = {
          par_def_fil_ann <-
            paths$params$default$filter$annotations
        }
      ),
      tar_target(
        name = par_def_pre_ann_gnp,
        format = "file",
        command = {
          par_def_pre_ann_gnp <- paths$params$default$prepare$annotations$gnps
        }
      ),
      tar_target(
        name = par_def_pre_ann_sir,
        format = "file",
        command = {
          par_def_pre_ann_sir <-
            paths$params$default$prepare$annotations$sirius
        }
      ),
      tar_target(
        name = par_def_pre_ann_spe,
        format = "file",
        command = {
          par_def_pre_ann_spe <-
            paths$params$default$prepare$annotations$spectra
        }
      ),
      tar_target(
        name = par_def_pre_fea_com,
        format = "file",
        command = {
          par_def_pre_fea_com <-
            paths$params$default$prepare$features$components
        }
      ),
      tar_target(
        name = par_def_pre_fea_edg,
        format = "file",
        command = {
          par_def_pre_fea_edg <- paths$params$default$prepare$features$edges
        }
      ),
      tar_target(
        name = par_def_pre_fea_tab,
        format = "file",
        command = {
          par_def_pre_fea_tab <- paths$params$default$prepare$features$tables
        }
      ),
      tar_target(
        name = par_def_pre_lib_rt,
        format = "file",
        command = {
          par_def_pre_lib_rt <-
            paths$params$default$prepare$libraries$rt
        }
      ),
      tar_target(
        name = par_def_pre_lib_sop_clo,
        format = "file",
        command = {
          par_def_pre_lib_sop_clo <-
            paths$params$default$prepare$libraries$sop$closed
        }
      ),
      tar_target(
        name = par_def_pre_lib_sop_ecm,
        format = "file",
        command = {
          par_def_pre_lib_sop_ecm <-
            paths$params$default$prepare$libraries$sop$ecmdb
        }
      ),
      tar_target(
        name = par_def_pre_lib_sop_hmd,
        format = "file",
        command = {
          par_def_pre_lib_sop_hmd <-
            paths$params$default$prepare$libraries$sop$hmdb
        }
      ),
      tar_target(
        name = par_def_pre_lib_sop_lot,
        format = "file",
        command = {
          par_def_pre_lib_sop_lot <-
            paths$params$default$prepare$libraries$sop$lotus
        }
      ),
      tar_target(
        name = par_def_pre_lib_sop_mer,
        format = "file",
        command = {
          par_def_pre_lib_sop_mer <-
            paths$params$default$prepare$libraries$sop$merged
        }
      ),
      tar_target(
        name = par_def_pre_lib_spe,
        format = "file",
        command = {
          par_def_pre_lib_spe <-
            paths$params$default$prepare$libraries$spectra
        }
      ),
      tar_target(
        name = par_def_pre_tax,
        format = "file",
        command = {
          par_def_pre_tax <- paths$params$default$prepare$taxa
        }
      ),
      tar_target(
        name = par_def_wei_ann,
        format = "file",
        command = {
          par_def_wei_ann <- paths$params$default$weight$annotations
        }
      )
    ),
    list(
      ## Prepare params
      list(
        tar_target(
          name = par_pre_par,
          format = "file",
          command = {
            par_pre_par <- paths$params$prepare_params
          }
        ),
        tar_target(
          name = par_pre_par2,
          format = "file",
          command = {
            par_pre_par2 <- paths$params$prepare_params_advanced
          }
        ),
        tar_target(
          name = par_fin_par,
          command = {
            par_fin_par <- parse_yaml_params(
              def = par_pre_par,
              usr = par_pre_par
            )
          }
        ),
        tar_target(
          name = par_fin_par2,
          command = {
            par_fin_par2 <- parse_yaml_params(
              def = par_pre_par2,
              usr = par_pre_par2
            )
          }
        )
      ),
      ## User
      list(
        tar_target(
          name = par_usr_ann_mas,
          format = "file",
          command = {
            par_usr_ann_mas <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "annotate_masses"
              )
          }
        ),
        tar_target(
          name = par_usr_ann_spe,
          format = "file",
          command = {
            par_usr_ann_spe <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "annotate_spectra"
              )
          }
        ),
        tar_target(
          name = par_usr_cre_com,
          format = "file",
          command = {
            par_usr_cre_com <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "create_components"
              )
          }
        ),
        tar_target(
          name = par_usr_fil_ann,
          format = "file",
          command = {
            par_usr_fil_ann <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "filter_annotations"
              )
          }
        ),
        tar_target(
          name = par_usr_cre_edg_spe,
          format = "file",
          command = {
            par_usr_cre_edg_spe <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "create_edges_spectra"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_ann_gnp,
          format = "file",
          command = {
            par_usr_pre_ann_gnp <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_annotations_gnps"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_ann_sir,
          format = "file",
          command = {
            par_usr_pre_ann_sir <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_annotations_sirius"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_ann_spe,
          format = "file",
          command = {
            par_usr_pre_ann_spe <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_annotations_spectra"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_fea_com,
          format = "file",
          command = {
            par_usr_pre_fea_com <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_features_components"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_fea_edg,
          format = "file",
          command = {
            par_usr_pre_fea_edg <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_features_edges"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_fea_tab,
          format = "file",
          command = {
            par_usr_pre_fea_tab <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_features_tables"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_lib_rt,
          format = "file",
          command = {
            par_usr_pre_lib_rt <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_rt"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_lib_sop_clo,
          format = "file",
          command = {
            par_usr_pre_lib_sop_clo <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_sop_closed"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_lib_sop_ecm,
          format = "file",
          command = {
            par_usr_pre_lib_sop_ecm <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_sop_ecmdb"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_lib_sop_hmd,
          format = "file",
          command = {
            par_usr_pre_lib_sop_hmd <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_sop_hmdb"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_lib_sop_lot,
          format = "file",
          command = {
            par_usr_pre_lib_sop_lot <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_sop_lotus"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_lib_sop_mer,
          format = "file",
          command = {
            par_usr_pre_lib_sop_mer <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_sop_merged"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_lib_spe,
          format = "file",
          command = {
            par_usr_pre_lib_spe <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_spectra"
              )
          }
        ),
        tar_target(
          name = par_usr_pre_tax,
          format = "file",
          command = {
            par_usr_pre_tax <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_taxa"
              )
          }
        ),
        tar_target(
          name = par_usr_wei_ann,
          format = "file",
          command = {
            par_usr_wei_ann <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "weight_annotations"
              )
          }
        )
      )
    ),
    ## Final
    list(
      tar_target(
        name = par_ann_mas,
        command = {
          par_ann_mas <-
            parse_yaml_params(
              def = par_def_ann_mas,
              usr = par_usr_ann_mas[1]
            )
        }
      ),
      tar_target(
        name = par_ann_spe,
        command = {
          par_ann_spe <-
            parse_yaml_params(
              def = par_def_ann_spe,
              usr = par_usr_ann_spe[1]
            )
        }
      ),
      tar_target(
        name = par_cre_com,
        command = {
          par_cre_com <-
            parse_yaml_params(
              def = par_def_cre_com,
              usr = par_usr_cre_com[1]
            )
        }
      ),
      tar_target(
        name = par_cre_edg_spe,
        command = {
          par_cre_edg_spe <-
            parse_yaml_params(
              def = par_def_cre_edg_spe,
              usr = par_usr_cre_edg_spe[1]
            )
        }
      ),
      tar_target(
        name = par_fil_ann,
        command = {
          par_fil_ann <-
            parse_yaml_params(
              def = par_def_fil_ann,
              usr = par_usr_fil_ann[1]
            )
        }
      ),
      tar_target(
        name = par_pre_ann_gnp,
        command = {
          par_pre_ann_gnp <-
            parse_yaml_params(
              def = par_def_pre_ann_gnp,
              usr = par_usr_pre_ann_gnp[1]
            )
        }
      ),
      tar_target(
        name = par_pre_ann_sir,
        command = {
          par_pre_ann_sir <-
            parse_yaml_params(
              def = par_def_pre_ann_sir,
              usr = par_usr_pre_ann_sir[1]
            )
        }
      ),
      tar_target(
        name = par_pre_ann_spe,
        command = {
          par_pre_ann_spe <-
            parse_yaml_params(
              def = par_def_pre_ann_spe,
              usr = par_usr_pre_ann_spe[1]
            )
        }
      ),
      tar_target(
        name = par_pre_fea_com,
        command = {
          par_pre_fea_com <-
            parse_yaml_params(
              def = par_def_pre_fea_com,
              usr = par_usr_pre_fea_com[1]
            )
        }
      ),
      tar_target(
        name = par_pre_fea_edg,
        command = {
          par_pre_fea_edg <-
            parse_yaml_params(
              def = par_def_pre_fea_edg,
              usr = par_usr_pre_fea_edg[1]
            )
        }
      ),
      tar_target(
        name = par_pre_fea_tab,
        command = {
          par_pre_fea_tab <-
            parse_yaml_params(
              def = par_def_pre_fea_tab,
              usr = par_usr_pre_fea_tab[1]
            )
        }
      ),
      tar_target(
        name = par_pre_lib_rt,
        command = {
          par_pre_lib_rt <-
            parse_yaml_params(
              def = par_def_pre_lib_rt,
              usr = par_usr_pre_lib_rt[1]
            )
        }
      ),
      tar_target(
        name = par_pre_lib_sop_clo,
        command = {
          par_pre_lib_sop_clo <-
            parse_yaml_params(
              def = par_def_pre_lib_sop_clo,
              usr = par_usr_pre_lib_sop_clo[1]
            )
        }
      ),
      tar_target(
        name = par_pre_lib_sop_ecm,
        command = {
          par_pre_lib_sop_ecm <-
            parse_yaml_params(
              def = par_def_pre_lib_sop_ecm,
              usr = par_usr_pre_lib_sop_ecm[1]
            )
        }
      ),
      tar_target(
        name = par_pre_lib_sop_hmd,
        command = {
          par_pre_lib_sop_hmd <-
            parse_yaml_params(
              def = par_def_pre_lib_sop_hmd,
              usr = par_usr_pre_lib_sop_hmd[1]
            )
        }
      ),
      tar_target(
        name = par_pre_lib_sop_lot,
        command = {
          par_pre_lib_sop_lot <-
            parse_yaml_params(
              def = par_def_pre_lib_sop_lot,
              usr = par_usr_pre_lib_sop_lot[1]
            )
        }
      ),
      tar_target(
        name = par_pre_lib_sop_mer,
        command = {
          par_pre_lib_sop_mer <-
            parse_yaml_params(
              def = par_def_pre_lib_sop_mer,
              usr = par_usr_pre_lib_sop_mer[1]
            )
        }
      ),
      tar_target(
        name = par_pre_lib_spe,
        command = {
          par_pre_lib_spe <-
            parse_yaml_params(
              def = par_def_pre_lib_spe,
              usr = par_usr_pre_lib_spe[1]
            )
        }
      ),
      tar_target(
        name = par_pre_tax,
        command = {
          par_pre_tax <-
            parse_yaml_params(
              def = par_def_pre_tax,
              usr = par_usr_pre_tax[1]
            )
        }
      ),
      tar_target(
        name = par_wei_ann,
        command = {
          par_wei_ann <-
            parse_yaml_params(
              def = par_def_wei_ann,
              usr = par_usr_wei_ann[1]
            )
        }
      )
    )
  ),
  # ## GNPS
  # list(
  #   tar_target(
  #     name = gnps_tables,
  #     format = "file",
  #     command = {
  #       gnps_tables <- get_gnps_tables(
  #         gnps_job_id = par_fin_par$gnps$id,
  #         gnps_job_example = paths_gnps_example_id,
  #         filename = par_fin_par$files$pattern,
  #         workflow = par_fin_par$gnps$workflow,
  #         path_features = par_pre_fea_tab$files$features$raw,
  #         path_metadata = par_pre_tax$files$metadata$raw,
  #         path_spectra = par_ann_spe$files$spectral$raw,
  #         path_source = paths_source,
  #         path_interim_a = paths_interim_a,
  #         path_interim_f = paths_interim_f
  #       )
  #     }
  #   ),
  #   list(
  #     tar_target(
  #       name = gnps_features,
  #       format = "file",
  #       command = {
  #         gnps_features <- gnps_tables[[1]]
  #       }
  #     ),
  #     tar_target(
  #       name = gnps_metadata,
  #       format = "file",
  #       command = {
  #         gnps_metadata <- gnps_tables[[2]]
  #       }
  #     ),
  #     tar_target(
  #       name = gnps_spectra,
  #       format = "file",
  #       command = {
  #         gnps_spectra <- gnps_tables[[3]]
  #       }
  #     ),
  #     # TODO think to change this also if uncommenting
  #     tar_target(
  #       name = gnps_annotations,
  #       format = "file",
  #       command = {
  #         gnps_annotations <- gnps_tables[[4]]
  #       }
  #     ),
  #     tar_target(
  #       name = gnps_components,
  #       format = "file",
  #       command = {
  #         gnps_components <- gnps_tables[[5]]
  #       }
  #     ),
  #     tar_target(
  #       name = gnps_edges,
  #       format = "file",
  #       command = {
  #         gnps_edges <- gnps_tables[[6]]
  #       }
  #     )
  #   )
  # ),
  ## Inputs
  list(
    tar_target(
      name = input_features,
      format = "file",
      command = {
        input_features <- par_pre_tax$files$features$raw
        # input_features <-
        #   ifelse(
        #     test = !is.null(gnps_features),
        #     yes = ifelse(test = file.exists(gnps_features),
        #       yes = gnps_features,
        #       no = par_pre_tax$files$features$raw
        #     ),
        #     no = par_pre_tax$files$features$raw
        #   )
      }
    ),
    tar_target(
      name = input_spectra,
      format = "file",
      command = {
        input_spectra <-
          ifelse(
            test = paths_test_mode == FALSE,
            yes = par_ann_spe$files$spectral$raw,
            # yes = ifelse(
            #   test = !is.null(gnps_spectra),
            #   yes =
            #     ifelse(
            #       test = file.exists(gnps_spectra),
            #       yes = gnps_spectra,
            #       no = par_ann_spe$files$spectral$raw
            #     ),
            #   no = par_ann_spe$files$spectral$raw
            # ),
            no = {
              get_file(
                url = paths_urls_examples_spectra_mini,
                export = paths_data_source_spectra
              )
            }
          )
      }
    ),
    tar_target(
      name = input_metadata,
      format = "file",
      command = {
        input_metadata <- par_pre_tax$files$metadata$raw
        # input_metadata <-
        #   ifelse(
        #     test = !is.null(gnps_metadata),
        #     yes = ifelse(test = file.exists(gnps_metadata),
        #       yes = gnps_metadata,
        #       no = par_pre_tax$files$metadata$raw
        #     ),
        #     no = par_pre_tax$files$metadata$raw
        #   )
      }
    )
  ),
  ## libraries
  list(
    ## Spectra
    list( ## In silico
      list( ## Raw
        list(
          ## TODO ADD ISDB HMDB,
          tar_target(
            name = lib_spe_is_lot_pos,
            format = "file",
            command = {
              lib_spe_is_lot_pos <-
                # if (paths_test_mode == FALSE) {
                get_file(
                  url = paths_urls_examples_spectral_lib_pos,
                  export = paths_data_source_libraries_spectra_is_lotus_pos |>
                    gsub(
                      pattern = "isdb_pos.mgf",
                      replacement = "lotus_pos.rds",
                      fixed = TRUE
                    )
                )
              # get_last_version_from_zenodo(
              #   doi = paths_urls_lotus_isdb_doi,
              #   pattern = paths_urls_lotus_isdb_pattern_pos,
              #   path = paths_data_source_libraries_spectra_is_lotus_pos
              # )
              # } else {
              #   get_file(
              #     url = paths_urls_examples_spectral_lib_pos,
              #     export = paths_data_source_libraries_spectra_is_lotus_pos
              #   )
              # }
            }
            ## To always check if a newest version is available
            ,
            cue = tar_cue(mode = "always")
            # cue = tar_cue(mode = "thorough")
          ),
          tar_target(
            name = lib_spe_is_lot_neg,
            format = "file",
            command = {
              lib_spe_is_lot_neg <-
                # if (paths_test_mode == FALSE) {
                get_file(
                  url = paths_urls_examples_spectral_lib_neg,
                  export = paths_data_source_libraries_spectra_is_lotus_neg |>
                    gsub(
                      pattern = "isdb_neg.mgf",
                      replacement = "lotus_neg.rds",
                      fixed = TRUE
                    )
                )
              # get_last_version_from_zenodo(
              #   doi = paths_urls_lotus_isdb_doi,
              #   pattern = paths_urls_lotus_isdb_pattern_neg,
              #   path = paths_data_source_libraries_spectra_is_lotus_neg
              # )
              # } else {
              #   get_file(
              #     url = paths_urls_examples_spectral_lib_neg,
              #     export = paths_data_source_libraries_spectra_is_lotus_neg
              #   )
              # }
            }
            ## To always check if a newest version is available
            ,
            cue = tar_cue(mode = "always")
            # cue = tar_cue(mode = "thorough")
          )
        )
      ),
      ## Prepared
      list(
        ## TODO ADD IS HMDB PREPARED,
        tar_target(
          name = lib_spe_is_lot_pre_pos,
          format = "file",
          command = {
            lib_spe_is_lot_pre_pos <-
              lib_spe_is_lot_pos
          }
        ),
        tar_target(
          name = lib_spe_is_lot_pre_neg,
          format = "file",
          command = {
            lib_spe_is_lot_pre_neg <-
              lib_spe_is_lot_neg
          }
        )
      ),
      ## Experimental
      list(
        ## RAW
        list(
          ### Internal
          ## This does not work as it forces the file to exist.
          ## So targets will not check if the input file changed automatically.
          # tar_target(
          #   name = lib_spe_exp_int_raw,
          # .  format = "file",
          #   command = {
          #     lib_spe_exp_int_raw <-
          #       par_pre_lib_spe$files$libraries$spectral$exp$raw
          #   }
          # ),
          ### MassBank
          tar_target(
            name = lib_spe_exp_mb_raw,
            format = "file",
            command = {
              lib_spe_exp_mb_raw <- get_massbank_spectra(
                mb_file = paths_urls_massbank_file,
                mb_url = paths_urls_massbank_url,
                mb_version = paths_urls_massbank_version
              )
            }
          )
        ),
        ## Prepared
        list(
          tar_target(
            name = lib_spe_exp_int_pre,
            format = "file",
            command = {
              lib_spe_exp_int_pre <-
                prepare_libraries_spectra(
                  input = par_pre_lib_spe$files$libraries$spectral$exp$raw,
                  output_pos = "data/interim/libraries/spectra/exp/internal_pos.rds",
                  output_neg = "data/interim/libraries/spectra/exp/internal_neg.rds",
                  output_sop = "data/interim/libraries/sop/spectral_int_prepared.tsv.gz",
                  metad = "InternalLib",
                  col_ad = NULL,
                  col_ce = NULL,
                  col_ci = "FILENAME",
                  col_em = "EXACTMASS",
                  col_in = "INCHI",
                  col_io = NULL,
                  col_ik = NULL,
                  col_il = NULL,
                  col_mf = NULL,
                  col_na = "NAME",
                  col_po = "IONMODE",
                  col_sm = "SMILES",
                  col_sn = NULL,
                  col_si = "SPECTRUMID",
                  col_sp = NULL,
                  col_sy = NULL,
                  col_xl = NULL
                )
            }
          ),
          tar_target(
            name = lib_spe_exp_int_pre_pos,
            format = "file",
            command = {
              lib_spe_exp_int_pre_pos <- lib_spe_exp_int_pre[[1]]
            }
          ),
          tar_target(
            name = lib_spe_exp_int_pre_neg,
            format = "file",
            command = {
              lib_spe_exp_int_pre_neg <- lib_spe_exp_int_pre[[2]]
            }
          ),
          tar_target(
            name = lib_spe_exp_int_pre_sop,
            format = "file",
            command = {
              lib_spe_exp_int_pre_sop <- lib_spe_exp_int_pre[[3]]
            }
          ),
          tar_target(
            name = lib_spe_exp_mb_pre,
            format = "file",
            command = {
              lib_spe_exp_mb_pre <-
                prepare_libraries_spectra(
                  input = lib_spe_exp_mb_raw,
                  output_pos = "data/interim/libraries/spectra/exp/massbank_pos.rds",
                  output_neg = "data/interim/libraries/spectra/exp/massbank_neg.rds",
                  output_sop = "data/interim/libraries/sop/spectral_mb_prepared.tsv.gz",
                  metad = paste("MassBank",
                    paths_urls_massbank_version,
                    sep = " - "
                  ),
                  col_ad = "Precursor_type",
                  col_ce = "Collision_energy",
                  col_ci = NULL,
                  col_em = "ExactMass",
                  col_in = "InChI",
                  col_io = NULL,
                  col_ik = "InChIKey",
                  col_il = NULL,
                  col_mf = "Formula",
                  col_na = "Name",
                  col_po = "Ion_mode",
                  col_sm = "smiles",
                  col_sn = NULL,
                  col_si = "accession",
                  col_sp = "Splash",
                  col_sy = "Synon",
                  col_xl = NULL
                )
            }
          ),
          tar_target(
            name = lib_spe_exp_mb_pre_pos,
            format = "file",
            command = {
              lib_spe_exp_mb_pre_pos <- lib_spe_exp_mb_pre[[1]]
            }
          ),
          tar_target(
            name = lib_spe_exp_mb_pre_neg,
            format = "file",
            command = {
              lib_spe_exp_mb_pre_neg <- lib_spe_exp_mb_pre[[2]]
            }
          ),
          tar_target(
            name = lib_spe_exp_mb_pre_sop,
            format = "file",
            command = {
              lib_spe_exp_mb_pre_sop <- lib_spe_exp_mb_pre[[3]]
            }
          )
        )
      )
    ),
    ## Retention times
    list(tar_target(
      name = lib_rt,
      format = "file",
      command = {
        lib_rt <- prepare_libraries_rt(
          mgf_exp = list(
            lib_spe_exp_int_pre_neg,
            lib_spe_exp_int_pre_pos
          ),
          mgf_is = list(
            lib_spe_is_lot_pre_neg,
            lib_spe_is_lot_pre_pos
          ),
          temp_exp = par_pre_lib_rt$files$libraries$temporal$exp$csv,
          temp_is = par_pre_lib_rt$files$libraries$temporal$is$csv,
          output_rt = par_pre_lib_rt$files$libraries$temporal$prepared,
          output_sop = par_pre_lib_rt$files$libraries$sop$prepared$rt,
          col_ik = par_pre_lib_rt$names$mgf$inchikey,
          col_rt = par_pre_lib_rt$names$mgf$retention_time,
          col_sm = par_pre_lib_rt$names$mgf$smiles,
          name_inchikey = par_pre_lib_rt$names$inchikey,
          name_rt = par_pre_lib_rt$names$rt$library,
          name_smiles = par_pre_lib_rt$names$smiles,
          unit_rt = par_pre_lib_rt$units$rt
        )
      }
    )),
    tar_target(
      name = lib_rt_rts,
      format = "file",
      command = {
        lib_rt_rts <- lib_rt[[1]]
      }
    ),
    tar_target(
      name = lib_rt_sop,
      format = "file",
      command = {
        lib_rt_sop <- lib_rt[[2]]
      }
    ),
    ## Structure organism pairs
    list(
      ## Raw
      list(
        ## This does not work as it forces the file to exist.
        ## So targets will not check if the input file changed automatically.
        # tar_target(
        #   name = lib_sop_clo,
        #   format = "file",
        #   command = {
        #     lib_sop_clo <- paths$data$source$libraries$sop$closed
        #   }
        # ),
        tar_target(
          name = lib_sop_ecm,
          format = "file",
          command = {
            ## Because ECMDB certificate is expired
            lib_sop_ecm <- tryCatch(
              expr = {
                get_file(
                  url = paths_urls_ecmdb_metabolites,
                  export = paths_data_source_libraries_sop_ecmdb
                )
              },
              error = function(e) {
                return(fake_ecmdb(export = paths_data_source_libraries_sop_ecmdb))
              }, finally = {
                return(paths_data_source_libraries_sop_ecmdb)
              }
            )
          }
        ),
        tar_target(
          name = lib_sop_hmd,
          format = "file",
          command = {
            lib_sop_hmd <- tryCatch(
              expr = {
                get_file(
                  url = paths_urls_hmdb_structures,
                  export = paths_data_source_libraries_sop_hmdb
                )
              },
              warning = function(w) {
                ## See #118
                log_debug("HMDB download failed partially, returning empty file instead")
                unlink(paths_data_source_libraries_sop_hmdb)
                return(fake_hmdb(export = paths_data_source_libraries_sop_hmdb))
              },
              error = function(e) {
                return(fake_hmdb(export = paths_data_source_libraries_sop_hmdb))
              }, finally = {
                return(paths_data_source_libraries_sop_hmdb)
              }
            )
          }
        ),
        ## TODO ADD GET HMDB
        tar_target(
          name = lib_sop_lot,
          format = "file",
          command = {
            lib_sop_lot <- tryCatch(
              expr = {
                get_last_version_from_zenodo(
                  doi = paths_urls_lotus_doi,
                  pattern = paths_urls_lotus_pattern,
                  path = paths_data_source_libraries_sop_lotus
                )
              },
              error = function(e) {
                return(fake_lotus(export = paths_data_source_libraries_sop_lotus))
              }, finally = {
                return(paths_data_source_libraries_sop_lotus)
              }
            )
          },
          ## To always check if a newest version is available
          cue = tar_cue(mode = "always")
          # cue = tar_cue(mode = "thorough")
        )
      ),
      ## Prepared
      list(
        tar_target(
          name = lib_sop_clo_pre,
          format = "file",
          command = {
            lib_sop_clo_pre <-
              prepare_libraries_sop_closed(
                input = par_pre_lib_sop_clo$files$libraries$sop$raw$closed,
                output = par_pre_lib_sop_clo$files$libraries$sop$prepared$closed
              )
          }
        ),
        tar_target(
          name = lib_sop_ecm_pre,
          format = "file",
          command = {
            lib_sop_ecm_pre <-
              prepare_libraries_sop_ecmdb(
                input = lib_sop_ecm,
                output = par_pre_lib_sop_ecm$files$libraries$sop$prepared$ecmdb
              )
          }
        ),
        tar_target(
          name = lib_sop_hmd_pre,
          format = "file",
          command = {
            lib_sop_hmd_pre <-
              prepare_libraries_sop_hmdb(
                input = lib_sop_hmd,
                output = par_pre_lib_sop_hmd$files$libraries$sop$prepared$hmdb
              )
          }
        ),
        tar_target(
          name = lib_sop_lot_pre,
          format = "file",
          command = {
            lib_sop_lot_pre <-
              prepare_libraries_sop_lotus(
                input = if (paths_test_mode == FALSE) {
                  lib_sop_lot
                } else {
                  paths_data_source_libraries_sop_lotus
                },
                output = par_pre_lib_sop_lot$files$libraries$sop$prepared$lotus
              )
          }
        )
      ),
      ## Merged
      list(
        tar_target(
          name = lib_sop_mer,
          format = "file",
          command = {
            lib_sop_mer <- prepare_libraries_sop_merged(
              files = c(
                lib_sop_clo_pre,
                lib_sop_ecm_pre,
                lib_sop_hmd_pre,
                lib_sop_lot_pre,
                lib_rt_sop,
                lib_spe_exp_int_pre_sop,
                lib_spe_exp_mb_pre_sop
              ),
              filter = par_pre_lib_sop_mer$organisms$filter$mode,
              level = par_pre_lib_sop_mer$organisms$filter$level,
              value = par_pre_lib_sop_mer$organisms$filter$value,
              output_key = par_pre_lib_sop_mer$files$libraries$sop$merged$keys,
              output_org_tax_ott = par_pre_lib_sop_mer$files$libraries$sop$merged$organisms$taxonomies$ott,
              output_str_stereo = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$stereo,
              output_str_met = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$metadata,
              output_str_nam = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$names,
              output_str_tax_cla = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$taxonomies$cla,
              output_str_tax_npc = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$taxonomies$npc
            )
          }
        ),
        tar_target(
          name = lib_mer_key,
          format = "file", command = {
            lib_mer_key <- lib_sop_mer[[1]]
          }
        ),
        tar_target(
          name = lib_mer_org_tax_ott,
          format = "file", command = {
            lib_mer_org_tax_ott <- lib_sop_mer[[2]]
          }
        ),
        tar_target(
          name = lib_mer_str_stereo,
          format = "file", command = {
            lib_mer_str_stereo <- lib_sop_mer[[3]]
          }
        ),
        tar_target(
          name = lib_mer_str_met,
          format = "file", command = {
            lib_mer_str_met <- lib_sop_mer[[4]]
          }
        ),
        tar_target(
          name = lib_mer_str_nam,
          format = "file", command = {
            lib_mer_str_nam <- lib_sop_mer[[5]]
          }
        ),
        tar_target(
          name = lib_mer_str_tax_cla,
          format = "file", command = {
            lib_mer_str_tax_cla <- lib_sop_mer[[6]]
          }
        ),
        tar_target(
          name = lib_mer_str_tax_npc,
          format = "file", command = {
            lib_mer_str_tax_npc <- lib_sop_mer[[7]]
          }
        )
      )
    )
  ),
  ## Annotations
  list(
    ## MS1
    list(
      tar_target(
        name = ann_ms1_pre,
        format = "file",
        command = {
          ann_ms1_pre <-
            annotate_masses(
              features = fea_pre,
              filter_nitro = par_ann_mas$options$nitrogen_rule,
              library = lib_mer_key,
              output_annotations = par_ann_mas$files$annotations$prepared$structural$ms1,
              output_edges = par_ann_mas$files$networks$spectral$edges$raw,
              name_source = par_ann_mas$names$source,
              name_target = par_ann_mas$names$target,
              str_stereo = lib_mer_str_stereo,
              str_met = lib_mer_str_met,
              str_nam = lib_mer_str_nam,
              str_tax_cla = lib_mer_str_tax_cla,
              str_tax_npc = lib_mer_str_tax_npc,
              adducts_list = par_ann_mas$ms$adducts,
              clusters_list = par_ann_mas$ms$clusters,
              neutral_losses_list = par_ann_mas$ms$neutral_losses,
              ms_mode = par_ann_mas$ms$polarity,
              tolerance_ppm = par_ann_mas$ms$tolerances$mass$ppm$ms1,
              tolerance_rt = par_ann_mas$ms$tolerances$rt$minutes
            )
        }
      ),
      tar_target(
        name = ann_ms1_pre_ann,
        format = "file",
        command = {
          ann_ms1_pre_ann <-
            ann_ms1_pre[[1]]
        }
      ),
      tar_target(
        name = ann_ms1_pre_edg,
        format = "file",
        command = {
          ann_ms1_pre_edg <- ann_ms1_pre[[2]]
        }
      )
    ),
    ## Spectral
    list(
      # ## GNPS
      list(
        tar_target(
          name = ann_spe_exp_gnp_pre,
          format = "file",
          command = {
            ann_spe_exp_gnp_pre <-
              prepare_annotations_gnps(
                # input = gnps_annotations,
                input = par_pre_ann_gnp$files$annotations$raw$spectral$gnps,
                output = par_pre_ann_gnp$files$annotations$prepared$structural$gnps,
                str_stereo = lib_mer_str_stereo,
                str_met = lib_mer_str_met,
                str_nam = lib_mer_str_nam,
                str_tax_cla = lib_mer_str_tax_cla,
                str_tax_npc = lib_mer_str_tax_npc
              )
          }
        )
      ),
      ## Classic
      list(
        ## TODO improve polarity handling, suboptimal
        tar_target(
          name = ann_spe_pos,
          format = "file",
          command = {
            ann_spe_pos <- annotate_spectra(
              input = input_spectra,
              library = c(
                lib_spe_is_lot_pre_pos,
                ## TODO add is hmdb
                lib_spe_exp_int_pre_pos,
                lib_spe_exp_mb_pre_pos
              ),
              polarity = "pos",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_pos.tsv.gz",
                x = par_ann_spe$files$annotations$raw$spectral$spectral,
                fixed = TRUE
              ),
              threshold = par_ann_spe$annotations$thresholds$ms2$similarity$annotation,
              ppm = par_ann_spe$ms$tolerances$mass$ppm$ms2,
              dalton = par_ann_spe$ms$tolerances$mass$dalton$ms2,
              qutoff = par_ann_spe$ms$thresholds$ms2$intensity,
              approx = par_ann_spe$annotations$ms2approx
            )
          }
        ),
        tar_target(
          name = ann_spe_neg,
          format = "file",
          command = {
            ann_spe_neg <- annotate_spectra(
              input = input_spectra,
              library = c(
                lib_spe_is_lot_pre_neg,
                ## TODO add is hmdb
                lib_spe_exp_int_pre_neg,
                lib_spe_exp_mb_pre_neg
              ),
              polarity = "neg",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_neg.tsv.gz",
                x = par_ann_spe$files$annotations$raw$spectral$spectral,
                fixed = TRUE
              ),
              threshold = par_ann_spe$annotations$thresholds$ms2$similarity$annotation,
              ppm = par_ann_spe$ms$tolerances$mass$ppm$ms2,
              dalton = par_ann_spe$ms$tolerances$mass$dalton$ms2,
              qutoff = par_ann_spe$ms$thresholds$ms2$intensity,
              approx = par_ann_spe$annotations$ms2approx
            )
          }
        ),
        tar_target(
          name = ann_spe_pre,
          format = "file",
          command = {
            ann_spe_pre <- prepare_annotations_spectra(
              input = c(
                ann_spe_neg,
                ann_spe_pos
              ),
              output = par_pre_ann_spe$files$annotations$prepared$structural$spectral,
              str_stereo = lib_mer_str_stereo,
              str_met = lib_mer_str_met,
              str_nam = lib_mer_str_nam,
              str_tax_cla = lib_mer_str_tax_cla,
              str_tax_npc = lib_mer_str_tax_npc
            )
          }
        )
      )
    ),
    # SIRIUS
    tar_target(
      name = ann_sir_pre,
      format = "file",
      command = {
        ann_sir_pre <-
          prepare_annotations_sirius(
            input_directory = par_pre_ann_sir$files$annotations$raw$sirius,
            output_ann = par_pre_ann_sir$files$annotations$prepared$structural$sirius,
            output_can = par_pre_ann_sir$files$annotations$prepared$canopus,
            output_for = par_pre_ann_sir$files$annotations$prepared$formula,
            sirius_version = par_pre_ann_sir$tools$sirius$version,
            str_stereo = lib_mer_str_stereo,
            str_met = lib_mer_str_met,
            str_nam = lib_mer_str_nam,
            str_tax_cla = lib_mer_str_tax_cla,
            str_tax_npc = lib_mer_str_tax_npc
          )
      }
    ),
    tar_target(
      name = ann_sir_pre_can,
      format = "file", command = {
        ann_sir_pre_can <- ann_sir_pre[[1]]
      }
    ),
    tar_target(
      name = ann_sir_pre_for,
      format = "file", command = {
        ann_sir_pre_for <- ann_sir_pre[[2]]
      }
    ),
    tar_target(
      name = ann_sir_pre_str,
      format = "file", command = {
        ann_sir_pre_str <- ann_sir_pre[[3]]
      }
    ),
    list()
  ),
  ## Features
  list(
    tar_target(
      name = fea_edg_spe,
      format = "file",
      command = {
        fea_edg_spe <- create_edges_spectra(
          input = input_spectra,
          output = par_cre_edg_spe$files$networks$spectral$edges$raw,
          name_source = par_cre_edg_spe$names$source,
          name_target = par_cre_edg_spe$names$target,
          threshold = par_cre_edg_spe$annotations$thresholds$ms2$similarity$edges,
          ppm = par_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
          dalton = par_cre_edg_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = par_cre_edg_spe$ms$thresholds$ms2$intensity
        )
      }
    ),
    tar_target(
      name = fea_com,
      format = "file",
      command = {
        fea_com <- create_components(
          input = fea_edg_pre,
          output = par_cre_com$files$networks$spectral$components$raw
        )
      }
    ),
    ## Interim
    list(
      tar_target(
        name = int_com,
        format = "file",
        command = {
          int_com <- fea_com
          # int_com <-
          #   if (file.exists(fea_com)) {
          #     fea_com
          #   } else {
          #     gnps_components
          #   }
        }
      ),
      tar_target(
        name = edg_spe,
        format = "file",
        command = {
          edg_spe <- fea_edg_spe
          # edg_spe <-
          #   ifelse(test = file.exists(fea_edg_spe),
          #     yes = fea_edg_spe,
          #     no = gnps_edges
          #   )
        }
      )
    ),
    tar_target(
      name = fea_edg_pre,
      format = "file",
      command = {
        fea_edg_pre <- prepare_features_edges(
          input = c("ms1" = ann_ms1_pre_edg, "spectral" = edg_spe),
          output = par_pre_fea_edg$files$networks$spectral$edges$prepared,
          name_source = par_pre_fea_edg$names$source,
          name_target = par_pre_fea_edg$names$target
        )
      }
    ),
    tar_target(
      name = fea_com_pre,
      format = "file",
      command = {
        fea_com_pre <- prepare_features_components(
          input = int_com,
          output = par_pre_fea_com$files$networks$spectral$components$prepared
        )
      }
    ),
    tar_target(
      name = fea_pre,
      format = "file",
      command = {
        fea_pre <- prepare_features_tables(
          features = input_features,
          output = par_pre_fea_tab$files$features$prepared,
          name_features = par_pre_fea_tab$names$features,
          name_rt = par_pre_fea_tab$names$rt$features,
          name_mz = par_pre_fea_tab$names$precursor
        )
      }
    )
  ),
  tar_target(
    name = tax_pre,
    format = "file",
    command = {
      tax_pre <- prepare_taxa(
        input = input_features,
        name_features = par_pre_tax$names$features,
        name_filename = par_pre_tax$names$filename,
        extension = par_pre_tax$names$extension,
        colname = par_pre_tax$names$taxon,
        metadata = input_metadata,
        top_k = par_pre_tax$organisms$candidates,
        org_tax_ott = lib_mer_org_tax_ott,
        output = par_pre_tax$files$metadata$prepared,
        taxon = par_pre_tax$organisms$taxon
      )
    }
  ),
  tar_target(
    name = ann_fil,
    format = "file",
    command = {
      ann_fil <- filter_annotations(
        annotations = c(
          ann_spe_exp_gnp_pre,
          ann_spe_pre,
          ann_sir_pre_str,
          ann_ms1_pre_ann
        ),
        features = fea_pre,
        filter_nitro = par_fil_ann$options$nitrogen_rule,
        rts = lib_rt_rts,
        output = par_fil_ann$files$annotations$filtered,
        tolerance_rt = par_fil_ann$ms$tolerances$rt$minutes
      )
    }
  ),
  tar_target(
    name = ann_pre,
    format = "file",
    command = {
      ann_pre <- weight_annotations(
        library = lib_mer_key,
        org_tax_ott = lib_mer_org_tax_ott,
        str_stereo = lib_mer_str_stereo,
        annotations = ann_fil,
        canopus = ann_sir_pre_can,
        formula = ann_sir_pre_for,
        components = fea_com_pre,
        edges = fea_edg_pre,
        taxa = tax_pre,
        output = par_wei_ann$files$annotations$processed,
        candidates_final = par_wei_ann$annotations$candidates$final,
        weight_spectral = par_wei_ann$weights$global$spectral,
        weight_chemical = par_wei_ann$weights$global$chemical,
        weight_biological = par_wei_ann$weights$global$biological,
        score_biological_domain = par_wei_ann$weights$biological$domain,
        score_biological_kingdom = par_wei_ann$weights$biological$kingdom,
        score_biological_phylum = par_wei_ann$weights$biological$phylum,
        score_biological_class = par_wei_ann$weights$biological$class,
        score_biological_order = par_wei_ann$weights$biological$order,
        score_biological_infraorder = par_wei_ann$weights$biological$infraorder,
        score_biological_family = par_wei_ann$weights$biological$family,
        score_biological_subfamily = par_wei_ann$weights$biological$subfamily,
        score_biological_tribe = par_wei_ann$weights$biological$tribe,
        score_biological_subtribe = par_wei_ann$weights$biological$subtribe,
        score_biological_genus = par_wei_ann$weights$biological$genus,
        score_biological_subgenus = par_wei_ann$weights$biological$subgenus,
        score_biological_species = par_wei_ann$weights$biological$species,
        score_biological_subspecies = par_wei_ann$weights$biological$subspecies,
        score_biological_variety = par_wei_ann$weights$biological$variety,
        score_chemical_cla_kingdom = par_wei_ann$weights$chemical$cla$kingdom,
        score_chemical_cla_superclass =
          par_wei_ann$weights$chemical$cla$superclass,
        score_chemical_cla_class = par_wei_ann$weights$chemical$cla$class,
        score_chemical_cla_parent = par_wei_ann$weights$chemical$cla$parent,
        score_chemical_npc_pathway = par_wei_ann$weights$chemical$npc$pathway,
        score_chemical_npc_superclass =
          par_wei_ann$weights$chemical$npc$superclass,
        score_chemical_npc_class = par_wei_ann$weights$chemical$npc$class,
        minimal_consistency = par_wei_ann$annotations$thresholds$consistency,
        minimal_ms1_bio = par_wei_ann$annotations$thresholds$ms1$biological,
        minimal_ms1_chemo = par_wei_ann$annotations$thresholds$ms1$chemical,
        minimal_ms1_condition =
          par_wei_ann$annotations$thresholds$ms1$condition,
        ms1_only = par_wei_ann$annotations$ms1only,
        compounds_names = par_wei_ann$options$compounds_names,
        remove_ties = par_wei_ann$options$remove_ties,
        summarise = par_wei_ann$options$summarise,
        pattern = par_wei_ann$files$pattern,
        force = par_wei_ann$options$force
      )
    }
  ),
  list(
    ## Benchmark
    tar_target(
      name = benchmark_path_url,
      command = {
        benchmark_path_url <- paths_urls_benchmarking_set
      }
    ),
    tar_target(
      name = benchmark_path_zip,
      command = {
        benchmark_path_zip <- paths_data_source_benchmark_zip
      }
    ),
    tar_target(
      name = benchmark_path_file,
      command = {
        benchmark_path_file <- paths_data_source_benchmark_cleaned
      }
    ),
    tar_target(
      name = benchmark_path_copy,
      command = {
        benchmark_path_copy <- paths_data_source_benchmark_copy
      }
    ),
    tar_target(
      name = benchmark_path_mgf_neg,
      command = {
        benchmark_path_mgf_neg <- paths_data_source_benchmark_mgf_neg
      }
    ),
    tar_target(
      name = benchmark_path_mgf_pos,
      command = {
        benchmark_path_mgf_pos <- paths_data_source_benchmark_mgf_pos
      }
    ),
    tar_target(
      name = benchmark_zip,
      format = "file",
      command = {
        benchmark_zip <- get_file(
          url = benchmark_path_url,
          export = benchmark_path_zip
        )
        return(benchmark_path_export)
      }
    ),
    tar_target(
      name = benchmark_file,
      format = "file",
      command = {
        unzip(zipfile = benchmark_zip)
        dir.create(dirname(benchmark_path_file), recursive = TRUE)
        file.copy(from = "cleaned_libraries_matchms/results_library_cleaning/cleaned_spectra.mgf", to = benchmark_path_file)
        unlink("cleaned_libraries_matchms", recursive = TRUE)
        return(benchmark_path_file)
      }
    ),
    tar_target(
      name = benchmark_converted,
      format = "file",
      command = {
        sp <- benchmark_file |>
          import_spectra()
        sp |>
          Spectra::filterEmptySpectra() |>
          extract_spectra() |>
          data.frame() |>
          saveRDS(file = "data/interim/benchmark/benchmark_spectra.rds")
        return("data/interim/benchmark/benchmark_spectra.rds")
      }
    ),
    tar_target(
      name = benchmark_prepared,
      format = "file",
      command = {
        sp <- benchmark_converted |>
          import_spectra() |>
          sanitize_spectra(
            cutoff = 0,
            ratio = 10000,
            fragments = 5
          )

        sp@backend@spectraData$precursorMz <-
          sp@backend@spectraData$PRECURSOR_MZ |>
          as.numeric()

        log_debug("Imported")
        sp_clean <- sp |>
          Spectra::addProcessing(remove_above_precursor(),
            spectraVariables = c("precursorMz")
          ) |>
          Spectra::addProcessing(normalize_peaks()) |>
          Spectra::applyProcessing()

        log_debug("Cleaned")
        df_meta <- tidytable::tidytable(
          adduct = sp_clean$ADDUCT,
          inchikey = sp_clean$INCHIKEY,
          instrument = sp_clean$INSTRUMENT_TYPE,
          fragments = lapply(sp_clean@backend@peaksData, length) |>
            as.character() |>
            as.numeric() / 2,
          precursorMz = sp_clean$precursorMz,
          smiles = sp_clean$SMILES,
          ccmslib = sp_clean$SPECTRUM_ID,
          charge = sp_clean$precursorCharge,
          name = sp_clean$COMPOUND_NAME
        ) |>
          tidytable::mutate(
            tidytable::across(
              .cols = tidytable::everything(),
              .fns = function(x) {
                tidytable::na_if(x, "")
              }
            )
          )

        log_debug("Framed")
        df_clean <- df_meta |>
          tidytable::filter(!is.na(inchikey)) |>
          tidytable::filter(fragments >= 5) |>
          tidytable::filter(fragments <= 250) |>
          tidytable::filter(!grepl(
            pattern = "QQQ",
            x = instrument,
            fixed = TRUE
          )) |>
          ## fragments are nominal mass
          tidytable::filter(!grepl(
            pattern = "ReSpect",
            x = name,
            fixed = TRUE
          )) |>
          ## remove spectral matches
          tidytable::filter(!grepl(
            pattern = "Spectral Match to",
            x = name,
            fixed = TRUE
          )) |>
          ## remove putatives
          tidytable::filter(!grepl(
            pattern = "putative",
            x = name,
            fixed = TRUE
          )) |>
          tidytable::select(-name) |>
          tidytable::mutate(mass = precursorMz) |>
          tidytable::separate(
            col = mass,
            sep = "\\.",
            into = c("a", "b")
          ) |>
          tidytable::filter(!is.na(b)) |>
          tidytable::filter(stringr::str_length(as.numeric(b)) > 1) |>
          tidytable::select(-a, -b) |>
          tidytable::mutate(inchikey_no_stereo = gsub(
            pattern = "-.*",
            replacement = "",
            x = inchikey,
            perl = TRUE
          )) |>
          tidytable::distinct(inchikey_no_stereo, adduct, .keep_all = TRUE) |>
          tidytable::mutate(mz = precursorMz) |>
          ## Weird way to have some kind of retention time
          tidytable::mutate(
            rt = tidytable::cur_group_id(),
            .by = "inchikey_no_stereo"
          )

        df_clean_neg <- df_clean |>
          tidytable::filter(grepl(
            pattern = "]-",
            x = adduct,
            fixed = TRUE
          ))

        df_clean_pos <- df_clean |>
          tidytable::filter(grepl(
            pattern = "]+",
            x = adduct,
            fixed = TRUE
          ))

        sp_pos <-
          sp_clean[sp_clean$SPECTRUM_ID %in% df_clean_pos$ccmslib]
        sp_neg <-
          sp_clean[sp_clean$SPECTRUM_ID %in% df_clean_neg$ccmslib]

        extract_benchmark_spectra <- function(x, mode) {
          df <- x |>
            extract_spectra() |>
            tidytable::mutate(acquisitionNum = tidytable::row_number()) |>
            tidytable::mutate(spectrum_id = acquisitionNum) |>
            tidytable::mutate(short_ik = gsub(
              pattern = "-.*",
              replacement = "",
              INCHIKEY,
              perl = TRUE
            )) |>
            tidytable::mutate(
              rtime = tidytable::cur_group_id(),
              .by = "short_ik"
            ) |>
            tidytable::mutate(precursorCharge = ifelse(
              test = mode == "pos",
              yes = as.integer(1),
              no = as.integer(-1)
            )) |>
            tidytable::select(
              acquisitionNum,
              precursorCharge,
              precursorMz,
              MS_LEVEL,
              rtime,
              name = COMPOUND_NAME,
              smiles = SMILES,
              inchi = INCHI,
              inchikey = INCHIKEY,
              adduct = ADDUCT,
              instrument = INSTRUMENT_TYPE,
              ccmslib = SPECTRUM_ID,
              spectrum_id = acquisitionNum,
              mz,
              intensity
            ) |>
            data.frame()
          return(df)
        }

        spectra_harmonized_pos <- sp_pos |>
          extract_benchmark_spectra(mode = "pos")

        spectra_harmonized_neg <- sp_neg |>
          extract_benchmark_spectra(mode = "neg")

        select_benchmark_columns <- function(x) {
          df <- x |>
            tidytable::select(
              adduct,
              inchikey,
              instrument,
              smiles,
              ccmslib,
              charge = precursorCharge,
              mz = precursorMz,
              rt = rtime,
              feature_id = spectrum_id
            ) |>
            tidytable::mutate(inchikey_no_stereo = gsub(
              pattern = "-.*",
              replacement = "",
              x = inchikey,
              perl = TRUE
            )) |>
            data.frame()
          return(df)
        }

        df_clean_pos <- spectra_harmonized_pos |>
          select_benchmark_columns()

        df_clean_neg <- spectra_harmonized_neg |>
          select_benchmark_columns()

        log_debug("Exporting")
        spectra_harmonized_pos |>
          Spectra::Spectra() |>
          Spectra::export(
            backend = MsBackendMgf::MsBackendMgf(),
            file = benchmark_path_mgf_pos
          )
        spectra_harmonized_neg |>
          Spectra::Spectra() |>
          Spectra::export(
            backend = MsBackendMgf::MsBackendMgf(),
            file = benchmark_path_mgf_neg
          )
        df_clean_pos |>
          export_output("data/interim/benchmark/benchmark_meta_pos.tsv")
        df_clean_neg |>
          export_output("data/interim/benchmark/benchmark_meta_neg.tsv")

        return(
          c(
            "spectra_pos" = benchmark_path_mgf_pos,
            "spectra_neg" = benchmark_path_mgf_neg,
            "meta_pos" = "data/interim/benchmark/benchmark_meta_pos.tsv",
            "meta_neg" = "data/interim/benchmark/benchmark_meta_neg.tsv"
          )
        )
      }
    ),
    tar_target(
      name = benchmark_pre_mgf_pos,
      format = "file",
      command = {
        benchmark_pre_mgf_pos <- benchmark_prepared[[1]]
      }
    ),
    tar_target(
      name = benchmark_pre_mgf_neg,
      format = "file",
      command = {
        benchmark_pre_mgf_neg <- benchmark_prepared[[2]]
      }
    ),
    tar_target(
      name = benchmark_pre_meta_pos,
      format = "file",
      command = {
        benchmark_pre_meta_pos <- benchmark_prepared[[3]]
      }
    ),
    tar_target(
      name = benchmark_pre_meta_neg,
      format = "file",
      command = {
        benchmark_pre_meta_neg <- benchmark_prepared[[4]]
      }
    ),
    tar_target(
      name = benchmark_taxed_pos,
      format = "file",
      command = {
        benchmark_taxed_pos <- benchmark_pre_meta_pos |>
          taxize_spectra_benchmark(
            keys = lib_mer_key,
            org_tax_ott = lib_mer_org_tax_ott,
            output = "data/interim/benchmark/benchmark_taxed_pos.tsv.gz"
          )
      }
    ),
    tar_target(
      name = benchmark_taxed_neg,
      format = "file",
      command = {
        benchmark_taxed_neg <- benchmark_pre_meta_neg |>
          taxize_spectra_benchmark(
            keys = lib_mer_key,
            org_tax_ott = lib_mer_org_tax_ott,
            output = "data/interim/benchmark/benchmark_taxed_neg.tsv.gz"
          )
      }
    ),
    tar_target(
      name = benchmark_def_ann_mas,
      command = {
        benchmark_def_ann_mas <- parse_yaml_params(
          def = par_def_ann_mas,
          usr = par_def_ann_mas
        )
      }
    ),
    tar_target(
      name = benchmark_ann_ms1_pre_pos,
      format = "file",
      command = {
        benchmark_ann_ms1_pre_pos <-
          annotate_masses(
            features = benchmark_pre_meta_pos,
            library = lib_mer_key,
            output_annotations = "data/interim/benchmark/benchmark_ann_ms1_pos.tsv.gz",
            output_edges = "data/interim/benchmark/benchmark_edges_ms1_pos.tsv.gz",
            name_source = benchmark_def_ann_mas$names$source,
            name_target = benchmark_def_ann_mas$names$target,
            str_stereo = lib_mer_str_stereo,
            str_met = lib_mer_str_met,
            str_nam = lib_mer_str_nam,
            str_tax_cla = lib_mer_str_tax_cla,
            str_tax_npc = lib_mer_str_tax_npc,
            adducts_list = par_ann_mas$ms$adducts,
            clusters_list = par_ann_mas$ms$clusters,
            neutral_losses_list = par_ann_mas$ms$neutral_losses,
            ms_mode = "pos",
            tolerance_ppm = benchmark_def_ann_mas$ms$tolerances$mass$ppm$ms1,
            tolerance_rt = benchmark_def_ann_mas$ms$tolerances$rt$minutes
          )
      }
    ),
    tar_target(
      name = benchmark_ann_ms1_pre_neg,
      format = "file",
      command = {
        benchmark_ann_ms1_pre_neg <-
          annotate_masses(
            features = benchmark_pre_meta_neg,
            library = lib_mer_key,
            output_annotations = "data/interim/benchmark/benchmark_ann_ms1_neg.tsv.gz",
            output_edges = "data/interim/benchmark/benchmark_edges_ms1_neg.tsv.gz",
            name_source = benchmark_def_ann_mas$names$source,
            name_target = benchmark_def_ann_mas$names$target,
            str_stereo = lib_mer_str_stereo,
            str_met = lib_mer_str_met,
            str_nam = lib_mer_str_nam,
            str_tax_cla = lib_mer_str_tax_cla,
            str_tax_npc = lib_mer_str_tax_npc,
            adducts_list = par_ann_mas$ms$adducts,
            clusters_list = par_ann_mas$ms$clusters,
            neutral_losses_list = par_ann_mas$ms$neutral_losses,
            ms_mode = "neg",
            tolerance_ppm = benchmark_def_ann_mas$ms$tolerances$mass$ppm$ms1,
            tolerance_rt = benchmark_def_ann_mas$ms$tolerances$rt$minutes
          )
      }
    ),
    tar_target(
      name = benchmark_def_cre_edg_spe,
      command = {
        benchmark_def_cre_edg_spe <- parse_yaml_params(
          def = par_def_cre_edg_spe,
          usr = par_def_cre_edg_spe
        )
      }
    ),
    tar_target(
      name = benchmark_edg_spe_pos,
      format = "file",
      command = {
        benchmark_edg_spe_pos <- create_edges_spectra(
          input = benchmark_pre_mgf_pos,
          output = "data/interim/benchmark/benchmark_edges_spe_pos.tsv.gz",
          name_source = benchmark_def_cre_edg_spe$names$source,
          name_target = benchmark_def_cre_edg_spe$names$target,
          threshold = benchmark_def_cre_edg_spe$annotations$thresholds$ms2$similarity$edges,
          ppm = benchmark_def_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_cre_edg_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = 0
        )
      }
    ),
    tar_target(
      name = benchmark_edg_spe_neg,
      format = "file",
      command = {
        benchmark_edg_spe_neg <- create_edges_spectra(
          input = benchmark_pre_mgf_neg,
          output = "data/interim/benchmark/benchmark_edges_spe_neg.tsv.gz",
          name_source = benchmark_def_cre_edg_spe$names$source,
          name_target = benchmark_def_cre_edg_spe$names$target,
          threshold = benchmark_def_cre_edg_spe$annotations$thresholds$ms2$similarity$edges,
          ppm = benchmark_def_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_cre_edg_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = 0
        )
      }
    ),
    tar_target(
      name = benchmark_def_pre_fea_edg,
      command = {
        benchmark_def_pre_fea_edg <- parse_yaml_params(
          def = par_def_pre_fea_edg,
          usr = par_def_pre_fea_edg
        )
      }
    ),
    tar_target(
      name = benchmark_edg_pre_pos,
      format = "file",
      command = {
        benchmark_edg_pre_pos <- prepare_features_edges(
          input = c(benchmark_edg_spe_pos, benchmark_ann_ms1_pre_pos[[2]]),
          output = "data/interim/benchmark/benchmark_edges_pos.tsv.gz",
          name_source = benchmark_def_pre_fea_edg$names$source,
          name_target = benchmark_def_pre_fea_edg$names$target
        )
      }
    ),
    tar_target(
      name = benchmark_edg_pre_neg,
      format = "file",
      command = {
        benchmark_edg_pre_neg <- prepare_features_edges(
          input = c(benchmark_edg_spe_neg, benchmark_ann_ms1_pre_neg[[2]]),
          output = "data/interim/benchmark/benchmark_edges_neg.tsv.gz",
          name_source = benchmark_def_pre_fea_edg$names$source,
          name_target = benchmark_def_pre_fea_edg$names$target
        )
      }
    ),
    tar_target(
      name = benchmark_def_cre_edg_com,
      command = {
        benchmark_def_cre_edg_com <- parse_yaml_params(
          def = par_def_cre_com,
          usr = par_def_cre_com
        )
      }
    ),
    tar_target(
      name = benchmark_com_pos,
      format = "file",
      command = {
        benchmark_com_pos <- create_components(
          input = benchmark_edg_pre_pos,
          output = "data/interim/benchmark/benchmark_components_pos.tsv.gz"
        )
      }
    ),
    tar_target(
      name = benchmark_com_neg,
      format = "file",
      command = {
        benchmark_com_neg <- create_components(
          input = benchmark_edg_pre_neg,
          output = "data/interim/benchmark/benchmark_components_neg.tsv.gz"
        )
      }
    ),
    tar_target(
      name = benchmark_def_pre_fea_com,
      command = {
        benchmark_def_pre_fea_com <- parse_yaml_params(
          def = par_def_pre_fea_com,
          usr = par_def_pre_fea_com
        )
      }
    ),
    tar_target(
      name = benchmark_com_pre_pos,
      format = "file",
      command = {
        benchmark_com_pre_pos <- prepare_features_components(
          input = benchmark_com_pos,
          output = "data/interim/benchmark/benchmark_com_pre_pos.tsv.gz"
        )
      }
    ),
    tar_target(
      name = benchmark_com_pre_neg,
      format = "file",
      command = {
        benchmark_com_pre_neg <- prepare_features_components(
          input = benchmark_com_neg,
          output = "data/interim/benchmark/benchmark_com_pre_neg.tsv.gz"
        )
      }
    ),
    tar_target(
      name = benchmark_def_ann_spe,
      command = {
        benchmark_def_ann_spe <- parse_yaml_params(
          def = par_def_ann_spe,
          usr = par_def_ann_spe
        )
      }
    ),
    tar_target(
      name = benchmark_ann_spe_pos,
      format = "file",
      command = {
        benchmark_ann_spe_pos <- annotate_spectra(
          input = benchmark_pre_mgf_pos,
          library = c(
            lib_spe_is_lot_pre_pos,
            lib_spe_exp_mb_pre_pos
          ),
          polarity = "pos",
          output = "data/interim/benchmark/benchmark_ann_spe_pos.tsv.gz",
          threshold =
            benchmark_def_ann_spe$annotations$thresholds$ms2$similarity$annotation,
          ppm = benchmark_def_ann_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_ann_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = 0,
          approx = benchmark_def_ann_spe$annotations$ms2approx
        )
      }
    ),
    tar_target(
      name = benchmark_ann_spe_neg,
      format = "file",
      command = {
        benchmark_ann_spe_neg <- annotate_spectra(
          input = benchmark_pre_mgf_neg,
          library = c(
            lib_spe_is_lot_pre_neg,
            lib_spe_exp_mb_pre_neg
          ),
          polarity = "neg",
          output = "data/interim/benchmark/benchmark_ann_spe_neg.tsv.gz",
          threshold =
            benchmark_def_ann_spe$annotations$thresholds$ms2$similarity$annotation,
          ppm = benchmark_def_ann_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_ann_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = 0,
          approx = benchmark_def_ann_spe$annotations$ms2approx
        )
      }
    ),
    tar_target(
      name = benchmark_def_pre_ann_spe,
      command = {
        benchmark_def_pre_ann_spe <- parse_yaml_params(
          def = par_def_pre_ann_spe,
          usr = par_def_pre_ann_spe
        )
      }
    ),
    tar_target(
      name = benchmark_ann_spe_pre_pos,
      format = "file",
      command = {
        benchmark_ann_spe_pre_pos <- prepare_annotations_spectra(
          input = c(benchmark_ann_spe_pos),
          output = "data/interim/benchmark/benchmark_ann_spe_pre_pos.tsv.gz",
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_nam = lib_mer_str_nam,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc
        )
      }
    ),
    tar_target(
      name = benchmark_ann_spe_pre_neg,
      format = "file",
      command = {
        benchmark_ann_spe_pre_neg <- prepare_annotations_spectra(
          input = c(benchmark_ann_spe_neg),
          output = "data/interim/benchmark/benchmark_ann_spe_pre_neg.tsv.gz",
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_nam = lib_mer_str_nam,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc
        )
      }
    ),
    tar_target(
      name = benchmark_def_pre_ann_sir,
      command = {
        benchmark_def_pre_ann_sir <- parse_yaml_params(
          def = par_def_pre_ann_sir,
          usr = par_def_pre_ann_sir
        )
      }
    ),
    tar_target(
      name = benchmark_ann_sir_pre,
      format = "file",
      command = {
        benchmark_ann_sir_pre <-
          prepare_annotations_sirius(
            input_directory = "doesNotExist4Now",
            output_ann = "data/interim/benchmark/benchmark_ann_sir_pre.tsv.gz",
            output_can = "data/interim/benchmark/benchmark_ann_sir_pre_can.tsv.gz",
            output_for = "data/interim/benchmark/benchmark_ann_sir_pre_for.tsv.gz",
            str_stereo = lib_mer_str_stereo,
            str_met = lib_mer_str_met,
            str_nam = lib_mer_str_nam,
            str_tax_cla = lib_mer_str_tax_cla,
            str_tax_npc = lib_mer_str_tax_npc
          )
      }
    ),
    tar_target(
      name = benchmark_ann_sir_pre_can,
      format = "file", command = {
        benchmark_ann_sir_pre_can <- benchmark_ann_sir_pre[[1]]
      }
    ),
    tar_target(
      name = benchmark_ann_sir_pre_for,
      format = "file", command = {
        benchmark_ann_sir_pre_for <- benchmark_ann_sir_pre[[2]]
      }
    ),
    tar_target(
      name = benchmark_ann_sir_pre_str,
      format = "file", command = {
        benchmark_ann_sir_pre_str <- benchmark_ann_sir_pre[[3]]
      }
    ),
    tar_target(
      name = benchmark_def_fil_ann,
      command = {
        benchmark_def_fil_ann <- parse_yaml_params(
          def = par_def_fil_ann,
          usr = par_def_fil_ann
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_spe_neg,
      format = "file",
      command = {
        benchmark_ann_fil_spe_neg <- filter_annotations(
          annotations = c(
            benchmark_ann_spe_pre_neg,
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_neg,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_spe_fil_neg.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$minutes
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_spe_ms1_neg,
      format = "file",
      command = {
        benchmark_ann_fil_spe_ms1_neg <- filter_annotations(
          annotations = c(
            benchmark_ann_spe_pre_neg,
            benchmark_ann_ms1_pre_neg[[1]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_neg,
          rts = list(),
          output =
            "data/interim/benchmark/benchmark_ann_spe_ms1_fil_neg.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$minutes
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_ms1_neg,
      format = "file",
      command = {
        benchmark_ann_fil_ms1_neg <- filter_annotations(
          annotations = c(
            benchmark_ann_ms1_pre_neg[[1]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_neg,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_ms1_fil_neg.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$minutes
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_spe_pos,
      format = "file",
      command = {
        benchmark_ann_fil_spe_pos <- filter_annotations(
          annotations = c(
            benchmark_ann_spe_pre_pos,
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_pos,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_spe_fil_pos.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$minutes
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_spe_ms1_pos,
      format = "file",
      command = {
        benchmark_ann_fil_spe_ms1_pos <- filter_annotations(
          annotations = c(
            benchmark_ann_spe_pre_pos,
            benchmark_ann_ms1_pre_pos[[1]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_pos,
          rts = list(),
          output =
            "data/interim/benchmark/benchmark_ann_spe_ms1_fil_pos.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$minutes
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_ms1_pos,
      format = "file",
      command = {
        benchmark_ann_fil_ms1_pos <- filter_annotations(
          annotations = c(
            benchmark_ann_ms1_pre_pos[[1]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_pos,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_ms1_fil_pos.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$minutes
        )
      }
    ),
    tar_target(
      name = benchmark_def_wei_ann,
      command = {
        benchmark_def_wei_ann <- parse_yaml_params(
          def = par_def_wei_ann,
          usr = par_def_wei_ann
        )
      }
    ),
    tar_target(
      name = benchmark_wei_par,
      command = {
        benchmark_wei_par <- list(
          canopus = benchmark_ann_sir_pre_can,
          formula = benchmark_ann_sir_pre_for,
          library = lib_mer_key,
          org_tax_ott = lib_mer_org_tax_ott,
          str_stereo = lib_mer_str_stereo,
          candidates_final = 500,
          score_biological_domain =
            benchmark_def_wei_ann$weights$biological$domain,
          score_biological_kingdom =
            benchmark_def_wei_ann$weights$biological$kingdom,
          score_biological_phylum =
            benchmark_def_wei_ann$weights$biological$phylum,
          score_biological_class =
            benchmark_def_wei_ann$weights$biological$class,
          score_biological_order =
            benchmark_def_wei_ann$weights$biological$order,
          score_biological_infraorder =
            benchmark_def_wei_ann$weights$biological$infraorder,
          score_biological_family =
            benchmark_def_wei_ann$weights$biological$family,
          score_biological_subfamily =
            benchmark_def_wei_ann$weights$biological$subfamily,
          score_biological_tribe =
            benchmark_def_wei_ann$weights$biological$tribe,
          score_biological_subtribe =
            benchmark_def_wei_ann$weights$biological$subtribe,
          score_biological_genus =
            benchmark_def_wei_ann$weights$biological$genus,
          score_biological_subgenus =
            benchmark_def_wei_ann$weights$biological$subgenus,
          score_biological_species =
            benchmark_def_wei_ann$weights$biological$species,
          score_biological_subspecies =
            benchmark_def_wei_ann$weights$biological$subspecies,
          score_biological_variety =
            benchmark_def_wei_ann$weights$biological$variety,
          score_chemical_cla_kingdom =
            benchmark_def_wei_ann$weights$chemical$cla$kingdom,
          score_chemical_cla_superclass =
            benchmark_def_wei_ann$weights$chemical$cla$superclass,
          score_chemical_cla_class =
            benchmark_def_wei_ann$weights$chemical$cla$class,
          score_chemical_cla_parent =
            benchmark_def_wei_ann$weights$chemical$cla$parent,
          score_chemical_npc_pathway =
            benchmark_def_wei_ann$weights$chemical$npc$pathway,
          score_chemical_npc_superclass =
            benchmark_def_wei_ann$weights$chemical$npc$superclass,
          score_chemical_npc_class =
            benchmark_def_wei_ann$weights$chemical$npc$class,
          minimal_consistency =
            benchmark_def_wei_ann$annotations$thresholds$consistency,
          minimal_ms1_bio =
            benchmark_def_wei_ann$annotations$thresholds$ms1$biological,
          minimal_ms1_chemo =
            benchmark_def_wei_ann$annotations$thresholds$ms1$chemical,
          minimal_ms1_condition =
            benchmark_def_wei_ann$annotations$thresholds$ms1$condition,
          compounds_names = benchmark_def_wei_ann$options$compounds_names,
          remove_ties = benchmark_def_wei_ann$options$remove_ties,
          summarise = benchmark_def_wei_ann$options$summarise,
          pattern = benchmark_def_wei_ann$files$pattern,
          force = benchmark_def_wei_ann$options$force
        )
      }
    ),
    tar_target(
      name = benchmark_files_pos,
      command = {
        benchmark_files_pos <- list(
          components = benchmark_com_pre_pos,
          edges = benchmark_edg_pre_pos,
          taxa = benchmark_taxed_pos
        )
      }
    ),
    tar_target(
      name = benchmark_files_neg,
      command = {
        benchmark_files_pos <- list(
          components = benchmark_com_pre_neg,
          edges = benchmark_edg_pre_neg,
          taxa = benchmark_taxed_neg
        )
      }
    ),
    # tar_target(
    #   name = benchmark_ann_pre_ms1_pos,
    # .  format = "file",
    #   command = {
    #     benchmark_ann_pre_ms1_pos <-
    #       do.call(
    #         what = weight_annotations,
    #         args = c(benchmark_wei_par,
    #           benchmark_files_pos,
    #           annotations = benchmark_ann_fil_ms1_pos,
    #           weight_spectral = benchmark_def_wei_ann$weights$global$spectral,
    #           weight_chemical = benchmark_def_wei_ann$weights$global$chemical,
    #           weight_biological =
    # benchmark_def_wei_ann$weights$global$biological,
    #           ms1_only = TRUE,
    #           output = "benchmark_lotus_ms1_pos.tsv.gz"
    #         )
    #       )
    #   }
    # ),
    tar_target(
      name = benchmark_ann_pre_ms2_b_pos,
      format = "file",
      command = {
        benchmark_ann_pre_ms2_b_pos <-
          do.call(
            what = weight_annotations,
            args = c(
              benchmark_wei_par,
              benchmark_files_pos,
              annotations = benchmark_ann_fil_spe_pos,
              weight_spectral = 0.333,
              weight_chemical = 0,
              weight_biological = 0.666,
              ms1_only = FALSE,
              output = "benchmark_lotus_ms2_bio_pos.tsv.gz"
            )
          )
      }
    ),
    tar_target(
      name = benchmark_ann_pre_ms1_ms2_b_pos,
      format = "file",
      command = {
        benchmark_ann_pre_ms1_ms2_b_pos <-
          do.call(
            what = weight_annotations,
            args = c(
              benchmark_wei_par,
              benchmark_files_pos,
              annotations = benchmark_ann_fil_spe_ms1_pos,
              ms1_only = FALSE,
              weight_spectral = 0.333,
              weight_chemical = 0,
              weight_biological = 0.666,
              output = "benchmark_lotus_ms1_ms2_bio_pos.tsv.gz"
            )
          )
      }
    ),
    tar_target(
      name = benchmark_ann_pre_ms2_b_c_pos,
      format = "file",
      command = {
        benchmark_ann_pre_ms2_b_c_pos <-
          do.call(
            what = weight_annotations,
            args = c(
              benchmark_wei_par,
              benchmark_files_pos,
              annotations = benchmark_ann_fil_spe_pos,
              ms1_only = FALSE,
              weight_spectral = 0.333,
              weight_chemical = 0.166,
              weight_biological = 0.500,
              output = "benchmark_lotus_ms2_bio_chemo_pos.tsv.gz"
            )
          )
      }
    ),
    tar_target(
      name = benchmark_ann_pre_ms1_ms2_b_c_pos,
      format = "file",
      command = {
        benchmark_ann_pre_ms1_ms2_b_c_pos <-
          do.call(
            what = weight_annotations,
            args = c(
              benchmark_wei_par,
              benchmark_files_pos,
              annotations = benchmark_ann_fil_spe_ms1_pos,
              ms1_only = FALSE,
              weight_spectral = 0.333,
              weight_chemical = 0.166,
              weight_biological = 0.500,
              output = "benchmark_lotus_ms1_ms2_bio_chemo_pos.tsv.gz"
            )
          )
      }
    ),
    # tar_target(
    #   name = benchmark_ann_pre_ms1_neg,
    # .  format = "file",
    #   command = {
    #     benchmark_ann_pre_ms1_neg <-
    #       do.call(
    #         what = weight_annotations,
    #         args = c(benchmark_wei_par,
    #           benchmark_files_neg,
    #           annotations = benchmark_ann_fil_ms1_neg,
    #           ms1_only = TRUE,
    #           weight_spectral = benchmark_def_wei_ann$weights$global$spectral,
    #           weight_chemical = benchmark_def_wei_ann$weights$global$chemical,
    #           weight_biological =
    # benchmark_def_wei_ann$weights$global$biological,
    #           output = "benchmark_lotus_ms1_neg.tsv.gz"
    #         )
    #       )
    #   }
    # ),
    tar_target(
      name = benchmark_ann_pre_ms2_b_neg,
      format = "file",
      command = {
        benchmark_ann_pre_ms2_b_neg <-
          do.call(
            what = weight_annotations,
            args = c(
              benchmark_wei_par,
              benchmark_files_neg,
              annotations = benchmark_ann_fil_spe_neg,
              ms1_only = FALSE,
              weight_spectral = 0.333,
              weight_chemical = 0,
              weight_biological = 0.666,
              output = "benchmark_lotus_ms2_bio_neg.tsv.gz"
            )
          )
      }
    ),
    tar_target(
      name = benchmark_ann_pre_ms1_ms2_b_neg,
      format = "file",
      command = {
        benchmark_ann_pre_ms1_ms2_b_neg <-
          do.call(
            what = weight_annotations,
            args = c(
              benchmark_wei_par,
              benchmark_files_neg,
              annotations = benchmark_ann_fil_spe_ms1_neg,
              ms1_only = FALSE,
              weight_spectral = 0.333,
              weight_chemical = 0,
              weight_biological = 0.666,
              output = "benchmark_lotus_ms1_ms2_bio_neg.tsv.gz"
            )
          )
      }
    ),
    tar_target(
      name = benchmark_ann_pre_ms2_b_c_neg,
      format = "file",
      command = {
        benchmark_ann_pre_ms2_b_c_neg <-
          do.call(
            what = weight_annotations,
            args = c(
              benchmark_wei_par,
              benchmark_files_neg,
              annotations = benchmark_ann_fil_spe_neg,
              ms1_only = FALSE,
              weight_spectral = 0.333,
              weight_chemical = 0.166,
              weight_biological = 0.500,
              output = "benchmark_lotus_ms2_bio_chemo_neg.tsv.gz"
            )
          )
      }
    ),
    tar_target(
      name = benchmark_ann_pre_ms1_ms2_b_c_neg,
      format = "file",
      command = {
        benchmark_ann_pre_ms1_ms2_b_c_neg <-
          do.call(
            what = weight_annotations,
            args = c(
              benchmark_wei_par,
              benchmark_files_neg,
              annotations = benchmark_ann_fil_spe_ms1_neg,
              ms1_only = FALSE,
              weight_spectral = 0.333,
              weight_chemical = 0.166,
              weight_biological = 0.500,
              output = "benchmark_lotus_ms1_ms2_bio_chemo_neg.tsv.gz"
            )
          )
      }
    )
  )
)
