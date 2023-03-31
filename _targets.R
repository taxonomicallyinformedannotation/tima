# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  garbage_collection = TRUE,
  memory = "transient",
  packages = "timaR"
)

# tar_make_clustermq() configuration (okay to leave alone):

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
list(
  ## Architecture
  list( ## Paths
    list(
      tar_file(
        name = yaml_paths,
        command = {
          yaml_paths <- "paths.yaml"
        }
      ),
      tar_target(
        name = paths,
        command = {
          paths <- parse_yaml_paths(file = yaml_paths)
        }
      )
    ),
    ## Dictionaries
    list(
      tar_file(
        name = dic_adducts,
        command = {
          dic_adducts <- paths$inst$extdata$adducts
        }
      ),
      tar_file(
        name = dic_neutral_losses,
        command = {
          dic_neutral_losses <- paths$inst$extdata$neutral_losses
        }
      )
    )
  ),
  ## Params
  list(
    ## Default
    list(
      tar_file(
        name = par_def_ann_mas,
        command = {
          par_def_ann_mas <- paths$params$default$annotate$masses
        }
      ),
      tar_file(
        name = par_def_ann_spe,
        command = {
          par_def_ann_spe <- paths$params$default$annotate$spectra
        }
      ),
      tar_file(
        name = par_def_cre_com,
        command = {
          par_def_cre_com <- paths$params$default$create$components
        }
      ),
      tar_file(
        name = par_def_cre_edg_spe,
        command = {
          par_def_cre_edg_spe <- paths$params$default$create$edges$spectra
        }
      ),
      tar_file(
        name = par_def_pre_ann_gnp,
        command = {
          par_def_pre_ann_gnp <- paths$params$default$prepare$annotations$gnps
        }
      ),
      tar_file(
        name = par_def_pre_ann_sir,
        command = {
          par_def_pre_ann_sir <-
            paths$params$default$prepare$annotations$sirius
        }
      ),
      tar_file(
        name = par_def_pre_ann_spe,
        command = {
          par_def_pre_ann_spe <-
            paths$params$default$prepare$annotations$spectra
        }
      ),
      tar_file(
        name = par_def_pre_fea_com,
        command = {
          par_def_pre_fea_com <-
            paths$params$default$prepare$features$components
        }
      ),
      tar_file(
        name = par_def_pre_fea_edg,
        command = {
          par_def_pre_fea_edg <- paths$params$default$prepare$features$edges
        }
      ),
      tar_file(
        name = par_def_pre_fea_tab,
        command = {
          par_def_pre_fea_tab <- paths$params$default$prepare$features$tables
        }
      ),
      tar_file(
        name = par_def_pre_lib_add,
        command = {
          par_def_pre_lib_add <-
            paths$params$default$prepare$libraries$adducts
        }
      ),
      tar_file(
        name = par_def_pre_lib_sop_clo,
        command = {
          par_def_pre_lib_sop_clo <-
            paths$params$default$prepare$libraries$sop$closed
        }
      ),
      tar_file(
        name = par_def_pre_lib_sop_ecm,
        command = {
          par_def_pre_lib_sop_ecm <-
            paths$params$default$prepare$libraries$sop$ecmdb
        }
      ),
      # tar_file(
      #   name = par_def_pre_lib_sop_hmd,
      #   command = {
      #     par_def_pre_lib_sop_hmd <-
      #       paths$params$default$prepare$libraries$sop$hmdb
      #   }
      # ),
      tar_file(
        name = par_def_pre_lib_sop_lot,
        command = {
          par_def_pre_lib_sop_lot <-
            paths$params$default$prepare$libraries$sop$lotus
        }
      ),
      tar_file(
        name = par_def_pre_lib_sop_mer,
        command = {
          par_def_pre_lib_sop_mer <-
            paths$params$default$prepare$libraries$sop$merged
        }
      ),
      tar_file(
        name = par_def_pre_lib_spe,
        command = {
          par_def_pre_lib_spe <-
            paths$params$default$prepare$libraries$spectra
        }
      ),
      tar_file(
        name = par_def_pre_tax,
        command = {
          par_def_pre_tax <- paths$params$default$prepare$taxa
        }
      ),
      tar_file(
        name = par_def_wei_ann,
        command = {
          par_def_wei_ann <- paths$params$default$weight$annotations
        }
      )
    ),
    list(
      ## Prepare params
      list(
        tar_file(
          name = par_pre_par,
          command = {
            par_pre_par <- paths$params$prepare_params
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
        )
      ),
      ## User
      list(
        tar_file(
          name = par_usr_ann_mas,
          command = {
            par_usr_ann_mas <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
                parameters = par_fin_par,
                step = "annotate_masses"
              )
          }
        ),
        tar_file(
          name = par_usr_ann_spe,
          command = {
            par_usr_ann_spe <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "annotate_spectra"
              )
          }
        ),
        tar_file(
          name = par_usr_cre_com,
          command = {
            par_usr_cre_com <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "create_components"
              )
          }
        ),
        tar_file(
          name = par_usr_cre_edg_spe,
          command = {
            par_usr_cre_edg_spe <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "create_edges_spectra"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_ann_gnp,
          command = {
            par_usr_pre_ann_gnp <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_annotations_gnps"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_ann_sir,
          command = {
            par_usr_pre_ann_sir <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_annotations_sirius"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_ann_spe,
          command = {
            par_usr_pre_ann_spe <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_annotations_spectra"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_fea_com,
          command = {
            par_usr_pre_fea_com <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_features_components"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_fea_edg,
          command = {
            par_usr_pre_fea_edg <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_features_edges"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_fea_tab,
          command = {
            par_usr_pre_fea_tab <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_features_tables"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_lib_add,
          command = {
            par_usr_pre_lib_add <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_libraries_adducts"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_lib_sop_clo,
          command = {
            par_usr_pre_lib_sop_clo <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_libraries_sop_closed"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_lib_sop_ecm,
          command = {
            par_usr_pre_lib_sop_ecm <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_libraries_sop_ecmdb"
              )
          }
        ),
        # tar_file(
        #   name = par_usr_pre_lib_sop_hmd,
        #   command = {
        #     par_usr_pre_lib_sop_hmd <-
        #       prepare_params(
        #         filename = par_fin_par$files$pattern,
        #         gnps_job_id = par_fin_par$gnps$id,
        #         ms_mode = par_fin_par$ms$polarity,
        #         taxon = par_fin_par$organisms$taxon,
        #         parameters = par_fin_par,
        #         step = "prepare_libraries_sop_hmdb"
        #       )
        #   }
        # ),
        tar_file(
          name = par_usr_pre_lib_sop_lot,
          command = {
            par_usr_pre_lib_sop_lot <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_libraries_sop_lotus"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_lib_sop_mer,
          command = {
            par_usr_pre_lib_sop_mer <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_libraries_sop_merged"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_lib_spe,
          command = {
            par_usr_pre_lib_spe <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_libraries_spectra"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_tax,
          command = {
            par_usr_pre_tax <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "prepare_taxa"
              )
          }
        ),
        tar_file(
          name = par_usr_wei_ann,
          command = {
            par_usr_wei_ann <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                gnps_job_id = par_fin_par$gnps$id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                parameters = par_fin_par,
                step = "weight_annotations"
              )
          }
        )
      )
    ),
    ## Final
    list(
      tar_target(
        name = params_annotate_masses,
        command = {
          params_annotate_masses <-
            parse_yaml_params(
              def = par_def_ann_mas,
              usr = par_usr_ann_mas[1]
            )
        }
      ),
      tar_target(
        name = params_annotate_spectra,
        command = {
          params_annotate_spectra <-
            parse_yaml_params(
              def = par_def_ann_spe,
              usr = par_usr_ann_spe[1]
            )
        }
      ),
      tar_target(
        name = params_create_components,
        command = {
          params_create_components <-
            parse_yaml_params(
              def = par_def_cre_com,
              usr = par_usr_cre_com[1]
            )
        }
      ),
      tar_target(
        name = params_create_edges_spectra,
        command = {
          params_create_edges_spectra <-
            parse_yaml_params(
              def = par_def_cre_edg_spe,
              usr = par_usr_cre_edg_spe[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_annotations_gnps,
        command = {
          params_prepare_annotations_gnps <-
            parse_yaml_params(
              def = par_def_pre_ann_gnp,
              usr = par_usr_pre_ann_gnp[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_annotations_sirius,
        command = {
          params_prepare_annotations_sirius <-
            parse_yaml_params(
              def = par_def_pre_ann_sir,
              usr = par_usr_pre_ann_sir[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_annotations_spectra,
        command = {
          params_prepare_annotations_spectra <-
            parse_yaml_params(
              def = par_def_pre_ann_spe,
              usr = par_usr_pre_ann_spe[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_features_components,
        command = {
          params_prepare_features_components <-
            parse_yaml_params(
              def = par_def_pre_fea_com,
              usr = par_usr_pre_fea_com[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_features_edges,
        command = {
          params_prepare_features_edges <-
            parse_yaml_params(
              def = par_def_pre_fea_edg,
              usr = par_usr_pre_fea_edg[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_features_tables,
        command = {
          params_prepare_features_tables <-
            parse_yaml_params(
              def = par_def_pre_fea_tab,
              usr = par_usr_pre_fea_tab[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_libraries_adducts,
        command = {
          params_prepare_libraries_adducts <-
            parse_yaml_params(
              def = par_def_pre_lib_add,
              usr = par_usr_pre_lib_add[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_libraries_sop_closed,
        command = {
          params_prepare_libraries_sop_closed <-
            parse_yaml_params(
              def = par_def_pre_lib_sop_clo,
              usr = par_usr_pre_lib_sop_clo[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_libraries_sop_ecmdb,
        command = {
          params_prepare_libraries_sop_ecmdb <-
            parse_yaml_params(
              def = par_def_pre_lib_sop_ecm,
              usr = par_usr_pre_lib_sop_ecm[1]
            )
        }
      ),
      # tar_target(
      #   name = params_prepare_libraries_sop_hmdb,
      #   command = {
      #     params_prepare_libraries_sop_hmdb <-
      #       parse_yaml_params(
      #         def = par_def_pre_lib_sop_hmd,
      #         usr = par_usr_pre_lib_sop_hmd[1]
      #       )
      #   }
      # ),
      tar_target(
        name = params_prepare_libraries_sop_lotus,
        command = {
          params_prepare_libraries_sop_lotus <-
            parse_yaml_params(
              def = par_def_pre_lib_sop_lot,
              usr = par_usr_pre_lib_sop_lot[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_libraries_sop_merged,
        command = {
          params_prepare_libraries_sop_merged <-
            parse_yaml_params(
              def = par_def_pre_lib_sop_mer,
              usr = par_usr_pre_lib_sop_mer[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_libraries_spectra,
        command = {
          params_prepare_libraries_spectra <-
            parse_yaml_params(
              def = par_def_pre_lib_spe,
              usr = par_usr_pre_lib_spe[1]
            )
        }
      ),
      tar_target(
        name = params_prepare_taxa,
        command = {
          params_prepare_taxa <-
            parse_yaml_params(
              def = par_def_pre_tax,
              usr = par_usr_pre_tax[1]
            )
        }
      ),
      tar_target(
        name = params_weight_annotations,
        command = {
          params_weight_annotations <-
            parse_yaml_params(
              def = par_def_wei_ann,
              usr = par_usr_wei_ann[1]
            )
        }
      )
    )
  ),
  ## GNPS
  list(
    tar_file(
      name = gnps_tables,
      command = {
        gnps_tables <- get_gnps_tables(
          gnps_job_id = par_fin_par$gnps$id,
          workflow = par_fin_par$gnps$workflow,
          path_features = params_prepare_features_tables$files$features$raw,
          path_metadata = params_prepare_taxa$files$taxa$raw,
          path_spectra = params_annotate_spectra$files$spectral$raw,
          path_source = paths$data$source$path,
          path_interim_a = paths$data$interim$annotations$path,
          path_interim_f = paths$data$interim$features$path
        )
      }
    ),
    list(
      tar_file(
        name = gnps_features,
        command = {
          gnps_features <- gnps_tables[[1]]
        }
      ),
      tar_file(
        name = gnps_metadata,
        command = {
          gnps_metadata <- gnps_tables[[2]]
        }
      ),
      tar_file(
        name = gnps_spectra,
        command = {
          gnps_spectra <- gnps_tables[[3]]
        }
      ),
      tar_file(
        name = gnps_annotations,
        command = {
          gnps_annotations <- gnps_tables[[4]]
        }
      ),
      tar_file(
        name = gnps_components,
        command = {
          gnps_components <- gnps_tables[[5]]
        }
      ),
      tar_file(
        name = gnps_edges,
        command = {
          gnps_edges <- gnps_tables[[6]]
        }
      )
    )
  ),
  ## Inputs
  list(
    tar_file(
      name = input_features,
      command = {
        input_features <-
          ifelse(
            test = !is.null(gnps_features),
            yes =
              ifelse(
                test = file.exists(gnps_features),
                yes = gnps_features,
                no = params_prepare_taxa$files$features$raw
              ),
            no = params_prepare_taxa$files$features$raw
          )
      }
    ),
    tar_file(
      name = input_spectra,
      command = {
        input_spectra <-
          ifelse(
            test = paths$tests$mode == FALSE,
            yes = ifelse(
              test = !is.null(gnps_spectra),
              yes =
                ifelse(
                  test = file.exists(gnps_spectra),
                  yes = gnps_spectra,
                  no = params_annotate_spectra$files$spectral$raw
                ),
              no = params_annotate_spectra$files$spectral$raw
            ),
            no = {
              get_file(
                url = paths$url$examples$spectra_mini,
                export = paths$data$source$spectra
              )
              return(paths$data$source$spectra)
            }
          )
      }
    ),
    tar_file(
      name = input_metadata,
      command = {
        input_metadata <-
          ifelse(
            test = !is.null(gnps_metadata),
            yes =
              ifelse(
                test = file.exists(gnps_metadata),
                yes = gnps_metadata,
                no = params_prepare_taxa$files$taxa$raw
              ),
            no = params_prepare_taxa$files$taxa$raw
          )
      }
    )
  ),
  ## libraries
  list(
    ## Structure organism pairs
    list(
      ## Raw
      list(
        ## This does not work as it forces the file to exist.
        ## So targets will not check if the input file changed automatically.
        # tar_file(
        #   name = library_sop_closed,
        #   command = {
        #     library_sop_closed <- paths$data$source$libraries$sop$closed
        #   }
        # ),
        tar_file(
          name = library_sop_ecmdb,
          command = {
            library_sop_ecmdb <- get_file(
              url = paths$urls$ecmdb$metabolites,
              export = paths$data$source$libraries$sop$ecmdb
            )
          }
        ),
        ## TODO ADD  GET HMDB
        tar_file(
          name = library_sop_lotus,
          command = {
            library_sop_lotus <- get_last_version_from_zenodo(
              doi = paths$url$lotus$doi,
              pattern = paths$urls$lotus$pattern,
              path = paths$data$source$libraries$sop$lotus
            )
          },
          ## To always check if a newest version is available
          cue = tar_cue(mode = "always")
          # cue = tar_cue(mode = "thorough")
        )
      ),
      ## Prepared
      list(
        tar_file(
          name = library_sop_closed_prepared,
          command = {
            library_sop_closed_prepared <-
              prepare_libraries_sop_closed(
                ## TODO improve
                input = paths$data$source$libraries$sop$closed,
                output = params_prepare_libraries_sop_closed$files$libraries$sop$prepared,
                parameters = params_prepare_libraries_sop_closed
              )
          }
        ),
        tar_file(
          name = library_sop_ecmdb_prepared,
          command = {
            library_sop_ecmdb_prepared <-
              prepare_libraries_sop_ecmdb(
                input = library_sop_ecmdb,
                output = params_prepare_libraries_sop_ecmdb$files$libraries$sop$prepared,
                parameters = params_prepare_libraries_sop_ecmdb
              )
          }
        ),
        ## TODO ADD HMDB PREPARED
        tar_file(
          name = library_sop_lotus_prepared,
          command = {
            library_sop_lotus_prepared <-
              prepare_libraries_sop_lotus(
                input = if (paths$tests$mode == FALSE) {
                  library_sop_lotus
                } else {
                  paths$data$source$libraries$sop$lotus
                },
                output = params_prepare_libraries_sop_lotus$files$libraries$sop$prepared
              )
          }
        )
      ),
      ## Merged
      list(
        tar_file(
          name = library_sop_merged,
          command = {
            library_sop_merged <- prepare_libraries_sop_merged(
              files = c(
                library_sop_closed_prepared,
                library_sop_ecmdb_prepared,
                ## TODO
                # library_sop_hmdb_prepared,
                library_sop_lotus_prepared
              ),
              filter = params_prepare_libraries_sop_merged$organisms$filter$mode,
              level = params_prepare_libraries_sop_merged$organisms$filter$level,
              value = params_prepare_libraries_sop_merged$organisms$filter$value,
              output_key = paths$data$interim$libraries$sop$merged$keys,
              output_org_tax_ott = paths$data$interim$libraries$sop$merged$organisms$taxonomies$ott,
              output_str_2D_3D = paths$data$interim$libraries$sop$merged$structures$dd_ddd,
              output_str_met = paths$data$interim$libraries$sop$merged$structures$metadata,
              output_str_nam = paths$data$interim$libraries$sop$merged$structures$names,
              output_str_tax_cla = paths$data$interim$libraries$sop$merged$structures$taxonomies$classyfire,
              output_str_tax_npc = paths$data$interim$libraries$sop$merged$structures$taxonomies$npc,
              parameters = params_prepare_libraries_sop_merged
            )
          }
        ),
        tar_file(name = library_merged_key, command = {
          library_merged_key <- library_sop_merged[[1]]
        }),
        tar_file(name = library_merged_org_tax_ott, command = {
          library_merged_org_tax_ott <- library_sop_merged[[2]]
        }),
        tar_file(name = library_merged_str_2D_3D, command = {
          library_merged_str_2D_3D <- library_sop_merged[[3]]
        }),
        tar_file(name = library_merged_str_met, command = {
          library_merged_str_met <- library_sop_merged[[4]]
        }),
        tar_file(name = library_merged_str_nam, command = {
          library_merged_str_nam <- library_sop_merged[[5]]
        }),
        tar_file(name = library_merged_str_tax_cla, command = {
          library_merged_str_tax_cla <- library_sop_merged[[6]]
        }),
        tar_file(name = library_merged_str_tax_npc, command = {
          library_merged_str_tax_npc <- library_sop_merged[[7]]
        })
      )
    ),
    ## Adducts
    list(tar_file(
      name = library_adducts,
      command = {
        library_adducts <- prepare_libraries_adducts(
          str_met = library_merged_str_met,
          adducts_masses = dic_adducts,
          adducts_output_path = paths$data$interim$libraries$adducts$path,
          output_name = params_prepare_libraries_adducts$files$libraries$adducts$prepared,
          masses_pos_output_path = paths$data$interim$libraries$adducts$pos,
          masses_neg_output_path = paths$data$interim$libraries$adducts$neg,
          parameters = params_prepare_libraries_adducts
        )
      }
    )),
    ## Spectra
    list( ## In silico
      list( ## Raw
        list(
          ## TODO ADD ISDB HMDB,
          tar_file(
            name = library_spectra_is_lotus_pos,
            command = {
              library_spectra_is_lotus_pos <-
                if (paths$tests$mode == FALSE) {
                  get_file(
                    url = paths$urls$examples$spectral_lib$pos,
                    export = paths$data$source$libraries$spectra$is$lotus$pos
                  )
                  # get_last_version_from_zenodo(
                  #   doi = paths$url$lotus_isdb$doi,
                  #   pattern = paths$urls$lotus_isdb$pattern$pos,
                  #   path = paths$data$source$libraries$spectra$is$lotus$pos
                  # )
                } else {
                  get_file(
                    url = paths$urls$examples$spectral_lib$pos,
                    export = paths$data$source$libraries$spectra$is$lotus$pos
                  )
                }
            }
            ## To always check if a newest version is available
            ,
            cue = tar_cue(mode = "always")
            # cue = tar_cue(mode = "thorough")
          ),
          tar_file(
            name = library_spectra_is_lotus_neg,
            command = {
              library_spectra_is_lotus_neg <-
                if (paths$tests$mode == FALSE) {
                  get_file(
                    url = paths$urls$examples$spectral_lib$neg,
                    export = paths$data$source$libraries$spectra$is$lotus$neg
                  )
                  # get_last_version_from_zenodo(
                  #   doi = paths$url$lotus_isdb$doi,
                  #   pattern = paths$urls$lotus_isdb$pattern$neg,
                  #   path = paths$data$source$libraries$spectra$is$lotus$neg
                  # )
                } else {
                  get_file(
                    url = paths$urls$examples$spectral_lib$neg,
                    export = paths$data$source$libraries$spectra$is$lotus$neg
                  )
                }
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
        ## TODO improve polarity handling, suboptimal
        tar_file(
          name = library_spectra_is_lotus_prepared_pos,
          command = {
            library_spectra_is_lotus_prepared_pos <-
              library_spectra_is_lotus_pos
            # library_spectra_is_lotus_prepared_pos <- prepare_libraries_spectra(
            #   input = if (paths$tests$mode == FALSE) {
            #     library_spectra_is_lotus_pos
            #   } else {
            #     paths$data$source$libraries$spectra$is$lotus$pos
            #   },
            #   output = paths$data$interim$libraries$spectra$is$lotus$pos,
            #   col_ce = NULL,
            #   col_ci = "FILENAME",
            #   col_em = "EXACTMASS",
            #   col_in = NULL,
            #   col_io = "INCHI",
            #   col_ik = NULL,
            #   col_il = "NAME",
            #   col_mf = "MOLECULAR_FORMULA",
            #   col_na = NULL,
            #   col_po = "IONMODE",
            #   col_sm = NULL,
            #   col_sn = "SMILES",
            #   col_si = NULL,
            #   col_sp = NULL,
            #   col_sy = NULL,
            #   col_xl = NULL,
            #   polarity = "pos",
            #   metad = CompoundDb::make_metadata(
            #     source = "LOTUS",
            #     url = "https://doi.org/10.5281/zenodo.5607185",
            #     source_version = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")$doi_url,
            #     source_date = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")[["metadata"]][["publication_date"]],
            #     organism = "Life"
            #   )
            # )
          }
        ),
        tar_file(
          name = library_spectra_is_lotus_prepared_neg,
          command = {
            library_spectra_is_lotus_prepared_neg <-
              library_spectra_is_lotus_neg
            # library_spectra_is_lotus_prepared_neg <- prepare_libraries_spectra(
            #   input = if (paths$tests$mode == FALSE) {
            #     library_spectra_is_lotus_neg
            #   } else {
            #     paths$data$source$libraries$spectra$is$lotus$neg
            #   },
            #   output = paths$data$interim$libraries$spectra$is$lotus$neg,
            #   col_ce = NULL,
            #   col_ci = "FILENAME",
            #   col_em = "EXACTMASS",
            #   col_in = NULL,
            #   col_io = "INCHI",
            #   col_ik = NULL,
            #   col_il = "NAME",
            #   col_mf = "MOLECULAR_FORMULA",
            #   col_na = NULL,
            #   col_po = "IONMODE",
            #   col_sm = NULL,
            #   col_sn = "SMILES",
            #   col_si = NULL,
            #   col_sp = NULL,
            #   col_sy = NULL,
            #   col_xl = NULL,
            #   metad = CompoundDb::make_metadata(
            #     source = "LOTUS",
            #     url = "https://doi.org/10.5281/zenodo.5607185",
            #     source_version = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")$doi_url,
            #     source_date = jsonlite::fromJSON(txt = "https://zenodo.org/api/records/5607185")[["metadata"]][["publication_date"]],
            #     organism = "Life"
            #   ),
            #   polarity = "neg"
            # )
          }
        )
      ),
      ## Experimental
      list(
        ## RAW
        list(tar_file(
          name = library_spectra_exp_internal_raw,
          command = {
            library_spectra_exp_internal_raw <-
              params_prepare_libraries_spectra$files$libraries$spectral$exp$raw
          }
        )),
        ## Prepared
        list(
          ## TODO improve polarity handling, suboptimal
          tar_file(
            name = library_spectra_exp_internal_prepared_pos,
            command = {
              library_spectra_exp_internal_prepared_pos <-
                prepare_libraries_spectra(
                  input = library_spectra_exp_internal_raw,
                  output = params_prepare_libraries_spectra$files$libraries$spectral$exp,
                  polarity = "pos",
                  col_ce = params_prepare_libraries_spectra$names$mgf$collision_energy,
                  col_ci = params_prepare_libraries_spectra$names$mgf$compound_id,
                  col_em = params_prepare_libraries_spectra$names$mgf$exact_mass,
                  col_in = params_prepare_libraries_spectra$names$mgf$inchi,
                  col_io = params_prepare_libraries_spectra$names$mgf$inchi_2D,
                  col_ik = params_prepare_libraries_spectra$names$mgf$inchikey,
                  col_il = params_prepare_libraries_spectra$names$mgf$inchikey_2D,
                  col_mf = params_prepare_libraries_spectra$names$mgf$molecular_formula,
                  col_na = params_prepare_libraries_spectra$names$mgf$name,
                  col_po = params_prepare_libraries_spectra$names$mgf$polarity,
                  col_sm = params_prepare_libraries_spectra$names$mgf$smiles,
                  col_sn = params_prepare_libraries_spectra$names$mgf$smiles_2D,
                  col_si = params_prepare_libraries_spectra$names$mgf$spectrum_id,
                  col_sp = params_prepare_libraries_spectra$names$mgf$splash,
                  col_sy = params_prepare_libraries_spectra$names$mgf$synonyms,
                  col_xl = params_prepare_libraries_spectra$names$mgf$xlogp
                )
            }
          ),
          tar_file(
            name = library_spectra_exp_internal_prepared_neg,
            command = {
              library_spectra_exp_internal_prepared_neg <-
                prepare_libraries_spectra(
                  input = library_spectra_exp_internal_raw,
                  output = params_prepare_libraries_spectra$files$libraries$spectral$exp,
                  polarity = "neg",
                  col_ce = params_prepare_libraries_spectra$names$mgf$collision_energy,
                  col_ci = params_prepare_libraries_spectra$names$mgf$compound_id,
                  col_em = params_prepare_libraries_spectra$names$mgf$exact_mass,
                  col_in = params_prepare_libraries_spectra$names$mgf$inchi,
                  col_io = params_prepare_libraries_spectra$names$mgf$inchi_2D,
                  col_ik = params_prepare_libraries_spectra$names$mgf$inchikey,
                  col_il = params_prepare_libraries_spectra$names$mgf$inchikey_2D,
                  col_mf = params_prepare_libraries_spectra$names$mgf$molecular_formula,
                  col_na = params_prepare_libraries_spectra$names$mgf$name,
                  col_po = params_prepare_libraries_spectra$names$mgf$polarity,
                  col_sm = params_prepare_libraries_spectra$names$mgf$smiles,
                  col_sn = params_prepare_libraries_spectra$names$mgf$smiles_2D,
                  col_si = params_prepare_libraries_spectra$names$mgf$spectrum_id,
                  col_sp = params_prepare_libraries_spectra$names$mgf$splash,
                  col_sy = params_prepare_libraries_spectra$names$mgf$synonyms,
                  col_xl = params_prepare_libraries_spectra$names$mgf$xlogp
                )
            }
          )
        )
      )
    )
  ),
  ## Annotations
  list(
    ## MS1
    list(
      tar_file(
        name = annotations_ms1_prepared,
        command = {
          annotations_ms1_prepared <-
            annotate_masses(
              features = features_prepared,
              library = library_merged_key,
              output_annotations = params_annotate_masses$files$annotations$prepared,
              output_edges = params_annotate_masses$files$networks$spectral$edges$raw,
              name_source = params_annotate_masses$names$source,
              name_target = params_annotate_masses$names$target,
              str_2D_3D = library_merged_str_2D_3D,
              str_met = library_merged_str_met,
              str_nam = library_merged_str_nam,
              str_tax_cla = library_merged_str_tax_cla,
              str_tax_npc = library_merged_str_tax_npc,
              name = library_adducts[params_annotate_masses$ms$polarity],
              adducts_list = params_annotate_masses$ms$adducts,
              adducts_masses_list = dic_adducts,
              neutral_losses_list = dic_neutral_losses,
              msMode = params_annotate_masses$ms$polarity,
              tolerancePpm = params_annotate_masses$ms$tolerances$mass$ppm$ms1,
              toleranceRt = params_annotate_masses$ms$tolerances$rt$minutes,
              parameters = params_annotate_masses
            )
        }
      ),
      tar_file(
        name = annotations_ms1_prepared_annotations,
        command = {
          annotations_ms1_prepared_annotations <-
            annotations_ms1_prepared[[1]]
        }
      ),
      tar_file(
        name = annotations_ms1_prepared_edges,
        command = {
          annotations_ms1_prepared_edges <- annotations_ms1_prepared[[2]]
        }
      )
    ),
    ## Spectral
    list(
      ## Experimental
      list(
        tar_file(
          name = annotations_spectral_exp_gnps_prepared,
          command = {
            annotations_spectral_exp_gnps_prepared <-
              prepare_annotations_gnps(
                input = gnps_annotations,
                output = params_prepare_annotations_gnps$files$annotations$prepared,
                str_2D_3D = library_merged_str_2D_3D,
                str_met = library_merged_str_met,
                str_nam = library_merged_str_nam,
                str_tax_cla = library_merged_str_tax_cla,
                str_tax_npc = library_merged_str_tax_npc,
                parameters = params_prepare_annotations_gnps
              )
          }
        ),
        tar_file(
          name = annotations_spectral_exp_internal_pos,
          command = {
            annotations_spectral_exp_internal_pos <- annotate_spectra(
              input = input_spectra,
              library = library_spectra_exp_internal_prepared_pos,
              polarity = "pos",
              output = gsub(
                pattern = "matches.tsv.gz",
                replacement = "matches_exp_rt_pos.tsv.gz",
                x = params_annotate_spectra$files$annotations$raw$spectral,
                fixed = TRUE
              ),
              method = params_annotate_spectra$annotations$ms2$method,
              threshold = params_annotate_spectra$annotations$ms2$thresholds$similarity,
              ppm = params_annotate_spectra$ms$tolerances$mass$ppm$ms2,
              dalton = params_annotate_spectra$ms$tolerances$mass$dalton$ms2,
              npeaks = params_annotate_spectra$annotations$ms2$thresholds$peaks$absolute,
              rpeaks = params_annotate_spectra$annotations$ms2$thresholds$peaks$ratio,
              condition = params_annotate_spectra$annotations$ms2$thresholds$condition,
              qutoff = params_annotate_spectra$ms$intensity$thresholds$ms2,
              parallel = params_annotate_spectra$options$parallel,
              fast = params_annotate_spectra$options$fast,
              approx = params_annotate_spectra$annotations$ms2$approx,
              parameters = params_annotate_spectra
            )
          }
        ),
        tar_file(
          name = annotations_spectral_exp_internal_neg,
          command = {
            annotations_spectral_exp_internal_neg <- annotate_spectra(
              input = input_spectra,
              library = library_spectra_exp_internal_prepared_neg,
              polarity = "neg",
              output = gsub(
                pattern = "matches.tsv.gz",
                replacement = "matches_exp_rt_neg.tsv.gz",
                x = params_annotate_spectra$files$annotations$raw$spectral,
                fixed = TRUE
              ),
              method = params_annotate_spectra$annotations$ms2$method,
              threshold = params_annotate_spectra$annotations$ms2$thresholds$similarity,
              ppm = params_annotate_spectra$ms$tolerances$mass$ppm$ms2,
              dalton = params_annotate_spectra$ms$tolerances$mass$dalton$ms2,
              npeaks = params_annotate_spectra$annotations$ms2$thresholds$peaks$absolute,
              rpeaks = params_annotate_spectra$annotations$ms2$thresholds$peaks$ratio,
              condition = params_annotate_spectra$annotations$ms2$thresholds$condition,
              qutoff = params_annotate_spectra$ms$intensity$thresholds$ms2,
              parallel = params_annotate_spectra$options$parallel,
              fast = params_annotate_spectra$options$fast,
              approx = params_annotate_spectra$annotations$ms2$approx,
              parameters = params_annotate_spectra
            )
          }
        ),
        tar_file(
          name = annotations_spectral_exp_internal_prepared,
          command = {
            annotations_spectral_exp_internal_prepared <-
              prepare_annotations_spectra(
                input = list(
                  annotations_spectral_exp_internal_neg,
                  annotations_spectral_exp_internal_pos
                ),
                output = gsub(
                  pattern = "_prepared.tsv.gz",
                  replacement = "_exp_rt_prepared.tsv.gz",
                  x = params_prepare_annotations_spectra$files$annotations$prepared,
                  fixed = TRUE
                ),
                str_2D_3D = library_merged_str_2D_3D,
                str_met = library_merged_str_met,
                str_nam = library_merged_str_nam,
                str_tax_cla = library_merged_str_tax_cla,
                str_tax_npc = library_merged_str_tax_npc,
                parameters = params_prepare_annotations_spectra
              )
          }
        )
      ),
      ## In silico
      list(
        ## TODO add HMDB is spectral matches
        ## TODO improve polarity handling, suboptimal
        tar_file(
          name = annotations_spectral_is_lotus_pos,
          command = {
            annotations_spectral_is_lotus_pos <- annotate_spectra(
              input = input_spectra,
              library = library_spectra_is_lotus_prepared_pos,
              polarity = "pos",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_pos.tsv.gz",
                x = params_annotate_spectra$files$annotations$raw$spectral,
                fixed = TRUE
              ),
              method = params_annotate_spectra$annotations$ms2$method,
              threshold = params_annotate_spectra$annotations$ms2$thresholds$similarity,
              ppm = params_annotate_spectra$ms$tolerances$mass$ppm$ms2,
              dalton = params_annotate_spectra$ms$tolerances$mass$dalton$ms2,
              npeaks = params_annotate_spectra$annotations$ms2$thresholds$peaks$absolute,
              rpeaks = params_annotate_spectra$annotations$ms2$thresholds$peaks$ratio,
              condition = params_annotate_spectra$annotations$ms2$thresholds$condition,
              qutoff = params_annotate_spectra$ms$intensity$thresholds$ms2,
              parallel = params_annotate_spectra$options$parallel,
              fast = params_annotate_spectra$options$fast,
              approx = params_annotate_spectra$annotations$ms2$approx,
              parameters = params_annotate_spectra
            )
          }
        ),
        tar_file(
          name = annotations_spectral_is_lotus_neg,
          command = {
            annotations_spectral_is_lotus_neg <- annotate_spectra(
              input = input_spectra,
              library = library_spectra_is_lotus_prepared_neg,
              polarity = "neg",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_neg.tsv.gz",
                x = params_annotate_spectra$files$annotations$raw$spectral,
                fixed = TRUE
              ),
              method = params_annotate_spectra$annotations$ms2$method,
              threshold = params_annotate_spectra$annotations$ms2$thresholds$similarity,
              ppm = params_annotate_spectra$ms$tolerances$mass$ppm$ms2,
              dalton = params_annotate_spectra$ms$tolerances$mass$dalton$ms2,
              npeaks = params_annotate_spectra$annotations$ms2$thresholds$peaks$absolute,
              rpeaks = params_annotate_spectra$annotations$ms2$thresholds$peaks$ratio,
              condition = params_annotate_spectra$annotations$ms2$thresholds$condition,
              qutoff = params_annotate_spectra$ms$intensity$thresholds$ms2,
              parallel = params_annotate_spectra$options$parallel,
              fast = params_annotate_spectra$options$fast,
              approx = params_annotate_spectra$annotations$ms2$approx,
              parameters = params_annotate_spectra
            )
          }
        ),
        tar_file(
          name = annotations_spectral_is_prepared,
          command = {
            annotations_spectral_is_prepared <- prepare_annotations_spectra(
              input = list(
                annotations_spectral_is_lotus_neg,
                ## TODO add is hmdb
                annotations_spectral_is_lotus_pos
              ),
              output = params_prepare_annotations_spectra$files$annotations$prepared,
              str_2D_3D = library_merged_str_2D_3D,
              str_met = library_merged_str_met,
              str_nam = library_merged_str_nam,
              str_tax_cla = library_merged_str_tax_cla,
              str_tax_npc = library_merged_str_tax_npc,
              parameters = params_prepare_annotations_spectra
            )
          }
        )
      )
    ),
    # SIRIUS
    tar_file(
      name = annotations_sirius_prepared,
      command = {
        annotations_sirius_prepared <-
          prepare_annotations_sirius(
            input_directory = params_prepare_annotations_sirius$files$annotations$raw$sirius,
            output = params_prepare_annotations_sirius$files$annotations$prepared,
            str_2D_3D = library_merged_str_2D_3D,
            str_met = library_merged_str_met,
            str_nam = library_merged_str_nam,
            str_tax_cla = library_merged_str_tax_cla,
            str_tax_npc = library_merged_str_tax_npc,
            parameters = params_prepare_annotations_sirius
          )
      }
    ),
    list()
  ),
  ## Features
  list(
    tar_file(
      name = features_edges_spectra,
      command = {
        features_edges_spectra <- create_edges_spectra(
          input = input_spectra,
          output = params_create_edges_spectra$files$networks$spectral$edges$raw,
          name_source = params_create_edges_spectra$names$source,
          name_target = params_create_edges_spectra$names$target,
          method = params_create_edges_spectra$annotations$ms2$method,
          threshold = params_create_edges_spectra$annotations$ms2$thresholds$similarity,
          ppm = params_create_edges_spectra$ms$tolerances$mass$ppm$ms2,
          dalton = params_create_edges_spectra$ms$tolerances$mass$dalton$ms2,
          npeaks = params_create_edges_spectra$annotations$ms2$thresholds$peaks$absolute,
          rpeaks = params_create_edges_spectra$annotations$ms2$thresholds$peaks$ratio,
          condition = params_create_edges_spectra$annotations$ms2$thresholds$condition,
          qutoff = params_create_edges_spectra$ms$intensity$thresholds$ms2,
          parallel = params_create_edges_spectra$options$parallel,
          fast = params_create_edges_spectra$options$fast,
          parameters = params_create_edges_spectra
        )
      }
    ),
    tar_file(
      name = features_components,
      command = {
        features_components <- create_components(
          input = features_edges_prepared,
          output = params_create_components$files$networks$spectral$components$raw,
          parameters = params_create_components
        )
      }
    ),
    ## Interim
    list(
      tar_file(
        name = interim_components,
        command = {
          interim_components <-
            ifelse(test = file.exists(gnps_components),
              yes = gnps_components,
              no = features_components
            )
        }
      ),
      tar_file(
        name = edges_spectra,
        command = {
          edges_spectra <-
            ifelse(test = file.exists(gnps_edges),
              yes = gnps_edges,
              no = features_edges_spectra
            )
        }
      )
    ),
    tar_file(
      name = features_edges_prepared,
      command = {
        features_edges_prepared <- prepare_features_edges(
          input = list(annotations_ms1_prepared_edges, edges_spectra),
          output = params_prepare_features_edges$files$networks$spectral$edges$prepared,
          name_source = params_prepare_features_edges$names$source,
          name_target = params_prepare_features_edges$names$target,
          parameters = params_prepare_features_edges
        )
      }
    ),
    tar_file(
      name = features_components_prepared,
      command = {
        features_components_prepared <- prepare_features_components(
          input = interim_components,
          output = params$files$networks$spectral$components$prepared,
          parameters = params_prepare_features_components
        )
      }
    ),
    tar_file(
      name = features_prepared,
      command = {
        features_prepared <- prepare_features_tables(
          features = params_prepare_features_tables$files$features$raw,
          output = params_prepare_features_tables$files$features$prepared,
          name_features = params_prepare_features_tables$names$features,
          name_rt = params_prepare_features_tables$names$rt,
          name_mz = params_prepare_features_tables$names$precursor,
          parameters = params_prepare_features_tables
        )
      }
    )
  ),
  tar_file(
    name = taxa_prepared,
    command = {
      taxa_prepared <- prepare_taxa(
        input = input_features,
        extension = params_prepare_taxa$names$extension,
        colname = params_prepare_taxa$names$taxon,
        metadata = input_metadata,
        top_k = params_prepare_taxa$organisms$candidates,
        org_tax_ott = library_merged_org_tax_ott,
        output = params_prepare_taxa$files$taxa$prepared,
        taxon = params_prepare_taxa$organisms$taxon,
        parameters = params_prepare_taxa
      )
    }
  ),
  tar_file(
    name = annotations_prepared,
    command = {
      annotations_prepared <- weight_annotations(
        library = library_merged_key,
        str_2D_3D = library_merged_str_2D_3D,
        annotations = list(
          annotations_spectral_is_prepared,
          annotations_ms1_prepared_annotations
        ),
        components = features_components_prepared,
        edges = features_edges_prepared,
        features = features_prepared,
        taxa = taxa_prepared,
        output = params_weight_annotations$files$annotations$processed,
        candidates_initial = params_weight_annotations$annotations$candidates$initial,
        candidates_final = params_weight_annotations$annotations$candidates$final,
        weight_spectral = params_weight_annotations$weights$global$spectral,
        weight_chemical = params_weight_annotations$weights$global$chemical,
        weight_biological = params_weight_annotations$weights$global$biological,
        score_biological_domain = params_weight_annotations$weights$biological$domain,
        score_biological_kingdom = params_weight_annotations$weights$biological$kingdom,
        score_biological_phylum = params_weight_annotations$weights$biological$phylum,
        score_biological_class = params_weight_annotations$weights$biological$class,
        score_biological_order = params_weight_annotations$weights$biological$order,
        score_biological_infraorder = params_weight_annotations$weights$biological$infraorder,
        score_biological_family = params_weight_annotations$weights$biological$family,
        score_biological_subfamily = params_weight_annotations$weights$biological$subfamily,
        score_biological_tribe = params_weight_annotations$weights$biological$tribe,
        score_biological_subtribe = params_weight_annotations$weights$biological$subtribe,
        score_biological_genus = params_weight_annotations$weights$biological$genus,
        score_biological_subgenus = params_weight_annotations$weights$biological$subgenus,
        score_biological_species = params_weight_annotations$weights$biological$species,
        score_biological_subspecies = params_weight_annotations$weights$biological$subspecies,
        score_biological_variety = params_weight_annotations$weights$biological$variety,
        score_chemical_cla_kingdom = params_weight_annotations$weights$chemical$cla$kingdom,
        score_chemical_cla_superclass = params_weight_annotations$weights$chemical$cla$superclass,
        score_chemical_cla_class = params_weight_annotations$weights$chemical$cla$class,
        score_chemical_cla_parent = params_weight_annotations$weights$chemical$cla$parent,
        score_chemical_npc_pathway = params_weight_annotations$weights$chemical$npc$pathway,
        score_chemical_npc_superclass = params_weight_annotations$weights$chemical$npc$superclass,
        score_chemical_npc_class = params_weight_annotations$weights$chemical$npc$class,
        minimal_ms1_bio = params_weight_annotations$annotations$ms1$thresholds$biological,
        minimal_ms1_chemo = params_weight_annotations$annotations$ms1$thresholds$chemical,
        # TODO ADD CONDITION
        ms1_only = params_weight_annotations$annotations$ms1only,
        summarise = params_weight_annotations$options$summarise,
        force = params_weight_annotations$options$force,
        parameters = params_weight_annotations
      )
    }
  ),
  tar_file(
    name = annotations_prepared_crazy,
    command = {
      annotations_prepared_crazy <- weight_annotations(
        library = library_merged_key,
        str_2D_3D = library_merged_str_2D_3D,
        annotations = list(
          annotations_spectral_exp_gnps_prepared,
          annotations_spectral_exp_internal_prepared,
          annotations_spectral_is_prepared,
          annotations_sirius_prepared,
          annotations_ms1_prepared_annotations
        ),
        components = features_components_prepared,
        edges = features_edges_prepared,
        features = features_prepared,
        taxa = taxa_prepared,
        output = params_weight_annotations$files$annotations$processed,
        candidates_initial = params_weight_annotations$annotations$candidates$initial,
        candidates_final = params_weight_annotations$annotations$candidates$final,
        weight_spectral = params_weight_annotations$weights$global$spectral,
        weight_chemical = params_weight_annotations$weights$global$chemical,
        weight_biological = params_weight_annotations$weights$global$biological,
        score_biological_domain = params_weight_annotations$weights$biological$domain,
        score_biological_kingdom = params_weight_annotations$weights$biological$kingdom,
        score_biological_phylum = params_weight_annotations$weights$biological$phylum,
        score_biological_class = params_weight_annotations$weights$biological$class,
        score_biological_order = params_weight_annotations$weights$biological$order,
        score_biological_infraorder = params_weight_annotations$weights$biological$infraorder,
        score_biological_family = params_weight_annotations$weights$biological$family,
        score_biological_subfamily = params_weight_annotations$weights$biological$subfamily,
        score_biological_tribe = params_weight_annotations$weights$biological$tribe,
        score_biological_subtribe = params_weight_annotations$weights$biological$subtribe,
        score_biological_genus = params_weight_annotations$weights$biological$genus,
        score_biological_subgenus = params_weight_annotations$weights$biological$subgenus,
        score_biological_species = params_weight_annotations$weights$biological$species,
        score_biological_subspecies = params_weight_annotations$weights$biological$subspecies,
        score_biological_variety = params_weight_annotations$weights$biological$variety,
        score_chemical_cla_kingdom = params_weight_annotations$weights$chemical$cla$kingdom,
        score_chemical_cla_superclass = params_weight_annotations$weights$chemical$cla$superclass,
        score_chemical_cla_class = params_weight_annotations$weights$chemical$cla$class,
        score_chemical_cla_parent = params_weight_annotations$weights$chemical$cla$parent,
        score_chemical_npc_pathway = params_weight_annotations$weights$chemical$npc$pathway,
        score_chemical_npc_superclass = params_weight_annotations$weights$chemical$npc$superclass,
        score_chemical_npc_class = params_weight_annotations$weights$chemical$npc$class,
        minimal_ms1_bio = params_weight_annotations$annotations$ms1$thresholds$biological,
        minimal_ms1_chemo = params_weight_annotations$annotations$ms1$thresholds$chemical,
        # TODO ADD CONDITION
        ms1_only = params_weight_annotations$annotations$ms1only,
        summarise = params_weight_annotations$options$summarise,
        force = params_weight_annotations$options$force,
        parameters = params_weight_annotations
      )
    }
  )
)

## TODO Add benchmark
