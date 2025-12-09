# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tima"),
  memory = "transient",
  garbage_collection = TRUE
)

# tar_make_clustermq() configuration (okay to leave alone):

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}}
# to allow use_targets() to configure tar_make_future() options.

# Replace the target list below with your own:
list(
  ## Architecture
  list(
    ## Paths
    list(
      tar_target(
        name = yaml_paths,
        command = {
          yaml_paths <- system.file("paths.yaml", package = "tima")
        },
        format = "file"
      ),
      tar_target(
        name = paths,
        command = {
          paths <- tima:::get_default_paths(yaml = yaml_paths)
        },
        format = "rds"
      )
    )
  ),
  ## Params
  list(
    ## Default
    list(
      tar_target(
        name = par_def_ann_mas,
        command = {
          par_def_ann_mas <- system.file(
            "params/default/annotate_masses.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_ann_spe,
        command = {
          par_def_ann_spe <- system.file(
            "params/default/annotate_spectra.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_cre_com,
        command = {
          par_def_cre_com <- system.file(
            "params/default/create_components.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_cre_edg_spe,
        command = {
          par_def_cre_edg_spe <- system.file(
            "params/default/create_edges_spectra.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_fil_ann,
        command = {
          par_def_fil_ann <- system.file(
            "params/default/filter_annotations.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_ann_gnp,
        command = {
          par_def_pre_ann_gnp <- system.file(
            "params/default/prepare_annotations_gnps.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_ann_sir,
        command = {
          par_def_pre_ann_sir <- system.file(
            "params/default/prepare_annotations_sirius.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_ann_spe,
        command = {
          par_def_pre_ann_spe <- system.file(
            "params/default/prepare_annotations_spectra.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_fea_com,
        command = {
          par_def_pre_fea_com <- system.file(
            "params/default/prepare_features_components.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_fea_edg,
        command = {
          par_def_pre_fea_edg <- system.file(
            "params/default/prepare_features_edges.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_fea_tab,
        command = {
          par_def_pre_fea_tab <- system.file(
            "params/default/prepare_features_tables.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_lib_rt,
        command = {
          par_def_pre_lib_rt <- system.file(
            "params/default/prepare_libraries_rt.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_lib_sop_big,
        command = {
          par_def_pre_lib_sop_big <- system.file(
            "params/default/prepare_libraries_sop_bigg.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_lib_sop_clo,
        command = {
          par_def_pre_lib_sop_clo <- system.file(
            "params/default/prepare_libraries_sop_closed.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_lib_sop_ecm,
        command = {
          par_def_pre_lib_sop_ecm <- system.file(
            "params/default/prepare_libraries_sop_ecmdb.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_lib_sop_hmd,
        command = {
          par_def_pre_lib_sop_hmd <- system.file(
            "params/default/prepare_libraries_sop_hmdb.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_lib_sop_lot,
        command = {
          par_def_pre_lib_sop_lot <- system.file(
            "params/default/prepare_libraries_sop_lotus.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_lib_sop_mer,
        command = {
          par_def_pre_lib_sop_mer <- system.file(
            "params/default/prepare_libraries_sop_merged.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_lib_spe,
        command = {
          par_def_pre_lib_spe <- system.file(
            "params/default/prepare_libraries_spectra.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_pre_tax,
        command = {
          par_def_pre_tax <- system.file(
            "params/default/prepare_taxa.yaml",
            package = "tima"
          )
        },
        format = "file"
      ),
      tar_target(
        name = par_def_wei_ann,
        command = {
          par_def_wei_ann <- system.file(
            "params/default/weight_annotations.yaml",
            package = "tima"
          )
        },
        format = "file"
      )
    ),
    list(
      ## Prepare params
      list(
        tar_target(
          name = par_pre_par,
          command = {
            par_pre_par <- paths$params$prepare_params
          },
          format = "file"
        ),
        tar_target(
          name = par_pre_par2,
          command = {
            par_pre_par2 <- paths$params$prepare_params_advanced
          },
          format = "file"
        ),
        tar_target(
          name = par_fin_par,
          command = {
            par_fin_par <- tima:::parse_yaml_params(
              def = par_pre_par,
              usr = par_pre_par
            )
          },
          format = "rds"
        ),
        tar_target(
          name = par_fin_par2,
          command = {
            par_fin_par2 <- tima:::parse_yaml_params(
              def = par_pre_par2,
              usr = par_pre_par2
            )
          },
          format = "rds"
        )
      ),
      ## User
      list(
        tar_target(
          name = par_usr_ann_mas,
          command = {
            par_usr_ann_mas <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "annotate_masses"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_ann_spe,
          command = {
            par_usr_ann_spe <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "annotate_spectra"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_cre_com,
          command = {
            par_usr_cre_com <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "create_components"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_fil_ann,
          command = {
            par_usr_fil_ann <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "filter_annotations"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_cre_edg_spe,
          command = {
            par_usr_cre_edg_spe <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "create_edges_spectra"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_ann_gnp,
          command = {
            par_usr_pre_ann_gnp <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_annotations_gnps"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_ann_sir,
          command = {
            par_usr_pre_ann_sir <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_annotations_sirius"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_ann_spe,
          command = {
            par_usr_pre_ann_spe <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_annotations_spectra"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_fea_com,
          command = {
            par_usr_pre_fea_com <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_features_components"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_fea_edg,
          command = {
            par_usr_pre_fea_edg <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_features_edges"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_fea_tab,
          command = {
            par_usr_pre_fea_tab <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_features_tables"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_lib_rt,
          command = {
            par_usr_pre_lib_rt <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_rt"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_lib_sop_big,
          command = {
            par_usr_pre_lib_sop_big <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_sop_bigg"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_lib_sop_clo,
          command = {
            par_usr_pre_lib_sop_clo <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_sop_closed"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_lib_sop_ecm,
          command = {
            par_usr_pre_lib_sop_ecm <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_sop_ecmdb"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_lib_sop_hmd,
          command = {
            par_usr_pre_lib_sop_hmd <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_sop_hmdb"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_lib_sop_lot,
          command = {
            par_usr_pre_lib_sop_lot <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_sop_lotus"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_lib_sop_mer,
          command = {
            par_usr_pre_lib_sop_mer <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_sop_merged"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_lib_spe,
          command = {
            par_usr_pre_lib_spe <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_libraries_spectra"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_pre_tax,
          command = {
            par_usr_pre_tax <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "prepare_taxa"
              )
          },
          format = "file"
        ),
        tar_target(
          name = par_usr_wei_ann,
          command = {
            par_usr_wei_ann <-
              prepare_params(
                params_small = par_fin_par,
                params_advanced = par_fin_par2,
                step = "weight_annotations"
              )
          },
          format = "file"
        )
      )
    ),
    ## Final
    list(
      tar_target(
        name = par_ann_mas,
        command = {
          par_ann_mas <-
            tima:::parse_yaml_params(
              def = par_def_ann_mas,
              usr = par_usr_ann_mas[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_ann_spe,
        command = {
          par_ann_spe <-
            tima:::parse_yaml_params(
              def = par_def_ann_spe,
              usr = par_usr_ann_spe[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_cre_com,
        command = {
          par_cre_com <-
            tima:::parse_yaml_params(
              def = par_def_cre_com,
              usr = par_usr_cre_com[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_cre_edg_spe,
        command = {
          par_cre_edg_spe <-
            tima:::parse_yaml_params(
              def = par_def_cre_edg_spe,
              usr = par_usr_cre_edg_spe[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_fil_ann,
        command = {
          par_fil_ann <-
            tima:::parse_yaml_params(
              def = par_def_fil_ann,
              usr = par_usr_fil_ann[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_ann_gnp,
        command = {
          par_pre_ann_gnp <-
            tima:::parse_yaml_params(
              def = par_def_pre_ann_gnp,
              usr = par_usr_pre_ann_gnp[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_ann_sir,
        command = {
          par_pre_ann_sir <-
            tima:::parse_yaml_params(
              def = par_def_pre_ann_sir,
              usr = par_usr_pre_ann_sir[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_ann_spe,
        command = {
          par_pre_ann_spe <-
            tima:::parse_yaml_params(
              def = par_def_pre_ann_spe,
              usr = par_usr_pre_ann_spe[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_fea_com,
        command = {
          par_pre_fea_com <-
            tima:::parse_yaml_params(
              def = par_def_pre_fea_com,
              usr = par_usr_pre_fea_com[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_fea_edg,
        command = {
          par_pre_fea_edg <-
            tima:::parse_yaml_params(
              def = par_def_pre_fea_edg,
              usr = par_usr_pre_fea_edg[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_fea_tab,
        command = {
          par_pre_fea_tab <-
            tima:::parse_yaml_params(
              def = par_def_pre_fea_tab,
              usr = par_usr_pre_fea_tab[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_lib_rt,
        command = {
          par_pre_lib_rt <-
            tima:::parse_yaml_params(
              def = par_def_pre_lib_rt,
              usr = par_usr_pre_lib_rt[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_lib_sop_big,
        command = {
          par_pre_lib_sop_big <-
            tima:::parse_yaml_params(
              def = par_def_pre_lib_sop_big,
              usr = par_usr_pre_lib_sop_big[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_lib_sop_clo,
        command = {
          par_pre_lib_sop_clo <-
            tima:::parse_yaml_params(
              def = par_def_pre_lib_sop_clo,
              usr = par_usr_pre_lib_sop_clo[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_lib_sop_ecm,
        command = {
          par_pre_lib_sop_ecm <-
            tima:::parse_yaml_params(
              def = par_def_pre_lib_sop_ecm,
              usr = par_usr_pre_lib_sop_ecm[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_lib_sop_hmd,
        command = {
          par_pre_lib_sop_hmd <-
            tima:::parse_yaml_params(
              def = par_def_pre_lib_sop_hmd,
              usr = par_usr_pre_lib_sop_hmd[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_lib_sop_lot,
        command = {
          par_pre_lib_sop_lot <-
            tima:::parse_yaml_params(
              def = par_def_pre_lib_sop_lot,
              usr = par_usr_pre_lib_sop_lot[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_lib_sop_mer,
        command = {
          par_pre_lib_sop_mer <-
            tima:::parse_yaml_params(
              def = par_def_pre_lib_sop_mer,
              usr = par_usr_pre_lib_sop_mer[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_lib_spe,
        command = {
          par_pre_lib_spe <-
            tima:::parse_yaml_params(
              def = par_def_pre_lib_spe,
              usr = par_usr_pre_lib_spe[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_pre_tax,
        command = {
          par_pre_tax <-
            tima:::parse_yaml_params(
              def = par_def_pre_tax,
              usr = par_usr_pre_tax[[1]]
            )
        },
        format = "rds"
      ),
      tar_target(
        name = par_wei_ann,
        command = {
          par_wei_ann <-
            tima:::parse_yaml_params(
              def = par_def_wei_ann,
              usr = par_usr_wei_ann[[1]]
            )
        },
        format = "rds"
      )
    )
  ),
  ## Inputs
  list(
    tar_target(
      name = par_pre_fea_tab_fil_fea_raw,
      command = {
        par_pre_fea_tab_fil_fea_raw <- par_pre_fea_tab$files$features$raw
      },
      format = "file"
    ),
    tar_target(
      name = input_features,
      command = {
        input_features <- par_pre_fea_tab_fil_fea_raw
        # input_features <-
        #   ifelse(
        #     test = !is.null(gnps_features),
        #     yes = ifelse(test = file.exists(gnps_features),
        #       yes = gnps_features,
        #       no = par_pre_tax$files$features$raw
        #     ),
        #     no = par_pre_tax$files$features$raw
        #   )
      },
      format = "file"
    ),
    tar_target(
      name = par_ann_spe_fil_spe_raw,
      command = {
        par_ann_spe_fil_spe_raw <- par_ann_spe$files$spectral$raw
      },
      format = "file"
    ),
    tar_target(
      name = input_spectra,
      command = {
        input_spectra <-
          ifelse(
            test = paths$test$mode == FALSE,
            yes = par_ann_spe_fil_spe_raw,
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
                url = paths$urls$examples$spectra$mini,
                export = paths$data$source$spectra
              )
            }
          )
      },
      format = "file"
    )
  ),
  ## libraries
  list(
    ## Spectra
    list(
      ## In silico
      list(
        ## TODO ADD ISDB HMDB,
        tar_target(
          name = lib_spe_is_wik_pre_pos,
          command = {
            lib_spe_is_wik_pre_pos <-
              get_file(
                url = paths$urls$spectra$pos$isdb,
                export = paths$data$interim$libraries$spectra$is$pos$isdb
              )
          },
          format = "file"
        ),
        tar_target(
          name = lib_spe_is_wik_pre_neg,
          command = {
            lib_spe_is_wik_pre_neg <-
              get_file(
                url = paths$urls$spectra$neg$isdb,
                export = paths$data$interim$libraries$spectra$is$neg$isdb
              )
          },
          format = "file"
        ),
        ## TODO ADD IS HMDB PREPARED
        tar_target(
          name = lib_spe_is_wik_pre_sop,
          command = {
            lib_spe_is_wik_pre_sop <- get_file(
              url = paths$urls$sop$isdb,
              export = paths$data$interim$libraries$sop$isdb
            )
          },
          format = "file"
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
          #   command = {
          #     lib_spe_exp_int_raw <-
          #       par_pre_lib_spe$files$libraries$spectral$exp$raw
          #   }
          # ),
          ### GNPS
          tar_target(
            name = lib_spe_exp_gnp_pre_pos,
            command = {
              lib_spe_exp_gnp_pre_pos <-
                get_file(
                  url = paths$urls$spectra$pos$gnps,
                  export = paths$data$interim$libraries$spectra$exp$pos$gnps
                )
            },
            format = "file"
          ),
          tar_target(
            name = lib_spe_exp_gnp_pre_neg,
            command = {
              lib_spe_exp_gnp_pre_neg <-
                get_file(
                  url = paths$urls$spectra$neg$gnps,
                  export = paths$data$interim$libraries$spectra$exp$neg$gnps
                )
            },
            format = "file"
          ),
          tar_target(
            name = lib_spe_exp_gnp_pre_sop,
            command = {
              lib_spe_exp_gnp_pre_sop <-
                get_file(
                  url = paths$urls$sop$gnps,
                  export = paths$data$interim$libraries$sop$gnps
                )
            },
            format = "file"
          ),
          ### MassBank
          tar_target(
            name = lib_spe_exp_mb_pre_pos,
            command = {
              lib_spe_exp_mb_pre_pos <-
                get_file(
                  url = paths$urls$spectra$pos$massbank,
                  export = paths$data$interim$libraries$spectra$exp$pos$massbank
                )
            },
            format = "file"
          ),
          tar_target(
            name = lib_spe_exp_mb_pre_neg,
            command = {
              lib_spe_exp_mb_pre_neg <-
                get_file(
                  url = paths$urls$spectra$neg$massbank,
                  export = paths$data$interim$libraries$spectra$exp$neg$massbank
                )
            },
            format = "file"
          ),
          tar_target(
            name = lib_spe_exp_mb_pre_sop,
            command = {
              lib_spe_exp_mb_pre_sop <-
                get_file(
                  url = paths$urls$sop$massbank,
                  export = paths$data$interim$libraries$sop$massbank
                )
            },
            format = "file"
          ),
          ### Merlin
          tar_target(
            name = lib_spe_exp_mer_pre_pos,
            command = {
              lib_spe_exp_mer_pre_pos <-
                get_file(
                  url = paths$urls$spectra$pos$merlin,
                  export = paths$data$interim$libraries$spectra$exp$pos$merlin
                )
            },
            format = "file"
          ),
          tar_target(
            name = lib_spe_exp_mer_pre_neg,
            command = {
              lib_spe_exp_mer_pre_neg <-
                get_file(
                  url = paths$urls$spectra$neg$merlin,
                  export = paths$data$interim$libraries$spectra$exp$neg$merlin
                )
            },
            format = "file"
          ),
          tar_target(
            name = lib_spe_exp_mer_pre_sop,
            command = {
              lib_spe_exp_mer_pre_sop <-
                get_file(
                  url = paths$urls$sop$merlin,
                  export = paths$data$interim$libraries$sop$merlin
                )
            },
            format = "file"
          )
        ),
        ## Prepared
        list(
          tar_target(
            name = lib_spe_exp_int_pre,
            command = {
              lib_spe_exp_int_pre <-
                prepare_libraries_spectra(
                  input = par_pre_lib_spe$files$libraries$spectral$raw,
                  nam_lib = par_pre_lib_spe$names$libraries,
                  col_ad = par_pre_lib_spe$names$mgf$adduct,
                  col_ce = par_pre_lib_spe$names$mgf$collision_energy,
                  col_ci = par_pre_lib_spe$names$mgf$compound_id,
                  col_em = par_pre_lib_spe$names$mgf$exact_mass,
                  col_in = par_pre_lib_spe$names$mgf$inchi,
                  col_io = par_pre_lib_spe$names$mgf$inchi_no_stereo,
                  col_ik = par_pre_lib_spe$names$mgf$inchikey,
                  col_il = par_pre_lib_spe$names$mgf$inchikey_connectivity_layer,
                  col_mf = par_pre_lib_spe$names$mgf$molecular_formula,
                  col_na = par_pre_lib_spe$names$mgf$name,
                  col_po = par_pre_lib_spe$names$mgf$polarity,
                  col_sm = par_pre_lib_spe$names$mgf$smiles,
                  col_sn = par_pre_lib_spe$names$mgf$smiles_no_stereo,
                  col_si = par_pre_lib_spe$names$mgf$spectrum_id,
                  col_sp = par_pre_lib_spe$names$mgf$splash,
                  col_sy = par_pre_lib_spe$names$mgf$synonyms,
                  col_xl = par_pre_lib_spe$names$mgf$xlogp
                )
            },
            format = "file"
          ),
          tar_target(
            name = lib_spe_exp_int_pre_pos,
            command = {
              lib_spe_exp_int_pre_pos <- lib_spe_exp_int_pre[[1]]
            },
            format = "file"
          ),
          tar_target(
            name = lib_spe_exp_int_pre_neg,
            command = {
              lib_spe_exp_int_pre_neg <- lib_spe_exp_int_pre[[2]]
            },
            format = "file"
          ),
          tar_target(
            name = lib_spe_exp_int_pre_sop,
            command = {
              lib_spe_exp_int_pre_sop <- lib_spe_exp_int_pre[[3]]
            },
            format = "file"
          )
        )
      )
    ),
    ## Retention times
    list(
      tar_target(
        name = lib_rt,
        command = {
          lib_rt <- prepare_libraries_rt(
            mgf_exp = par_pre_lib_rt$files$libraries$temporal$exp$mgf,
            mgf_is = par_pre_lib_rt$files$libraries$temporal$is$mgf,
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
        },
        format = "file"
      )
    ),
    tar_target(
      name = lib_rt_rts,
      command = {
        lib_rt_rts <- lib_rt[[1]]
      },
      format = "file"
    ),
    tar_target(
      name = lib_rt_sop,
      command = {
        lib_rt_sop <- lib_rt[[2]]
      },
      format = "file"
    ),
    ## Structure organism pairs
    list(
      ## Raw
      list(
        ## This does not work as it forces the file to exist.
        ## So targets will not check if the input file changed automatically.
        # tar_target(
        #   name = lib_sop_clo,
        #   command = {
        #     lib_sop_clo <- paths$data$source$libraries$sop$closed
        #   }
        # ),
        tar_target(
          name = lib_sop_ecm,
          command = {
            ## Because ECMDB certificate is expired
            lib_sop_ecm <- tryCatch(
              expr = {
                get_file(
                  url = paths$urls$ecmdb$metabolites,
                  export = paths$data$source$libraries$sop$ecmdb
                )
              },
              error = function(e) {
                tima:::fake_ecmdb(
                  export = paths$data$source$libraries$sop$ecmdb
                )
              },
              finally = {
                paths$data$source$libraries$sop$ecmdb
              }
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_hmd,
          command = {
            lib_sop_hmd <- tryCatch(
              expr = {
                get_file(
                  url = paths$urls$hmdb$structures,
                  export = paths$data$source$libraries$sop$hmdb
                )
              },
              warning = function(w) {
                ## See #118
                log_warn(
                  "HMDB download failed partially, returning empty file instead"
                )
                unlink(paths$data$source$libraries$sop$hmdb)
                tima:::fake_hmdb(
                  export = paths$data$source$libraries$sop$hmdb
                )
              },
              error = function(e) {
                tima:::fake_hmdb(
                  export = paths$data$source$libraries$sop$hmdb
                )
              },
              finally = {
                paths$data$source$libraries$sop$hmdb
              }
            )
          },
          format = "file"
        ),
        ## TODO ADD GET HMDB
        tar_target(
          name = lib_sop_lot,
          command = {
            lib_sop_lot <- tryCatch(
              expr = {
                get_last_version_from_zenodo(
                  doi = paths$urls$lotus$doi,
                  pattern = paths$urls$lotus$pattern,
                  path = paths$data$source$libraries$sop$lotus
                )
              },
              error = function(e) {
                tima:::fake_lotus(
                  export = paths$data$source$libraries$sop$lotus
                )
              },
              finally = {
                paths$data$source$libraries$sop$lotus
              }
            )
          },
          format = "file"
        )
      ),
      ## Prepared
      list(
        tar_target(
          name = lib_sop_big_pre,
          command = {
            lib_sop_big_pre <-
              prepare_libraries_sop_bigg(
                output = par_pre_lib_sop_big$files$libraries$sop$prepared$bigg
              )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_clo_pre,
          command = {
            lib_sop_clo_pre <-
              prepare_libraries_sop_closed(
                input = par_pre_lib_sop_clo$files$libraries$sop$raw$closed,
                output = par_pre_lib_sop_clo$files$libraries$sop$prepared$closed
              )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_ecm_pre,
          command = {
            lib_sop_ecm_pre <-
              prepare_libraries_sop_ecmdb(
                input = lib_sop_ecm,
                output = par_pre_lib_sop_ecm$files$libraries$sop$prepared$ecmdb
              )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_hmd_pre,
          command = {
            lib_sop_hmd_pre <-
              prepare_libraries_sop_hmdb(
                input = lib_sop_hmd,
                output = par_pre_lib_sop_hmd$files$libraries$sop$prepared$hmdb
              )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_lot_pre,
          command = {
            lib_sop_lot_pre <-
              prepare_libraries_sop_lotus(
                input = if (paths$test$mode == FALSE) {
                  lib_sop_lot
                } else {
                  paths$data$source$libraries$sop$lotus
                },
                output = par_pre_lib_sop_lot$files$libraries$sop$prepared$lotus
              )
          },
          format = "file"
        )
      ),
      ## Merged
      list(
        tar_target(
          name = lib_sop_mer_str_pro,
          command = {
            lib_sop_mer_str_pro <- get_file(
              url = paths$urls$examples$structures_processed,
              export = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$processed
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_mer,
          command = {
            lib_sop_mer <- prepare_libraries_sop_merged(
              files = c(
                lib_sop_big_pre,
                lib_sop_clo_pre,
                lib_sop_ecm_pre,
                lib_sop_hmd_pre,
                lib_sop_lot_pre,
                lib_rt_sop,
                lib_spe_exp_int_pre_sop,
                lib_spe_exp_gnp_pre_sop,
                lib_spe_exp_mb_pre_sop,
                lib_spe_exp_mer_pre_sop,
                lib_spe_is_wik_pre_sop
              ),
              filter = par_pre_lib_sop_mer$organisms$filter$mode,
              level = par_pre_lib_sop_mer$organisms$filter$level,
              value = par_pre_lib_sop_mer$organisms$filter$value,
              cache = lib_sop_mer_str_pro,
              output_key = par_pre_lib_sop_mer$files$libraries$sop$merged$keys,
              output_org_tax_ott = par_pre_lib_sop_mer$files$libraries$sop$merged$organisms$taxonomies$ott,
              output_str_stereo = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$stereo,
              output_str_met = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$metadata,
              output_str_nam = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$names,
              output_str_tax_cla = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$taxonomies$cla,
              output_str_tax_npc = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$taxonomies$npc
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_key,
          command = {
            lib_mer_key <- lib_sop_mer[[1]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_org_tax_ott,
          command = {
            lib_mer_org_tax_ott <- lib_sop_mer[[2]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_str_stereo,
          command = {
            lib_mer_str_stereo <- lib_sop_mer[[3]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_str_met,
          command = {
            lib_mer_str_met <- lib_sop_mer[[4]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_str_nam,
          command = {
            lib_mer_str_nam <- lib_sop_mer[[5]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_str_tax_cla,
          command = {
            lib_mer_str_tax_cla <- lib_sop_mer[[6]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_str_tax_npc,
          command = {
            lib_mer_str_tax_npc <- lib_sop_mer[[7]]
          },
          format = "file"
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
        command = {
          ann_ms1_pre <-
            annotate_masses(
              features = fea_pre,
              library = lib_mer_key,
              output_annotations = par_ann_mas$files$annotations$prepared$structural$ms1,
              output_edges = par_ann_mas$files$networks$spectral$edges$raw$ms1,
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
              tolerance_rt = par_ann_mas$ms$tolerances$rt$adducts
            )
        },
        format = "file"
      ),
      tar_target(
        name = ann_ms1_pre_ann,
        command = {
          ann_ms1_pre_ann <-
            ann_ms1_pre[[1]]
        },
        format = "file"
      ),
      tar_target(
        name = ann_ms1_pre_edg,
        command = {
          ann_ms1_pre_edg <- ann_ms1_pre[[2]]
        },
        format = "file"
      )
    ),
    ## Spectral
    list(
      ## GNPS
      list(
        tar_target(
          name = ann_spe_exp_gnp_pre,
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
          },
          format = "file"
        )
      ),
      ## Classic
      list(
        ## TODO improve polarity handling, suboptimal
        tar_target(
          name = ann_spe_pos,
          command = {
            ann_spe_pos <- annotate_spectra(
              input = input_spectra,
              libraries = c(
                lib_spe_is_wik_pre_pos,
                ## TODO add is hmdb
                lib_spe_exp_int_pre_pos,
                lib_spe_exp_gnp_pre_pos,
                lib_spe_exp_mb_pre_pos,
                lib_spe_exp_mer_pre_pos
              ),
              polarity = "pos",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_pos.tsv.gz",
                x = par_ann_spe$files$annotations$raw$spectral$spectral,
                fixed = TRUE
              ),
              method = par_ann_spe$similarities$methods$annotations,
              threshold = par_ann_spe$similarities$thresholds$annotations,
              ppm = par_ann_spe$ms$tolerances$mass$ppm$ms2,
              dalton = par_ann_spe$ms$tolerances$mass$dalton$ms2,
              qutoff = par_ann_spe$ms$thresholds$ms2$intensity,
              approx = par_ann_spe$annotations$ms2approx
            )
          },
          format = "file"
        ),
        tar_target(
          name = ann_spe_neg,
          command = {
            ann_spe_neg <- annotate_spectra(
              input = input_spectra,
              libraries = c(
                lib_spe_is_wik_pre_neg,
                ## TODO add is hmdb
                lib_spe_exp_int_pre_neg,
                lib_spe_exp_gnp_pre_neg,
                lib_spe_exp_mb_pre_neg,
                lib_spe_exp_mer_pre_neg
              ),
              polarity = "neg",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_neg.tsv.gz",
                x = par_ann_spe$files$annotations$raw$spectral$spectral,
                fixed = TRUE
              ),
              method = par_ann_spe$similarities$methods$annotations,
              threshold = par_ann_spe$similarities$thresholds$annotations,
              ppm = par_ann_spe$ms$tolerances$mass$ppm$ms2,
              dalton = par_ann_spe$ms$tolerances$mass$dalton$ms2,
              qutoff = par_ann_spe$ms$thresholds$ms2$intensity,
              approx = par_ann_spe$annotations$ms2approx
            )
          },
          format = "file"
        ),
        tar_target(
          name = ann_spe_pre,
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
          },
          format = "file"
        )
      )
    ),
    # SIRIUS
    tar_target(
      name = ann_sir_pre,
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
      },
      format = "file"
    ),
    tar_target(
      name = ann_sir_pre_can,
      command = {
        ann_sir_pre_can <- ann_sir_pre[[1]]
      },
      format = "file"
    ),
    tar_target(
      name = ann_sir_pre_for,
      command = {
        ann_sir_pre_for <- ann_sir_pre[[2]]
      },
      format = "file"
    ),
    tar_target(
      name = ann_sir_pre_str,
      command = {
        ann_sir_pre_str <- ann_sir_pre[[3]]
      },
      format = "file"
    ),
    list()
  ),
  ## Features
  list(
    tar_target(
      name = fea_edg_spe,
      command = {
        fea_edg_spe <- create_edges_spectra(
          input = input_spectra,
          output = par_cre_edg_spe$files$networks$spectral$edges$raw$spectral,
          name_source = par_cre_edg_spe$names$source,
          name_target = par_cre_edg_spe$names$target,
          method = par_cre_edg_spe$similarities$methods$edges,
          threshold = par_cre_edg_spe$similarities$thresholds$edges,
          matched_peaks = par_cre_edg_spe$similarities$thresholds$matched_peaks,
          ppm = par_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
          dalton = par_cre_edg_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = par_cre_edg_spe$ms$thresholds$ms2$intensity
        )
      },
      format = "file"
    ),
    tar_target(
      name = fea_com,
      command = {
        fea_com <- create_components(
          input = fea_edg_pre,
          output = par_cre_com$files$networks$spectral$components$raw
        )
      },
      format = "file"
    ),
    ## Interim
    list(
      tar_target(
        name = int_com,
        command = {
          int_com <- fea_com
          # int_com <-
          #   if (file.exists(fea_com)) {
          #     fea_com
          #   } else {
          #     gnps_components
          #   }
        },
        format = "file"
      ),
      tar_target(
        name = edg_spe,
        command = {
          edg_spe <- fea_edg_spe
          # edg_spe <-
          #   ifelse(test = file.exists(fea_edg_spe),
          #     yes = fea_edg_spe,
          #     no = gnps_edges
          #   )
        },
        format = "file"
      )
    ),
    tar_target(
      name = fea_edg_pre,
      command = {
        fea_edg_pre <- prepare_features_edges(
          input = c("ms1" = ann_ms1_pre_edg, "spectral" = edg_spe),
          output = par_pre_fea_edg$files$networks$spectral$edges$prepared,
          name_source = par_pre_fea_edg$names$source,
          name_target = par_pre_fea_edg$names$target
        )
      },
      format = "file"
    ),
    tar_target(
      name = fea_com_pre,
      command = {
        fea_com_pre <- prepare_features_components(
          input = int_com,
          output = par_pre_fea_com$files$networks$spectral$components$prepared
        )
      },
      format = "file"
    ),
    tar_target(
      name = fea_pre,
      command = {
        fea_pre <- prepare_features_tables(
          features = input_features,
          output = par_pre_fea_tab$files$features$prepared,
          candidates = par_pre_fea_tab$annotations$candidates$samples,
          name_features = par_pre_fea_tab$names$features,
          name_rt = par_pre_fea_tab$names$rt$features,
          name_mz = par_pre_fea_tab$names$precursor
        )
      },
      format = "file"
    )
  ),
  tar_target(
    name = tax_pre,
    command = {
      tax_pre <- prepare_taxa(
        input = fea_pre,
        name_filename = par_pre_tax$names$filename,
        extension = par_pre_tax$names$extension,
        colname = par_pre_tax$names$taxon,
        metadata = par_pre_tax$files$metadata$raw,
        org_tax_ott = lib_mer_org_tax_ott,
        output = par_pre_tax$files$metadata$prepared,
        taxon = par_pre_tax$organisms$taxon
      )
    },
    format = "file"
  ),
  tar_target(
    name = ann_fil,
    command = {
      ann_fil <- filter_annotations(
        annotations = c(
          "gnps" = ann_spe_exp_gnp_pre,
          "spectral" = ann_spe_pre,
          "sirius" = ann_sir_pre_str,
          "ms1" = ann_ms1_pre_ann
        ),
        features = fea_pre,
        rts = lib_rt_rts,
        output = par_fil_ann$files$annotations$filtered,
        tolerance_rt = par_fil_ann$ms$tolerances$rt$library
      )
    },
    format = "file"
  ),
  tar_target(
    name = ann_wei,
    command = {
      ann_wei <- weight_annotations(
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
        best_percentile = par_wei_ann$annotations$candidates$best_percentile,
        candidates_neighbors = par_wei_ann$annotations$candidates$neighbors,
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
        score_chemical_cla_superclass = par_wei_ann$weights$chemical$cla$superclass,
        score_chemical_cla_class = par_wei_ann$weights$chemical$cla$class,
        score_chemical_cla_parent = par_wei_ann$weights$chemical$cla$parent,
        score_chemical_npc_pathway = par_wei_ann$weights$chemical$npc$pathway,
        score_chemical_npc_superclass = par_wei_ann$weights$chemical$npc$superclass,
        score_chemical_npc_class = par_wei_ann$weights$chemical$npc$class,
        minimal_consistency = par_wei_ann$annotations$thresholds$consistency,
        minimal_ms1_bio = par_wei_ann$annotations$thresholds$ms1$biological,
        minimal_ms1_chemo = par_wei_ann$annotations$thresholds$ms1$chemical,
        minimal_ms1_condition = par_wei_ann$annotations$thresholds$ms1$condition,
        ms1_only = par_wei_ann$annotations$ms1only,
        compounds_names = par_wei_ann$options$compounds_names,
        high_confidence = par_wei_ann$options$high_confidence,
        remove_ties = par_wei_ann$options$remove_ties,
        summarize = par_wei_ann$options$summarize,
        pattern = par_wei_ann$files$pattern,
        force = par_wei_ann$options$force
      )
    },
    format = "file"
  ),
  list(
    ## Benchmark
    tar_target(
      name = benchmark_path_url,
      command = {
        benchmark_path_url <- paths$urls$benchmarking$set
      }
    ),
    tar_target(
      name = benchmark_path_zip,
      command = {
        benchmark_path_zip <- paths$data$source$benchmark$zip
      }
    ),
    tar_target(
      name = benchmark_path_file,
      command = {
        benchmark_path_file <- paths$data$source$benchmark$cleaned
      }
    ),
    tar_target(
      name = benchmark_path_mgf_neg,
      command = {
        benchmark_path_mgf_neg <- paths$data$source$benchmark$mgf$neg
      }
    ),
    tar_target(
      name = benchmark_path_mgf_pos,
      command = {
        benchmark_path_mgf_pos <- paths$data$source$benchmark$mgf$pos
      }
    ),
    tar_target(
      name = benchmark_zip,
      command = {
        benchmark_zip <- get_file(
          url = benchmark_path_url,
          export = benchmark_path_zip
        )
        return(benchmark_path_zip)
      }
    ),
    tar_target(
      name = benchmark_file,
      command = {
        utils::unzip(zipfile = benchmark_zip)
        dir.create(dirname(benchmark_path_file), recursive = TRUE)
        file.copy(
          from = "cleaned_libraries_matchms/results_library_cleaning/cleaned_spectra.mgf",
          to = benchmark_path_file
        )
        unlink("cleaned_libraries_matchms", recursive = TRUE)
        return(benchmark_path_file)
      }
    ),
    tar_target(
      name = benchmark_converted,
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
      command = {
        sp <- benchmark_converted |>
          import_spectra()

        sp@backend@spectraData$precursorMz <-
          sp@backend@spectraData$PRECURSOR_MZ |>
          as.numeric()

        log_trace("Imported")
        sp_clean <- sp

        log_trace("Cleaned")
        df_meta <- tidytable::tidytable(
          adduct = sp_clean$ADDUCT,
          inchikey = sp_clean$INCHIKEY,
          instrument = sp_clean$INSTRUMENT_TYPE,
          fragments = purrr::map(
            .x = sp_clean@backend@peaksData,
            .f = length
          ) |>
            as.character() |>
            as.numeric() /
            2,
          precursorMz = sp_clean$precursorMz,
          smiles = sp_clean$SMILES,
          ccmslib = sp_clean$SPECTRUM_ID,
          charge = sp_clean$precursorCharge,
          name = sp_clean$COMPOUND_NAME
        ) |>
          tidytable::mutate(
            tidytable::across(
              .cols = tidyselect::everything(),
              .fns = function(x) {
                tidytable::na_if(x, "")
              }
            )
          )

        log_trace("Framed")
        df_clean <- df_meta |>
          tidytable::filter(!is.na(inchikey)) |>
          tidytable::filter(fragments >= 5) |>
          tidytable::filter(fragments <= 250) |>
          tidytable::filter(
            !grepl(
              pattern = "QQQ",
              x = instrument,
              fixed = TRUE
            )
          ) |>
          ## fragments are nominal mass
          tidytable::filter(
            !grepl(
              pattern = "ReSpect",
              x = name,
              fixed = TRUE
            )
          ) |>
          ## remove spectral matches
          tidytable::filter(
            !grepl(
              pattern = "Spectral Match to",
              x = name,
              fixed = TRUE
            )
          ) |>
          ## remove putatives
          tidytable::filter(
            !grepl(
              pattern = "putative",
              x = name,
              fixed = TRUE
            )
          ) |>
          tidytable::select(-name) |>
          tidytable::mutate(mass = precursorMz) |>
          tidytable::separate(
            col = mass,
            sep = "\\.",
            into = c("a", "b")
          ) |>
          tidytable::filter(!is.na(b)) |>
          tidytable::filter(stringi::stri_length(as.numeric(b)) > 1) |>
          tidytable::select(-a, -b) |>
          tidytable::mutate(
            inchikey_connectivity_layer = gsub(
              pattern = "-.*",
              replacement = "",
              x = inchikey,
              perl = TRUE
            )
          ) |>
          tidytable::distinct(
            inchikey_connectivity_layer,
            adduct,
            .keep_all = TRUE
          ) |>
          tidytable::mutate(mz = precursorMz) |>
          ## Weird way to have some kind of retention time
          tidytable::mutate(
            rt = tidytable::cur_group_id(),
            .by = "inchikey_connectivity_layer"
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
            tidytable::mutate(
              short_ik = gsub(
                pattern = "-.*",
                replacement = "",
                INCHIKEY,
                perl = TRUE
              )
            ) |>
            tidytable::mutate(
              rtime = tidytable::cur_group_id(),
              .by = "short_ik"
            ) |>
            tidytable::mutate(
              precursorCharge = tidytable::if_else(
                condition = mode == "pos",
                true = as.integer(1),
                false = as.integer(-1)
              )
            ) |>
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
            tidytable::mutate(
              inchikey_connectivity_layer = gsub(
                pattern = "-.*",
                replacement = "",
                x = inchikey,
                perl = TRUE
              )
            ) |>
            data.frame()
          return(df)
        }

        df_clean_pos <- spectra_harmonized_pos |>
          select_benchmark_columns()

        df_clean_neg <- spectra_harmonized_neg |>
          select_benchmark_columns()

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
      command = {
        benchmark_pre_mgf_pos <- benchmark_prepared[[1]]
      }
    ),
    tar_target(
      name = benchmark_pre_mgf_neg,
      command = {
        benchmark_pre_mgf_neg <- benchmark_prepared[[2]]
      }
    ),
    tar_target(
      name = benchmark_pre_meta_pos,
      command = {
        benchmark_pre_meta_pos <- benchmark_prepared[[3]]
      }
    ),
    tar_target(
      name = benchmark_pre_meta_neg,
      command = {
        benchmark_pre_meta_neg <- benchmark_prepared[[4]]
      }
    ),
    tar_target(
      name = benchmark_taxed_pos,
      command = {
        benchmark_taxed_pos <- benchmark_pre_meta_pos |>
          benchmark_taxize_spectra(
            keys = lib_mer_key,
            org_tax_ott = lib_mer_org_tax_ott,
            output = "data/interim/benchmark/benchmark_taxed_pos.tsv.gz"
          )
      }
    ),
    tar_target(
      name = benchmark_taxed_neg,
      command = {
        benchmark_taxed_neg <- benchmark_pre_meta_neg |>
          benchmark_taxize_spectra(
            keys = lib_mer_key,
            org_tax_ott = lib_mer_org_tax_ott,
            output = "data/interim/benchmark/benchmark_taxed_neg.tsv.gz"
          )
      }
    ),
    tar_target(
      name = benchmark_def_ann_mas,
      command = {
        benchmark_def_ann_mas <- tima:::parse_yaml_params(
          def = par_def_ann_mas,
          usr = par_def_ann_mas
        )
      }
    ),
    tar_target(
      name = benchmark_ann_ms1_pre_pos,
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
            tolerance_rt = benchmark_def_ann_mas$ms$tolerances$rt$adducts
          )
      }
    ),
    tar_target(
      name = benchmark_ann_ms1_pre_neg,
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
            tolerance_rt = benchmark_def_ann_mas$ms$tolerances$rt$adducts
          )
      }
    ),
    tar_target(
      name = benchmark_def_cre_edg_spe,
      command = {
        benchmark_def_cre_edg_spe <- tima:::parse_yaml_params(
          def = par_def_cre_edg_spe,
          usr = par_def_cre_edg_spe
        )
      }
    ),
    tar_target(
      name = benchmark_edg_spe_pos,
      command = {
        benchmark_edg_spe_pos <- create_edges_spectra(
          input = benchmark_pre_mgf_pos,
          output = "data/interim/benchmark/benchmark_edges_spe_pos.tsv.gz",
          name_source = benchmark_def_cre_edg_spe$names$source,
          name_target = benchmark_def_cre_edg_spe$names$target,
          threshold = benchmark_def_cre_edg_spe$similarities$thresholds$edges,
          matched_peaks = benchmark_def_cre_edg_spe$similarities$thresholds$matched_peaks,
          ppm = benchmark_def_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_cre_edg_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = 0
        )
      }
    ),
    tar_target(
      name = benchmark_edg_spe_neg,
      command = {
        benchmark_edg_spe_neg <- create_edges_spectra(
          input = benchmark_pre_mgf_neg,
          output = "data/interim/benchmark/benchmark_edges_spe_neg.tsv.gz",
          name_source = benchmark_def_cre_edg_spe$names$source,
          name_target = benchmark_def_cre_edg_spe$names$target,
          threshold = benchmark_def_cre_edg_spe$similarities$thresholds$edges,
          matched_peaks = benchmark_def_cre_edg_spe$similarities$thresholds$matched_peaks,
          ppm = benchmark_def_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_cre_edg_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = 0
        )
      }
    ),
    tar_target(
      name = benchmark_def_pre_fea_edg,
      command = {
        benchmark_def_pre_fea_edg <- tima:::parse_yaml_params(
          def = par_def_pre_fea_edg,
          usr = par_def_pre_fea_edg
        )
      }
    ),
    tar_target(
      name = benchmark_edg_pre_pos,
      command = {
        benchmark_edg_pre_pos <- prepare_features_edges(
          input = list(
            "spectral" = benchmark_edg_spe_pos,
            "ms1" = benchmark_ann_ms1_pre_pos[[2]]
          ),
          output = "data/interim/benchmark/benchmark_edges_pos.tsv.gz",
          name_source = benchmark_def_pre_fea_edg$names$source,
          name_target = benchmark_def_pre_fea_edg$names$target
        )
      }
    ),
    tar_target(
      name = benchmark_edg_pre_neg,
      command = {
        benchmark_edg_pre_neg <- prepare_features_edges(
          input = list(
            "spectral" = benchmark_edg_spe_neg,
            "ms1" = benchmark_ann_ms1_pre_neg[[2]]
          ),
          output = "data/interim/benchmark/benchmark_edges_neg.tsv.gz",
          name_source = benchmark_def_pre_fea_edg$names$source,
          name_target = benchmark_def_pre_fea_edg$names$target
        )
      }
    ),
    tar_target(
      name = benchmark_def_cre_edg_com,
      command = {
        benchmark_def_cre_edg_com <- tima:::parse_yaml_params(
          def = par_def_cre_com,
          usr = par_def_cre_com
        )
      }
    ),
    tar_target(
      name = benchmark_com_pos,
      command = {
        benchmark_com_pos <- create_components(
          input = benchmark_edg_pre_pos,
          output = "data/interim/benchmark/benchmark_components_pos.tsv.gz"
        )
      }
    ),
    tar_target(
      name = benchmark_com_neg,
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
        benchmark_def_pre_fea_com <- tima:::parse_yaml_params(
          def = par_def_pre_fea_com,
          usr = par_def_pre_fea_com
        )
      }
    ),
    tar_target(
      name = benchmark_com_pre_pos,
      command = {
        benchmark_com_pre_pos <- prepare_features_components(
          input = benchmark_com_pos,
          output = "data/interim/benchmark/benchmark_com_pre_pos.tsv.gz"
        )
      }
    ),
    tar_target(
      name = benchmark_com_pre_neg,
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
        benchmark_def_ann_spe <- tima:::parse_yaml_params(
          def = par_def_ann_spe,
          usr = par_def_ann_spe
        )
      }
    ),
    tar_target(
      name = benchmark_ann_spe_pos,
      command = {
        benchmark_ann_spe_pos <- annotate_spectra(
          input = benchmark_pre_mgf_pos,
          libraries = c(
            lib_spe_is_wik_pre_pos,
            lib_spe_exp_mb_pre_pos,
            lib_spe_exp_mer_pre_pos
          ),
          polarity = "pos",
          output = "data/interim/benchmark/benchmark_ann_spe_pos.tsv.gz",
          method = benchmark_def_ann_spe$similarities$methods$annotations,
          threshold = benchmark_def_ann_spe$similarities$thresholds$annotations,
          ppm = benchmark_def_ann_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_ann_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = 0,
          approx = benchmark_def_ann_spe$annotations$ms2approx
        )
      }
    ),
    tar_target(
      name = benchmark_ann_spe_neg,
      command = {
        benchmark_ann_spe_neg <- annotate_spectra(
          input = benchmark_pre_mgf_neg,
          libraries = c(
            lib_spe_is_wik_pre_neg,
            lib_spe_exp_mb_pre_neg,
            lib_spe_exp_mer_pre_neg
          ),
          polarity = "neg",
          output = "data/interim/benchmark/benchmark_ann_spe_neg.tsv.gz",
          method = benchmark_def_ann_spe$similarities$methods$annotations,
          threshold = benchmark_def_ann_spe$similarities$thresholds$annotations,
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
        benchmark_def_pre_ann_spe <- tima:::parse_yaml_params(
          def = par_def_pre_ann_spe,
          usr = par_def_pre_ann_spe
        )
      }
    ),
    tar_target(
      name = benchmark_ann_spe_pre_pos,
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
        benchmark_def_pre_ann_sir <- tima:::parse_yaml_params(
          def = par_def_pre_ann_sir,
          usr = par_def_pre_ann_sir
        )
      }
    ),
    tar_target(
      name = benchmark_ann_sir_pre,
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
      command = {
        benchmark_ann_sir_pre_can <- benchmark_ann_sir_pre[[1]]
      }
    ),
    tar_target(
      name = benchmark_ann_sir_pre_for,
      command = {
        benchmark_ann_sir_pre_for <- benchmark_ann_sir_pre[[2]]
      }
    ),
    tar_target(
      name = benchmark_ann_sir_pre_str,
      command = {
        benchmark_ann_sir_pre_str <- benchmark_ann_sir_pre[[3]]
      }
    ),
    tar_target(
      name = benchmark_def_fil_ann,
      command = {
        benchmark_def_fil_ann <- tima:::parse_yaml_params(
          def = par_def_fil_ann,
          usr = par_def_fil_ann
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_spe_neg,
      command = {
        benchmark_ann_fil_spe_neg <- filter_annotations(
          annotations = c(
            benchmark_ann_spe_pre_neg,
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_neg,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_spe_fil_neg.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_spe_ms1_neg,
      command = {
        benchmark_ann_fil_spe_ms1_neg <- filter_annotations(
          annotations = c(
            benchmark_ann_spe_pre_neg,
            benchmark_ann_ms1_pre_neg[[1]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_neg,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_spe_ms1_fil_neg.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_ms1_neg,
      command = {
        benchmark_ann_fil_ms1_neg <- filter_annotations(
          annotations = c(
            benchmark_ann_ms1_pre_neg[[1]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_neg,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_ms1_fil_neg.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_spe_pos,
      command = {
        benchmark_ann_fil_spe_pos <- filter_annotations(
          annotations = c(
            benchmark_ann_spe_pre_pos,
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_pos,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_spe_fil_pos.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_spe_ms1_pos,
      command = {
        benchmark_ann_fil_spe_ms1_pos <- filter_annotations(
          annotations = c(
            benchmark_ann_spe_pre_pos,
            benchmark_ann_ms1_pre_pos[[1]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_pos,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_spe_ms1_fil_pos.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      }
    ),
    tar_target(
      name = benchmark_ann_fil_ms1_pos,
      command = {
        benchmark_ann_fil_ms1_pos <- filter_annotations(
          annotations = c(
            benchmark_ann_ms1_pre_pos[[1]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_pos,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_ms1_fil_pos.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      }
    ),
    tar_target(
      name = benchmark_def_wei_ann,
      command = {
        benchmark_def_wei_ann <- tima:::parse_yaml_params(
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
          score_biological_domain = benchmark_def_wei_ann$weights$biological$domain,
          score_biological_kingdom = benchmark_def_wei_ann$weights$biological$kingdom,
          score_biological_phylum = benchmark_def_wei_ann$weights$biological$phylum,
          score_biological_class = benchmark_def_wei_ann$weights$biological$class,
          score_biological_order = benchmark_def_wei_ann$weights$biological$order,
          score_biological_infraorder = benchmark_def_wei_ann$weights$biological$infraorder,
          score_biological_family = benchmark_def_wei_ann$weights$biological$family,
          score_biological_subfamily = benchmark_def_wei_ann$weights$biological$subfamily,
          score_biological_tribe = benchmark_def_wei_ann$weights$biological$tribe,
          score_biological_subtribe = benchmark_def_wei_ann$weights$biological$subtribe,
          score_biological_genus = benchmark_def_wei_ann$weights$biological$genus,
          score_biological_subgenus = benchmark_def_wei_ann$weights$biological$subgenus,
          score_biological_species = benchmark_def_wei_ann$weights$biological$species,
          score_biological_subspecies = benchmark_def_wei_ann$weights$biological$subspecies,
          score_biological_variety = benchmark_def_wei_ann$weights$biological$variety,
          score_chemical_cla_kingdom = benchmark_def_wei_ann$weights$chemical$cla$kingdom,
          score_chemical_cla_superclass = benchmark_def_wei_ann$weights$chemical$cla$superclass,
          score_chemical_cla_class = benchmark_def_wei_ann$weights$chemical$cla$class,
          score_chemical_cla_parent = benchmark_def_wei_ann$weights$chemical$cla$parent,
          score_chemical_npc_pathway = benchmark_def_wei_ann$weights$chemical$npc$pathway,
          score_chemical_npc_superclass = benchmark_def_wei_ann$weights$chemical$npc$superclass,
          score_chemical_npc_class = benchmark_def_wei_ann$weights$chemical$npc$class,
          minimal_consistency = benchmark_def_wei_ann$annotations$thresholds$consistency,
          minimal_ms1_bio = benchmark_def_wei_ann$annotations$thresholds$ms1$biological,
          minimal_ms1_chemo = benchmark_def_wei_ann$annotations$thresholds$ms1$chemical,
          minimal_ms1_condition = benchmark_def_wei_ann$annotations$thresholds$ms1$condition,
          compounds_names = benchmark_def_wei_ann$options$compounds_names,
          high_confidence = FALSE,
          remove_ties = benchmark_def_wei_ann$options$remove_ties,
          summarize = benchmark_def_wei_ann$options$summarize,
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
