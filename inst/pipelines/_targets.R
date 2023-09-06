# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = "timaR",
  memory = "transient",
  garbage_collection = TRUE
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
      tar_file(
        name = yaml_paths,
        command = {
          yaml_paths <- "inst/paths.yaml"
        }
      ),
      tar_target(
        name = paths,
        command = {
          paths <- parse_yaml_paths(file = yaml_paths)
        }
      ),
      tar_target(
        name = path_gnps_example_id,
        command = {
          path_gnps_example_id <- paths$gnps$example
        }
      )
    ),
    ## Dictionaries
    list(
      tar_file(
        name = dic_add,
        command = {
          dic_add <- system.file("extdata", "adducts.tsv", package = "timaR")
        }
      ),
      tar_file(
        name = dic_clu,
        command = {
          dic_clu <- system.file("extdata",
            "clusters.tsv",
            package = "timaR"
          )
        }
      ),
      tar_file(
        name = dic_neu_los,
        command = {
          dic_neu_los <- system.file("extdata",
            "neutral_losses.tsv",
            package = "timaR"
          )
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
        name = par_def_fil_ann,
        command = {
          par_def_fil_ann <-
            paths$params$default$filter$annotations
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
        name = par_def_pre_lib_rt,
        command = {
          par_def_pre_lib_rt <-
            paths$params$default$prepare$libraries$rt
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
                step = "create_components"
              )
          }
        ),
        tar_file(
          name = par_usr_fil_ann,
          command = {
            par_usr_fil_ann <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
                step = "filter_annotations"
              )
          }
        ),
        tar_file(
          name = par_usr_cre_edg_spe,
          command = {
            par_usr_cre_edg_spe <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
                step = "prepare_libraries_adducts"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_lib_rt,
          command = {
            par_usr_pre_lib_rt <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
                step = "prepare_libraries_rt"
              )
          }
        ),
        tar_file(
          name = par_usr_pre_lib_sop_clo,
          command = {
            par_usr_pre_lib_sop_clo <-
              prepare_params(
                filename = par_fin_par$files$pattern,
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
        #         features = par_fin_par$files$features$raw,
        #         spectra = par_fin_par$files$spectral$raw,
        #         gnps_job_id = par_fin_par$gnps$id,
        #         gnps_example_id = path_gnps_example_id,
        #         ms_mode = par_fin_par$ms$polarity,
        #         taxon = par_fin_par$organisms$taxon,
        # .        summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
                features = par_fin_par$files$features$raw,
                spectra = par_fin_par$files$spectral$raw,
                gnps_job_id = par_fin_par$gnps$id,
                gnps_example_id = path_gnps_example_id,
                ms_mode = par_fin_par$ms$polarity,
                taxon = par_fin_par$organisms$taxon,
                summarise = par_fin_par$options$summarise,
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
        name = par_pre_lib_add,
        command = {
          par_pre_lib_add <-
            parse_yaml_params(
              def = par_def_pre_lib_add,
              usr = par_usr_pre_lib_add[1]
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
      # tar_target(
      #   name = par_pre_lib_sop_hmd,
      #   command = {
      #     par_pre_lib_sop_hmd <-
      #       parse_yaml_params(
      #         def = par_def_pre_lib_sop_hmd,
      #         usr = par_usr_pre_lib_sop_hmd[1]
      #       )
      #   }
      # ),
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
  ## GNPS
  list(
    tar_file(
      name = gnps_tables,
      command = {
        gnps_tables <- get_gnps_tables(
          gnps_job_id = par_fin_par$gnps$id,
          gnps_job_example = paths$gnps$example,
          filename = par_fin_par$files$pattern,
          workflow = par_fin_par$gnps$workflow,
          path_features = par_pre_fea_tab$files$features$raw,
          path_metadata = par_pre_tax$files$taxa$raw,
          path_spectra = par_ann_spe$files$spectral$raw,
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
                no = par_pre_tax$files$features$raw
              ),
            no = par_pre_tax$files$features$raw
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
                  no = par_ann_spe$files$spectral$raw
                ),
              no = par_ann_spe$files$spectral$raw
            ),
            no = {
              get_file(
                url = paths$urls$examples$spectra_mini,
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
                no = par_pre_tax$files$taxa$raw
              ),
            no = par_pre_tax$files$taxa$raw
          )
      }
    )
  ),
  ## libraries
  list(
    ## Retention times
    list(tar_file(
      name = lib_rt,
      command = {
        lib_rt <- prepare_libraries_rt(
          ## TODO refactor to avoid "pos/neg"
          mgf_exp = list(
            "neg" = par_pre_lib_rt$files$libraries$spectral$exp$neg,
            "pos" = par_pre_lib_rt$files$libraries$spectral$exp$pos
          ),
          mgf_is = list(
            "neg" = par_pre_lib_rt$files$libraries$spectral$is$neg,
            "pos" = par_pre_lib_rt$files$libraries$spectral$exp$pos
          ),
          temp_exp = par_pre_lib_rt$files$libraries$temporal$exp,
          temp_is = par_pre_lib_rt$files$libraries$temporal$is,
          output_rt = par_pre_lib_rt$files$libraries$temporal$prepared,
          output_sop = par_pre_lib_rt$files$libraries$sop$prepared,
          col_ik = par_pre_lib_rt$names$mgf$inchikey,
          col_rt = par_pre_lib_rt$names$mgf$retention_time,
          col_sm = par_pre_lib_rt$names$mgf$smiles,
          name_inchikey = par_pre_lib_rt$names$inchikey,
          name_rt = par_pre_lib_rt$names$rt,
          name_smiles = par_pre_lib_rt$names$smiles,
          unit_rt = par_pre_lib_rt$units$rt
        )
      }
    )),
    tar_file(
      name = lib_rt_rts,
      command = {
        lib_rt_rts <- lib_rt[[1]]
      }
    ),
    tar_file(
      name = lib_rt_sop,
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
        # tar_file(
        #   name = lib_sop_clo,
        #   command = {
        #     lib_sop_clo <- paths$data$source$libraries$sop$closed
        #   }
        # ),
        tar_file(
          name = lib_sop_ecm,
          command = {
            lib_sop_ecm <- get_file(
              url = paths$urls$ecmdb$metabolites,
              export = paths$data$source$libraries$sop$ecmdb
            )
          }
        ),
        ## TODO ADD  GET HMDB
        tar_file(
          name = lib_sop_lot,
          command = {
            lib_sop_lot <- get_last_version_from_zenodo(
              doi = paths$urls$lotus$doi,
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
          name = lib_sop_clo_pre,
          command = {
            lib_sop_clo_pre <-
              prepare_libraries_sop_closed(
                ## TODO improve
                input = par_pre_lib_sop_clo$files$libraries$sop$raw$closed,
                output = par_pre_lib_sop_clo$files$libraries$sop$prepared
              )
          }
        ),
        tar_file(
          name = lib_sop_ecm_pre,
          command = {
            lib_sop_ecm_pre <-
              prepare_libraries_sop_ecmdb(
                input = lib_sop_ecm,
                output = par_pre_lib_sop_ecm$files$libraries$sop$prepared
              )
          }
        ),
        ## TODO ADD HMDB PREPARED
        tar_file(
          name = lib_sop_lot_pre,
          command = {
            lib_sop_lot_pre <-
              prepare_libraries_sop_lotus(
                input = if (paths$tests$mode == FALSE) {
                  lib_sop_lot
                } else {
                  paths$data$source$libraries$sop$lotus
                },
                output = par_pre_lib_sop_lot$files$libraries$sop$prepared
              )
          }
        )
      ),
      ## Merged
      list(
        tar_file(
          name = lib_sop_mer,
          command = {
            lib_sop_mer <- prepare_libraries_sop_merged(
              files = c(
                lib_sop_clo_pre,
                lib_sop_ecm_pre,
                ## TODO
                # lib_sop_hmd_pre,
                lib_sop_lot_pre,
                lib_rt_sop
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
        tar_file(name = lib_mer_key, command = {
          lib_mer_key <- lib_sop_mer[[1]]
        }),
        tar_file(name = lib_mer_org_tax_ott, command = {
          lib_mer_org_tax_ott <- lib_sop_mer[[2]]
        }),
        tar_file(name = lib_mer_str_stereo, command = {
          lib_mer_str_stereo <- lib_sop_mer[[3]]
        }),
        tar_file(name = lib_mer_str_met, command = {
          lib_mer_str_met <- lib_sop_mer[[4]]
        }),
        tar_file(name = lib_mer_str_nam, command = {
          lib_mer_str_nam <- lib_sop_mer[[5]]
        }),
        tar_file(name = lib_mer_str_tax_cla, command = {
          lib_mer_str_tax_cla <- lib_sop_mer[[6]]
        }),
        tar_file(name = lib_mer_str_tax_npc, command = {
          lib_mer_str_tax_npc <- lib_sop_mer[[7]]
        })
      )
    ),
    ## Adducts
    list(tar_file(
      name = lib_add,
      command = {
        lib_add <- prepare_libraries_adducts(
          str_met = lib_mer_str_met,
          adducts_masses = dic_add,
          adducts_output_path = paths$data$interim$libraries$adducts$path,
          output_name = par_pre_lib_add$files$libraries$adducts$prepared,
          masses_pos_output_path = par_pre_lib_add$files$libraries$adducts$pos,
          masses_neg_output_path = par_pre_lib_add$files$libraries$adducts$neg
        )
      }
    )),
    ## Spectra
    list( ## In silico
      list( ## Raw
        list(
          ## TODO ADD ISDB HMDB,
          tar_file(
            name = lib_spe_is_lot_pos,
            command = {
              lib_spe_is_lot_pos <-
                # if (paths$tests$mode == FALSE) {
                get_file(
                  url = paths$urls$examples$spectral_lib$pos,
                  export = paths$data$source$libraries$spectra$is$lotus$pos |>
                    gsub(
                      pattern = "isdb_pos.mgf",
                      replacement = "lotus_pos.rds"
                    )
                )
              # get_last_version_from_zenodo(
              #   doi = paths$urls$lotus_isdb$doi,
              #   pattern = paths$urls$lotus_isdb$pattern$pos,
              #   path = paths$data$source$libraries$spectra$is$lotus$pos
              # )
              # } else {
              #   get_file(
              #     url = paths$urls$examples$spectral_lib$pos,
              #     export = paths$data$source$libraries$spectra$is$lotus$pos
              #   )
              # }
            }
            ## To always check if a newest version is available
            ,
            cue = tar_cue(mode = "always")
            # cue = tar_cue(mode = "thorough")
          ),
          tar_file(
            name = lib_spe_is_lot_neg,
            command = {
              lib_spe_is_lot_neg <-
                # if (paths$tests$mode == FALSE) {
                get_file(
                  url = paths$urls$examples$spectral_lib$neg,
                  export = paths$data$source$libraries$spectra$is$lotus$neg |>
                    gsub(
                      pattern = "isdb_neg.mgf",
                      replacement = "lotus_neg.rds"
                    )
                )
              # get_last_version_from_zenodo(
              #   doi = paths$urls$lotus_isdb$doi,
              #   pattern = paths$urls$lotus_isdb$pattern$neg,
              #   path = paths$data$source$libraries$spectra$is$lotus$neg
              # )
              # } else {
              #   get_file(
              #     url = paths$urls$examples$spectral_lib$neg,
              #     export = paths$data$source$libraries$spectra$is$lotus$neg
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
        ## TODO improve polarity handling, suboptimal
        tar_file(
          name = lib_spe_is_lot_pre_pos,
          command = {
            lib_spe_is_lot_pre_pos <-
              lib_spe_is_lot_pos
            # lib_spe_is_lot_pre_pos <- prepare_libraries_spectra(
            #   input = if (paths$tests$mode == FALSE) {
            #     lib_spe_is_lot_pos
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
            # source_version = jsonlite::fromJSON(
            #   txt = "https://zenodo.org/api/records/5607185")$doi_url,
            # source_date = jsonlite::fromJSON(
            #   txt = "https://zenodo.org/api/records/5607185")
            # [["metadata"]][["publication_date"]],
            #     organism = "Life"
            #   )
            # )
          }
        ),
        tar_file(
          name = lib_spe_is_lot_pre_neg,
          command = {
            lib_spe_is_lot_pre_neg <-
              lib_spe_is_lot_neg
            # lib_spe_is_lot_pre_neg <- prepare_libraries_spectra(
            #   input = if (paths$tests$mode == FALSE) {
            #     lib_spe_is_lot_neg
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
            # source_version = jsonlite::fromJSON(
            #   txt = "https://zenodo.org/api/records/5607185")$doi_url,
            # source_date = jsonlite::fromJSON(
            #   txt = "https://zenodo.org/api/records/5607185")
            # [["metadata"]][["publication_date"]],
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
        list(
          ### JLW
          tar_file(
            name = lib_spe_exp_int_raw,
            command = {
              lib_spe_exp_int_raw <-
                par_pre_lib_spe$files$libraries$spectral$exp$raw
            }
          ),
          ### MassBank
          tar_file(
            name = lib_spe_exp_mb_raw,
            command = {
              lib_spe_exp_mb_raw <- get_massbank_spectra()
            }
          )
        ),
        ## Prepared
        list(
          ## TODO improve polarity handling, suboptimal
          tar_file(
            name = lib_spe_exp_int_pre_pos,
            command = {
              lib_spe_exp_int_pre_pos <-
                prepare_libraries_spectra(
                  input = lib_spe_exp_int_raw,
                  output =
                    "data/interim/libraries/spectra/exp/internal_pos.rds",
                  polarity = "pos",
                  metad = "JLW",
                  col_ce = par_pre_lib_spe$names$mgf$collision_energy,
                  col_ci = par_pre_lib_spe$names$mgf$compound_id,
                  col_em = par_pre_lib_spe$names$mgf$exact_mass,
                  col_in = par_pre_lib_spe$names$mgf$inchi,
                  col_io = par_pre_lib_spe$names$mgf$inchi_no_stereo,
                  col_ik = par_pre_lib_spe$names$mgf$inchikey,
                  col_il = par_pre_lib_spe$names$mgf$inchikey_no_stereo,
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
            }
          ),
          tar_file(
            name = lib_spe_exp_int_pre_neg,
            command = {
              lib_spe_exp_int_pre_neg <-
                prepare_libraries_spectra(
                  input = lib_spe_exp_int_raw,
                  output =
                    "data/interim/libraries/spectra/exp/internal_neg.rds",
                  polarity = "neg",
                  metad = "JLW",
                  col_ce = par_pre_lib_spe$names$mgf$collision_energy,
                  col_ci = par_pre_lib_spe$names$mgf$compound_id,
                  col_em = par_pre_lib_spe$names$mgf$exact_mass,
                  col_in = par_pre_lib_spe$names$mgf$inchi,
                  col_io = par_pre_lib_spe$names$mgf$inchi_no_stereo,
                  col_ik = par_pre_lib_spe$names$mgf$inchikey,
                  col_il = par_pre_lib_spe$names$mgf$inchikey_no_stereo,
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
            }
          ),
          tar_file(
            name = lib_spe_exp_mb_pre_neg,
            command = {
              lib_spe_exp_mb_pre_neg <-
                prepare_libraries_spectra(
                  input = lib_spe_exp_mb_raw,
                  output =
                    "data/interim/libraries/spectra/exp/massbank_neg.rds",
                  polarity = "neg",
                  metad = "massBank",
                  col_ce = "collisionEnergy_text",
                  col_ci = NULL,
                  col_em = "exactmass",
                  col_in = "inchi",
                  col_io = NULL,
                  col_ik = "inchikey",
                  col_il = NULL,
                  col_mf = "formula",
                  col_na = "name",
                  col_po = "polarity",
                  col_sm = "smiles",
                  col_sn = NULL,
                  col_si = "spectrum_id",
                  col_sp = "splash",
                  col_sy = NULL,
                  col_xl = NULL
                )
            }
          ),
          tar_file(
            name = lib_spe_exp_mb_pre_pos,
            command = {
              lib_spe_exp_mb_pre_pos <-
                prepare_libraries_spectra(
                  input = lib_spe_exp_mb_raw,
                  output =
                    "data/interim/libraries/spectra/exp/massbank_pos.rds",
                  polarity = "pos",
                  metad = "massBank",
                  col_ce = "collisionEnergy_text",
                  col_ci = NULL,
                  col_em = "exactmass",
                  col_in = "inchi",
                  col_io = NULL,
                  col_ik = "inchikey",
                  col_il = NULL,
                  col_mf = "formula",
                  col_na = "name",
                  col_po = "polarity",
                  col_sm = "smiles",
                  col_sn = NULL,
                  col_si = "spectrum_id",
                  col_sp = "splash",
                  col_sy = NULL,
                  col_xl = NULL
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
        name = ann_ms1_pre,
        command = {
          ann_ms1_pre <-
            annotate_masses(
              features = fea_pre,
              library = lib_mer_key,
              output_annotations = par_ann_mas$files$annotations$prepared$structural,
              output_edges = par_ann_mas$files$networks$spectral$edges$raw,
              name_source = par_ann_mas$names$source,
              name_target = par_ann_mas$names$target,
              str_stereo = lib_mer_str_stereo,
              str_met = lib_mer_str_met,
              str_nam = lib_mer_str_nam,
              str_tax_cla = lib_mer_str_tax_cla,
              str_tax_npc = lib_mer_str_tax_npc,
              name = lib_add[par_ann_mas$ms$polarity],
              adducts_list = par_ann_mas$ms$adducts,
              adducts_neg = par_ann_mas$files$libraries$adducts$neg,
              adducts_pos = par_ann_mas$files$libraries$adducts$pos,
              adducts_masses_list = dic_add,
              clusters_neg = par_ann_mas$ms$clusters$neg,
              clusters_pos = par_ann_mas$ms$clusters$pos,
              clusters_list = dic_clu,
              neutral_losses_list = dic_neu_los,
              ms_mode = par_ann_mas$ms$polarity,
              tolerance_ppm = par_ann_mas$ms$tolerances$mass$ppm$ms1,
              tolerance_rt = par_ann_mas$ms$tolerances$rt$minutes
            )
        }
      ),
      tar_file(
        name = ann_ms1_pre_ann,
        command = {
          ann_ms1_pre_ann <-
            ann_ms1_pre[[1]]
        }
      ),
      tar_file(
        name = ann_ms1_pre_edg,
        command = {
          ann_ms1_pre_edg <- ann_ms1_pre[[2]]
        }
      )
    ),
    ## Spectral
    list(
      ## Experimental
      list(
        tar_file(
          name = ann_spe_exp_gnp_pre,
          command = {
            ann_spe_exp_gnp_pre <-
              prepare_annotations_gnps(
                input = gnps_annotations,
                output = par_pre_ann_gnp$files$annotations$prepared$structural,
                str_stereo = lib_mer_str_stereo,
                str_met = lib_mer_str_met,
                str_nam = lib_mer_str_nam,
                str_tax_cla = lib_mer_str_tax_cla,
                str_tax_npc = lib_mer_str_tax_npc
              )
          }
        ),
        tar_file(
          name = ann_spe_exp_int_pos,
          command = {
            ann_spe_exp_int_pos <- annotate_spectra(
              input = input_spectra,
              library = lib_spe_exp_int_pre_pos,
              polarity = "pos",
              output = gsub(
                pattern = "matches.tsv.gz",
                replacement = "matches_exp_rt_pos.tsv.gz",
                x = par_ann_spe$files$annotations$raw$spectral,
                fixed = TRUE
              ),
              threshold = par_ann_spe$annotations$thresholds$ms2$similarity,
              ppm = par_ann_spe$ms$tolerances$mass$ppm$ms2,
              dalton = par_ann_spe$ms$tolerances$mass$dalton$ms2,
              qutoff = par_ann_spe$ms$thresholds$ms2$intensity,
              approx = par_ann_spe$annotations$ms2approx
            )
          }
        ),
        tar_file(
          name = ann_spe_exp_int_neg,
          command = {
            ann_spe_exp_int_neg <- annotate_spectra(
              input = input_spectra,
              library = lib_spe_exp_int_pre_neg,
              polarity = "neg",
              output = gsub(
                pattern = "matches.tsv.gz",
                replacement = "matches_exp_rt_neg.tsv.gz",
                x = par_ann_spe$files$annotations$raw$spectral,
                fixed = TRUE
              ),
              threshold = par_ann_spe$annotations$thresholds$ms2$similarity,
              ppm = par_ann_spe$ms$tolerances$mass$ppm$ms2,
              dalton = par_ann_spe$ms$tolerances$mass$dalton$ms2,
              qutoff = par_ann_spe$ms$thresholds$ms2$intensity,
              approx = par_ann_spe$annotations$ms2approx
            )
          }
        ),
        tar_file(
          name = ann_spe_exp_int_pre,
          command = {
            ann_spe_exp_int_pre <-
              prepare_annotations_spectra(
                input = list(
                  ann_spe_exp_int_neg,
                  ann_spe_exp_int_pos
                ),
                output = gsub(
                  pattern = "_prepared.tsv.gz",
                  replacement = "_exp_rt_prepared.tsv.gz",
                  x = par_pre_ann_spe$files$annotations$prepared$structural,
                  fixed = TRUE
                ),
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
        tar_file(
          name = ann_spe_pos,
          command = {
            ann_spe_pos <- annotate_spectra(
              input = input_spectra,
              library = list(
                lib_spe_is_lot_pre_pos,
                lib_spe_exp_mb_pre_pos
              ),
              polarity = "pos",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_pos.tsv.gz",
                x = par_ann_spe$files$annotations$raw$spectral,
                fixed = TRUE
              ),
              threshold = par_ann_spe$annotations$thresholds$ms2$similarity,
              ppm = par_ann_spe$ms$tolerances$mass$ppm$ms2,
              dalton = par_ann_spe$ms$tolerances$mass$dalton$ms2,
              qutoff = par_ann_spe$ms$thresholds$ms2$intensity,
              approx = par_ann_spe$annotations$ms2approx
            )
          }
        ),
        tar_file(
          name = ann_spe_neg,
          command = {
            ann_spe_neg <- annotate_spectra(
              input = input_spectra,
              library = list(
                lib_spe_is_lot_pre_neg,
                lib_spe_exp_mb_pre_neg
              ),
              polarity = "neg",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_neg.tsv.gz",
                x = par_ann_spe$files$annotations$raw$spectral,
                fixed = TRUE
              ),
              threshold = par_ann_spe$annotations$thresholds$ms2$similarity,
              ppm = par_ann_spe$ms$tolerances$mass$ppm$ms2,
              dalton = par_ann_spe$ms$tolerances$mass$dalton$ms2,
              qutoff = par_ann_spe$ms$thresholds$ms2$intensity,
              approx = par_ann_spe$annotations$ms2approx
            )
          }
        ),
        tar_file(
          name = ann_spe_pre,
          command = {
            ann_spe_pre <- prepare_annotations_spectra(
              input = list(
                ann_spe_neg,
                ## TODO add is hmdb
                ann_spe_pos
              ),
              output = par_pre_ann_spe$files$annotations$prepared$structural,
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
    tar_file(
      name = ann_sir_pre,
      command = {
        ann_sir_pre <-
          prepare_annotations_sirius(
            input_directory = par_pre_ann_sir$files$annotations$raw$sirius,
            output_ann = par_pre_ann_sir$files$annotations$prepared$structural,
            output_can = par_pre_ann_sir$files$annotations$prepared$canopus,
            output_for = par_pre_ann_sir$files$annotations$prepared$formula,
            str_stereo = lib_mer_str_stereo,
            str_met = lib_mer_str_met,
            str_nam = lib_mer_str_nam,
            str_tax_cla = lib_mer_str_tax_cla,
            str_tax_npc = lib_mer_str_tax_npc
          )
      }
    ),
    tar_file(name = ann_sir_pre_can, command = {
      ann_sir_pre_can <- ann_sir_pre[[1]]
    }),
    tar_file(name = ann_sir_pre_for, command = {
      ann_sir_pre_for <- ann_sir_pre[[2]]
    }),
    tar_file(name = ann_sir_pre_str, command = {
      ann_sir_pre_str <- ann_sir_pre[[3]]
    }),
    list()
  ),
  ## Features
  list(
    tar_file(
      name = fea_edg_spe,
      command = {
        fea_edg_spe <- create_edges_spectra(
          input = input_spectra,
          output = par_cre_edg_spe$files$networks$spectral$edges$raw,
          name_source = par_cre_edg_spe$names$source,
          name_target = par_cre_edg_spe$names$target,
          threshold = par_cre_edg_spe$annotations$thresholds$ms2$similarity,
          ppm = par_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
          dalton = par_cre_edg_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = par_cre_edg_spe$ms$thresholds$ms2$intensity
        )
      }
    ),
    tar_file(
      name = fea_com,
      command = {
        fea_com <- create_components(
          input = fea_edg_pre,
          output = par_cre_com$files$networks$spectral$components$raw
        )
      }
    ),
    ## Interim
    list(
      tar_file(
        name = int_com,
        command = {
          int_com <-
            if (file.exists(fea_com)) {
              fea_com
            } else {
              gnps_components
            }
        }
      ),
      tar_file(
        name = edg_spe,
        command = {
          edg_spe <-
            ifelse(test = file.exists(fea_edg_spe),
              yes = fea_edg_spe,
              no = gnps_edges
            )
        }
      )
    ),
    tar_file(
      name = fea_edg_pre,
      command = {
        fea_edg_pre <- prepare_features_edges(
          input = list(edg_spe, ann_ms1_pre_edg),
          output = par_pre_fea_edg$files$networks$spectral$edges$prepared,
          name_source = par_pre_fea_edg$names$source,
          name_target = par_pre_fea_edg$names$target
        )
      }
    ),
    tar_file(
      name = fea_com_pre,
      command = {
        fea_com_pre <- prepare_features_components(
          input = int_com,
          output = par_pre_fea_com$files$networks$spectral$components$prepared
        )
      }
    ),
    tar_file(
      name = fea_pre,
      command = {
        fea_pre <- prepare_features_tables(
          features = input_features,
          output = par_pre_fea_tab$files$features$prepared,
          name_features = par_pre_fea_tab$names$features,
          name_rt = par_pre_fea_tab$names$rt,
          name_mz = par_pre_fea_tab$names$precursor
        )
      }
    )
  ),
  tar_file(
    name = tax_pre,
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
        output = par_pre_tax$files$taxa$prepared,
        taxon = par_pre_tax$organisms$taxon
      )
    }
  ),
  tar_file(
    name = ann_fil,
    command = {
      ann_fil <- filter_annotations(
        annotations = list(
          ann_spe_exp_gnp_pre,
          # ann_spe_exp_int_pre,
          ann_spe_pre,
          ann_sir_pre_str,
          ann_ms1_pre_ann
        ),
        features = fea_pre,
        rts = lib_rt_rts,
        output = par_fil_ann$files$annotations$filtered,
        tolerance_rt = par_fil_ann$ms$tolerances$rt$minutes
      )
    }
  ),
  tar_file(
    name = ann_fil_crazy,
    command = {
      ann_fil_crazy <- filter_annotations(
        annotations = list(
          ann_spe_exp_gnp_pre,
          ann_spe_exp_int_pre,
          ann_spe_pre,
          ann_sir_pre_str,
          ann_ms1_pre_ann
        ),
        features = fea_pre,
        rts = lib_rt_rts,
        output = par_fil_ann$files$annotations$filtered,
        tolerance_rt = par_fil_ann$ms$tolerances$rt$minutes
      )
    }
  ),
  tar_file(
    name = ann_pre,
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
        summarise = par_wei_ann$options$summarise,
        pattern = par_wei_ann$files$pattern,
        force = par_wei_ann$options$force
      )
    }
  ),
  tar_file(
    name = ann_pre_crazy,
    command = {
      ann_pre_crazy <- weight_annotations(
        library = lib_mer_key,
        org_tax_ott = lib_mer_org_tax_ott,
        str_stereo = lib_mer_str_stereo,
        annotations = ann_fil_crazy,
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
        benchmark_path_url <- paths$urls$benchmarking_set
      }
    ),
    tar_target(
      name = benchmark_path_export,
      command = {
        benchmark_path_export <- paths$data$source$benchmark$set
      }
    ),
    tar_target(
      name = benchmark_path_copy,
      command = {
        benchmark_path_copy <- paths$data$source$benchmark$copy
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
    tar_file(
      name = benchmark_file,
      command = {
        benchmark_file <- get_file(
          url = benchmark_path_url,
          export = benchmark_path_export
        )
        return(benchmark_path_export)
      }
    ),
    tar_file(
      name = benchmark_copy,
      command = {
        con <- file(benchmark_file, "r")
        text <- readLines(con)
        close(con)
        # # To get both pos and neg
        # text_1 <- text |>
        #   head(100471)
        # text_2 <- text |>
        #   tail(100387)
        # text <- text_1 |>
        #   append(text_2)

        ## reduce size
        text_corrected <- text |>
          gsub(
            pattern =
              "(\\()([0-9]{1,9}.[0-9]{1,9})(, None\\))",
            replacement = "\\2"
          )

        patterns_to_remove <- c(
          "FILENAME:",
          "SEQ:",
          "IONMODE:",
          "ORGANISM:",
          "PI:",
          "DATACOLLECTOR:",
          "INCHIAUX:",
          "PUBMED:",
          "SUBMITUSER:",
          "LIBRARYQUALITY:",
          "PARENT_MASS:"
        )

        text_corrected_2 <- text_corrected[!grepl(
          pattern = paste(patterns_to_remove, collapse = "|"),
          x = text_corrected
        )]

        text_corrected_2 |>
          writeLines(con = benchmark_path_copy)
        return(benchmark_path_copy)
      }
    ),
    tar_file(
      name = benchmark_converted,
      command = {
        sp <- benchmark_copy |>
          Spectra::Spectra(source = MsBackendMsp::MsBackendMsp()) |>
          Spectra::setBackend(Spectra::MsBackendMemory())
        sp |>
          saveRDS(file = "data/interim/benchmark/benchmark_spectra.rds")
        return("data/interim/benchmark/benchmark_spectra.rds")
      }
    ),
    tar_file(
      name = benchmark_prepared,
      command = {
        sp <- benchmark_converted |>
          readRDS() |>
          sanitize_spectra(
            cutoff = 0,
            ratio = 10000,
            fragments = 5,
            deeper = FALSE
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
          inchikey = sp_clean$inchikey,
          instrument = sp_clean$SOURCE_INSTRUMENT,
          fragments = lapply(sp_clean@backend@peaksData, length) |>
            as.character() |>
            as.numeric() / 2,
          precursorMz = sp_clean$precursorMz,
          pepmass = sp_clean$PEPMASS,
          smiles = sp_clean$smiles,
          ccmslib = sp_clean$SPECTRUMID,
          charge = sp_clean$precursorCharge,
          name = sp_clean$name
        ) |>
          tidytable::mutate(
            tidytable::across(
              .cols = tidytable::where(is.character),
              .funs = function(x) {
                tidytable::na_if(x, "")
              }
            )
          )

        log_debug("Framed")
        df_clean <- df_meta |>
          tidytable::filter(!is.na(inchikey)) |>
          tidytable::filter(fragments >= 5) |>
          tidytable::filter(fragments <= 100) |>
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
            x = inchikey
          )) |>
          tidytable::distinct(inchikey_no_stereo, adduct, .keep_all = TRUE) |>
          tidytable::mutate(mz = precursorMz) |>
          ## Weird way to have some kind of retention time
          tidytable::mutate(rt = tidytable::cur_group_id(), .by = "inchikey_no_stereo")

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
          sp_clean[sp_clean$SPECTRUMID %in% df_clean_pos$ccmslib]
        sp_neg <-
          sp_clean[sp_clean$SPECTRUMID %in% df_clean_neg$ccmslib]

        extract_benchmark_spectra <- function(x, mode) {
          df <- x |>
            extract_spectra() |>
            tidytable::mutate(acquisitionNum = tidytable::row_number()) |>
            tidytable::mutate(spectrum_id = acquisitionNum) |>
            tidytable::mutate(short_ik = gsub(
              pattern = "-.*",
              replacement = "",
              inchikey
            )) |>
            tidytable::mutate(rtime = tidytable::cur_group_id(), .by = "short_ik") |>
            tidytable::mutate(precursorCharge = ifelse(
              test = mode == "pos",
              yes = as.integer(1),
              no = as.integer(-1)
            )) |>
            tidytable::select(
              SCANS,
              acquisitionNum,
              precursorCharge,
              precursorMz,
              MSLEVEL,
              rtime,
              name,
              smiles,
              inchi,
              inchikey,
              adduct = ADDUCT,
              instrument = SOURCE_INSTRUMENT,
              ccmslib = SPECTRUMID,
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
              x = inchikey
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
    tar_file(
      name = benchmark_pre_mgf_pos,
      command = {
        benchmark_pre_mgf_pos <- benchmark_prepared[[1]]
      }
    ),
    tar_file(
      name = benchmark_pre_mgf_neg,
      command = {
        benchmark_pre_mgf_neg <- benchmark_prepared[[2]]
      }
    ),
    tar_file(
      name = benchmark_pre_meta_pos,
      command = {
        benchmark_pre_meta_pos <- benchmark_prepared[[3]]
      }
    ),
    tar_file(
      name = benchmark_pre_meta_neg,
      command = {
        benchmark_pre_meta_neg <- benchmark_prepared[[4]]
      }
    ),
    tar_file(
      name = benchmark_taxed_pos,
      command = {
        benchmark_taxed_pos <- benchmark_pre_meta_pos |>
          taxize_spectra_benchmark(
            keys = lib_mer_key,
            org_tax_ott = lib_mer_org_tax_ott,
            output = "data/interim/benchmark/benchmark_taxed_pos.tsv.gz"
          )
      }
    ),
    tar_file(
      name = benchmark_taxed_neg,
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
    tar_file(
      name = benchmark_ann_ms1_pre_pos,
      command = {
        benchmark_ann_ms1_pre_pos <-
          annotate_masses(
            features = benchmark_pre_meta_pos,
            library = lib_mer_key,
            output_annotations =
              "data/interim/benchmark/benchmark_ann_ms1_pos.tsv.gz",
            output_edges =
              "data/interim/benchmark/benchmark_edges_ms1_pos.tsv.gz",
            name_source = benchmark_def_ann_mas$names$source,
            name_target = benchmark_def_ann_mas$names$target,
            str_stereo = lib_mer_str_stereo,
            str_met = lib_mer_str_met,
            str_nam = lib_mer_str_nam,
            str_tax_cla = lib_mer_str_tax_cla,
            str_tax_npc = lib_mer_str_tax_npc,
            name = lib_add["pos"],
            adducts_list = benchmark_def_ann_mas$ms$adducts,
            adducts_neg = benchmark_def_ann_mas$files$libraries$adducts$neg,
            adducts_pos = benchmark_def_ann_mas$files$libraries$adducts$pos,
            adducts_masses_list = dic_add,
            clusters_neg = benchmark_def_ann_mas$ms$clusters$neg,
            clusters_pos = benchmark_def_ann_mas$ms$clusters$pos,
            clusters_list = dic_clu,
            neutral_losses_list = dic_neu_los,
            ms_mode = "pos",
            tolerance_ppm = benchmark_def_ann_mas$ms$tolerances$mass$ppm$ms1,
            tolerance_rt = benchmark_def_ann_mas$ms$tolerances$rt$minutes
          )
      }
    ),
    tar_file(
      name = benchmark_ann_ms1_pre_neg,
      command = {
        benchmark_ann_ms1_pre_neg <-
          annotate_masses(
            features = benchmark_pre_meta_neg,
            library = lib_mer_key,
            output_annotations =
              "data/interim/benchmark/benchmark_ann_ms1_neg.tsv.gz",
            output_edges =
              "data/interim/benchmark/benchmark_edges_ms1_neg.tsv.gz",
            name_source = benchmark_def_ann_mas$names$source,
            name_target = benchmark_def_ann_mas$names$target,
            str_stereo = lib_mer_str_stereo,
            str_met = lib_mer_str_met,
            str_nam = lib_mer_str_nam,
            str_tax_cla = lib_mer_str_tax_cla,
            str_tax_npc = lib_mer_str_tax_npc,
            name = lib_add["neg"],
            adducts_list = benchmark_def_ann_mas$ms$adducts,
            adducts_neg = benchmark_def_ann_mas$files$libraries$adducts$neg,
            adducts_pos = benchmark_def_ann_mas$files$libraries$adducts$pos,
            adducts_masses_list = dic_add,
            clusters_neg = benchmark_def_ann_mas$ms$clusters$neg,
            clusters_pos = benchmark_def_ann_mas$ms$clusters$pos,
            clusters_list = dic_clu,
            neutral_losses_list = dic_neu_los,
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
    tar_file(
      name = benchmark_edg_spe_pos,
      command = {
        benchmark_edg_spe_pos <- create_edges_spectra(
          input = benchmark_pre_mgf_pos,
          output = "data/interim/benchmark/benchmark_edges_spe_pos.tsv.gz",
          name_source = benchmark_def_cre_edg_spe$names$source,
          name_target = benchmark_def_cre_edg_spe$names$target,
          threshold = benchmark_def_cre_edg_spe$annotations$thresholds$ms2$similarity,
          ppm = benchmark_def_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_cre_edg_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = 0
        )
      }
    ),
    tar_file(
      name = benchmark_edg_spe_neg,
      command = {
        benchmark_edg_spe_neg <- create_edges_spectra(
          input = benchmark_pre_mgf_neg,
          output = "data/interim/benchmark/benchmark_edges_spe_neg.tsv.gz",
          name_source = benchmark_def_cre_edg_spe$names$source,
          name_target = benchmark_def_cre_edg_spe$names$target,
          threshold = benchmark_def_cre_edg_spe$annotations$thresholds$ms2$similarity,
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
    tar_file(
      name = benchmark_edg_pre_pos,
      command = {
        benchmark_edg_pre_pos <- prepare_features_edges(
          input = list(benchmark_edg_spe_pos, benchmark_ann_ms1_pre_pos[[2]]),
          output = "data/interim/benchmark/benchmark_edges_pos.tsv.gz",
          name_source = benchmark_def_pre_fea_edg$names$source,
          name_target = benchmark_def_pre_fea_edg$names$target
        )
      }
    ),
    tar_file(
      name = benchmark_edg_pre_neg,
      command = {
        benchmark_edg_pre_neg <- prepare_features_edges(
          input = list(benchmark_edg_spe_neg, benchmark_ann_ms1_pre_neg[[2]]),
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
    tar_file(
      name = benchmark_com_pos,
      command = {
        benchmark_com_pos <- create_components(
          input = benchmark_edg_pre_pos,
          output = "data/interim/benchmark/benchmark_components_pos.tsv.gz"
        )
      }
    ),
    tar_file(
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
        benchmark_def_pre_fea_com <- parse_yaml_params(
          def = par_def_pre_fea_com,
          usr = par_def_pre_fea_com
        )
      }
    ),
    tar_file(
      name = benchmark_com_pre_pos,
      command = {
        benchmark_com_pre_pos <- prepare_features_components(
          input = benchmark_com_pos,
          output = "data/interim/benchmark/benchmark_com_pre_pos.tsv.gz"
        )
      }
    ),
    tar_file(
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
        benchmark_def_ann_spe <- parse_yaml_params(
          def = par_def_ann_spe,
          usr = par_def_ann_spe
        )
      }
    ),
    tar_file(
      name = benchmark_ann_spe_pos,
      command = {
        benchmark_ann_spe_pos <- annotate_spectra(
          input = benchmark_pre_mgf_pos,
          library = list(
            lib_spe_is_lot_pre_pos,
            lib_spe_exp_mb_pre_pos
          ),
          polarity = "pos",
          output = "data/interim/benchmark/benchmark_ann_spe_pos.tsv.gz",
          threshold =
            benchmark_def_ann_spe$annotations$thresholds$ms2$similarity,
          ppm = benchmark_def_ann_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_ann_spe$ms$tolerances$mass$dalton$ms2,
          qutoff = 0,
          approx = benchmark_def_ann_spe$annotations$ms2approx
        )
      }
    ),
    tar_file(
      name = benchmark_ann_spe_neg,
      command = {
        benchmark_ann_spe_neg <- annotate_spectra(
          input = benchmark_pre_mgf_neg,
          library = list(
            lib_spe_is_lot_pre_neg,
            lib_spe_exp_mb_pre_neg
          ),
          polarity = "neg",
          output = "data/interim/benchmark/benchmark_ann_spe_neg.tsv.gz",
          threshold =
            benchmark_def_ann_spe$annotations$thresholds$ms2$similarity,
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
    tar_file(
      name = benchmark_ann_spe_pre_pos,
      command = {
        benchmark_ann_spe_pre_pos <- prepare_annotations_spectra(
          input = list(benchmark_ann_spe_pos),
          output = "data/interim/benchmark/benchmark_ann_spe_pre_pos.tsv.gz",
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_nam = lib_mer_str_nam,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc
        )
      }
    ),
    tar_file(
      name = benchmark_ann_spe_pre_neg,
      command = {
        benchmark_ann_spe_pre_neg <- prepare_annotations_spectra(
          input = list(benchmark_ann_spe_neg),
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
    tar_file(
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
    tar_file(name = benchmark_ann_sir_pre_can, command = {
      benchmark_ann_sir_pre_can <- benchmark_ann_sir_pre[[1]]
    }),
    tar_file(name = benchmark_ann_sir_pre_for, command = {
      benchmark_ann_sir_pre_for <- benchmark_ann_sir_pre[[2]]
    }),
    tar_file(name = benchmark_ann_sir_pre_str, command = {
      benchmark_ann_sir_pre_str <- benchmark_ann_sir_pre[[3]]
    }),
    tar_target(
      name = benchmark_def_fil_ann,
      command = {
        benchmark_def_fil_ann <- parse_yaml_params(
          def = par_def_fil_ann,
          usr = par_def_fil_ann
        )
      }
    ),
    tar_file(
      name = benchmark_ann_fil_spe_neg,
      command = {
        benchmark_ann_fil_spe_neg <- filter_annotations(
          annotations = list(
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
    tar_file(
      name = benchmark_ann_fil_spe_ms1_neg,
      command = {
        benchmark_ann_fil_spe_ms1_neg <- filter_annotations(
          annotations = list(
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
    tar_file(
      name = benchmark_ann_fil_ms1_neg,
      command = {
        benchmark_ann_fil_ms1_neg <- filter_annotations(
          annotations = list(
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
    tar_file(
      name = benchmark_ann_fil_spe_pos,
      command = {
        benchmark_ann_fil_spe_pos <- filter_annotations(
          annotations = list(
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
    tar_file(
      name = benchmark_ann_fil_spe_ms1_pos,
      command = {
        benchmark_ann_fil_spe_ms1_pos <- filter_annotations(
          annotations = list(
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
    tar_file(
      name = benchmark_ann_fil_ms1_pos,
      command = {
        benchmark_ann_fil_ms1_pos <- filter_annotations(
          annotations = list(
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
    # tar_file(
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
    tar_file(
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
    tar_file(
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
    tar_file(
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
    tar_file(
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
    # tar_file(
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
    tar_file(
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
    tar_file(
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
    tar_file(
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
    tar_file(
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
