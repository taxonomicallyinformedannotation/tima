# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set()

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
list(
  ## Architecture
  list(
    ## Paths
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
      ),
      tar_file(
        name = dic_ranks,
        command = {
          dic_ranks <- paths$inst$extdata$ranks
        }
      )
    )
  ),
  ## Config
  list(
    ## Default
    list(
      tar_file(
        name = config_default_adducts,
        command = {
          config_default_adducts <- paths$config$default$prepare$adducts
        }
      ),
      tar_file(
        name = config_default_closed,
        command = {
          config_default_closed <- paths$config$default$prepare$closed
        }
      ),
      tar_file(
        name = config_default_features_classification,
        command = {
          config_default_features_classification <-
            paths$config$default$prepare$features_classification
        }
      ),
      tar_file(
        name = config_default_features_components,
        command = {
          config_default_features_components <-
            paths$config$default$prepare$features_components
        }
      ),
      tar_file(
        name = config_default_features_edges,
        command = {
          config_default_features_edges <-
            paths$config$default$prepare$features_edges
        }
      ),
      # tar_file(name = config_default_gnps,
      #          command = {
      #            config_default_gnps <- paths$config$default$prepare$gnps
      #          }),
      tar_file(
        name = config_default_library,
        command = {
          config_default_library <- paths$config$default$prepare$library
        }
      ),
      # tar_file(name = config_default_sirius,
      #          command = {
      #            config_default_sirius <- paths$config$default$prepare$sirius
      #          }),
      tar_file(
        name = config_default_spectral_matches,
        command = {
          config_default_spectral_matches <-
            paths$config$default$prepare$spectral_matches
        }
      ),
      tar_file(
        name = config_default_taxa,
        command = {
          config_default_taxa <- paths$config$default$prepare$taxa
        }
      ),
      tar_file(
        name = config_default_annotations,
        command = {
          config_default_annotations <-
            paths$config$default$process$annotations
        }
      ),
      tar_file(
        name = config_default_spectra,
        command = {
          config_default_spectra <- paths$config$default$process$spectra
        }
      )
    ),
    ## User prepared
    list(
      tar_file(
        name = config_prepare_config,
        command = {
          config_prepare_config <- paths$config$prepare_config
        }
      ),
      tar_target(
        name = params_config,
        command = {
          params_config <- parse_yaml_params(
            def = config_prepare_config,
            usr = config_prepare_config
          )
        }
      ),
      tar_file(
        name = config_user_adducts,
        command = {
          config_user_adducts <-
            prepare_config(
              filename = params_config$files$pattern,
              gnps_job_id = params_config$gnps$id,
              ms_mode = params_config$ms$polarity,
              taxon = params_config$organisms$taxon,
              parameters = params_config,
              step = "prepare_adducts"
            )
        }
      ),
      tar_file(
        name = config_user_closed,
        command = {
          config_user_closed <-
            prepare_config(
              filename = params_config$files$pattern,
              gnps_job_id = params_config$gnps$id,
              ms_mode = params_config$ms$polarity,
              taxon = params_config$organisms$taxon,
              parameters = params_config,
              step = "prepare_closed"
            )
        }
      ),
      tar_file(
        name = config_user_features_classification,
        command = {
          config_user_features_classification <-
            prepare_config(
              filename = params_config$files$pattern,
              gnps_job_id = params_config$gnps$id,
              ms_mode = params_config$ms$polarity,
              taxon = params_config$organisms$taxon,
              parameters = params_config,
              step = "prepare_features_classification"
            )
        }
      ),
      tar_file(
        name = config_user_features_components,
        command = {
          config_user_features_components <-
            prepare_config(
              filename = params_config$files$pattern,
              gnps_job_id = params_config$gnps$id,
              ms_mode = params_config$ms$polarity,
              taxon = params_config$organisms$taxon,
              parameters = params_config,
              step = "prepare_features_components"
            )
        }
      ),
      tar_file(
        name = config_user_features_edges,
        command = {
          config_user_features_edges <-
            prepare_config(
              filename = params_config$files$pattern,
              gnps_job_id = params_config$gnps$id,
              ms_mode = params_config$ms$polarity,
              taxon = params_config$organisms$taxon,
              parameters = params_config,
              step = "prepare_features_edges"
            )
        }
      ),
      # tar_file(name = config_user_gnps,
      #          command = {
      #            config_user_gnps <-
      #              prepare_config(
      #                filename = params_config$files$pattern,
      #                gnps_job_id = params_config$gnps$id,
      #                ms_mode = params_config$ms$polarity,
      #                taxon = params_config$organisms$taxon,
      #                parameters = params_config,
      #                step = "prepare_gnps"
      #              )
      #          }),
      tar_file(
        name = config_user_library,
        command = {
          config_user_library <-
            prepare_config(
              filename = params_config$files$pattern,
              gnps_job_id = params_config$gnps$id,
              ms_mode = params_config$ms$polarity,
              taxon = params_config$organisms$taxon,
              parameters = params_config,
              step = "prepare_library"
            )
        }
      ),
      # tar_file(name = config_user_sirius,
      #          command = {
      #            config_user_sirius <-
      #              prepare_config(
      #                filename = params_config$files$pattern,
      #                gnps_job_id = params_config$gnps$id,
      #                ms_mode = params_config$ms$polarity,
      #                taxon = params_config$organisms$taxon,
      #                parameters = params_config,
      #                step = "prepare_sirius"
      #              )
      #          }),
      tar_file(
        name = config_user_spectral_matches,
        command = {
          config_user_spectral_matches <-
            prepare_config(
              filename = params_config$files$pattern,
              gnps_job_id = params_config$gnps$id,
              ms_mode = params_config$ms$polarity,
              taxon = params_config$organisms$taxon,
              parameters = params_config,
              step = "prepare_spectral_matches"
            )
        }
      ),
      tar_file(
        name = config_user_taxa,
        command = {
          config_user_taxa <-
            prepare_config(
              filename = params_config$files$pattern,
              gnps_job_id = params_config$gnps$id,
              ms_mode = params_config$ms$polarity,
              taxon = params_config$organisms$taxon,
              parameters = params_config,
              step = "prepare_taxa"
            )
        }
      ),
      tar_file(
        name = config_user_annotations,
        command = {
          config_user_annotations <-
            prepare_config(
              filename = params_config$files$pattern,
              gnps_job_id = params_config$gnps$id,
              ms_mode = params_config$ms$polarity,
              taxon = params_config$organisms$taxon,
              parameters = params_config,
              step = "process_annotations"
            )
        }
      ),
      tar_file(
        name = config_user_spectra,
        command = {
          config_user_spectra <-
            prepare_config(
              filename = params_config$files$pattern,
              gnps_job_id = params_config$gnps$id,
              ms_mode = params_config$ms$polarity,
              taxon = params_config$organisms$taxon,
              parameters = params_config,
              step = "process_spectra"
            )
        }
      )
    ),
    ## Final
    list(
      tar_target(
        name = params_adducts,
        command = {
          params_adducts <-
            parse_yaml_params(
              def = config_default_adducts,
              usr = config_user_adducts[1]
            )
        }
      ),
      tar_target(
        name = params_closed,
        command = {
          params_closed <-
            parse_yaml_params(
              def = config_default_closed,
              usr = config_user_closed[1]
            )
        }
      ),
      tar_target(
        name = params_features_classification,
        command = {
          params_features_classification <-
            parse_yaml_params(
              def = config_default_features_classification,
              usr = config_user_features_classification[1]
            )
        }
      ),
      tar_target(
        name = params_features_components,
        command = {
          params_features_components <-
            parse_yaml_params(
              def = config_default_features_components,
              usr = config_user_features_components[1]
            )
        }
      ),
      tar_target(
        name = params_features_edges,
        command = {
          params_features_edges <-
            parse_yaml_params(
              def = config_default_features_edges,
              usr = config_user_features_edges[1]
            )
        }
      ),
      # tar_target(name = params_gnps,
      #            command = {
      #              params_gnps <-
      #                parse_yaml_params(def = config_default_gnps,
      #                                  usr = config_user_gnps[1])
      #            }),
      ## TODO ADD PARAMS HMDB,
      tar_target(
        name = params_library,
        command = {
          params_library <-
            parse_yaml_params(
              def = config_default_library,
              usr = config_user_library[1]
            )
        }
      ),
      # tar_target(name = params_sirius,
      #            command = {
      #              params_sirius <-
      #                parse_yaml_params(def = config_default_sirius,
      #                                  usr = config_user_sirius[1])
      #            }),
      tar_target(
        name = params_spectral_matches,
        command = {
          params_spectral_matches <-
            parse_yaml_params(
              def = config_default_spectral_matches,
              usr = config_user_spectral_matches[1]
            )
        }
      ),
      tar_target(
        name = params_taxa,
        command = {
          params_taxa <-
            parse_yaml_params(
              def = config_default_taxa,
              usr = config_user_taxa[1]
            )
        }
      ),
      tar_target(
        name = params_annotations,
        command = {
          params_annotations <-
            parse_yaml_params(
              def = config_default_annotations,
              usr = config_user_annotations[1]
            )
        }
      ),
      tar_target(
        name = params_spectra,
        command = {
          params_spectra <-
            parse_yaml_params(
              def = config_default_spectra,
              usr = config_user_spectra[1]
            )
        }
      )
    )
  ),
  ## LIST INPUT

  ## TODO ADD INITIAL MGF

  ## libraries
  list(
    ## Structure organism pairs
    list(
      ## Raw
      list(
        tar_file(
          name = library_sop_closed,
          command = {
            library_sop_closed <- paths$data$source$libraries$closed
          }
        ),
        ## TODO ADD HMDB,
        tar_file(
          name = library_sop_lotus,
          command = {
            library_sop_lotus <- get_last_version_from_zenodo(
              doi = paths$url$lotus$doi,
              pattern = paths$urls$lotus$pattern,
              path = paths$data$source$libraries$lotus
            )
          }
        )
      ),
      ## Prepared
      list(
        tar_file(
          name = library_sop_closed_prepared,
          command = {
            library_sop_closed_prepared <-
              prepare_closed(
                input = library_sop_closed,
                output = paths$data$interim$libraries$closed,
                parameters = params_closed
              )
          }
        ),
        ## TODO ADD HMDB PREPARED,
        tar_file(
          name = library_sop_lotus_prepared,
          command = {
            library_sop_lotus_prepared <-
              prepare_lotus(
                input = library_sop_lotus,
                output = paths$data$interim$libraries$lotus
              )
          }
        )
      ),
      ## Merged
      list(tar_file(
        name = library_sop_merged,
        command = {
          library_sop_merged <- prepare_library(
            files = c(library_sop_closed_prepared, library_sop_lotus_prepared),
            filter = params_library$organisms$filter$mode,
            level = params_library$organisms$filter$level,
            value = params_library$organisms$filter$value,
            output = params_library$files$libraries$sop$merged,
            parameters = params_library
          )
        }
      ))
    ),
    ## Adducts
    list(tar_file(
      name = library_adducts,
      command = {
        library_adducts <- prepare_adducts(
          adducts_input = library_sop_merged,
          adducts_table_input = paths$inst$extdata$adducts,
          config_output_path = paths$data$interim$config$path,
          adducts_output_path = paths$data$interim$adducts$path,
          output_name = params_adducts$files$libraries$adducts$processed,
          masses_pos_output_path = paths$data$interim$adducts$pos,
          masses_neg_output_path = paths$data$interim$adducts$neg,
          parameters = params_adducts
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
              library_spectra_is_lotus_pos <- get_last_version_from_zenodo(
                doi = paths$url$lotus_isdb$doi,
                pattern = paths$urls$lotus_isdb$pattern$pos,
                path = paths$data$source$spectra$lotus$pos
              )
            }
          ),
          tar_file(
            name = library_spectra_is_lotus_neg,
            command = {
              library_spectra_is_lotus_neg <- get_last_version_from_zenodo(
                doi = paths$url$lotus_isdb$doi,
                pattern = paths$urls$lotus_isdb$pattern$neg,
                path = paths$data$source$spectra$lotus$neg
              )
            }
          )
        )
      ),
      ## Prepared
      list(
        ## TODO ADD ISDB HMDB PREPARED,
        tar_file(
          name = library_spectra_is_lotus_pos_prepared,
          command = {
            library_spectra_is_lotus_pos_prepared <- prepare_isdb_lotus(
              input = library_spectra_is_lotus_pos,
              output = paths$data$interim$spectra$lotus$pos,
              polarity = "pos",
              export_sqlite = TRUE
            )
          }
        ),
        tar_file(
          name = library_spectra_is_lotus_neg_prepared,
          command = {
            library_spectra_is_lotus_neg_prepared <- prepare_isdb_lotus(
              input = library_spectra_is_lotus_neg,
              output = paths$data$interim$spectra$lotus$pos,
              polarity = "neg",
              export_sqlite = TRUE
            )
          }
        )
      ),
      ## Experimental
      list( ## RAW
        list(),
        ## Prepared
        list()
      )
    )
  ),
  ## Annotations
  list(
    ## Spectral
    list(
      ## TODO Extend to other libraries
      ## TODO improve polarity handling, suboptimal
      annotations_spectral_is_lotus_pos <-
        tar_file(
          name = annotations_spectral_is_lotus_pos,
          command = {
            annotations_spectral_is_lotus_pos <- process_spectra(
              input = params_spectra$files$spectral$raw,
              library = library_spectra_is_lotus_pos_prepared,
              polarity = "pos",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_pos.tsv.gz",
                x = params_spectra$files$annotations$raw$spectral,
                fixed = TRUE
              ),
              method = params_spectra$annotations$ms2$method,
              threshold = params_spectra$annotations$ms2$thresholds$similarity,
              ppm = params_spectra$ms$tolerances$mass$ppm$ms2,
              dalton = params_spectra$ms$tolerances$mass$dalton$ms2,
              npeaks = params_spectra$annotations$ms2$thresholds$peaks$absolute,
              rpeaks = params_spectra$annotations$ms2$thresholds$peaks$ratio,
              condition = params_spectra$annotations$ms2$thresholds$condition,
              qutoff = params_spectra$ms$intensity$thresholds$ms2,
              parallel = params_spectra$options$parallel,
              fast = params_spectra$options$fast,
              approx = params_spectra$annotations$ms2$approx,
              parameters = params_spectra
            )
          }
        ),
      # tar_file(name = annotations_spectral_gnps_prepared,
      #          command = {
      #            annotations_spectral_gnps_prepared <-
      #              prepare_gnps(
      #                gnps_job_id = params_gnps$gnps$id,
      #                output = params_gnps$files$annotations$pretreated,
      #                parameters = params_gnps
      #              )
      #          }),
      annotations_spectral_is_lotus_neg <-
        tar_file(
          name = annotations_spectral_is_lotus_neg,
          command = {
            annotations_spectral_is_lotus_neg <- process_spectra(
              input = params_spectra$files$spectral$raw,
              library = library_spectra_is_lotus_neg_prepared,
              polarity = "neg",
              output = gsub(
                pattern = ".tsv.gz",
                replacement = "_neg.tsv.gz",
                x = params_spectra$files$annotations$raw$spectral,
                fixed = TRUE
              ),
              method = params_spectra$annotations$ms2$method,
              threshold = params_spectra$annotations$ms2$thresholds$similarity,
              ppm = params_spectra$ms$tolerances$mass$ppm$ms2,
              dalton = params_spectra$ms$tolerances$mass$dalton$ms2,
              npeaks = params_spectra$annotations$ms2$thresholds$peaks$absolute,
              rpeaks = params_spectra$annotations$ms2$thresholds$peaks$ratio,
              condition = params_spectra$annotations$ms2$thresholds$condition,
              qutoff = params_spectra$ms$intensity$thresholds$ms2,
              parallel = params_spectra$options$parallel,
              fast = params_spectra$options$fast,
              approx = params_spectra$annotations$ms2$approx,
              parameters = params_spectra
            )
          }
        ),
      tar_combine(
        name = annotations_spectral_merged,
        annotations_spectral_is_lotus_pos,
        annotations_spectral_is_lotus_neg,
        command = list(!!!.x)
      ),
      tar_file(
        name = annotations_spectral_prepared,
        command = {
          annotations_spectral_prepared <- prepare_spectral_matches(
            input = annotations_spectral_merged,
            output = params_spectral_matches$files$annotations$pretreated,
            parameters = params_spectral_matches
          )
        }
      )
    ),
    ## SIRIUS
    # tar_file(name = annotations_sirius_prepared,
    #          command = {
    #            annotations_sirius_prepared <-
    #              prepare_sirius(
    #                input_directory = params_sirius$files$annotations$raw$sirius,
    #                npc = params_sirius$tools$taxonomies$chemical,
    #                output = params_sirius$files$annotations$pretreated,
    #                parameters = params_sirius
    #              )
    #          }),
    list()
  ),
  ## Features
  list(
    tar_file(
      name = features_edges_prepared,
      command = {
        features_edges_prepared <- prepare_features_edges(
          tool = params_features_edges$tools$networks$spectral$edges,
          gnps_job_id = params_features_edges$gnps$id,
          input = params_features_edges$files$networks$spectral$edges$raw,
          output = params_features_edges$files$networks$spectral$edges$processed,
          name_source = params_features_edges$names$source,
          name_target = params_features_edges$names$target,
          parameters = params_features_edges
        )
      }
    ),
    tar_file(
      name = features_components_prepared,
      command = {
        features_components_prepared <- prepare_features_components(
          # input = list(annotations_spectral_prepared, sirius_prepared),
          input = list(annotations_spectral_prepared),
          output = params_features_components$files$annotations$filled,
          tool = params_features_components$tools$networks$spectral$components,
          components = params_features_components$files$networks$spectral$components$raw,
          gnps_job_id = params_features_components$gnps$id,
          ms_mode = params_features_components$ms$polarity,
          parameters = params_features_components
        )
      }
    ),
    tar_file(
      name = features_classification_prepared,
      command = {
        features_classification_prepared <- prepare_features_classification(
          library = library_sop_merged,
          input = features_components_prepared,
          output = params_features_classification$files$annotations$treated,
          quickmode = params_features_classification$options$fast,
          parameters = params_features_classification
        )
      }
    )
  ),
  tar_file(
    name = taxa_prepared,
    command = {
      taxa_prepared <- prepare_taxa(
        input = params_taxa$files$features$raw,
        tool = params_taxa$tools$metadata,
        extension = params_taxa$names$extension,
        colname = params_taxa$names$taxon,
        gnps_job_id = params_taxa$gnps$id,
        metadata = params_taxa$files$taxa$raw,
        top_k = params_taxa$organisms$candidates,
        output = params_taxa$files$taxa$processed,
        taxon = params_taxa$organisms$taxon,
        dictionary = dic_ranks,
        parameters = params_taxa
      )
    }
  ),
  tar_file(
    name = annotations_processed,
    command = {
      annotations_processed <- process_annotations(
        library = library_sop_merged,
        name = library_adducts[params_annotations$ms$polarity],
        annotations = features_classification_prepared,
        taxa = taxa_prepared,
        edges = features_edges_prepared,
        output = params_annotations$files$annotations$processed,
        candidates_initial = params_annotations$annotations$candidates$initial,
        candidates_final = params_annotations$annotations$candidates$final,
        weight_spectral = params_annotations$weights$global$spectral,
        weight_chemical = params_annotations$weights$global$chemical,
        weight_biological = params_annotations$weights$global$biological,
        score_chemical_pathway = params_annotations$weights$chemical$pathway,
        score_chemical_superclass = params_annotations$weights$chemical$superclass,
        score_chemical_class = params_annotations$weights$chemical$class,
        score_biological_domain = params_annotations$weights$biological$domain,
        score_biological_kingdom = params_annotations$weights$biological$kingdom,
        score_biological_phylum = params_annotations$weights$biological$phylum,
        score_biological_class = params_annotations$weights$biological$class,
        score_biological_order = params_annotations$weights$biological$order,
        score_biological_infraorder = params_annotations$weights$biological$infraorder,
        score_biological_family = params_annotations$weights$biological$family,
        score_biological_subfamily = params_annotations$weights$biological$subfamily,
        score_biological_tribe = params_annotations$weights$biological$tribe,
        score_biological_subtribe = params_annotations$weights$biological$subtribe,
        score_biological_genus = params_annotations$weights$biological$genus,
        score_biological_subgenus = params_annotations$weights$biological$subgenus,
        score_biological_species = params_annotations$weights$biological$species,
        score_biological_subspecies = params_annotations$weights$biological$subspecies,
        score_biological_variety = params_annotations$weights$biological$variety,
        ms_mode = params_annotations$ms$polarity,
        annotate = params_annotations$annotations$ms1$annotate,
        tolerance_ppm = params_annotations$ms$tolerances$mass$ppm$ms1,
        tolerance_rt = params_annotations$ms$tolerances$rt$minutes,
        adducts_list = params_annotations$ms$adducts,
        adducts_masses_list = dic_adducts,
        neutral_losses_list = dic_neutral_losses,
        minimal_ms1_bio = params_annotations$annotations$ms1$thresholds$biological,
        minimal_ms1_chemo = params_annotations$annotations$ms1$thresholds$chemical,
        # TODO ADD CONDITION
        ms1_only = params_annotations$annotations$ms1only,
        force = params_annotations$options$force,
        parameters = params_annotations
      )
    }
  )
)
