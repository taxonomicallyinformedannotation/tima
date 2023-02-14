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
## TODO ADD ALL FILES (input + config)
list(
  tar_file(name = file_paths,
           command = {
             "paths.yaml"
           }),
  tar_file(name = file_config_default_adducts,
           command = {
             "config/default/prepare_adducts.yaml"
           }),
  # tar_file(name = file_config_default_closed,
  #          command = {
  #            "config/default/prepare_closed.yaml"
  #          }),
  tar_file(name = file_config_default_edges,
           command = {
             "config/default/prepare_edges.yaml"
           }),
  tar_file(name = file_config_default_features_classification,
           command = {
             "config/default/prepare_features_classification.yaml"
           }),
  tar_file(name = file_config_default_features_components,
           command = {
             "config/default/prepare_features_components.yaml"
           }),
  tar_file(name = file_config_default_gnps,
           command = {
             "config/default/prepare_gnps.yaml"
           }),
  tar_file(name = file_config_default_library,
           command = {
             "config/default/prepare_library.yaml"
           }),
  tar_file(name = file_config_default_params,
           command = {
             "config/default/prepare_params.yaml"
           }),
  # tar_file(name = file_config_default_sirius,
  #          command = {
  #            "config/default/prepare_sirius.yaml"
  #          }),
  tar_file(name = file_config_default_spectral_matches,
           command = {
             "config/default/prepare_spectral_matches.yaml"
           }),
  tar_file(name = file_config_default_taxa,
           command = {
             "config/default/prepare_taxa.yaml"
           }),
  tar_file(name = file_config_default_annotations,
           command = {
             "config/default/process_annotations.yaml"
           }),
  tar_file(name = file_config_default_spectra,
           command = {
             "config/default/process_spectra.yaml"
           }),
  tar_file(name = file_config_params_adducts,
           command = {
             "config/params/prepare_adducts.yaml"
           }),
  # tar_file(name = file_config_params_closed,
  #          command = {
  #            "config/params/prepare_closed.yaml"
  #          }),
  tar_file(name = file_config_params_edges,
           command = {
             "config/params/prepare_edges.yaml"
           }),
  tar_file(name = file_config_params_features_classification,
           command = {
             "config/params/prepare_features_classification.yaml"
           }),
  tar_file(name = file_config_params_features_components,
           command = {
             "config/params/prepare_features_components.yaml"
           }),
  tar_file(name = file_config_params_gnps,
           command = {
             "config/params/prepare_gnps.yaml"
           }),
  tar_file(name = file_config_params_library,
           command = {
             "config/params/prepare_library.yaml"
           }),
  tar_file(name = file_config_params_params,
           command = {
             "config/params/prepare_params.yaml"
           }),
  # tar_file(name = file_config_params_sirius,
  #          command = {
  #            "config/params/prepare_sirius.yaml"
  #          }),
  tar_file(name = file_config_params_spectral_matches,
           command = {
             "config/params/prepare_spectral_matches.yaml"
           }),
  tar_file(name = file_config_params_taxa,
           command = {
             "config/params/prepare_taxa.yaml"
           }),
  tar_file(name = file_config_params_annotations,
           command = {
             "config/params/process_annotations.yaml"
           }),
  tar_file(name = file_config_params_spectra,
           command = {
             "config/params/process_spectra.yaml"
           }),
  tar_target(name = paths,
             command = {
               parse_yaml_paths(file = file_paths)
             }),
  tar_target(name = params_prepared,
             command = {
               parse_yaml_params(def = file_config_default_params,
                                 par = file_config_params_params)
             }),
  ## TODO ADD INITIAL MGF
  tar_file(name = lotus, command = {
    get_last_version_from_zenodo(
      doi = paths$url$lotus$doi,
      pattern = paths$urls$lotus$pattern,
      path = paths$data$source$libraries$lotus
    )
  }),
  tar_file(name = isdb_lotus_pos, command = {
    get_last_version_from_zenodo(
      doi = paths$url$lotus_isdb$doi,
      pattern = paths$urls$lotus_isdb$pattern$pos,
      path = paths$data$source$spectra$lotus$pos
    )
  }),
  tar_file(name = isdb_lotus_neg, command = {
    get_last_version_from_zenodo(
      doi = paths$url$lotus_isdb$doi,
      pattern = paths$urls$lotus_isdb$pattern$neg,
      path = paths$data$source$spectra$lotus$neg
    )
  }),
  tar_file(name = lotus_prepared,
           command = {
             prepare_lotus(input = lotus,
                           output = paths$data$interim$libraries$lotus)
           }),
  ## TODO ADD PREPARE CLOSED,
  ## TODO ADD PREPARE HMDB,
  tar_target(name = params_library,
             command = {
               parse_yaml_params(def = file_config_default_library,
                                 par = file_config_params_library)
             }),
  tar_file(name = library_prepared,
           command = {
             prepare_library(
               files = lotus_prepared,
               filter = params_library$organisms$filter$mode,
               level = params_library$organisms$filter$level,
               value = params_library$organisms$filter$value,
               output = params_library$files$libraries$sop$merged,
               parameters = params_library
             )
           }),
  tar_target(name = params_adducts,
             command = {
               parse_yaml_params(def = file_config_default_adducts,
                                 par = file_config_params_adducts)
             }),
  tar_file(name = adducts_prepared,
           command = {
             prepare_adducts(
               adducts_input = library_prepared,
               adducts_table_input = paths$data$source$adducts,
               config_output_path = paths$data$interim$config$path,
               adducts_output_path = paths$data$interim$adducts$path,
               output_name = params_adducts$files$libraries$adducts$processed,
               masses_pos_output_path = paths$data$interim$adducts$pos,
               masses_neg_output_path = paths$data$interim$adducts$neg,
               parameters = params_adducts
             )
           }),
  tar_file(name = isdb_lotus_prepared_pos,
           command = {
             prepare_isdb_lotus(
               input = isdb_lotus_pos,
               output = paths$data$interim$spectra$lotus$pos,
               polarity = "pos",
               export_sqlite = TRUE
             )
           }),
  tar_file(name = isdb_lotus_prepared_neg,
           command = {
             prepare_isdb_lotus(
               input = isdb_lotus_neg,
               output = paths$data$interim$spectra$lotus$pos,
               polarity = "neg",
               export_sqlite = TRUE
             )
           }),
  ## TODO ADD ISDB HMDB,
  ## TODO ADD MONA,
  tar_target(name = params_spectra,
             command = {
               parse_yaml_params(def = file_config_default_spectra,
                                 par = file_config_params_spectra)
             }),
  ## TODO improve polarity handling, suboptimal
  spectra_processed_pos <- tar_file(name = spectra_processed_pos,
                                    command = {
                                      process_spectra(
                                        input = params_spectra$files$spectral$raw,
                                        library = isdb_lotus_prepared_pos,
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
                                    }),
  spectra_processed_neg <- tar_file(name = spectra_processed_neg,
                                    command = {
                                      process_spectra(
                                        input = params_spectra$files$spectral$raw,
                                        library = isdb_lotus_prepared_neg,
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
                                    }),
  tar_target(name = params_gnps,
             command = {
               parse_yaml_params(def = file_config_default_gnps,
                                 par = file_config_params_gnps)
             }),
  tar_file(name = gnps_prepared,
           command = {
             prepare_gnps(
               gnps_job_id = params_gnps$gnps$id,
               output = params_gnps$files$annotations$pretreated,
               parameters = params_gnps
             )
           }),
  ## TODO ADD GET SIRIUS,
  ## TODO ADD PREPARE SIRIUS,
  tar_combine(
    name = spectra_processed,
    spectra_processed_pos,
    spectra_processed_neg,
    command = list(!!!.x)
  ),
  tar_target(name = params_spectral_matches,
             command = {
               parse_yaml_params(def = file_config_default_spectral_matches,
                                 par = file_config_params_spectral_matches)
             }),
  tar_file(name = spectral_matches_prepared,
           command = {
             prepare_spectral_matches(
               input = spectra_processed,
               output = params_spectral_matches$files$annotations$pretreated,
               parameters = params_spectral_matches
             )
           }),
  tar_target(name = params_edges,
             command = {
               parse_yaml_params(def = file_config_default_edges,
                                 par = file_config_params_edges)
             }),
  tar_file(name = edges_prepared,
           command = {
             prepare_edges(
               tool = params_edges$tools$networks$spectral$edges,
               gnps_job_id = params_edges$gnps$id,
               input = params_edges$files$networks$spectral$edges$raw,
               output = params_edges$files$networks$spectral$edges$processed,
               name_source = params_edges$names$source,
               name_target = params_edges$names$target,
               parameters = params_edges
             )
           }),
  tar_target(name = params_features_components,
             command = {
               parse_yaml_params(def = file_config_default_features_components,
                                 par = file_config_params_features_components)
             }),
  tar_file(name = features_components_prepared,
           command = {
             prepare_features_components(
               # input = list(spectral_matches_prepared,gnps_prepared),
               input = list(spectral_matches_prepared),
               output = params_features_components$files$annotations$filled,
               tool = params_features_components$tools$networks$spectral$components,
               components = params_features_components$files$networks$spectral$components$raw,
               gnps_job_id = params_features_components$gnps$id,
               ms_mode = params_features_components$ms$polarity,
               parameters = params_features_components
             )
           }),
  tar_target(name = params_features_classification,
             command = {
               parse_yaml_params(def = file_config_default_features_classification,
                                 par = file_config_params_features_classification)
             }),
  tar_file(name = features_classification_prepared,
           command = {
             prepare_features_classification(
               library = library_prepared,
               input = features_components_prepared,
               output = params_features_classification$files$annotations$treated,
               quickmode = params_features_classification$options$fast,
               parameters = params_features_classification
             )
           }),
  tar_target(name = params_taxa,
             command = {
               parse_yaml_params(def = file_config_default_taxa,
                                 par = file_config_params_taxa)
             }),
  tar_file(name = taxa_prepared,
           command = {
             prepare_taxa(
               input = params_taxa$files$features$raw,
               tool = params_taxa$tools$metadata,
               extension = params_taxa$names$extension,
               colname = params_taxa$names$taxon,
               gnps_job_id = params_taxa$gnps$id,
               metadata = params_taxa$files$taxa$raw,
               top_k = params_taxa$organisms$candidates,
               output = params_taxa$files$taxa$processed,
               taxon = params_taxa$organisms$taxon,
               parameters = params_taxa
             )
           }),
  tar_target(name = params_annotations,
             command = {
               parse_yaml_params(def = file_config_default_annotations,
                                 par = file_config_params_annotations)
             }),
  tar_file(name = annotations_processed,
           command = {
             process_annotations(
               library = library_prepared,
               name = adducts_prepared[params_annotations$ms$polarity],
               annotations = features_classification_prepared,
               taxa = taxa_prepared,
               edges = edges_prepared,
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
               minimal_ms1_bio = params_annotations$annotations$ms1$thresholds$biological,
               minimal_ms1_chemo = params_annotations$annotations$ms1$thresholds$chemical,
               # TODO ADD CONDITION
               ms1_only = params_annotations$annotations$ms1only,
               force = params_annotations$options$force,
               parameters = params_annotations
             )
           })
)
