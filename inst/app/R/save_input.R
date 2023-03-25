# save the results to a file
save_input <- function(input) {
  setwd("../../")

  paths <- parse_yaml_paths()
  paths_data_source <- paths$data$source$path
  log_debug(x = "Loading default params")
  yaml_files <- c(
    list.files(
      path = file.path(paths$params$default),
      pattern = ".yaml",
      full.names = TRUE
    ),
    paths$params$prepare_params
  )

  if (length(list.files(paths$params$user$path)) >=
    length(list.files(paths$params$default$path)) -
      ## because of params.yaml
      1) {
    yaml_files <- c(
      list.files(
        path = file.path(paths$params$user),
        pattern = ".yaml",
        full.names = TRUE
      ),
      paths$params$prepare_params
    )
  }

  yaml_names <- yaml_files |>
    gsub(pattern = "params/default/", replacement = "") |>
    gsub(pattern = "params/user/", replacement = "") |>
    gsub(pattern = ".yaml", replacement = "")

  yamls_default <- lapply(
    X = yaml_files,
    FUN = yaml::read_yaml
  )

  names(yamls_default) <- yaml_names

  yamls_params <- yamls_default

  ## Params occurring more than once
  fil_fea_raw <<- isolate(data$fil_fea_raw)[[1]]
  fil_spe_raw <<- isolate(data$fil_spe_raw)[[1]]
  fil_tax_raw <<- isolate(data$fil_tax_raw)[[1]]

  filename <<- isolate(data$fil_pat)
  gnps_job_id <<- isolate(data$gnps_id)
  if (gnps_job_id == "") {
    gnps_job_id <<- NULL
  }
  gnps_workflow <<- isolate(data$gnps_workflow)
  org_tax <<- isolate(data$org_tax)
  if (org_tax == "") {
    org_tax <<- NULL
  }
  ms_mode <<- isolate(data$ms_pol)
  ms_tol_rt_min <<- isolate(data$ms_tol_rt_min)
  names_source <<- isolate(data$names_source)
  names_target <<- isolate(data$names_target)
  fast <<- isolate(data$fast)
  forceps <<- isolate(data$force)
  parallel <<- isolate(data$parallel)
  summarise <<- isolate(data$summarise)

  ## Change 1
  log_debug(x = "Changing parameters")
  yamls_params$`params/prepare_params`$files$pattern <- filename
  yamls_params$`params/prepare_params`$gnps$id <- gnps_job_id
  yamls_params$`params/prepare_params`$gnps$workflow <-
    gnps_workflow
  yamls_params$`params/prepare_params`$ms$polarity <- ms_mode
  yamls_params$`params/prepare_params`$organisms$taxon <- org_tax
  ## crazy
  yamls_params$`params/prepare_params`$options$summarise <-
    summarise
  yaml::write_yaml(x = yamls_params$`params/prepare_params`, file = "params/prepare_params.yaml")

  yamls_params$annotate_masses$ms$adducts$neg <-
    isolate(data$ms_add_neg)
  yamls_params$annotate_masses$ms$adducts$pos <-
    isolate(data$ms_add_pos)
  yamls_params$annotate_masses$ms$intensity$thresholds$ms1 <-
    isolate(data$ms_int_thr_ms1)
  yamls_params$annotate_masses$ms$polarity <- ms_mode
  yamls_params$annotate_masses$ms$tolerances$mass$ppm$ms1 <-
    isolate(data$ms_tol_mas_ppm_ms1)
  yamls_params$annotate_masses$ms$tolerances$mass$dalton$ms1 <-
    isolate(data$ms_tol_mas_dal_ms1)
  yamls_params$annotate_masses$ms$tolerances$rt$minutes <-
    ms_tol_rt_min
  yamls_params$annotate_masses$names$source <- names_source
  yamls_params$annotate_masses$names$target <- names_target
  yamls_params$annotate_masses$options$force <- forceps

  yamls_params$annotate_spectra$files$spectral$raw <-
    file.path(paths_data_source, fil_spe_raw)
  yamls_params$annotate_spectra$annotations$ms2$approx <-
    isolate(data$ann_ms2_app)
  yamls_params$annotate_spectra$annotations$ms2$method <-
    isolate(data$ann_ms2_met)
  yamls_params$annotate_spectra$annotations$ms2$thresholds$condition <-
    isolate(data$ann_ms2_thr_con)
  yamls_params$annotate_spectra$annotations$ms2$thresholds$peaks$absolute <-
    isolate(data$ann_ms2_thr_pea_abs)
  yamls_params$annotate_spectra$annotations$ms2$thresholds$peaks$ratio <-
    isolate(data$ann_ms2_thr_pea_rat)
  yamls_params$annotate_spectra$annotations$ms2$thresholds$similarity <-
    isolate(data$ann_ms2_thr_sim)
  yamls_params$annotate_spectra$ms$intensity$thresholds$ms2 <-
    isolate(data$ms_int_thr_ms2)
  yamls_params$annotate_spectra$ms$polarity <- ms_mode
  yamls_params$annotate_spectra$ms$tolerances$mass$ppm$ms2 <-
    isolate(data$ms_tol_mas_ppm_ms2)
  yamls_params$annotate_spectra$ms$tolerances$mass$dalton$ms2 <-
    isolate(data$ms_tol_mas_dal_ms2)
  yamls_params$annotate_spectra$ms$tolerances$rt$minutes <-
    ms_tol_rt_min
  yamls_params$annotate_spectra$options$fast <- fast
  yamls_params$annotate_spectra$options$parallel <- parallel

  yamls_params$create_edges_spectra$files$spectral$raw <-
    file.path(paths_data_source, fil_spe_raw)
  yamls_params$create_edges_spectra$annotations$ms2$method <-
    isolate(data$ann_ms2_met)
  yamls_params$create_edges_spectra$annotations$ms2$thresholds$condition <-
    isolate(data$ann_ms2_met)
  yamls_params$create_edges_spectra$annotations$ms2$thresholds$peaks$absolute <-
    isolate(data$ann_ms2_thr_pea_abs)
  yamls_params$create_edges_spectra$annotations$ms2$thresholds$peaks$ratio <-
    isolate(data$ann_ms2_thr_pea_rat)
  yamls_params$create_edges_spectra$annotations$ms2$thresholds$similarity <-
    isolate(data$ann_ms2_thr_sim)
  yamls_params$create_edges_spectra$ms$intensity$thresholds$ms2 <-
    isolate(data$ms_int_thr_ms2)
  yamls_params$create_edges_spectra$ms$polarity <- ms_mode
  yamls_params$create_edges_spectra$ms$tolerances$mass$ppm$ms2 <-
    isolate(data$ms_tol_mas_ppm_ms2)
  yamls_params$create_edges_spectra$ms$tolerances$mass$dalton$ms2 <-
    isolate(data$ms_tol_mas_dal_ms2)
  yamls_params$create_edges_spectra$ms$tolerances$rt$minutes <-
    ms_tol_rt_min
  yamls_params$create_edges_spectra$names$source <- names_source
  yamls_params$create_edges_spectra$names$target <- names_target
  yamls_params$create_edges_spectra$options$fast <- fast
  yamls_params$create_edges_spectra$options$parallel <- parallel

  yamls_params$prepare_features_edges$names$source <- names_source
  yamls_params$prepare_features_edges$names$target <- names_target

  yamls_params$prepare_features_tables$files$features$raw <-
    file.path(paths_data_source, fil_fea_raw)
  yamls_params$prepare_features_tables$names$features <-
    isolate(data$names_features)
  yamls_params$prepare_features_tables$names$precursor <-
    isolate(data$names_precursor)
  yamls_params$prepare_features_tables$names$rt <-
    isolate(data$names_rt)

  yamls_params$prepare_libraries_sop_merged$organisms$filter$mode <-
    isolate(data$org_fil_mod)
  yamls_params$prepare_libraries_sop_merged$organisms$filter$level <-
    isolate(data$org_fil_lev)
  yamls_params$prepare_libraries_sop_merged$organisms$filter$value <-
    isolate(data$org_fil_val)

  yamls_params$prepare_libraries_spectra$ms$polarity <- ms_mode

  yamls_params$prepare_taxa$files$features$raw <-
    file.path(paths_data_source, fil_fea_raw)
  yamls_params$prepare_taxa$files$taxa$raw <-
    file.path(paths_data_source, fil_tax_raw)
  yamls_params$prepare_taxa$names$extension <-
    isolate(data$names_extension)
  yamls_params$prepare_taxa$names$taxon <- isolate(data$names_taxon)
  yamls_params$prepare_taxa$organisms$candidates <-
    isolate(data$org_can)
  yamls_params$prepare_taxa$organisms$taxon <- org_tax

  yamls_params$weight_annotations$annotations$candidates$initial <-
    isolate(data$ann_can_ini)
  yamls_params$weight_annotations$annotations$candidates$final <-
    isolate(data$ann_can_fin)
  yamls_params$weight_annotations$annotations$ms1only <-
    isolate(data$ann_ms1only)
  yamls_params$weight_annotations$annotations$ms1$thresholds$biological <-
    isolate(data$ann_ms1_thr_bio)
  yamls_params$weight_annotations$annotations$ms1$thresholds$chemical <-
    isolate(data$ann_ms1_thr_che)
  yamls_params$weight_annotations$annotations$ms1$thresholds$condition <-
    isolate(data$ann_ms1_thr_con)
  yamls_params$weight_annotations$weights$global$biological <-
    isolate(data$wei_glo_bio)
  yamls_params$weight_annotations$weights$global$chemical <-
    isolate(data$wei_glo_che)
  yamls_params$weight_annotations$weights$global$spectral <-
    isolate(data$wei_glo_spe)
  yamls_params$weight_annotations$weights$biological$domain <-
    isolate(data$wei_bio_01)
  yamls_params$weight_annotations$weights$biological$kingdom <-
    isolate(data$wei_bio_02)
  yamls_params$weight_annotations$weights$biological$phylum <-
    isolate(data$wei_bio_03)
  yamls_params$weight_annotations$weights$biological$class <-
    isolate(data$wei_bio_04)
  yamls_params$weight_annotations$weights$biological$order <-
    isolate(data$wei_bio_05)
  yamls_params$weight_annotations$weights$biological$infraorder <-
    isolate(data$wei_bio_06)
  yamls_params$weight_annotations$weights$biological$family <-
    isolate(data$wei_bio_07)
  yamls_params$weight_annotations$weights$biological$subfamily <-
    isolate(data$wei_bio_08)
  yamls_params$weight_annotations$weights$biological$tribe <-
    isolate(data$wei_bio_09)
  yamls_params$weight_annotations$weights$biological$subtribe <-
    isolate(data$wei_bio_10)
  yamls_params$weight_annotations$weights$biological$genus <-
    isolate(data$wei_bio_11)
  yamls_params$weight_annotations$weights$biological$subgenus <-
    isolate(data$wei_bio_12)
  yamls_params$weight_annotations$weights$biological$species <-
    isolate(data$wei_bio_13)
  yamls_params$weight_annotations$weights$biological$subspecies <-
    isolate(data$wei_bio_14)
  yamls_params$weight_annotations$weights$biological$variety <-
    isolate(data$wei_bio_15)
  yamls_params$weight_annotations$weights$chemical$cla$kingdom <-
    isolate(data$wei_che_11)
  yamls_params$weight_annotations$weights$chemical$cla$superclass <-
    isolate(data$wei_che_12)
  yamls_params$weight_annotations$weights$chemical$cla$superclass <-
    isolate(data$wei_che_13)
  yamls_params$weight_annotations$weights$chemical$cla$parent <-
    isolate(data$wei_che_14)
  yamls_params$weight_annotations$weights$chemical$npc$pathway <-
    isolate(data$wei_che_11)
  yamls_params$weight_annotations$weights$chemical$npc$superclass <-
    isolate(data$wei_che_12)
  yamls_params$weight_annotations$weights$chemical$npc$class <-
    isolate(data$wei_che_13)
  yamls_params$weight_annotations$options$force <- forceps
  yamls_params$weight_annotations$options$summarise <-
    isolate(data$summarise)

  # Change 3
  log_debug(x = "Changing filenames")

  # yamls_params$annotate_masses$files$annotations$prepared <-
  #   yamls_params$annotate_masses$files$annotations$prepared |>
  #   lapply(FUN = replace_id)
  # yamls_params$annotate_masses$files$features$prepared <-
  #   yamls_params$annotate_masses$files$features$prepared |>
  #   lapply(FUN = replace_id)
  # yamls_params$annotate_masses$files$networks$spectral$edges$raw <-
  #   yamls_params$annotate_masses$files$networks$spectral$edges$raw |>
  #   lapply(FUN = replace_id)
  #
  # yamls_params$annotate_spectra$files$annotations$raw$spectral <-
  #   yamls_params$annotate_spectra$files$annotations$raw$spectral |>
  #   lapply(FUN = replace_id)
  # # yamls_params$annotate_spectra$files$spectral$raw <-
  # #   yamls_params$annotate_spectra$files$spectral$raw |>
  # #   lapply(FUN = replace_id)
  #
  # yamls_params$create_edges_spectra$files$networks$spectral$edges$raw <-
  #   yamls_params$create_edges_spectra$files$networks$spectral$edges$raw |>
  #   lapply(FUN = replace_id)
  # # yamls_params$create_edges_spectra$files$spectral$raw <-
  # #   yamls_params$create_edges_spectra$files$spectral$raw |>
  # #   lapply(FUN = replace_id)
  #
  # yamls_params$create_components$files$networks$spectral$edges$prepared <-
  #   yamls_params$create_components$files$networks$spectral$edges$prepared |>
  #   lapply(FUN = replace_id)
  # yamls_params$create_components$files$networks$spectral$components$raw <-
  #   yamls_params$create_components$files$networks$spectral$components$raw |>
  #   lapply(FUN = replace_id)
  #
  # # yamls_params$prepare_features_tables$files$features$raw <-
  # #   yamls_params$prepare_features_tables$files$features$raw |>
  # #   lapply(FUN = replace_id)
  # yamls_params$prepare_features_tables$files$features$prepared <-
  #   yamls_params$prepare_features_tables$files$features$prepared |>
  #   lapply(FUN = replace_id)
  #
  # yamls_params$prepare_features_components$files$networks$spectral$components$raw <-
  #   yamls_params$prepare_features_components$files$networks$spectral$components$raw |>
  #   lapply(FUN = replace_id)
  # yamls_params$prepare_features_components$files$networks$spectral$components$prepared <-
  #   yamls_params$prepare_features_components$files$networks$spectral$components$prepared |>
  #   lapply(FUN = replace_id)
  #
  # yamls_params$prepare_features_edges$files$networks$spectral$edges$raw <-
  #   yamls_params$prepare_features_edges$files$networks$spectral$edges$raw |>
  #   lapply(FUN = replace_id)
  # yamls_params$prepare_features_edges$files$networks$spectral$edges$prepared <-
  #   yamls_params$prepare_features_edges$files$networks$spectral$edges$prepared |>
  #   lapply(FUN = replace_id)
  #
  # yamls_params$prepare_annotations_gnps$files$annotations$raw$spectral <-
  #   yamls_params$prepare_annotations_gnps$files$annotations$raw$spectral |>
  #   lapply(FUN = replace_id)
  # yamls_params$prepare_annotations_gnps$files$annotations$prepared <-
  #   yamls_params$prepare_annotations_gnps$files$annotations$prepared |>
  #   lapply(FUN = replace_id)
  #
  # yamls_params$prepare_annotations_sirius$files$annotations$raw$sirius <-
  #   yamls_params$prepare_annotations_sirius$files$annotations$raw$sirius |>
  #   lapply(FUN = replace_id)
  # yamls_params$prepare_annotations_sirius$files$annotations$prepared <-
  #   yamls_params$prepare_annotations_sirius$files$annotations$prepared |>
  #   lapply(FUN = replace_id)
  #
  # yamls_params$prepare_annotations_spectra$files$annotations$raw$spectral <-
  #   yamls_params$prepare_annotations_spectra$files$annotations$raw$spectral |>
  #   lapply(FUN = replace_id)
  # yamls_params$prepare_annotations_spectra$files$annotations$prepared <-
  #   yamls_params$prepare_annotations_spectra$files$annotations$prepared |>
  #   lapply(FUN = replace_id)
  #
  # # yamls_params$prepare_taxa$files$features$raw <-
  # #   yamls_params$prepare_taxa$files$features$raw |>
  # #   lapply(FUN = replace_id)
  # # yamls_params$prepare_taxa$files$taxa$raw <-
  # #   yamls_params$prepare_taxa$files$taxa$raw |>
  # #   lapply(FUN = replace_id)
  # yamls_params$prepare_taxa$files$taxa$prepared <-
  #   yamls_params$prepare_taxa$files$taxa$prepared |>
  #   lapply(FUN = replace_id)
  #
  # yamls_params$weight_annotations$files$annotations$prepared <-
  #   yamls_params$weight_annotations$files$annotations$prepared |>
  #   lapply(FUN = replace_id)
  # yamls_params$weight_annotations$files$annotations$processed <-
  #   yamls_params$weight_annotations$files$annotations$processed |>
  #   lapply(FUN = replace_id)
  # yamls_params$weight_annotations$files$features$prepared <-
  #   yamls_params$weight_annotations$files$features$prepared |>
  #   lapply(FUN = replace_id)
  # yamls_params$weight_annotations$files$networks$spectral$components$prepared <-
  #   yamls_params$weight_annotations$files$networks$spectral$components$prepared |>
  #   lapply(FUN = replace_id)
  # yamls_params$weight_annotations$files$networks$spectral$edges$prepared <-
  #   yamls_params$weight_annotations$files$networks$spectral$edges$prepared |>
  #   lapply(FUN = replace_id)
  # yamls_params$weight_annotations$files$taxa$prepared <-
  #   yamls_params$weight_annotations$files$taxa$prepared |>
  #   lapply(FUN = replace_id)

  yaml_export <- yaml_files |>
    gsub(pattern = "default", replacement = "user")
  names(yaml_export) <- yaml_names

  create_dir(export = yaml_export[[1]])

  lapply(
    X = seq_along(yamls_params),
    FUN = function(x) {
      yaml::write_yaml(
        x = yamls_params[[x]],
        file = yaml_export[x]
      )
    }
  )
  setwd("inst/app")
}
