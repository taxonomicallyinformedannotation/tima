# save the results to a file
save_input <- function(input) {
  setwd("../../")

  paths <- timaR::parse_yaml_paths()
  paths_data_source <- paths$data$source$path
  timaR::log_debug(x = "Loading default params")
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
  fil_fea_raw <<- isolate(input$fil_fea_raw)[[1]]
  fil_spe_raw <<- isolate(input$fil_spe_raw)[[1]]
  fil_tax_raw <<- isolate(input$fil_tax_raw)[[1]]

  filename <<- isolate(input$fil_pat)
  gnps_job_id <<- isolate(input$gnps_id)
  if (gnps_job_id == "") {
    gnps_job_id <<- NULL
  }
  gnps_workflow <<- isolate(input$gnps_workflow)
  org_tax <<- isolate(input$org_tax)
  if (org_tax == "") {
    org_tax <<- NULL
  }
  ms_mode <<- isolate(input$ms_pol)
  ms_tol_rt_min <<- isolate(input$ms_tol_rt_min)
  names_source <<- isolate(input$names_source)
  names_target <<- isolate(input$names_target)
  fast <<- isolate(input$fast)
  forceps <<- isolate(input$force)
  parallel <<- isolate(input$parallel)
  summarise <<- isolate(input$summarise)

  ## Change 1
  timaR::log_debug(x = "Changing parameters")
  yamls_params$`params/prepare_params`$files$pattern <- filename
  yamls_params$`params/prepare_params`$files$features$raw <-
    file.path(paths_data_source, fil_fea_raw)
  yamls_params$`params/prepare_params`$files$spectral$raw <-
    file.path(paths_data_source, fil_spe_raw)
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
    isolate(input$ms_add_neg)
  yamls_params$annotate_masses$ms$adducts$pos <-
    isolate(input$ms_add_pos)
  yamls_params$annotate_masses$ms$intensity$thresholds$ms1 <-
    isolate(input$ms_int_thr_ms1)
  yamls_params$annotate_masses$ms$polarity <- ms_mode
  yamls_params$annotate_masses$ms$tolerances$mass$ppm$ms1 <-
    isolate(input$ms_tol_mas_ppm_ms1)
  yamls_params$annotate_masses$ms$tolerances$mass$dalton$ms1 <-
    isolate(input$ms_tol_mas_dal_ms1)
  yamls_params$annotate_masses$ms$tolerances$rt$minutes <-
    ms_tol_rt_min
  yamls_params$annotate_masses$names$source <- names_source
  yamls_params$annotate_masses$names$target <- names_target
  yamls_params$annotate_masses$options$force <- forceps

  yamls_params$annotate_spectra$files$spectral$raw <-
    file.path(paths_data_source, fil_spe_raw)
  yamls_params$annotate_spectra$annotations$ms2$approx <-
    isolate(input$ann_ms2_app)
  yamls_params$annotate_spectra$annotations$ms2$method <-
    isolate(input$ann_ms2_met)
  yamls_params$annotate_spectra$annotations$ms2$thresholds$condition <-
    isolate(input$ann_ms2_thr_con)
  yamls_params$annotate_spectra$annotations$ms2$thresholds$peaks$absolute <-
    isolate(input$ann_ms2_thr_pea_abs)
  yamls_params$annotate_spectra$annotations$ms2$thresholds$peaks$ratio <-
    isolate(input$ann_ms2_thr_pea_rat)
  yamls_params$annotate_spectra$annotations$ms2$thresholds$similarity <-
    isolate(input$ann_ms2_thr_sim)
  yamls_params$annotate_spectra$ms$intensity$thresholds$ms2 <-
    isolate(input$ms_int_thr_ms2)
  yamls_params$annotate_spectra$ms$polarity <- ms_mode
  yamls_params$annotate_spectra$ms$tolerances$mass$ppm$ms2 <-
    isolate(input$ms_tol_mas_ppm_ms2)
  yamls_params$annotate_spectra$ms$tolerances$mass$dalton$ms2 <-
    isolate(input$ms_tol_mas_dal_ms2)
  yamls_params$annotate_spectra$ms$tolerances$rt$minutes <-
    ms_tol_rt_min
  yamls_params$annotate_spectra$options$fast <- fast
  yamls_params$annotate_spectra$options$parallel <- parallel

  yamls_params$create_edges_spectra$files$spectral$raw <-
    file.path(paths_data_source, fil_spe_raw)
  yamls_params$create_edges_spectra$annotations$ms2$method <-
    isolate(input$ann_ms2_met)
  yamls_params$create_edges_spectra$annotations$ms2$thresholds$condition <-
    isolate(input$ann_ms2_met)
  yamls_params$create_edges_spectra$annotations$ms2$thresholds$peaks$absolute <-
    isolate(input$ann_ms2_thr_pea_abs)
  yamls_params$create_edges_spectra$annotations$ms2$thresholds$peaks$ratio <-
    isolate(input$ann_ms2_thr_pea_rat)
  yamls_params$create_edges_spectra$annotations$ms2$thresholds$similarity <-
    isolate(input$ann_ms2_thr_sim)
  yamls_params$create_edges_spectra$ms$intensity$thresholds$ms2 <-
    isolate(input$ms_int_thr_ms2)
  yamls_params$create_edges_spectra$ms$polarity <- ms_mode
  yamls_params$create_edges_spectra$ms$tolerances$mass$ppm$ms2 <-
    isolate(input$ms_tol_mas_ppm_ms2)
  yamls_params$create_edges_spectra$ms$tolerances$mass$dalton$ms2 <-
    isolate(input$ms_tol_mas_dal_ms2)
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
    isolate(input$names_features)
  yamls_params$prepare_features_tables$names$precursor <-
    isolate(input$names_precursor)
  yamls_params$prepare_features_tables$names$rt <-
    isolate(input$names_rt)

  yamls_params$prepare_libraries_sop_merged$organisms$filter$mode <-
    isolate(input$org_fil_mod)
  yamls_params$prepare_libraries_sop_merged$organisms$filter$level <-
    isolate(input$org_fil_lev)
  yamls_params$prepare_libraries_sop_merged$organisms$filter$value <-
    isolate(input$org_fil_val)

  yamls_params$prepare_libraries_spectra$ms$polarity <- ms_mode

  yamls_params$prepare_taxa$files$features$raw <-
    file.path(paths_data_source, fil_fea_raw)
  yamls_params$prepare_taxa$files$taxa$raw <-
    file.path(paths_data_source, fil_tax_raw)
  yamls_params$prepare_taxa$names$extension <-
    isolate(input$names_extension)
  yamls_params$prepare_taxa$names$features <-
    isolate(input$names_features)
  yamls_params$prepare_taxa$names$taxon <- isolate(input$names_taxon)
  yamls_params$prepare_taxa$organisms$candidates <-
    isolate(input$org_can)
  yamls_params$prepare_taxa$organisms$taxon <- org_tax

  yamls_params$weight_annotations$annotations$candidates$initial <-
    isolate(input$ann_can_ini)
  yamls_params$weight_annotations$annotations$candidates$final <-
    isolate(input$ann_can_fin)
  yamls_params$weight_annotations$annotations$ms1only <-
    isolate(input$ann_ms1only)
  yamls_params$weight_annotations$annotations$ms1$thresholds$biological <-
    isolate(input$ann_ms1_thr_bio)
  yamls_params$weight_annotations$annotations$ms1$thresholds$chemical <-
    isolate(input$ann_ms1_thr_che)
  yamls_params$weight_annotations$annotations$ms1$thresholds$condition <-
    isolate(input$ann_ms1_thr_con)
  yamls_params$weight_annotations$weights$global$biological <-
    isolate(input$wei_glo_bio)
  yamls_params$weight_annotations$weights$global$chemical <-
    isolate(input$wei_glo_che)
  yamls_params$weight_annotations$weights$global$spectral <-
    isolate(input$wei_glo_spe)
  yamls_params$weight_annotations$weights$biological$domain <-
    isolate(input$wei_bio_01)
  yamls_params$weight_annotations$weights$biological$kingdom <-
    isolate(input$wei_bio_02)
  yamls_params$weight_annotations$weights$biological$phylum <-
    isolate(input$wei_bio_03)
  yamls_params$weight_annotations$weights$biological$class <-
    isolate(input$wei_bio_04)
  yamls_params$weight_annotations$weights$biological$order <-
    isolate(input$wei_bio_05)
  yamls_params$weight_annotations$weights$biological$infraorder <-
    isolate(input$wei_bio_06)
  yamls_params$weight_annotations$weights$biological$family <-
    isolate(input$wei_bio_07)
  yamls_params$weight_annotations$weights$biological$subfamily <-
    isolate(input$wei_bio_08)
  yamls_params$weight_annotations$weights$biological$tribe <-
    isolate(input$wei_bio_09)
  yamls_params$weight_annotations$weights$biological$subtribe <-
    isolate(input$wei_bio_10)
  yamls_params$weight_annotations$weights$biological$genus <-
    isolate(input$wei_bio_11)
  yamls_params$weight_annotations$weights$biological$subgenus <-
    isolate(input$wei_bio_12)
  yamls_params$weight_annotations$weights$biological$species <-
    isolate(input$wei_bio_13)
  yamls_params$weight_annotations$weights$biological$subspecies <-
    isolate(input$wei_bio_14)
  yamls_params$weight_annotations$weights$biological$variety <-
    isolate(input$wei_bio_15)
  yamls_params$weight_annotations$weights$chemical$cla$kingdom <-
    isolate(input$wei_che_11)
  yamls_params$weight_annotations$weights$chemical$cla$superclass <-
    isolate(input$wei_che_12)
  yamls_params$weight_annotations$weights$chemical$cla$superclass <-
    isolate(input$wei_che_13)
  yamls_params$weight_annotations$weights$chemical$cla$parent <-
    isolate(input$wei_che_14)
  yamls_params$weight_annotations$weights$chemical$npc$pathway <-
    isolate(input$wei_che_11)
  yamls_params$weight_annotations$weights$chemical$npc$superclass <-
    isolate(input$wei_che_12)
  yamls_params$weight_annotations$weights$chemical$npc$class <-
    isolate(input$wei_che_13)
  yamls_params$weight_annotations$options$force <- forceps
  yamls_params$weight_annotations$options$summarise <- summarise
  yamls_params$weight_annotations$files$pattern <- filename

  # Change 3
  timaR::log_debug(x = "Changing filenames")

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

  timaR::create_dir(export = yaml_export[[1]])

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
