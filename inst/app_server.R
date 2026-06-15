# save the results to a file
.get_default_paths <- getFromNamespace("get_default_paths", "tima")
.create_dir <- getFromNamespace("create_dir", "tima")
.load_yaml_files <- getFromNamespace("load_yaml_files", "tima")
.plan_mztab_app_outputs <- getFromNamespace("plan_mztab_app_outputs", "tima")
.log_trace <- getFromNamespace("log_trace", "tima")
.replace_id <- getFromNamespace("replace_id", "tima")

.save_input <- function(input) {
  paths_data_source <- .get_default_paths()$data$source$path
  paths_data_interim_annotations <-
    .get_default_paths()$data$interim$annotations$path
  ## safety
  .create_dir(paths_data_source)

  list <- .load_yaml_files()

  yamls_params <- list$yamls_params

  ## This allows to keep files correctly placed in `data/source` clean
  prefil_fea_raw <- input$fil_fea_raw
  prefil_spe_raw <- input$fil_spe_raw
  prefil_met_raw <- input$fil_met_raw
  prefil_mzm_raw <- input$fil_ann_raw_mzm
  prefil_mzt_raw <- input$fil_mzt_raw
  prefil_sir_raw <- input$fil_ann_raw_sir
  lib_tmp_exp_csv <- input$lib_tmp_exp_csv
  lib_tmp_is_csv <- input$lib_tmp_is_csv

  prefil_fea_raw_1 <- NULL
  prefil_spe_raw_1 <- NULL
  prefil_met_raw_1 <- NULL
  prefil_mzm_raw_1 <- NULL
  prefil_mzt_raw_1 <- NULL
  prefil_sir_raw_1 <- NULL

  if (!is.null(prefil_fea_raw)) {
    prefil_fea_raw_1 <- file.path(paths_data_source, prefil_fea_raw[[1L]])
  }
  if (!is.null(prefil_spe_raw)) {
    prefil_spe_raw_1 <- file.path(paths_data_source, prefil_spe_raw[[1L]])
  }
  if (!is.null(prefil_met_raw)) {
    prefil_met_raw_1 <- file.path(paths_data_source, prefil_met_raw[[1L]])
  }
  if (!is.null(prefil_mzm_raw)) {
    prefil_mzm_raw_1 <- file.path(
      paths_data_interim_annotations,
      prefil_mzm_raw[[1L]]
    )
  }
  if (!is.null(prefil_mzt_raw)) {
    prefil_mzt_raw_1 <- file.path(paths_data_source, prefil_mzt_raw[[1L]])
  }
  if (!is.null(prefil_sir_raw)) {
    prefil_sir_raw_1 <-
      file.path(paths_data_interim_annotations, prefil_sir_raw[[1L]])
  }

  if (!is.null(prefil_fea_raw) && !file.exists(prefil_fea_raw_1)) {
    fs::file_copy(
      path = prefil_fea_raw[[4L]],
      new_path = file.path(prefil_fea_raw_1),
      overwrite = TRUE
    )
  }
  if (!is.null(prefil_spe_raw) && !file.exists(prefil_spe_raw_1)) {
    fs::file_copy(
      path = prefil_spe_raw[[4L]],
      new_path = file.path(prefil_spe_raw_1),
      overwrite = TRUE
    )
  }
  if (!is.null(prefil_met_raw)) {
    if (!file.exists(prefil_met_raw_1)) {
      fs::file_copy(
        path = prefil_met_raw[[4L]],
        new_path = file.path(prefil_met_raw_1),
        overwrite = TRUE
      )
    }
  } else {
    prefil_met_raw_1 <- NULL
  }
  if (!is.null(prefil_mzm_raw)) {
    if (!file.exists(prefil_mzm_raw_1)) {
      ## safety
      .create_dir(paths_data_interim_annotations)
      fs::file_copy(
        path = prefil_mzm_raw[[4L]],
        new_path = file.path(prefil_mzm_raw_1),
        overwrite = TRUE
      )
    }
  } else {
    prefil_mzm_raw_1 <- NULL
  }
  if (!is.null(prefil_mzt_raw)) {
    if (!file.exists(prefil_mzt_raw_1)) {
      ## safety
      .create_dir(paths_data_source)
      fs::file_copy(
        path = prefil_mzt_raw[[4L]],
        new_path = file.path(prefil_mzt_raw_1),
        overwrite = TRUE
      )
    }
  }
  if (!is.null(prefil_sir_raw)) {
    if (!file.exists(prefil_sir_raw_1)) {
      ## safety
      .create_dir(paths_data_interim_annotations)
      fs::file_copy(
        path = prefil_sir_raw[[4L]],
        new_path = file.path(prefil_sir_raw_1),
        overwrite = TRUE
      )
    }
  } else {
    prefil_sir_raw_1 <- NULL
  }

  # If mzTab is provided, derive missing core inputs so launch is possible.
  if (
    !is.null(prefil_mzt_raw_1) &&
      (is.null(prefil_fea_raw_1) ||
        is.null(prefil_spe_raw_1) ||
        is.null(prefil_met_raw_1))
  ) {
    mztab_plan <- .plan_mztab_app_outputs(
      mztab_path = prefil_mzt_raw_1,
      data_source_path = paths_data_source,
      features_path = prefil_fea_raw_1,
      spectra_path = prefil_spe_raw_1,
      metadata_path = prefil_met_raw_1
    )

    prefil_fea_raw_1 <- mztab_plan$resolved$features
    prefil_spe_raw_1 <- mztab_plan$resolved$spectra
    prefil_met_raw_1 <- mztab_plan$resolved$metadata

    if (mztab_plan$needs_read) {
      .log_trace("Rebuilding missing mzTab-derived source files ...")
      mztab_outputs <- tima::read_mztab(
        input = prefil_mzt_raw_1,
        output_features = mztab_plan$write$features,
        output_spectra = mztab_plan$write$spectra,
        output_metadata = mztab_plan$write$metadata
      )

      if (is.null(prefil_fea_raw_1)) {
        prefil_fea_raw_1 <- mztab_outputs$features
      }
      if (is.null(prefil_spe_raw_1)) {
        prefil_spe_raw_1 <- mztab_outputs$spectra
      }
      if (is.null(prefil_met_raw_1)) {
        prefil_met_raw_1 <- mztab_outputs$metadata
      }
    } else {
      .log_trace("Reusing cached mzTab-derived source files")
    }
  }
  if (!is.null(lib_tmp_exp_csv)) {
    if (!file.exists(lib_tmp_exp_csv)) {
      fs::file_copy(
        path = lib_tmp_exp_csv[[4L]],
        new_path = file.path(lib_tmp_exp_csv),
        overwrite = TRUE
      )
    }
  } else {
    lib_tmp_exp_csv <- NULL
  }
  if (!is.null(lib_tmp_is_csv)) {
    if (!file.exists(lib_tmp_is_csv)) {
      fs::file_copy(
        path = lib_tmp_is_csv[[4L]],
        new_path = file.path(lib_tmp_is_csv),
        overwrite = TRUE
      )
    }
  } else {
    lib_tmp_is_csv <- NULL
  }

  fil_fea_raw <- prefil_fea_raw_1
  fil_spe_raw <- prefil_spe_raw_1
  fil_met_raw <- prefil_met_raw_1
  fil_mzm_raw <- prefil_mzm_raw_1
  fil_mzt_raw <- prefil_mzt_raw_1
  fil_sir_raw <- prefil_sir_raw_1

  fil_pat <- input$fil_pat

  gnps_job_id <- input$gnps_id
  if (gnps_job_id == "") {
    gnps_job_id <- NULL
  }

  org_tax <- input$org_tax
  if (org_tax == "") {
    org_tax <- NULL
  }
  hig_evi <- input$high_evidence
  ms_pol <- input$ms_pol
  summarize <- input$summarize

  .log_trace("Changing parameters ...")
  .log_trace("... Small")
  yaml_small <- yamls_params[["prepare_params"]]
  yaml_small$files$pattern <- fil_pat
  yaml_small$files$features$raw <- fil_fea_raw
  yaml_small$files$metadata$raw <- fil_met_raw
  yaml_small$files$annotations$raw$mzmine <- fil_mzm_raw
  yaml_small$files$mztab$raw <- fil_mzt_raw
  yaml_small$files$annotations$raw$sirius <- fil_sir_raw
  yaml_small$files$spectral$raw <- fil_spe_raw
  yaml_small$ms$polarity <- ms_pol
  yaml_small$organisms$taxon <- org_tax
  yaml_small$options$high_evidence <- hig_evi
  yaml_small$options$summarize <- summarize
  .create_dir("params")
  yaml::write_yaml(
    x = yaml_small,
    file = .get_default_paths()$params$prepare_params
  )

  .log_trace("... Advanced")
  yaml_advanced <- yamls_params[["prepare_params_advanced"]]
  yaml_advanced$annotations$candidates$final <-
    input$ann_can_fin
  yaml_advanced$annotations$candidates$neighbors <-
    input$ann_can_nei
  yaml_advanced$annotations$candidates$samples <-
    input$ann_can_sam
  yaml_advanced$annotations$candidates$best_percentile <-
    input$ann_can_bes
  yaml_advanced$annotations$ms1only <-
    input$ann_ms1only
  yaml_advanced$annotations$ms2approx <-
    input$ann_ms2_app
  yaml_advanced$annotations$thresholds$consistency <-
    input$ann_thr_con
  yaml_advanced$annotations$thresholds$ms1$biological <-
    input$ann_thr_ms1_bio
  yaml_advanced$annotations$thresholds$ms1$chemical <-
    input$ann_thr_ms1_che
  yaml_advanced$annotations$thresholds$ms1$condition <-
    input$ann_thr_ms1_con
  yaml_advanced$files$pattern <- fil_pat
  yaml_advanced$files$annotations$raw$spectral$gnps <-
    yaml_advanced$files$annotations$raw$spectral$gnps |>
    .replace_id()
  yaml_advanced$files$annotations$raw$mzmine <- fil_mzm_raw
  yaml_advanced$files$annotations$raw$spectral$spectral <-
    yaml_advanced$files$annotations$raw$spectral$spectral |>
    .replace_id()
  yaml_advanced$files$mztab$raw <- fil_mzt_raw
  # yaml_advanced$files$annotations$raw$sirius <-
  #   yaml_advanced$files$annotations$raw$sirius |>
  #   .replace_id()
  yaml_advanced$files$annotations$raw$sirius <- fil_sir_raw
  yaml_advanced$files$annotations$prepared$canopus <-
    yaml_advanced$files$annotations$prepared$canopus |>
    .replace_id()
  yaml_advanced$files$annotations$prepared$formula <-
    yaml_advanced$files$annotations$prepared$formula |>
    .replace_id()
  yaml_advanced$files$annotations$prepared$structural$gnps <-
    yaml_advanced$files$annotations$prepared$structural$gnps |>
    .replace_id()
  yaml_advanced$files$annotations$prepared$structural$ms1 <-
    yaml_advanced$files$annotations$prepared$structural$ms1 |>
    .replace_id()
  yaml_advanced$files$annotations$prepared$structural$sirius <-
    yaml_advanced$files$annotations$prepared$structural$sirius |>
    .replace_id()
  yaml_advanced$files$annotations$prepared$structural$spectral <-
    yaml_advanced$files$annotations$prepared$structural$spectral |>
    .replace_id()
  yaml_advanced$files$annotations$filtered <-
    yaml_advanced$files$annotations$filtered |>
    .replace_id()
  # yaml_advanced$files$annotations$processed <-
  #   yaml_advanced$files$annotations$processed |>
  #   .replace_id()
  yaml_advanced$files$features$raw <-
    fil_fea_raw
  yaml_advanced$files$features$prepared <-
    yaml_advanced$files$features$prepared |>
    .replace_id()
  # TODO
  # yaml_advanced$files$libraries$sop$raw$closed <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$raw$ecmdb <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$raw$hmdb <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$raw$lotus <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$prepared$closed <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$prepared$ecmdb <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$prepared$hmdb <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$prepared$lotus <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$prepared$rt <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$prepared$spectral <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$merged$keys <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$merged$organisms$names <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$merged$organisms$taxonomies$ott <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$merged$structures$processed <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$merged$structures$stereo <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$merged$structures$metadata <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$merged$structures$names <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$merged$structures$taxonomies$cla <-
  #   input$todo
  # yaml_advanced$files$libraries$sop$merged$structures$taxonomies$npc <-
  #   input$todo
  yaml_advanced$files$libraries$spectral$raw <-
    input$lib_spe_mgf
  yaml_advanced$files$libraries$temporal$exp$csv <- lib_tmp_exp_csv
  yaml_advanced$files$libraries$temporal$is$csv <- lib_tmp_is_csv
  # TODO
  # other relative paths, not necessary
  # TODO
  # yaml_advanced$files$libraries$temporal$prepared <-
  #   input$todo
  yaml_advanced$files$networks$spectral$edges$raw$ms1 <-
    yaml_advanced$files$networks$spectral$edges$raw$ms1 |>
    .replace_id()
  yaml_advanced$files$networks$spectral$edges$raw$spectral <-
    yaml_advanced$files$networks$spectral$edges$raw$spectral |>
    .replace_id()
  yaml_advanced$files$networks$spectral$edges$prepared <-
    yaml_advanced$files$networks$spectral$edges$prepared |>
    .replace_id()
  yaml_advanced$files$networks$spectral$components$raw <-
    yaml_advanced$files$networks$spectral$components$raw |>
    .replace_id()
  yaml_advanced$files$networks$spectral$components$prepared <-
    yaml_advanced$files$networks$spectral$components$prepared |>
    .replace_id()
  yaml_advanced$files$metadata$raw <- fil_met_raw
  yaml_advanced$files$metadata$prepared <-
    yaml_advanced$files$metadata$prepared |>
    .replace_id()
  yaml_advanced$files$spectral$raw <- fil_spe_raw
  yaml_advanced$gnps$id <- gnps_job_id
  yaml_advanced$gnps$workflow <-
    input$gnps_workflow
  yaml_advanced$ms$adducts$neg <-
    input$ms_add_neg
  yaml_advanced$ms$adducts$pos <-
    input$ms_add_pos
  yaml_advanced$ms$adducts$consistency$type <-
    input$ms_add_con_type
  yaml_advanced$ms$adducts$consistency$min_degree <-
    input$ms_add_con_min_degree
  yaml_advanced$ms$adducts$consistency$min_support <-
    input$ms_add_con_min_support
  yaml_advanced$ms$clusters$neg <-
    input$ms_clu_neg
  yaml_advanced$ms$clusters$pos <-
    input$ms_clu_pos
  yaml_advanced$ms$neutral_losses <-
    input$ms_neu
  yaml_advanced$ms$polarity <- ms_pol
  yaml_advanced$ms$thresholds$ms2$intensity <-
    input$ms_thr_ms2_int
  yaml_advanced$ms$thresholds$ms2$min_fragments <-
    input$ms_thr_ms2_min_fragments
  yaml_advanced$ms$tolerances$mass$ppm$ms1 <-
    input$ms_tol_mas_ppm_ms1
  yaml_advanced$ms$tolerances$mass$ppm$ms2 <-
    input$ms_tol_mas_ppm_ms2
  yaml_advanced$ms$tolerances$mass$dalton$ms1 <-
    input$ms_tol_mas_dal_ms1
  yaml_advanced$ms$tolerances$mass$dalton$ms2 <-
    input$ms_tol_mas_dal_ms2
  yaml_advanced$ms$tolerances$rt$adducts <-
    input$ms_tol_rt_add
  yaml_advanced$ms$tolerances$rt$library <-
    input$ms_tol_rt_lib
  yaml_advanced$names$adduct <-
    input$names_adduct
  yaml_advanced$names$compound_name <-
    input$names_compound_name
  yaml_advanced$names$extension <-
    input$names_extension
  yaml_advanced$names$features <-
    input$names_features
  yaml_advanced$names$filename <-
    input$names_filename
  yaml_advanced$names$inchikey <-
    input$names_inchikey
  yaml_advanced$names$libraries <-
    input$names_libraries
  yaml_advanced$names$mgf$adduct <-
    input$names_mgf_ad
  yaml_advanced$names$mgf$collision_energy <-
    input$names_mgf_ce
  yaml_advanced$names$mgf$compound_id <-
    input$names_mgf_ci
  yaml_advanced$names$mgf$inchi <-
    input$names_mgf_in
  yaml_advanced$names$mgf$inchi_no_stereo <-
    input$names_mgf_io
  yaml_advanced$names$mgf$inchikey <-
    input$names_mgf_ik
  yaml_advanced$names$mgf$inchikey_connectivity_layer <-
    input$names_mgf_il
  yaml_advanced$names$mgf$name <-
    input$names_mgf_na
  yaml_advanced$names$mgf$polarity <-
    input$names_mgf_po
  yaml_advanced$names$mgf$retention_time <-
    input$names_mgf_rt
  yaml_advanced$names$mgf$smiles <-
    input$names_mgf_sm
  yaml_advanced$names$mgf$smiles_no_stereo <-
    input$names_mgf_sn
  yaml_advanced$names$mgf$spectrum_id <-
    input$names_mgf_si
  yaml_advanced$names$mgf$splash <-
    input$names_mgf_sp
  yaml_advanced$names$mgf$synonyms <-
    input$names_mgf_sy
  yaml_advanced$names$precursor <-
    input$names_precursor
  yaml_advanced$names$rt$features <-
    input$names_rt
  yaml_advanced$names$rt$library <-
    input$names_rt_2
  yaml_advanced$names$smiles <-
    input$names_smiles
  yaml_advanced$names$source <-
    input$names_source
  yaml_advanced$names$target <-
    input$names_target
  yaml_advanced$names$taxon <-
    input$names_taxon
  yaml_advanced$organisms$filter$mode <-
    input$org_fil_mod
  yaml_advanced$organisms$filter$level <-
    input$org_fil_lev
  yaml_advanced$organisms$filter$value <-
    input$org_fil_val
  yaml_advanced$organisms$taxon <-
    org_tax
  yaml_advanced$similarities$methods$annotations <-
    input$sim_met_ann
  yaml_advanced$similarities$methods$edges <-
    input$sim_met_edg
  yaml_advanced$similarities$thresholds$annotations <-
    input$sim_thr_ann
  yaml_advanced$similarities$thresholds$edges <-
    input$sim_thr_edg
  yaml_advanced$similarities$thresholds$matched_peaks <-
    input$sim_thr_mat
  # TODO
  # yaml_advanced$tools$metadata <-
  #   input$too_x
  # yaml_advanced$tools$networks$spectral$components <-
  #   input$too_x
  # yaml_advanced$tools$networks$spectral$edges <-
  #   input$too_x
  yaml_advanced$tools$sirius$version <-
    input$too_sir_ver
  yaml_advanced$tools$sirius$max_analog_abs_mz_error <-
    input$too_sir_max_analog_abs_mz_error
  # TODO
  # yaml_advanced$tools$taxonomies$biological <-
  #   input$too_x
  # yaml_advanced$tools$taxonomies$chemical <-
  #   input$too_x
  yaml_advanced$units$rt <-
    input$uni_rt
  yaml_advanced$weights$global$biological <-
    input$wei_glo_bio
  yaml_advanced$weights$global$chemical <-
    input$wei_glo_che
  yaml_advanced$weights$global$spectral <-
    input$wei_glo_spe
  yaml_advanced$weights$biological$domain <-
    input$wei_bio_01
  yaml_advanced$weights$biological$kingdom <-
    input$wei_bio_02
  yaml_advanced$weights$biological$phylum <-
    input$wei_bio_03
  yaml_advanced$weights$biological$class <-
    input$wei_bio_04
  yaml_advanced$weights$biological$order <-
    input$wei_bio_05
  yaml_advanced$weights$biological$infraorder <-
    input$wei_bio_06
  yaml_advanced$weights$biological$family <-
    input$wei_bio_07
  yaml_advanced$weights$biological$subfamily <-
    input$wei_bio_08
  yaml_advanced$weights$biological$tribe <-
    input$wei_bio_09
  yaml_advanced$weights$biological$subtribe <-
    input$wei_bio_10
  yaml_advanced$weights$biological$genus <-
    input$wei_bio_11
  yaml_advanced$weights$biological$subgenus <-
    input$wei_bio_12
  yaml_advanced$weights$biological$species <-
    input$wei_bio_13
  yaml_advanced$weights$biological$subspecies <-
    input$wei_bio_14
  yaml_advanced$weights$biological$variety <-
    input$wei_bio_15
  yaml_advanced$weights$biological$biota <-
    input$wei_bio_16
  yaml_advanced$weights$chemical$cla$kingdom <-
    input$wei_che_11
  yaml_advanced$weights$chemical$npc$pathway <-
    input$wei_che_21
  yaml_advanced$weights$chemical$cla$superclass <-
    input$wei_che_12
  yaml_advanced$weights$chemical$npc$superclass <-
    input$wei_che_22
  yaml_advanced$weights$chemical$cla$class <-
    input$wei_che_13
  yaml_advanced$weights$chemical$npc$class <-
    input$wei_che_23
  yaml_advanced$weights$chemical$cla$parent <-
    input$wei_che_14
  yaml_advanced$options$compounds_names <-
    input$compounds_names
  yaml_advanced$options$high_evidence <- hig_evi
  yaml_advanced$options$force <-
    input$force
  yaml_advanced$options$remove_ties <-
    input$remove_ties
  yaml_advanced$options$summarize <- summarize
  if (!is.null(prefil_met_raw)) {
    yamls_params$prepare_taxa$files$metadata$raw <- fil_met_raw
  }
  if (!is.null(gnps_job_id)) {
    yamls_params$prepare_taxa$files$metadata$raw <-
      file.path(paths_data_source, paste0(gnps_job_id, "_metadata.tsv"))
  }
  yaml::write_yaml(
    x = yaml_advanced,
    file = .get_default_paths()$params$prepare_params_advanced
  )
}

server <- function(input, output) {
  ## Observe helpers
  shinyhelper::observe_helpers()

  output$demo_spe <- shiny::downloadHandler(
    filename = basename(.get_default_paths()$urls$examples$spectra),
    content = function(file) {
      writeLines(
        readLines(.get_default_paths()$urls$examples$spectra_mini),
        file
      )
    }
  )

  output$demo_fea <- shiny::downloadHandler(
    filename = basename(.get_default_paths()$urls$examples$features),
    content = function(file) {
      writeLines(
        readLines(.get_default_paths()$urls$examples$features),
        file
      )
    }
  )

  output$demo_met <- shiny::downloadHandler(
    filename = basename(.get_default_paths()$urls$examples$metadata),
    content = function(file) {
      writeLines(
        readLines(.get_default_paths()$urls$examples$metadata),
        file
      )
    }
  )

  ## Enable Save/Launch depending on input mode:
  ## - classic mode: features + spectra (+ metadata or taxon)
  ## - mzTab mode: mzTab alone is enough; other files are optional overlays
  shiny::observe(x = {
    .has_upload <- function(upload) {
      !is.null(upload) && length(upload[[1L]]) > 0L && nzchar(upload[[1L]])
    }

    has_mztab <- .has_upload(input[["fil_mzt_raw"]])
    has_features <- .has_upload(input[["fil_fea_raw"]])
    has_spectra <- .has_upload(input[["fil_spe_raw"]])
    has_metadata <- .has_upload(input[["fil_met_raw"]])
    has_taxon <- !is.null(input[["org_tax"]]) &&
      nzchar(trimws(input[["org_tax"]]))
    has_pattern <- !is.null(input[["fil_pat"]]) &&
      nzchar(trimws(input[["fil_pat"]]))

    classic_ready <- has_features && has_spectra && (has_metadata || has_taxon)
    all_conditions <- has_pattern && (has_mztab || classic_ready)

    shinyjs::toggleState(id = "save", condition = all_conditions)
    shinyjs::toggleState(id = "launch", condition = input$save >= 1)
  })

  ## Special check for taxon name
  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("org_tax", function(taxon) {
    if (is.null(taxon) || !nzchar(trimws(taxon))) {
      return(NULL)
    }
    if (
      !grepl(
        pattern = "^[[:upper:]]",
        x = taxon,
        perl = TRUE
      )
    ) {
      "Please provide your taxon name with capital letter"
    }
  })
  iv$add_rule("org_tax", function(taxon) {
    if (is.null(taxon) || !nzchar(trimws(taxon))) {
      return(NULL)
    }
    if (
      anyNA(
        stringi::stri_split_fixed(str = taxon, pattern = "|") |>
          purrr::map(
            .f = function(taxon) {
              rotl::tnrs_match_names(
                names = taxon,
                do_approximate_matching = FALSE
              )$ott_id
            }
          )
      )
    ) {
      "Taxon not found in Open Tree of Life"
    }
  })
  iv$enable()

  ## When the Save button is clicked, save the response
  shiny::observeEvent(eventExpr = input$save, handlerExpr = {
    ## User-experience stuff
    shinyjs::show("save_msg")
    shinyjs::enable("launch")
    shinyjs::hide("error")
    shinyjs::hide("job_msg")
    shinyjs::hide("job_end")
    shinyjs::hide("results_mini")
    shinyjs::hide("results_filtered")
    shinyjs::hide("results_full")
    shinyjs::hide("results_mztab")
    shinyjs::hide("close")

    ## Save the data (show an error message in case of error)
    tryCatch(
      expr = {
        .save_input(input = input)
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(
          id = "error",
          anim = TRUE,
          animType = "fade"
        )
      },
      finally = {
        shinyjs::enable("save")
        shinyjs::enable("launch")
        shinyjs::hide("save_msg")
        shinyjs::hide("error")
        shinyjs::hide("job_msg")
        shinyjs::hide("job_end")
        shinyjs::hide("results_mini")
        shinyjs::hide("results_filtered")
        shinyjs::hide("results_full")
        shinyjs::hide("results_mztab")
        shinyjs::hide("close")
      }
    )
  })

  shiny::observeEvent(eventExpr = input$launch, handlerExpr = {
    shinyjs::show("job_msg")
    shinyjs::hide("thankyou_msg")
    shinyjs::hide("error")
    shinyjs::hide("params")
    shinyjs::hide("form")
    shinyjs::hide("job_end")
    shinyjs::hide("results_mini")
    shinyjs::hide("results_filtered")
    shinyjs::hide("results_full")
    shinyjs::hide("results_mztab")
    shinyjs::hide("close")
    ## For shinylive, see
    ## https://github.com/taxonomicallyinformedannotation/tima-shinylive/pull/7
    if (R.Version()$os != "emscripten") {
      targets::tar_make(
        names = tidyselect::matches("^(ann_wei|exp_mzt)$")
      )
    } else {
      targets::tar_make(
        names = tidyselect::matches("^(ann_wei|exp_mzt)$"),
        callr_function = NULL
      )
    }

    results <- utils::tail(
      list.files(
        path = "data/processed",
        pattern = ".tsv",
        full.names = TRUE,
        recursive = TRUE
      ),
      n = 3L
    )
    names(results) <- c("filtered", "mini", "full")
    results_mztab <- targets::tar_read(exp_mzt)
    observe({
      shiny::invalidateLater(millis = 5000)
      shinyjs::hide("job_msg")
      shinyjs::show("job_end")
      lapply(names(results), function(name) {
        output[[paste0("results_", name)]] <- downloadHandler(
          filename = basename(results[[name]]),
          content = function(file) {
            ok <- file.copy(results[[name]], file, overwrite = TRUE)
            if (!ok) {
              cli::cli_abort(
                "failed to copy result file {.file {results[[name]]}}",
                class = c("tima_runtime_error", "tima_error"),
                call = NULL
              )
            }
          }
        )
        shinyjs::show(paste0("results_", name))
      })
      output[["results_mztab"]] <- downloadHandler(
        filename = basename(results_mztab),
        content = function(file) {
          ok <- file.copy(results_mztab, file, overwrite = TRUE)
          if (!ok) {
            cli::cli_abort(
              "failed to copy result file {.file {results_mztab}}",
              class = c("tima_runtime_error", "tima_error"),
              call = NULL
            )
          }
        }
      )
      shinyjs::show("results_mztab")
      shinyjs::show("close")
      shiny::observeEvent(eventExpr = input$close, handlerExpr = {
        shiny::stopApp()
      })
    })
  })
}
