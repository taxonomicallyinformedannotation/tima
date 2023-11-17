options(shiny.host = "0.0.0.0")
options(shiny.port = 3838)
options(shiny.maxRequestSize = 1000 * 1024^2)

# save the results to a file
save_input <- function(input) {
  setwd("../../")
  paths_data_source <- timaR::parse_yaml_paths()$data$source$path
  ## safety
  timaR::create_dir(paths_data_source)

  list <- timaR::load_yaml_files()
  yamls_params <- list$yamls_params
  yaml_files <- list$yaml_files
  yaml_names <- list$yaml_names

  ## This allows to keep files correctly placed in `data/source` clean
  prefil_fea_raw <- shiny::isolate(input$fil_fea_raw)
  prefil_spe_raw <- shiny::isolate(input$fil_spe_raw)
  prefil_met_raw <- shiny::isolate(input$fil_met_raw)
  prefil_fea_raw_1 <- file.path(paths_data_source, prefil_fea_raw[[1]])
  prefil_spe_raw_1 <- file.path(paths_data_source, prefil_spe_raw[[1]])
  if (!is.null(prefil_met_raw)) {
    prefil_met_raw_1 <- file.path(paths_data_source, prefil_met_raw[[1]])
  }
  if (file.exists(prefil_fea_raw_1)) {
    fil_fea_raw <- prefil_fea_raw_1
  } else {
    fil_fea_raw <- prefil_fea_raw[[4]]
  }
  if (file.exists(prefil_spe_raw_1)) {
    fil_spe_raw <- prefil_spe_raw_1
  } else {
    fil_spe_raw <- prefil_spe_raw[[4]]
  }
  if (!is.null(prefil_met_raw)) {
    if (file.exists(prefil_met_raw_1)) {
      fil_met_raw <- prefil_met_raw_1
    } else {
      fil_met_raw <- prefil_met_raw[[4]]
    }
  }

  fil_pat <- shiny::isolate(input$fil_pat)

  gnps_job_id <- shiny::isolate(input$gnps_id)
  if (gnps_job_id == "") {
    gnps_job_id <- NULL
  }
  gnps_workflow <- shiny::isolate(input$gnps_workflow)

  org_tax <- shiny::isolate(input$org_tax)
  if (org_tax == "") {
    org_tax <- NULL
  }

  timaR::log_debug(x = "Changing parameters ...")
  timaR::log_debug(x = "... Small")
  yaml_small <- yamls_params[["inst/params/prepare_params"]]
  yaml_small$files$pattern <-
    fil_pat
  yaml_small$files$features$raw <-
    fil_fea_raw
  yaml_small$files$metadata$raw <-
    fil_met_raw
  yaml_small$files$spectral$raw <-
    fil_spe_raw
  yaml_small$ms$polarity <-
    shiny::isolate(input$ms_pol)
  yaml_small$organisms$taxon <-
    org_tax
  yaml_small$options$summarise <-
    shiny::isolate(input$summarise)
  timaR::create_dir("inst/params")
  yaml::write_yaml(
    x = yaml_small,
    file = "inst/params/prepare_params.yaml"
  )

  timaR::log_debug(x = "... Advanced")
  yaml_advanced <- yamls_params[["inst/params/prepare_params_advanced"]]
  yaml_advanced$annotations$candidates$final <-
    shiny::isolate(input$ann_can_fin)
  yaml_advanced$annotations$ms1only <-
    shiny::isolate(input$ann_ms1only)
  yaml_advanced$annotations$ms2approx <-
    shiny::isolate(input$ann_ms2_app)
  yaml_advanced$annotations$thresholds$consistency <-
    shiny::isolate(input$ann_thr_con)
  yaml_advanced$annotations$thresholds$ms1$biological <-
    shiny::isolate(input$ann_thr_ms1_bio)
  yaml_advanced$annotations$thresholds$ms1$chemical <-
    shiny::isolate(input$ann_thr_ms1_che)
  yaml_advanced$annotations$thresholds$ms1$condition <-
    shiny::isolate(input$ann_thr_ms1_con)
  yaml_advanced$annotations$thresholds$ms2$similarity <-
    shiny::isolate(input$ann_thr_ms2_sim)
  yaml_advanced$files$pattern <-
    fil_pat
  # TODO
  # yaml_advanced$files$annotations$raw$spectral <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$raw$spectral$gnps <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$raw$spectral$sirius <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$raw$spectral$spectral <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$raw$sirius <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$prepared$canopus <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$prepared$formula <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$prepared$structural <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$prepared$structural$gnps <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$prepared$structural$ms1 <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$prepared$structural$sirius <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$prepared$structural$spectral <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$filtered <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$annotations$processed <-
  #   shiny::isolate(input$todo)
  yaml_advanced$files$features$raw <-
    fil_fea_raw
  # TODO
  # yaml_advanced$files$features$prepared <-
  #   shiny::isolate(input$fil_fea_pre)
  # TODO
  # yaml_advanced$files$libraries$adducts$neg <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$adducts$pos <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$adducts$prepared <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$raw$closed <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$raw$ecmdb <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$raw$lotus <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$prepared <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$prepared$closed <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$prepared$ecmdb <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$prepared$lotus <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$prepared$rt <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$keys <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$organisms$names <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$organisms$taxonomies$ott <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$structures$stereo <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$structures$metadata <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$structures$names <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$structures$taxonomies$cla <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$sop$merged$structures$taxonomies$npc <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$spectral$exp$neg <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$spectral$exp$pos <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$spectral$exp$raw <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$spectral$is$neg <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$spectral$is$pos <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$spectral$is$raw <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$temporal$exp <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$temporal$is <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$libraries$temporal$prepared <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$networks$spectral$edges$raw <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$networks$spectral$edges$raw$ms1 <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$networks$spectral$edges$raw$spectral <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$networks$spectral$edges$prepared <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$networks$spectral$components$raw <-
  #   shiny::isolate(input$todo)
  # yaml_advanced$files$networks$spectral$components$prepared <-
  #   shiny::isolate(input$todo)
  yaml_advanced$files$metadata$raw <-
    fil_met_raw
  # TODO
  # yaml_advanced$files$metadata$prepared <-
  #   shiny::isolate(input$fil_met_pre)
  yaml_advanced$files$spectral$raw <-
    fil_spe_raw
  yaml_advanced$gnps$id <-
    gnps_job_id
  yaml_advanced$gnps$workflow <-
    shiny::isolate(input$gnps_workflow)
  yaml_advanced$ms$adducts$neg <-
    shiny::isolate(input$ms_add_neg)
  yaml_advanced$ms$adducts$pos <-
    shiny::isolate(input$ms_add_pos)
  yaml_advanced$ms$clusters$neg <-
    shiny::isolate(input$ms_clu_neg)
  yaml_advanced$ms$clusters$pos <-
    shiny::isolate(input$ms_clu_pos)
  yaml_advanced$ms$polarity <-
    shiny::isolate(input$ms_pol)
  yaml_advanced$ms$thresholds$ms1$intensity <-
    shiny::isolate(input$ms_thr_ms1_int)
  yaml_advanced$ms$thresholds$ms2$intensity <-
    shiny::isolate(input$ms_thr_ms2_int)
  yaml_advanced$ms$tolerances$mass$ppm$ms1 <-
    shiny::isolate(input$ms_tol_mas_ppm_ms1)
  yaml_advanced$ms$tolerances$mass$ppm$ms2 <-
    shiny::isolate(input$ms_tol_mas_ppm_ms2)
  yaml_advanced$ms$tolerances$mass$dalton$ms1 <-
    shiny::isolate(input$ms_tol_mas_dal_ms1)
  yaml_advanced$ms$tolerances$mass$dalton$ms2 <-
    shiny::isolate(input$ms_tol_mas_dal_ms2)
  yaml_advanced$ms$tolerances$rt$minutes <-
    shiny::isolate(input$ms_tol_rt_min)
  yaml_advanced$names$extension <-
    shiny::isolate(input$names_extension)
  yaml_advanced$names$features <-
    shiny::isolate(input$names_features)
  yaml_advanced$names$filename <-
    shiny::isolate(input$names_filename)
  # TODO
  # yaml_advanced$names$inchikey <-
  #   shiny::isolate(input$names_inchikey)
  # TODO
  # yaml_advanced$names$mgf$collision_energy <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$compound_id <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$exact_mass <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$inchi <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$inchi_no_stereo <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$inchikey <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$inchikey_no_stereo <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$molecular_formula <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$name <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$polarity <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$retention_time <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$smiles <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$smiles_no_stereo <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$spectrum_id <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$splash <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$synonyms <-
  #   shiny::isolate(input$names_mgf_x)
  # yaml_advanced$names$mgf$xlogp <-
  #   shiny::isolate(input$names_mgf_x)
  yaml_advanced$names$precursor <-
    shiny::isolate(input$names_precursor)
  yaml_advanced$names$rt$features <-
    shiny::isolate(input$names_rt)
  yaml_advanced$names$rt$library <-
    shiny::isolate(input$names_rt_2)
  # TODO
  # yaml_advanced$names$smiles <-
  #   shiny::isolate(input$names_smiles)
  yaml_advanced$names$source <-
    shiny::isolate(input$names_source)
  yaml_advanced$names$target <-
    shiny::isolate(input$names_target)
  yaml_advanced$names$taxon <-
    shiny::isolate(input$names_taxon)
  yaml_advanced$organisms$candidates <-
    shiny::isolate(input$org_can)
  yaml_advanced$organisms$filter$mode <-
    shiny::isolate(input$org_fil_mod)
  yaml_advanced$organisms$filter$level <-
    shiny::isolate(input$org_fil_lev)
  yaml_advanced$organisms$filter$value <-
    shiny::isolate(input$org_fil_val)
  yaml_advanced$organisms$taxon <-
    org_tax
  # TODO
  # yaml_advanced$tools$metadata <-
  #   shiny::isolate(input$too_x)
  # yaml_advanced$tools$networks$spectral$components <-
  #   shiny::isolate(input$too_x)
  # yaml_advanced$tools$networks$spectral$edges <-
  #   shiny::isolate(input$too_x)
  # yaml_advanced$tools$taxonomies$biological <-
  #   shiny::isolate(input$too_x)
  # yaml_advanced$tools$taxonomies$chemical <-
  #   shiny::isolate(input$too_x)
  # TODO
  # yaml_advanced$units$rt <-
  #   shiny::isolate(input$uni_rt)
  yaml_advanced$weights$global$biological <-
    shiny::isolate(input$wei_glo_bio)
  yaml_advanced$weights$global$chemical <-
    shiny::isolate(input$wei_glo_che)
  yaml_advanced$weights$global$spectral <-
    shiny::isolate(input$wei_glo_spe)
  yaml_advanced$weights$biological$domain <-
    shiny::isolate(input$wei_bio_01)
  yaml_advanced$weights$biological$kingdom <-
    shiny::isolate(input$wei_bio_02)
  yaml_advanced$weights$biological$phylum <-
    shiny::isolate(input$wei_bio_03)
  yaml_advanced$weights$biological$class <-
    shiny::isolate(input$wei_bio_04)
  yaml_advanced$weights$biological$order <-
    shiny::isolate(input$wei_bio_05)
  yaml_advanced$weights$biological$infraorder <-
    shiny::isolate(input$wei_bio_06)
  yaml_advanced$weights$biological$family <-
    shiny::isolate(input$wei_bio_07)
  yaml_advanced$weights$biological$subfamily <-
    shiny::isolate(input$wei_bio_08)
  yaml_advanced$weights$biological$tribe <-
    shiny::isolate(input$wei_bio_09)
  yaml_advanced$weights$biological$subtribe <-
    shiny::isolate(input$wei_bio_10)
  yaml_advanced$weights$biological$genus <-
    shiny::isolate(input$wei_bio_11)
  yaml_advanced$weights$biological$subgenus <-
    shiny::isolate(input$wei_bio_12)
  yaml_advanced$weights$biological$species <-
    shiny::isolate(input$wei_bio_13)
  yaml_advanced$weights$biological$subspecies <-
    shiny::isolate(input$wei_bio_14)
  yaml_advanced$weights$biological$variety <-
    shiny::isolate(input$wei_bio_15)
  yaml_advanced$weights$chemical$cla$kingdom <-
    shiny::isolate(input$wei_che_11)
  yaml_advanced$weights$chemical$npc$pathway <-
    shiny::isolate(input$wei_che_21)
  yaml_advanced$weights$chemical$cla$superclass <-
    shiny::isolate(input$wei_che_12)
  yaml_advanced$weights$chemical$npc$superclass <-
    shiny::isolate(input$wei_che_22)
  yaml_advanced$weights$chemical$cla$class <-
    shiny::isolate(input$wei_che_13)
  yaml_advanced$weights$chemical$npc$class <-
    shiny::isolate(input$wei_che_23)
  yaml_advanced$weights$chemical$cla$parent <-
    shiny::isolate(input$wei_che_14)
  yaml_advanced$options$compounds_names <-
    shiny::isolate(input$compounds_names)
  yaml_advanced$options$force <-
    shiny::isolate(input$force)
  yaml_advanced$options$summarise <-
    shiny::isolate(input$summarise)

  if (!is.null(prefil_met_raw)) {
    yamls_params$prepare_taxa$files$metadata$raw <- fil_met_raw
  }
  if (!is.null(gnps_job_id)) {
    yamls_params$prepare_taxa$files$metadata$raw <-
      file.path(paths_data_source, paste0(gnps_job_id, "_metadata.tsv"))
  }

  yaml::write_yaml(
    x = yaml_advanced,
    file = "inst/params/prepare_params_advanced.yaml"
  )

  setwd("inst/app/")
}

server <- function(input, output, session) {
  ## Observe helpers
  shinyhelper::observe_helpers()

  ## Mandatory fields
  fields_mandatory <- c("fil_fea_raw", "fil_spe_raw", "fil_pat")

  ## Enable the Submit button when all mandatory fields are filled out
  shiny::observe(x = {
    mandatory_filled <-
      vapply(
        X = fields_mandatory,
        FUN = function(x) {
          ## TODO improve
          suppressWarnings(any(
            !is.null(input[[x]]),
            input[[x]] != ""
          ))
        },
        FUN.VALUE = logical(1)
      )
    mandatory_filled <- all(mandatory_filled)

    shinyjs::toggleState(id = "save", condition = mandatory_filled)
    shinyjs::toggleState(id = "launch", condition = input$save >= 1)
  })

  ## Special check for taxon name
  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("org_tax", function(taxon) {
    if (!grepl(pattern = "^[[:upper:]]", x = taxon)) {
      "Please provide your taxon name with capital letter"
    }
  })
  iv$add_rule("org_tax", function(taxon) {
    if (is.na(rotl::tnrs_match_names(
      names = taxon,
      do_approximate_matching = FALSE
    )$ott_id)) {
      "Taxon not found in Open Tree of Life"
    }
  })
  iv$enable()

  ## When the Save button is clicked, save the response
  shiny::observeEvent(
    eventExpr = input$save,
    handlerExpr = {
      ## User-experience stuff
      shinyjs::show("save_msg")
      shinyjs::enable("launch")
      shinyjs::hide("error")

      ## Save the data (show an error message in case of error)
      tryCatch(
        expr = {
          save_input(input = input)
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
        }
      )
    }
  )

  shiny::observeEvent(
    eventExpr = input$launch,
    handlerExpr = {
      shinyjs::show("job_msg")
      shinyjs::hide("error")
      shinyjs::hide("params")
      shinyjs::hide("form")
      shinyjs::show("tar_watch")
      tryCatch(expr = {
        setwd("../..")
        targets::tar_watch_server(id = "tar_watch")
        targets::tar_watch(
          host = "127.0.0.1",
          port = 3839,
          display = "graph",
          displays = c("summary", "graph"),
          degree_from = 10,
          outdated = TRUE,
          targets_only = TRUE,
          supervise = TRUE,
          verbose = TRUE,
          exclude = c(
            "yaml_paths",
            "benchmark_ann_fil_ms1_neg",
            "benchmark_ann_fil_ms1_pos",
            "benchmark_ann_fil_spe_neg",
            "benchmark_ann_fil_spe_pos",
            "benchmark_ann_fil_spe_ms1_neg",
            "benchmark_ann_fil_spe_ms1_pos",
            "benchmark_ann_ms1_neg",
            "benchmark_ann_ms2_pos",
            "benchmark_ann_ms1_pre_neg",
            "benchmark_ann_ms1_pre_pos",
            "benchmark_ann_pre_ms1_ms2_b_c_neg",
            "benchmark_ann_pre_ms1_ms2_b_c_pos",
            "benchmark_ann_pre_ms1_ms2_b_neg",
            "benchmark_ann_pre_ms1_ms2_b_pos",
            "benchmark_ann_pre_ms2_b_c_neg",
            "benchmark_ann_pre_ms2_b_c_pos",
            "benchmark_ann_pre_ms2_b_neg",
            "benchmark_ann_pre_ms2_b_pos",
            "benchmark_ann_sir_pre",
            "benchmark_ann_sir_pre_can",
            "benchmark_ann_sir_pre_for",
            "benchmark_ann_sir_pre_str",
            "benchmark_ann_spe_neg",
            "benchmark_ann_spe_pos",
            "benchmark_ann_spe_pre_neg",
            "benchmark_ann_spe_pre_pos",
            "benchmark_com_neg",
            "benchmark_com_pos",
            "benchmark_com_pre_neg",
            "benchmark_com_pre_pos",
            "benchmark_converted",
            "benchmark_copy",
            "benchmark_def_ann_mas",
            "benchmark_def_ann_spe",
            "benchmark_def_cre_edg_com",
            "benchmark_def_cre_edg_spe",
            "benchmark_def_fil_ann",
            "benchmark_def_pre_ann_sir",
            "benchmark_def_pre_ann_spe",
            "benchmark_def_pre_fea_com",
            "benchmark_def_pre_fea_edg",
            "benchmark_def_wei_ann",
            "benchmark_edg_pre_neg",
            "benchmark_edg_pre_pos",
            "benchmark_edg_spe_neg",
            "benchmark_edg_spe_pos",
            "benchmark_file",
            "benchmark_files_neg",
            "benchmark_files_pos",
            "benchmark_path_copy",
            "benchmark_path_export",
            "benchmark_path_mgf_neg",
            "benchmark_path_mgf_pos",
            "benchmark_path_url",
            "benchmark_prepared",
            "benchmark_pre_meta_neg",
            "benchmark_pre_meta_pos",
            "benchmark_pre_mgf_neg",
            "benchmark_pre_mgf_pos",
            "benchmark_taxed_neg",
            "benchmark_taxed_pos",
            "benchmark_wei_par",
            "paths",
            "paths_data_interim_libraries_adducts_path",
            "paths_data_source_benchmark_copy",
            "paths_data_source_benchmark_mgf_neg",
            "paths_data_source_benchmark_mgf_pos",
            "paths_data_source_benchmark_set",
            "paths_data_source_libraries_sop_ecmdb",
            "paths_data_source_libraries_sop_lotus",
            "paths_data_source_libraries_spectra_is_lotus_pos",
            "paths_data_source_libraries_spectra_is_lotus_neg",
            "paths_data_source_spectra",
            # "paths_gnps_example_id",
            "paths_interim_a",
            "paths_interim_f",
            "paths_source",
            "paths_test_mode",
            "paths_urls_benchmarking_set",
            "paths_urls_ecmdb_metabolites",
            "paths_urls_lotus_doi",
            "paths_urls_lotus_pattern",
            "paths_urls_massbank_file",
            "paths_urls_massbank_url",
            "paths_urls_massbank_version",
            "paths_urls_examples_spectra_mini",
            "paths_urls_examples_spectral_lib_pos",
            "paths_urls_examples_spectral_lib_neg",
            "par_def_ann_mas",
            "par_def_ann_spe",
            "par_def_cre_com",
            "par_def_cre_edg_spe",
            "par_def_fil_ann",
            "par_def_pre_ann_gnp",
            "par_def_pre_ann_sir",
            "par_def_pre_ann_spe",
            "par_def_pre_fea_com",
            "par_def_pre_fea_edg",
            "par_def_pre_fea_tab",
            "par_def_pre_lib_add",
            "par_def_pre_lib_rt",
            "par_def_pre_lib_sop_clo",
            "par_def_pre_lib_sop_ecm",
            "par_def_pre_lib_sop_lot",
            "par_def_pre_lib_sop_mer",
            "par_def_pre_lib_spe",
            "par_def_pre_tax",
            "par_def_wei_ann",
            "par_fin_par",
            "par_fin_par2",
            "par_pre_par",
            "par_pre_par2",
            "par_usr_ann_mas",
            "par_usr_ann_spe",
            "par_usr_cre_com",
            "par_usr_cre_edg_spe",
            "par_usr_fil_ann",
            "par_usr_pre_ann_gnp",
            "par_usr_pre_ann_sir",
            "par_usr_pre_ann_spe",
            "par_usr_pre_fea_com",
            "par_usr_pre_fea_edg",
            "par_usr_pre_fea_tab",
            "par_usr_pre_lib_add",
            "par_usr_pre_lib_rt",
            "par_usr_pre_lib_sop_clo",
            "par_usr_pre_lib_sop_ecm",
            "par_usr_pre_lib_sop_lot",
            "par_usr_pre_lib_sop_mer",
            "par_usr_pre_lib_spe",
            "par_usr_pre_tax",
            "par_usr_wei_ann",
            "par_ann_mas",
            "par_ann_spe",
            "par_cre_com",
            "par_cre_edg_spe",
            "par_fil_ann",
            "par_pre_ann_gnp",
            "par_pre_ann_sir",
            "par_pre_ann_spe",
            "par_pre_fea_com",
            "par_pre_fea_edg",
            "par_pre_fea_tab",
            "par_pre_lib_add",
            "par_pre_lib_rt",
            "par_pre_lib_sop_clo",
            "par_pre_lib_sop_ecm",
            "par_pre_lib_sop_lot",
            "par_pre_lib_sop_mer",
            "par_pre_lib_spe",
            "par_pre_tax",
            "par_wei_ann",
            ".Random.seed"
          )
        )
        targets::tar_make(
          names = targets::matches("^ann_pre$"),
          garbage_collection = TRUE,
          reporter = "verbose_positives"
        )
        setwd("inst/app")
      }, finally = {
        shiny::stopApp()
      })
      process <-
        shiny::reactiveValues(status = targets::tar_active())
      shiny::observe({
        shiny::invalidateLater(millis = 5000)
        process$status <- targets::tar_active()
      })

      shiny::observeEvent(
        eventExpr = process$status,
        handlerExpr = {
          message("Job finished")
          shiny::stopApp()
        }
      )
    }
  )
}
