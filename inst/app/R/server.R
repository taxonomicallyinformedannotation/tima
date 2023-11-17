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

  ## Params occurring more than once
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

  filename <- shiny::isolate(input$fil_pat)
  gnps_job_id <- shiny::isolate(input$gnps_id)
  if (gnps_job_id == "") {
    gnps_job_id <- NULL
  }
  gnps_workflow <- shiny::isolate(input$gnps_workflow)
  org_tax <- shiny::isolate(input$org_tax)
  if (org_tax == "") {
    org_tax <- NULL
  }
  ms_mode <- shiny::isolate(input$ms_pol)
  ms_tol_rt_min <- shiny::isolate(input$ms_tol_rt_min)
  names_source <- shiny::isolate(input$names_source)
  names_target <- shiny::isolate(input$names_target)
  forceps <- shiny::isolate(input$force)
  summarise <- shiny::isolate(input$summarise)
  compound_names <- shiny::isolate(input$compounds_names)

  ## Change 1
  timaR::log_debug(x = "Changing parameters")
  yamls_params$
    `inst/params/prepare_params`$
    files$
    pattern <- filename
  yamls_params$
    `inst/params/prepare_params`$
    files$
    features$
    raw <- fil_fea_raw
  yamls_params$
    `inst/params/prepare_params`$
    files$
    spectral$
    raw <- fil_spe_raw
  yamls_params$
    `inst/params/prepare_params`$
    files$
    spectral$
    raw <- fil_spe_raw
  # yamls_params$`inst/params/prepare_params`$gnps$id <- gnps_job_id
  # yamls_params$
  #   `inst/params/prepare_params`$
  #   gnps$
  #   workflow <-
  #   gnps_workflow
  yamls_params$
    `inst/params/prepare_params`$
    files$
    metadata$
    raw <- fil_met_raw
  yamls_params$
    `inst/params/prepare_params`$
    ms$
    polarity <- ms_mode
  yamls_params$
    `inst/params/prepare_params`$
    organisms$
    taxon <- org_tax
  ## summarise
  yamls_params$
    `inst/params/prepare_params`$
    options$
    summarise <-
    summarise

  timaR::create_dir("inst/params")
  yaml::write_yaml(
    x = yamls_params$`inst/params/prepare_params`,
    file = "inst/params/prepare_params.yaml"
  )

  yamls_params$annotate_masses$ms$adducts$neg <-
    shiny::isolate(input$ms_add_neg)
  yamls_params$annotate_masses$ms$adducts$pos <-
    shiny::isolate(input$ms_add_pos)
  yamls_params$annotate_masses$ms$clusters$neg <-
    shiny::isolate(input$ms_clu_neg)
  yamls_params$annotate_masses$ms$clusters$pos <-
    shiny::isolate(input$ms_clu_pos)
  yamls_params$
    annotate_masses$
    ms$
    thresholds$
    ms1$
    intensity <-
    shiny::isolate(input$ms_thr_ms1_int)
  yamls_params$annotate_masses$ms$polarity <- ms_mode
  yamls_params$
    annotate_masses$
    ms$
    tolerances$
    mass$
    ppm$
    ms1 <-
    shiny::isolate(input$ms_tol_mas_ppm_ms1)
  yamls_params$
    annotate_masses$
    ms$
    tolerances$
    mass$
    dalton$
    ms1 <-
    shiny::isolate(input$ms_tol_mas_dal_ms1)
  yamls_params$
    annotate_masses$
    ms$
    tolerances$
    rt$
    minutes <-
    ms_tol_rt_min
  yamls_params$annotate_masses$names$source <- names_source
  yamls_params$annotate_masses$names$target <- names_target
  yamls_params$annotate_masses$options$force <- forceps

  yamls_params$annotate_spectra$files$spectral$raw <- fil_spe_raw
  yamls_params$
    annotate_spectra$
    annotations$
    ms2approx <-
    shiny::isolate(input$ann_ms2_app)
  yamls_params$
    annotate_spectra$
    annotations$
    thresholds$
    ms2$
    similarity <-
    shiny::isolate(input$ann_thr_ms2_sim)
  yamls_params$
    annotate_spectra$
    ms$
    thresholds$
    ms2$
    intensity <-
    shiny::isolate(input$ms_thr_ms2_int)
  yamls_params$annotate_spectra$ms$polarity <- ms_mode
  yamls_params$
    annotate_spectra$
    ms$
    tolerances$
    mass$
    ppm$
    ms2 <-
    shiny::isolate(input$ms_tol_mas_ppm_ms2)
  yamls_params$
    annotate_spectra$
    ms$
    tolerances$
    mass$
    dalton$
    ms2 <-
    shiny::isolate(input$ms_tol_mas_dal_ms2)

  yamls_params$
    create_edges_spectra$
    files$
    spectral$
    raw <- fil_spe_raw
  yamls_params$
    create_edges_spectra$
    annotations$
    ms2$
    method <-
    shiny::isolate(input$ann_ms2_met)
  yamls_params$
    create_edges_spectra$
    annotations$
    thresholds$
    ms2$
    similarity <-
    shiny::isolate(input$edg_thr_ms2_sim)
  yamls_params$
    create_edges_spectra$
    ms$
    thresholds$
    ms2$
    intensity <-
    shiny::isolate(input$ms_thr_ms2_int)
  yamls_params$create_edges_spectra$ms$polarity <- ms_mode
  yamls_params$
    create_edges_spectra$
    ms$
    tolerances$
    mass$
    ppm$
    ms2 <-
    shiny::isolate(input$ms_tol_mas_ppm_ms2)
  yamls_params$
    create_edges_spectra$
    ms$
    tolerances$
    mass$
    dalton$
    ms2 <-
    shiny::isolate(input$ms_tol_mas_dal_ms2)
  yamls_params$create_edges_spectra$names$source <- names_source
  yamls_params$create_edges_spectra$names$target <- names_target

  yamls_params$
    filter_annotations$
    ms$
    tolerances$
    rt$
    minutes <-
    ms_tol_rt_min

  yamls_params$prepare_features_edges$names$source <- names_source
  yamls_params$prepare_features_edges$names$target <- names_target

  yamls_params$
    prepare_features_tables$
    files$
    features$
    raw <- fil_fea_raw
  yamls_params$
    prepare_features_tables$
    names$
    features <-
    shiny::isolate(input$names_features)
  yamls_params$
    prepare_features_tables$
    names$
    precursor <-
    shiny::isolate(input$names_precursor)
  yamls_params$prepare_features_tables$names$rt <-
    shiny::isolate(input$names_rt)

  yamls_params$prepare_libraries_rt$names$rt <-
    shiny::isolate(input$names_rt_2)

  yamls_params$
    prepare_libraries_sop_merged$
    organisms$
    filter$
    mode <-
    shiny::isolate(input$org_fil_mod)
  yamls_params$
    prepare_libraries_sop_merged$
    organisms$
    filter$
    level <-
    shiny::isolate(input$org_fil_lev)
  yamls_params$
    prepare_libraries_sop_merged$
    organisms$
    filter$
    value <-
    shiny::isolate(input$org_fil_val)

  yamls_params$prepare_taxa$files$features$raw <- fil_fea_raw
  if (!is.null(prefil_met_raw)) {
    yamls_params$prepare_taxa$files$metadata$raw <- fil_met_raw
  }
  # if (!is.null(gnps_job_id)) {
  #   yamls_params$prepare_taxa$files$metadata$raw <-
  #     file.path(paths_data_source, paste0(gnps_job_id, "_metadata.tsv"))
  # }
  yamls_params$prepare_taxa$names$extension <-
    shiny::isolate(input$names_extension)
  yamls_params$prepare_taxa$names$features <-
    shiny::isolate(input$names_features)
  yamls_params$prepare_taxa$names$filename <-
    shiny::isolate(input$names_filename)
  yamls_params$prepare_taxa$names$taxon <-
    shiny::isolate(input$names_taxon)
  yamls_params$prepare_taxa$organisms$candidates <-
    shiny::isolate(input$org_can)
  yamls_params$prepare_taxa$organisms$taxon <- org_tax

  yamls_params$
    weight_annotations$
    annotations$
    candidates$
    final <-
    shiny::isolate(input$ann_can_fin)
  yamls_params$
    weight_annotations$
    annotations$
    ms1only <-
    shiny::isolate(input$ann_ms1only)
  yamls_params$
    weight_annotations$
    annotations$
    thresholds$
    consistency <-
    shiny::isolate(input$ann_thr_con)
  yamls_params$
    weight_annotations$
    annotations$
    thresholds$
    ms1$
    biological <-
    shiny::isolate(input$ann_thr_ms1_bio)
  yamls_params$
    weight_annotations$
    annotations$
    thresholds$
    ms1$
    chemical <-
    shiny::isolate(input$ann_thr_ms1_che)
  yamls_params$
    weight_annotations$
    annotations$
    thresholds$
    ms1$
    condition <-
    shiny::isolate(input$ann_thr_ms1_con)
  yamls_params$
    weight_annotations$
    weights$
    global$
    biological <-
    shiny::isolate(input$wei_glo_bio)
  yamls_params$
    weight_annotations$
    weights$
    global$
    chemical <-
    shiny::isolate(input$wei_glo_che)
  yamls_params$
    weight_annotations$
    weights$
    global$
    spectral <-
    shiny::isolate(input$wei_glo_spe)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    domain <-
    shiny::isolate(input$wei_bio_01)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    kingdom <-
    shiny::isolate(input$wei_bio_02)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    phylum <-
    shiny::isolate(input$wei_bio_03)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    class <-
    shiny::isolate(input$wei_bio_04)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    order <-
    shiny::isolate(input$wei_bio_05)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    infraorder <-
    shiny::isolate(input$wei_bio_06)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    family <-
    shiny::isolate(input$wei_bio_07)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    subfamily <-
    shiny::isolate(input$wei_bio_08)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    tribe <-
    shiny::isolate(input$wei_bio_09)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    subtribe <-
    shiny::isolate(input$wei_bio_10)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    genus <-
    shiny::isolate(input$wei_bio_11)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    subgenus <-
    shiny::isolate(input$wei_bio_12)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    species <-
    shiny::isolate(input$wei_bio_13)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    subspecies <-
    shiny::isolate(input$wei_bio_14)
  yamls_params$
    weight_annotations$
    weights$
    biological$
    variety <-
    shiny::isolate(input$wei_bio_15)
  yamls_params$
    weight_annotations$
    weights$
    chemical$
    cla$
    kingdom <-
    shiny::isolate(input$wei_che_11)
  yamls_params$
    weight_annotations$
    weights$
    chemical$
    cla$
    superclass <-
    shiny::isolate(input$wei_che_12)
  yamls_params$
    weight_annotations$
    weights$
    chemical$
    cla$
    superclass <-
    shiny::isolate(input$wei_che_13)
  yamls_params$
    weight_annotations$
    weights$
    chemical$
    cla$
    parent <-
    shiny::isolate(input$wei_che_14)
  yamls_params$
    weight_annotations$
    weights$
    chemical$
    npc$
    pathway <-
    shiny::isolate(input$wei_che_11)
  yamls_params$
    weight_annotations$
    weights$
    chemical$
    npc$
    superclass <-
    shiny::isolate(input$wei_che_12)
  yamls_params$
    weight_annotations$
    weights$
    chemical$
    npc$
    class <-
    shiny::isolate(input$wei_che_13)
  yamls_params$weight_annotations$options$force <- forceps
  yamls_params$weight_annotations$options$compound_names <- compound_names
  yamls_params$weight_annotations$options$summarise <- summarise
  yamls_params$weight_annotations$files$pattern <- filename

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
            "par_pre_par",
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
