# save the results to a file
.get_default_paths <- getFromNamespace("get_default_paths", "tima")
.create_dir <- getFromNamespace("create_dir", "tima")
.load_yaml_files <- getFromNamespace("load_yaml_files", "tima")
.get_params <- getFromNamespace("get_params", "tima")
.plan_mztab_app_outputs <- getFromNamespace("plan_mztab_app_outputs", "tima")
.log_trace <- getFromNamespace("log_trace", "tima")
.replace_id <- getFromNamespace("replace_id", "tima")

.RESULT_AND_STATUS_IDS <- c(
  "thankyou_msg",
  "error",
  "job_end",
  "results_mini",
  "results_filtered",
  "results_full",
  "results_mztab",
  "close"
)

.empty_to_null <- function(x) {
  if (is.null(x) || identical(x, "")) NULL else x
}

.has_upload <- function(upload) {
  !is.null(upload) && length(upload[[1L]]) > 0L && nzchar(upload[[1L]])
}

.hide_ids <- function(ids) {
  for (id in ids) {
    shinyjs::hide(id)
  }
}

.make_url_download <- function(filename_url, content_url = filename_url) {
  force(filename_url)
  force(content_url)
  shiny::downloadHandler(
    filename = basename(filename_url),
    content = function(file) {
      writeLines(readLines(content_url), file)
    }
  )
}

.register_result_download <- function(output, output_id, src_path) {
  force(src_path)
  output[[output_id]] <- shiny::downloadHandler(
    filename = basename(src_path),
    content = function(file) {
      ok <- file.copy(src_path, file, overwrite = TRUE)
      if (!ok) {
        cli::cli_abort(
          "failed to copy result file {.file {src_path}}",
          class = c("tima_runtime_error", "tima_error"),
          call = NULL
        )
      }
    }
  )
  shinyjs::show(output_id)
}

.stage_single_upload <- function(upload_df, dest_dir) {
  if (is.null(upload_df)) {
    return(NULL)
  }
  dest <- file.path(dest_dir, upload_df[[1L]])
  if (!file.exists(dest)) {
    .create_dir(dest_dir)
    file.copy(from = upload_df[[4L]], to = dest, overwrite = TRUE)
  }
  dest
}

.stage_multi_upload <- function(upload_df, dest_dir, default = NULL) {
  if (is.null(upload_df) || length(upload_df[[1L]]) == 0L) {
    return(default)
  }
  dest_paths <- file.path(dest_dir, basename(upload_df[[1L]]))
  .create_dir(dest_dir)
  to_copy <- !file.exists(dest_paths)
  if (any(to_copy)) {
    file.copy(
      from = upload_df[[4L]][to_copy],
      to = dest_paths[to_copy],
      overwrite = TRUE
    )
  }
  dest_paths
}

.save_input <- function(input) {
  default_paths <- .get_default_paths()
  paths_data_source <- default_paths$data$source$path
  paths_data_interim_annotations <-
    default_paths$data$interim$annotations$path
  ## safety
  .create_dir(paths_data_source)

  yamls_params <- .load_yaml_files()$yamls_params

  ## This allows to keep files correctly placed in `data/source` clean
  fil_fea_raw <- .stage_single_upload(input$fil_fea_raw, paths_data_source)
  fil_spe_raw <- .stage_single_upload(input$fil_spe_raw, paths_data_source)
  fil_met_raw <- .stage_single_upload(input$fil_met_raw, paths_data_source)
  fil_mzm_raw <- .stage_single_upload(
    input$fil_ann_raw_mzm,
    paths_data_interim_annotations
  )
  fil_mzt_raw <- .stage_single_upload(input$fil_mzt_raw, paths_data_source)
  fil_sir_raw <- .stage_single_upload(
    input$fil_ann_raw_sir,
    paths_data_interim_annotations
  )

  # If mzTab is provided, derive missing core inputs so launch is possible.
  if (
    !is.null(fil_mzt_raw) &&
      (is.null(fil_fea_raw) ||
        is.null(fil_spe_raw) ||
        is.null(fil_met_raw))
  ) {
    mztab_plan <- .plan_mztab_app_outputs(
      mztab_path = fil_mzt_raw,
      data_source_path = paths_data_source,
      features_path = fil_fea_raw,
      spectra_path = fil_spe_raw,
      metadata_path = fil_met_raw
    )

    fil_fea_raw <- mztab_plan$resolved$features
    fil_spe_raw <- mztab_plan$resolved$spectra
    fil_met_raw <- mztab_plan$resolved$metadata

    if (mztab_plan$needs_read) {
      .log_trace("Rebuilding missing mzTab-derived source files ...")
      mztab_outputs <- tima::read_mztab(
        input = fil_mzt_raw,
        output_features = mztab_plan$write$features,
        output_spectra = mztab_plan$write$spectra,
        output_metadata = mztab_plan$write$metadata
      )

      if (is.null(fil_fea_raw)) {
        fil_fea_raw <- mztab_outputs$features
      }
      if (is.null(fil_spe_raw)) {
        fil_spe_raw <- mztab_outputs$spectra
      }
      if (is.null(fil_met_raw)) {
        fil_met_raw <- mztab_outputs$metadata
      }
    } else {
      .log_trace("Reusing cached mzTab-derived source files")
    }
  }
  lib_tmp_exp_csv <- .stage_multi_upload(
    upload_df = input$lib_tmp_exp_csv,
    dest_dir = file.path(paths_data_source, "libraries", "temporal", "exp")
  )
  lib_tmp_is_csv <- .stage_multi_upload(
    upload_df = input$lib_tmp_is_csv,
    dest_dir = file.path(paths_data_source, "libraries", "temporal", "is")
  )

  fil_pat <- input$fil_pat

  gnps_job_id <- .empty_to_null(input$gnps_id)
  if (!is.null(gnps_job_id) && is.null(fil_met_raw)) {
    fil_met_raw <- file.path(
      paths_data_source,
      paste0(gnps_job_id, "_metadata.tsv")
    )
  }

  org_tax <- .empty_to_null(input$org_tax)
  hig_evi <- input$high_evidence
  ms_pol <- input$ms_pol
  summarize <- input$summarize

  .log_trace("Changing parameters ...")
  .log_trace("... Small")
  yaml_small <- yamls_params[["prepare_params"]]
  yaml_small$files$pattern <- fil_pat
  yaml_small$files$features$raw <- fil_fea_raw
  yaml_small$files$metadata$raw <- fil_met_raw
  yaml_small$files$annotations$raw$spectral$mzmine <- fil_mzm_raw
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
    file = default_paths$params$prepare_params
  )

  par_small <- .get_params(step = "prepare_params")
  replace_path <- function(x) {
    .replace_id(
      x = x,
      user_filename = par_small$files$pattern,
      user_gnps = par_small$gnps$id,
      example_gnps = default_paths$gnps$example
    )
  }

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
    replace_path(yaml_advanced$files$annotations$raw$spectral$gnps)
  yaml_advanced$files$annotations$raw$spectral$mzmine <- fil_mzm_raw
  yaml_advanced$files$annotations$raw$spectral$spectral <-
    replace_path(yaml_advanced$files$annotations$raw$spectral$spectral)
  yaml_advanced$files$mztab$raw <- fil_mzt_raw
  yaml_advanced$files$annotations$raw$sirius <- fil_sir_raw
  yaml_advanced$files$annotations$prepared$canopus <-
    replace_path(yaml_advanced$files$annotations$prepared$canopus)
  yaml_advanced$files$annotations$prepared$formula <-
    replace_path(yaml_advanced$files$annotations$prepared$formula)
  yaml_advanced$files$annotations$prepared$structural$gnps <-
    replace_path(yaml_advanced$files$annotations$prepared$structural$gnps)
  yaml_advanced$files$annotations$prepared$structural$ms1 <-
    replace_path(yaml_advanced$files$annotations$prepared$structural$ms1)
  yaml_advanced$files$annotations$prepared$structural$sirius <-
    replace_path(yaml_advanced$files$annotations$prepared$structural$sirius)
  yaml_advanced$files$annotations$prepared$structural$spectral <-
    replace_path(yaml_advanced$files$annotations$prepared$structural$spectral)
  yaml_advanced$files$annotations$filtered <-
    replace_path(yaml_advanced$files$annotations$filtered)
  yaml_advanced$files$features$raw <-
    fil_fea_raw
  yaml_advanced$files$features$prepared <-
    replace_path(yaml_advanced$files$features$prepared)
  yaml_advanced$files$libraries$spectral$raw <- .stage_multi_upload(
    upload_df = input$lib_spe_mgf,
    dest_dir = file.path(paths_data_source, "libraries", "spectra", "exp"),
    default = yaml_advanced$files$libraries$spectral$raw
  )
  yaml_advanced$files$libraries$temporal$exp$csv <- lib_tmp_exp_csv
  yaml_advanced$files$libraries$temporal$is$csv <- lib_tmp_is_csv
  yaml_advanced$files$networks$spectral$edges$raw$ms1 <-
    replace_path(yaml_advanced$files$networks$spectral$edges$raw$ms1)
  yaml_advanced$files$networks$spectral$edges$raw$spectral <-
    replace_path(yaml_advanced$files$networks$spectral$edges$raw$spectral)
  yaml_advanced$files$networks$spectral$edges$prepared <-
    replace_path(yaml_advanced$files$networks$spectral$edges$prepared)
  yaml_advanced$files$networks$spectral$components$raw <-
    replace_path(yaml_advanced$files$networks$spectral$components$raw)
  yaml_advanced$files$networks$spectral$components$prepared <-
    replace_path(yaml_advanced$files$networks$spectral$components$prepared)
  yaml_advanced$files$metadata$raw <- fil_met_raw
  yaml_advanced$files$metadata$prepared <-
    replace_path(yaml_advanced$files$metadata$prepared)
  yaml_advanced$files$spectral$raw <- fil_spe_raw
  yaml_advanced$gnps$id <- gnps_job_id
  yaml_advanced$gnps$workflow <-
    input$gnps_workflow
  yaml_advanced$ms$adducts$neg <-
    input$ms_add_neg
  yaml_advanced$ms$adducts$pos <-
    input$ms_add_pos
  yaml_advanced$ms$clusters <- unique(as.character(input$ms_clu))
  yaml_advanced$ms$solvents <- unique(as.character(input$ms_sol))
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
  mgf_map <- c(
    adduct = "names_mgf_ad",
    collision_energy = "names_mgf_ce",
    compound_id = "names_mgf_ci",
    inchi = "names_mgf_in",
    inchi_no_stereo = "names_mgf_io",
    inchikey = "names_mgf_ik",
    inchikey_connectivity_layer = "names_mgf_il",
    name = "names_mgf_na",
    polarity = "names_mgf_po",
    retention_time = "names_mgf_rt",
    smiles = "names_mgf_sm",
    smiles_no_stereo = "names_mgf_sn",
    spectrum_id = "names_mgf_si",
    splash = "names_mgf_sp",
    synonyms = "names_mgf_sy"
  )
  for (nm in names(mgf_map)) {
    yaml_advanced$names$mgf[[nm]] <- input[[mgf_map[[nm]]]]
  }
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
  yaml_advanced$tools$sirius$version <-
    input$too_sir_ver
  yaml_advanced$tools$sirius$max_analog_abs_mz_error <-
    input$too_sir_max_analog_abs_mz_error
  yaml_advanced$units$rt <-
    input$uni_rt
  yaml_advanced$weights$global$biological <-
    input$wei_glo_bio
  yaml_advanced$weights$global$chemical <-
    input$wei_glo_che
  yaml_advanced$weights$global$spectral <-
    input$wei_glo_spe
  bio_ranks <- c(
    "domain",
    "kingdom",
    "phylum",
    "class",
    "order",
    "infraorder",
    "family",
    "subfamily",
    "tribe",
    "subtribe",
    "genus",
    "subgenus",
    "species",
    "subspecies",
    "variety",
    "biota"
  )
  for (i in seq_along(bio_ranks)) {
    yaml_advanced$weights$biological[[bio_ranks[[i]]]] <-
      input[[sprintf("wei_bio_%02d", i)]]
  }
  che_cla_map <- c(
    kingdom = "wei_che_11",
    superclass = "wei_che_12",
    class = "wei_che_13",
    parent = "wei_che_14"
  )
  for (nm in names(che_cla_map)) {
    yaml_advanced$weights$chemical$cla[[nm]] <- input[[che_cla_map[[nm]]]]
  }
  che_npc_map <- c(
    pathway = "wei_che_21",
    superclass = "wei_che_22",
    class = "wei_che_23"
  )
  for (nm in names(che_npc_map)) {
    yaml_advanced$weights$chemical$npc[[nm]] <- input[[che_npc_map[[nm]]]]
  }
  yaml_advanced$options$compounds_names <-
    input$compounds_names
  yaml_advanced$options$high_evidence <- hig_evi
  yaml_advanced$options$force <-
    input$force
  yaml_advanced$options$remove_ties <-
    input$remove_ties
  yaml_advanced$options$summarize <- summarize
  yaml::write_yaml(
    x = yaml_advanced,
    file = default_paths$params$prepare_params_advanced
  )
}

server <- function(input, output) {
  ## Observe helpers
  shinyhelper::observe_helpers()

  example_urls <- .get_default_paths()$urls$examples
  output$demo_spe <- .make_url_download(
    example_urls$spectra,
    example_urls$spectra_mini
  )
  output$demo_fea <- .make_url_download(example_urls$features)
  output$demo_met <- .make_url_download(example_urls$metadata)

  ## Enable Save/Launch depending on input mode:
  ## - classic mode: features + spectra (+ metadata or taxon)
  ## - mzTab mode: mzTab alone is enough; other files are optional overlays
  shiny::observe(x = {
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
        lapply(
          X = strsplit(x = taxon, split = "|", fixed = TRUE),
          FUN = function(taxon) {
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

  shiny::observeEvent(eventExpr = input$close, handlerExpr = {
    shiny::stopApp()
  })

  ## When the Save button is clicked, save the response
  shiny::observeEvent(eventExpr = input$save, handlerExpr = {
    ## User-experience stuff
    shinyjs::show("save_msg")
    shinyjs::disable("save")
    shinyjs::disable("launch")
    .hide_ids(c("job_msg", .RESULT_AND_STATUS_IDS))

    ## Save the data (show an error message in case of error)
    saved_ok <- FALSE
    tryCatch(
      expr = {
        .save_input(input = input)
        saved_ok <- TRUE
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
        shinyjs::hide("save_msg")
        if (saved_ok) {
          shinyjs::enable("launch")
        }
      }
    )
  })

  shiny::observeEvent(eventExpr = input$launch, handlerExpr = {
    shinyjs::show("job_msg")
    .hide_ids(c("params", "form", .RESULT_AND_STATUS_IDS))
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

    result_files <- utils::tail(
      list.files(
        path = "data/processed",
        pattern = "[.]tsv$",
        full.names = TRUE,
        recursive = TRUE
      ),
      n = 3L
    )
    result_slots <- c("filtered", "mini", "full")
    results <- stats::setNames(
      result_files,
      result_slots[seq_along(result_files)]
    )
    results_mztab <- targets::tar_read(exp_mzt)
    shinyjs::hide("job_msg")
    shinyjs::show("job_end")
    for (name in names(results)) {
      .register_result_download(
        output = output,
        output_id = paste0("results_", name),
        src_path = results[[name]]
      )
    }
    if (
      is.character(results_mztab) &&
        length(results_mztab) == 1L &&
        file.exists(results_mztab)
    ) {
      .register_result_download(
        output = output,
        output_id = "results_mztab",
        src_path = results_mztab
      )
    }
    shinyjs::show("close")
  })
}
