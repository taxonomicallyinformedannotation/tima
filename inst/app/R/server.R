source("R/save_input.R")

options(shiny.maxRequestSize = 1000 * 1024^2)

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
            "paths_gnps_example_id",
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
          names = matches("^ann_pre$"),
          garbage_collection = TRUE,
          reporter = "verbose_positives"
        )
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
