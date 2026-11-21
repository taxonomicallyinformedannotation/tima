# Benchmark targets section.

targets_section_benchmark <- function() {
  list(
    tar_target(name = benchmark_path_mgf_neg, command = {
      paths$data$source$benchmark$mgf$neg
    }),
    tar_target(name = benchmark_path_mgf_pos, command = {
      paths$data$source$benchmark$mgf$pos
    }),
    tar_target(
      name = benchmark_file,
      command = {
        c(lib_spe_exp_gnp_pre_pos, lib_spe_exp_gnp_pre_neg)
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_converted,
      command = {
        sp <- getFromNamespace("import_and_clean_library_collection", "tima")(
          benchmark_file,
          dalton = 0.01,
          polarity = NA,
          ppm = 10
        )
        getFromNamespace("export_spectra_rds", "tima")(
          Spectra::filterEmptySpectra(sp),
          file = "data/interim/benchmark/benchmark_spectra.rds"
        )
        "data/interim/benchmark/benchmark_spectra.rds"
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_prepared,
      command = {
        .ik_short <- function(x) sub("-.*", "", x)
        sp <- import_spectra(benchmark_converted, sanitize = FALSE)
        sp@backend@spectraData$precursorMz <- as.numeric(
          sp@backend@spectraData$precursor_mz
        )
        getFromNamespace("log_trace", "tima")("Imported")
        getFromNamespace("log_trace", "tima")("Cleaned")
        df_meta <- tidytable::tidytable(
          adduct = sp$adduct,
          inchikey = sp$inchikey,
          fragments = lengths(sp@backend@peaksData) / 2,
          precursorMz = sp$precursorMz,
          smiles = sp$smiles,
          ccmslib = sp$spectrum_id,
          charge = sp$precursorCharge,
          name = sp$name
        ) |>
          tidytable::mutate(
            tidytable::across(
              .cols = tidyselect::everything(),
              .fns = function(x) tidytable::na_if(x, "")
            )
          )
        getFromNamespace("log_trace", "tima")("Framed")
        df_clean <- df_meta |>
          tidytable::filter(
            !is.na(inchikey),
            fragments >= 5,
            fragments <= 250,
            !grepl(pattern = "ReSpect", x = name, fixed = TRUE),
            !grepl(pattern = "Spectral Match to", x = name, fixed = TRUE),
            !grepl(pattern = "putative", x = name, fixed = TRUE)
          ) |>
          tidytable::select(-name) |>
          tidytable::mutate(mass = precursorMz) |>
          tidytable::separate(
            col = mass,
            sep = "\\.",
            into = c("a", "b")
          ) |>
          tidytable::filter(!is.na(b)) |>
          tidytable::filter(nchar(as.character(as.numeric(b))) > 1L) |>
          tidytable::select(-a, -b) |>
          tidytable::mutate(
            inchikey_connectivity_layer = .ik_short(inchikey)
          ) |>
          tidytable::distinct(
            inchikey_connectivity_layer,
            adduct,
            .keep_all = TRUE
          ) |>
          tidytable::mutate(mz = precursorMz) |>
          tidytable::mutate(
            rt = tidytable::cur_group_id(),
            .by = "inchikey_connectivity_layer"
          )
        df_clean_neg <- tidytable::filter(
          df_clean,
          grepl(pattern = "]-", x = adduct, fixed = TRUE)
        )
        df_clean_pos <- tidytable::filter(
          df_clean,
          grepl(pattern = "]+", x = adduct, fixed = TRUE)
        )
        sp_pos <- sp[sp$spectrum_id %in% df_clean_pos$ccmslib]
        sp_neg <- sp[sp$spectrum_id %in% df_clean_neg$ccmslib]
        extract_benchmark_spectra <- function(x, mode) {
          getFromNamespace("extract_spectra", "tima")(x) |>
            tidytable::mutate(acquisitionNum = tidytable::row_number()) |>
            tidytable::mutate(spectrum_id = acquisitionNum) |>
            tidytable::mutate(
              short_ik = .ik_short(inchikey)
            ) |>
            tidytable::mutate(
              rtime = tidytable::cur_group_id(),
              .by = "short_ik"
            ) |>
            tidytable::mutate(
              precursorCharge = if (mode == "pos") 1L else -1L,
              MS_LEVEL = 2L
            ) |>
            tidytable::select(
              acquisitionNum,
              precursorCharge,
              precursorMz,
              MS_LEVEL,
              rtime,
              name,
              smiles,
              inchi,
              inchikey,
              adduct,
              ccmslib = spectrum_id,
              spectrum_id = acquisitionNum,
              mz,
              intensity
            ) |>
            data.frame()
        }
        spectra_harmonized_pos <- extract_benchmark_spectra(
          sp_pos,
          mode = "pos"
        )
        spectra_harmonized_neg <- extract_benchmark_spectra(
          sp_neg,
          mode = "neg"
        )
        select_benchmark_columns <- function(x) {
          x |>
            tidytable::select(
              adduct,
              inchikey,
              smiles,
              ccmslib,
              charge = precursorCharge,
              mz = precursorMz,
              rt = rtime,
              feature_id = spectrum_id
            ) |>
            tidytable::mutate(
              inchikey_connectivity_layer = .ik_short(inchikey)
            ) |>
            data.frame()
        }
        df_clean_pos <- select_benchmark_columns(spectra_harmonized_pos)
        df_clean_neg <- select_benchmark_columns(spectra_harmonized_neg)
        Spectra::export(
          Spectra::Spectra(spectra_harmonized_pos),
          backend = MsBackendMgf::MsBackendMgf(),
          file = benchmark_path_mgf_pos
        )
        Spectra::export(
          Spectra::Spectra(spectra_harmonized_neg),
          backend = MsBackendMgf::MsBackendMgf(),
          file = benchmark_path_mgf_neg
        )
        getFromNamespace("export_output", "tima")(
          df_clean_pos,
          "data/interim/benchmark/benchmark_meta_pos.tsv"
        )
        getFromNamespace("export_output", "tima")(
          df_clean_neg,
          "data/interim/benchmark/benchmark_meta_neg.tsv"
        )
        c(
          spectra_pos = benchmark_path_mgf_pos,
          spectra_neg = benchmark_path_mgf_neg,
          meta_pos = "data/interim/benchmark/benchmark_meta_pos.tsv",
          meta_neg = "data/interim/benchmark/benchmark_meta_neg.tsv"
        )
      },
      format = "file"
    ),
    .tar_file_slice(
      "benchmark_prepared",
      c(
        benchmark_pre_mgf_pos = 1L,
        benchmark_pre_mgf_neg = 2L,
        benchmark_pre_meta_pos = 3L,
        benchmark_pre_meta_neg = 4L
      )
    ),
    lapply(
      c(
        "ann_mas",
        "cre_edg_spe",
        "pre_fea_edg",
        "ann_spe",
        "pre_ann_sir",
        "fil_ann",
        "wei_ann"
      ),
      function(step_abbr) {
        tar_target_raw(
          name = paste0("benchmark_def_", step_abbr),
          command = substitute(
            getFromNamespace("parse_yaml_params", "tima")(
              def = par_def,
              usr = par_def
            ),
            list(par_def = as.symbol(paste0("par_def_", step_abbr)))
          ),
          format = "rds"
        )
      }
    ),
    lapply(c("pos", "neg"), function(pol) {
      pre_mgf <- as.symbol(paste0("benchmark_pre_mgf_", pol))
      pre_meta <- as.symbol(paste0("benchmark_pre_meta_", pol))
      taxed <- as.symbol(paste0("benchmark_taxed_", pol))
      ms1_pre <- as.symbol(paste0("benchmark_ann_ms1_pre_", pol))
      edg_spe <- as.symbol(paste0("benchmark_edg_spe_", pol))
      edg_pre <- as.symbol(paste0("benchmark_edg_pre_", pol))
      com <- as.symbol(paste0("benchmark_com_", pol))
      com_pre <- as.symbol(paste0("benchmark_com_pre_", pol))
      ann_spe <- as.symbol(paste0("benchmark_ann_spe_", pol))
      ann_spe_pre <- as.symbol(paste0("benchmark_ann_spe_pre_", pol))
      spe_libs <- as.call(c(
        quote(c),
        lapply(
          paste0(
            c(
              "lib_spe_is_nor_pre_",
              "lib_spe_is_wik_pre_",
              "lib_spe_exp_int_pre_",
              "lib_spe_exp_mb_pre_",
              "lib_spe_exp_mer_pre_"
            ),
            pol
          ),
          as.symbol
        )
      ))
      list(
        tar_target_raw(
          name = paste0("benchmark_taxed_", pol),
          command = substitute(
            getFromNamespace("benchmark_taxize_spectra", "tima")(
              pre_meta,
              keys = lib_mer_key,
              org_tax_ott = lib_mer_org_tax_ott,
              output = out_path
            ),
            list(
              pre_meta = pre_meta,
              out_path = sprintf(
                "data/interim/benchmark/benchmark_taxed_%s.tsv.gz",
                pol
              )
            )
          ),
          format = "file"
        ),
        tar_target_raw(
          name = paste0("benchmark_ann_ms1_pre_", pol),
          command = substitute(
            annotate_masses(
              features = pre_meta,
              library = lib_mer_key,
              output_annotations = out_ann,
              output_edges = out_edg,
              name_source = benchmark_def_ann_mas$names$source,
              name_target = benchmark_def_ann_mas$names$target,
              str_stereo = lib_mer_str_stereo,
              str_met = lib_mer_str_met,
              str_tax_cla = lib_mer_str_tax_cla,
              str_tax_npc = lib_mer_str_tax_npc,
              adducts_list = par_ann_mas$ms$adducts,
              clusters_list = par_ann_mas$ms$clusters,
              solvents_list = par_ann_mas$ms$solvents,
              neutral_losses_list = par_ann_mas$ms$neutral_losses,
              ms_mode = pol,
              tolerance_ppm = benchmark_def_ann_mas$ms$tolerances$mass$ppm$ms1,
              tolerance_rt = benchmark_def_ann_mas$ms$tolerances$rt$adducts
            ),
            list(
              pre_meta = pre_meta,
              pol = pol,
              out_ann = sprintf(
                "data/interim/benchmark/benchmark_ann_ms1_%s.tsv.gz",
                pol
              ),
              out_edg = sprintf(
                "data/interim/benchmark/benchmark_edges_ms1_%s.tsv.gz",
                pol
              )
            )
          ),
          format = "file"
        ),
        tar_target_raw(
          name = paste0("benchmark_edg_spe_", pol),
          command = substitute(
            create_edges_spectra(
              input = pre_mgf,
              output = out_path,
              name_source = benchmark_def_cre_edg_spe$names$source,
              name_target = benchmark_def_cre_edg_spe$names$target,
              ppm = benchmark_def_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
              dalton = benchmark_def_cre_edg_spe$ms$tolerances$mass$dalton$ms2,
              cutoff = 0
            ),
            list(
              pre_mgf = pre_mgf,
              out_path = sprintf(
                "data/interim/benchmark/benchmark_edges_spe_%s.tsv.gz",
                pol
              )
            )
          ),
          format = "file"
        ),
        tar_target_raw(
          name = paste0("benchmark_edg_pre_", pol),
          command = substitute(
            prepare_features_edges(
              input = list(
                spectral = edg_spe,
                ms1 = ms1_pre[[2L]]
              ),
              output = out_path,
              name_source = benchmark_def_pre_fea_edg$names$source,
              name_target = benchmark_def_pre_fea_edg$names$target
            ),
            list(
              edg_spe = edg_spe,
              ms1_pre = ms1_pre,
              out_path = sprintf(
                "data/interim/benchmark/benchmark_edges_%s.tsv.gz",
                pol
              )
            )
          ),
          format = "file"
        ),
        tar_target_raw(
          name = paste0("benchmark_com_", pol),
          command = substitute(
            create_components(input = edg_pre, output = out_path),
            list(
              edg_pre = edg_pre,
              out_path = sprintf(
                "data/interim/benchmark/benchmark_components_%s.tsv.gz",
                pol
              )
            )
          ),
          format = "file"
        ),
        tar_target_raw(
          name = paste0("benchmark_com_pre_", pol),
          command = substitute(
            prepare_features_components(input = com, output = out_path),
            list(
              com = com,
              out_path = sprintf(
                "data/interim/benchmark/benchmark_com_pre_%s.tsv.gz",
                pol
              )
            )
          ),
          format = "file"
        ),
        tar_target_raw(
          name = paste0("benchmark_ann_spe_", pol),
          command = substitute(
            annotate_spectra(
              input = pre_mgf,
              libraries = spe_libs,
              polarity = pol,
              output = out_path,
              method = benchmark_def_ann_spe$similarities$methods$annotations,
              threshold = benchmark_def_ann_spe$similarities$thresholds$annotations,
              ppm = benchmark_def_ann_spe$ms$tolerances$mass$ppm$ms2,
              dalton = benchmark_def_ann_spe$ms$tolerances$mass$dalton$ms2,
              cutoff = 0,
              approx = benchmark_def_ann_spe$annotations$ms2approx
            ),
            list(
              pre_mgf = pre_mgf,
              spe_libs = spe_libs,
              pol = pol,
              out_path = sprintf(
                "data/interim/benchmark/benchmark_ann_spe_%s.tsv.gz",
                pol
              )
            )
          ),
          format = "file"
        ),
        tar_target_raw(
          name = paste0("benchmark_ann_spe_pre_", pol),
          command = substitute(
            prepare_annotations_spectra(
              input = ann_spe,
              output = out_path,
              str_stereo = lib_mer_str_stereo,
              str_met = lib_mer_str_met,
              str_tax_cla = lib_mer_str_tax_cla,
              str_tax_npc = lib_mer_str_tax_npc
            ),
            list(
              ann_spe = ann_spe,
              out_path = sprintf(
                "data/interim/benchmark/benchmark_ann_spe_pre_%s.tsv.gz",
                pol
              )
            )
          ),
          format = "file"
        ),
        tar_target_raw(
          name = paste0("benchmark_files_", pol),
          command = substitute(
            list(
              components = com_pre,
              edges = edg_pre,
              taxa = taxed
            ),
            list(com_pre = com_pre, edg_pre = edg_pre, taxed = taxed)
          )
        )
      )
    }),
    tar_target(
      name = benchmark_ann_sir_pre,
      command = {
        prepare_annotations_sirius(
          input_directory = "doesNotExist4Now",
          output_ann = "data/interim/benchmark/benchmark_ann_sir_pre.tsv.gz",
          output_can = "data/interim/benchmark/benchmark_ann_sir_pre_can.tsv.gz",
          output_for = "data/interim/benchmark/benchmark_ann_sir_pre_for.tsv.gz",
          max_analog_abs_mz_error = benchmark_def_pre_ann_sir$tools$sirius$max_analog_abs_mz_error,
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc
        )
      },
      format = "file"
    ),
    .tar_file_slice(
      "benchmark_ann_sir_pre",
      c(
        benchmark_ann_sir_pre_can = 1L,
        benchmark_ann_sir_pre_for = 2L,
        benchmark_ann_sir_pre_str = 3L
      )
    ),
    lapply(c("neg", "pos"), function(pol) {
      spe_pre <- as.symbol(paste0("benchmark_ann_spe_pre_", pol))
      ms1_pre_ann <- call(
        "[[",
        as.symbol(paste0("benchmark_ann_ms1_pre_", pol)),
        1L
      )
      feat_sym <- as.symbol(paste0("benchmark_pre_meta_", pol))
      fil_specs <- list(
        spe = list(
          anns = substitute(
            c(spe_pre, benchmark_ann_sir_pre_str),
            list(spe_pre = spe_pre)
          ),
          out = "spe"
        ),
        spe_ms1 = list(
          anns = substitute(
            c(spe_pre, ms1_pre_ann, benchmark_ann_sir_pre_str),
            list(spe_pre = spe_pre, ms1_pre_ann = ms1_pre_ann)
          ),
          out = "spe_ms1"
        ),
        ms1 = list(
          anns = substitute(
            c(ms1_pre_ann, benchmark_ann_sir_pre_str),
            list(ms1_pre_ann = ms1_pre_ann)
          ),
          out = "ms1"
        )
      )
      lapply(names(fil_specs), function(kind) {
        spec <- fil_specs[[kind]]
        tar_target_raw(
          name = sprintf("benchmark_ann_fil_%s_%s", kind, pol),
          command = substitute(
            filter_annotations(
              annotations = anns,
              features = feat_sym,
              rts = list(),
              output = out_path,
              tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
            ),
            list(
              anns = spec$anns,
              feat_sym = feat_sym,
              out_path = sprintf(
                "data/interim/benchmark/benchmark_ann_%s_fil_%s.tsv.gz",
                spec$out,
                pol
              )
            )
          ),
          format = "file"
        )
      })
    }),
    tar_target(name = benchmark_wei_par, command = {
      list(
        canopus = benchmark_ann_sir_pre_can,
        formula = benchmark_ann_sir_pre_for,
        library = lib_mer_key,
        org_tax_ott = lib_mer_org_tax_ott,
        str_stereo = lib_mer_str_stereo,
        candidates_final = 500,
        score_biological_domain = benchmark_def_wei_ann$weights$biological$domain,
        score_biological_kingdom = benchmark_def_wei_ann$weights$biological$kingdom,
        score_biological_phylum = benchmark_def_wei_ann$weights$biological$phylum,
        score_biological_class = benchmark_def_wei_ann$weights$biological$class,
        score_biological_order = benchmark_def_wei_ann$weights$biological$order,
        score_biological_infraorder = benchmark_def_wei_ann$weights$biological$infraorder,
        score_biological_family = benchmark_def_wei_ann$weights$biological$family,
        score_biological_subfamily = benchmark_def_wei_ann$weights$biological$subfamily,
        score_biological_tribe = benchmark_def_wei_ann$weights$biological$tribe,
        score_biological_subtribe = benchmark_def_wei_ann$weights$biological$subtribe,
        score_biological_genus = benchmark_def_wei_ann$weights$biological$genus,
        score_biological_subgenus = benchmark_def_wei_ann$weights$biological$subgenus,
        score_biological_species = benchmark_def_wei_ann$weights$biological$species,
        score_biological_subspecies = benchmark_def_wei_ann$weights$biological$subspecies,
        score_biological_variety = benchmark_def_wei_ann$weights$biological$variety,
        score_chemical_cla_kingdom = benchmark_def_wei_ann$weights$chemical$cla$kingdom,
        score_chemical_cla_superclass = benchmark_def_wei_ann$weights$chemical$cla$superclass,
        score_chemical_cla_class = benchmark_def_wei_ann$weights$chemical$cla$class,
        score_chemical_cla_parent = benchmark_def_wei_ann$weights$chemical$cla$parent,
        score_chemical_npc_pathway = benchmark_def_wei_ann$weights$chemical$npc$pathway,
        score_chemical_npc_superclass = benchmark_def_wei_ann$weights$chemical$npc$superclass,
        score_chemical_npc_class = benchmark_def_wei_ann$weights$chemical$npc$class,
        minimal_consistency = benchmark_def_wei_ann$annotations$thresholds$consistency,
        minimal_ms1_bio = benchmark_def_wei_ann$annotations$thresholds$ms1$biological,
        minimal_ms1_chemo = benchmark_def_wei_ann$annotations$thresholds$ms1$chemical,
        minimal_ms1_condition = benchmark_def_wei_ann$annotations$thresholds$ms1$condition,
        compounds_names = benchmark_def_wei_ann$options$compounds_names,
        high_evidence = FALSE,
        remove_ties = benchmark_def_wei_ann$options$remove_ties,
        summarize = benchmark_def_wei_ann$options$summarize,
        pattern = benchmark_def_wei_ann$files$pattern,
        force = benchmark_def_wei_ann$options$force
      )
    }),
    lapply(c("pos", "neg"), function(pol) {
      files_sym <- as.symbol(paste0("benchmark_files_", pol))
      wei_specs <- list(
        list("ms2_b", "spe", 0, 0.666, "ms2_bio"),
        list("ms1_ms2_b", "spe_ms1", 0, 0.666, "ms1_ms2_bio"),
        list("ms2_b_c", "spe", 0.166, 0.5, "ms2_bio_chemo"),
        list("ms1_ms2_b_c", "spe_ms1", 0.166, 0.5, "ms1_ms2_bio_chemo")
      )
      lapply(wei_specs, function(s) {
        tar_target_raw(
          name = sprintf("benchmark_ann_pre_%s_%s", s[[1L]], pol),
          command = substitute(
            do.call(
              what = weight_annotations,
              args = c(
                benchmark_wei_par,
                files_sym,
                annotations = ann_sym,
                ms1_only = FALSE,
                weight_spectral = 0.333,
                weight_chemical = w_che,
                weight_biological = w_bio,
                output = out_path
              )
            ),
            list(
              files_sym = files_sym,
              ann_sym = as.symbol(sprintf(
                "benchmark_ann_fil_%s_%s",
                s[[2L]],
                pol
              )),
              w_che = s[[3L]],
              w_bio = s[[4L]],
              out_path = sprintf("benchmark_lotus_%s_%s.tsv.gz", s[[5L]], pol)
            )
          ),
          format = "file"
        )
      })
    })
  )
}
