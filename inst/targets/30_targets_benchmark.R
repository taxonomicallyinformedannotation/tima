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
        sp <- tima:::import_and_clean_library_collection(
          benchmark_file,
          dalton = 0.01,
          polarity = NA,
          ppm = 10
        )
        tima:::export_spectra_rds(
          Spectra::filterEmptySpectra(sp),
          file = "data/interim/benchmark/benchmark_spectra.rds"
        )
        return("data/interim/benchmark/benchmark_spectra.rds")
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_prepared,
      command = {
        sp <- import_spectra(benchmark_converted, sanitize = FALSE)
        sp@backend@spectraData$precursorMz <- as.numeric(
          sp@backend@spectraData$precursor_mz
        )
        tima:::log_trace("Imported")
        sp_clean <- sp
        tima:::log_trace("Cleaned")
        df_meta <- tidytable::mutate(
          tidytable::tidytable(
            adduct = sp_clean$adduct,
            inchikey = sp_clean$inchikey,
            fragments = as.numeric(as.character(purrr::map(
              .x = sp_clean@backend@peaksData,
              .f = length
            ))) /
              2,
            precursorMz = sp_clean$precursorMz,
            smiles = sp_clean$smiles,
            ccmslib = sp_clean$spectrum_id,
            charge = sp_clean$precursorCharge,
            name = sp_clean$name
          ),
          tidytable::across(
            .cols = tidyselect::everything(),
            .fns = function(x) {
              tidytable::na_if(x, "")
            }
          )
        )
        tima:::log_trace("Framed")
        df_clean <- tidytable::mutate(
          tidytable::mutate(
            tidytable::distinct(
              tidytable::mutate(
                tidytable::select(
                  tidytable::filter(
                    tidytable::filter(
                      tidytable::separate(
                        tidytable::mutate(
                          tidytable::select(
                            tidytable::filter(
                              tidytable::filter(
                                tidytable::filter(
                                  tidytable::filter(
                                    tidytable::filter(
                                      tidytable::filter(
                                        df_meta,
                                        !is.na(inchikey)
                                      ),
                                      fragments >= 5
                                    ),
                                    fragments <= 250
                                  ),
                                  !grepl(
                                    pattern = "ReSpect",
                                    x = name,
                                    fixed = TRUE
                                  )
                                ),
                                !grepl(
                                  pattern = "Spectral Match to",
                                  x = name,
                                  fixed = TRUE
                                )
                              ),
                              !grepl(
                                pattern = "putative",
                                x = name,
                                fixed = TRUE
                              )
                            ),
                            -name
                          ),
                          mass = precursorMz
                        ),
                        col = mass,
                        sep = "\\.",
                        into = c("a", "b")
                      ),
                      !is.na(b)
                    ),
                    stringi::stri_length(as.numeric(b)) > 1
                  ),
                  -a,
                  -b
                ),
                inchikey_connectivity_layer = gsub(
                  pattern = "-.*",
                  replacement = "",
                  x = inchikey,
                  perl = TRUE
                )
              ),
              inchikey_connectivity_layer,
              adduct,
              .keep_all = TRUE
            ),
            mz = precursorMz
          ),
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
        sp_pos <- sp_clean[sp_clean$spectrum_id %in% df_clean_pos$ccmslib]
        sp_neg <- sp_clean[sp_clean$spectrum_id %in% df_clean_neg$ccmslib]
        extract_benchmark_spectra <- function(x, mode) {
          df <- data.frame(tidytable::select(
            tidytable::mutate(
              tidytable::mutate(
                tidytable::mutate(
                  tidytable::mutate(
                    tidytable::mutate(
                      tidytable::mutate(
                        tima:::extract_spectra(x),
                        acquisitionNum = tidytable::row_number()
                      ),
                      spectrum_id = acquisitionNum
                    ),
                    short_ik = gsub(
                      pattern = "-.*",
                      replacement = "",
                      inchikey,
                      perl = TRUE
                    )
                  ),
                  rtime = tidytable::cur_group_id(),
                  .by = "short_ik"
                ),
                precursorCharge = tidytable::if_else(
                  condition = mode == "pos",
                  true = as.integer(1),
                  false = as.integer(-1)
                )
              ),
              MS_LEVEL = 2L
            ),
            acquisitionNum,
            precursorCharge,
            precursorMz,
            MS_LEVEL,
            rtime,
            name = name,
            smiles = smiles,
            inchi = inchi,
            inchikey = inchikey,
            adduct = adduct,
            ccmslib = spectrum_id,
            spectrum_id = acquisitionNum,
            mz,
            intensity
          ))
          df
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
          df <- data.frame(tidytable::mutate(
            tidytable::select(
              x,
              adduct,
              inchikey,
              smiles,
              ccmslib,
              charge = precursorCharge,
              mz = precursorMz,
              rt = rtime,
              feature_id = spectrum_id
            ),
            inchikey_connectivity_layer = gsub(
              pattern = "-.*",
              replacement = "",
              x = inchikey,
              perl = TRUE
            )
          ))
          df
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
        tima:::export_output(
          df_clean_pos,
          "data/interim/benchmark/benchmark_meta_pos.tsv"
        )
        tima:::export_output(
          df_clean_neg,
          "data/interim/benchmark/benchmark_meta_neg.tsv"
        )
        return(c(
          spectra_pos = benchmark_path_mgf_pos,
          spectra_neg = benchmark_path_mgf_neg,
          meta_pos = "data/interim/benchmark/benchmark_meta_pos.tsv",
          meta_neg = "data/interim/benchmark/benchmark_meta_neg.tsv"
        ))
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_pre_mgf_pos,
      command = {
        benchmark_prepared[[1L]]
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_pre_mgf_neg,
      command = {
        benchmark_prepared[[2L]]
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_pre_meta_pos,
      command = {
        benchmark_prepared[[3L]]
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_pre_meta_neg,
      command = {
        benchmark_prepared[[4L]]
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_taxed_pos,
      command = {
        tima:::benchmark_taxize_spectra(
          benchmark_pre_meta_pos,
          keys = lib_mer_key,
          org_tax_ott = lib_mer_org_tax_ott,
          output = "data/interim/benchmark/benchmark_taxed_pos.tsv.gz"
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_taxed_neg,
      command = {
        tima:::benchmark_taxize_spectra(
          benchmark_pre_meta_neg,
          keys = lib_mer_key,
          org_tax_ott = lib_mer_org_tax_ott,
          output = "data/interim/benchmark/benchmark_taxed_neg.tsv.gz"
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_def_ann_mas,
      command = {
        tima:::parse_yaml_params(def = par_def_ann_mas, usr = par_def_ann_mas)
      },
      format = "rds"
    ),
    tar_target(
      name = benchmark_ann_ms1_pre_pos,
      command = {
        annotate_masses(
          features = benchmark_pre_meta_pos,
          library = lib_mer_key,
          output_annotations = "data/interim/benchmark/benchmark_ann_ms1_pos.tsv.gz",
          output_edges = "data/interim/benchmark/benchmark_edges_ms1_pos.tsv.gz",
          name_source = benchmark_def_ann_mas$names$source,
          name_target = benchmark_def_ann_mas$names$target,
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc,
          adducts_list = par_ann_mas$ms$adducts,
          clusters_list = par_ann_mas$ms$clusters,
          neutral_losses_list = par_ann_mas$ms$neutral_losses,
          ms_mode = "pos",
          tolerance_ppm = benchmark_def_ann_mas$ms$tolerances$mass$ppm$ms1,
          tolerance_rt = benchmark_def_ann_mas$ms$tolerances$rt$adducts,
          adduct_consistency = benchmark_def_ann_mas$ms$adducts$consistency$type,
          adduct_min_support = benchmark_def_ann_mas$ms$adducts$consistency$min_support,
          adduct_consistency_min_degree = benchmark_def_ann_mas$ms$adducts$consistency$min_degree
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_ms1_pre_neg,
      command = {
        annotate_masses(
          features = benchmark_pre_meta_neg,
          library = lib_mer_key,
          output_annotations = "data/interim/benchmark/benchmark_ann_ms1_neg.tsv.gz",
          output_edges = "data/interim/benchmark/benchmark_edges_ms1_neg.tsv.gz",
          name_source = benchmark_def_ann_mas$names$source,
          name_target = benchmark_def_ann_mas$names$target,
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc,
          adducts_list = par_ann_mas$ms$adducts,
          clusters_list = par_ann_mas$ms$clusters,
          neutral_losses_list = par_ann_mas$ms$neutral_losses,
          ms_mode = "neg",
          tolerance_ppm = benchmark_def_ann_mas$ms$tolerances$mass$ppm$ms1,
          tolerance_rt = benchmark_def_ann_mas$ms$tolerances$rt$adducts,
          adduct_consistency = benchmark_def_ann_mas$ms$adducts$consistency$type,
          adduct_min_support = benchmark_def_ann_mas$ms$adducts$consistency$min_support,
          adduct_consistency_min_degree = benchmark_def_ann_mas$ms$adducts$consistency$min_degree
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_def_cre_edg_spe,
      command = {
        tima:::parse_yaml_params(
          def = par_def_cre_edg_spe,
          usr = par_def_cre_edg_spe
        )
      },
      format = "rds"
    ),
    tar_target(
      name = benchmark_edg_spe_pos,
      command = {
        create_edges_spectra(
          input = benchmark_pre_mgf_pos,
          output = "data/interim/benchmark/benchmark_edges_spe_pos.tsv.gz",
          name_source = benchmark_def_cre_edg_spe$names$source,
          name_target = benchmark_def_cre_edg_spe$names$target,
          threshold = benchmark_def_cre_edg_spe$similarities$thresholds$edges,
          matched_peaks = benchmark_def_cre_edg_spe$similarities$thresholds$matched_peaks,
          ppm = benchmark_def_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_cre_edg_spe$ms$tolerances$mass$dalton$ms2,
          cutoff = 0
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_edg_spe_neg,
      command = {
        create_edges_spectra(
          input = benchmark_pre_mgf_neg,
          output = "data/interim/benchmark/benchmark_edges_spe_neg.tsv.gz",
          name_source = benchmark_def_cre_edg_spe$names$source,
          name_target = benchmark_def_cre_edg_spe$names$target,
          threshold = benchmark_def_cre_edg_spe$similarities$thresholds$edges,
          matched_peaks = benchmark_def_cre_edg_spe$similarities$thresholds$matched_peaks,
          ppm = benchmark_def_cre_edg_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_cre_edg_spe$ms$tolerances$mass$dalton$ms2,
          cutoff = 0
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_def_pre_fea_edg,
      command = {
        tima:::parse_yaml_params(
          def = par_def_pre_fea_edg,
          usr = par_def_pre_fea_edg
        )
      },
      format = "rds"
    ),
    tar_target(
      name = benchmark_edg_pre_pos,
      command = {
        prepare_features_edges(
          input = list(
            spectral = benchmark_edg_spe_pos,
            ms1 = benchmark_ann_ms1_pre_pos[[2L]]
          ),
          output = "data/interim/benchmark/benchmark_edges_pos.tsv.gz",
          name_source = benchmark_def_pre_fea_edg$names$source,
          name_target = benchmark_def_pre_fea_edg$names$target
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_edg_pre_neg,
      command = {
        prepare_features_edges(
          input = list(
            spectral = benchmark_edg_spe_neg,
            ms1 = benchmark_ann_ms1_pre_neg[[2L]]
          ),
          output = "data/interim/benchmark/benchmark_edges_neg.tsv.gz",
          name_source = benchmark_def_pre_fea_edg$names$source,
          name_target = benchmark_def_pre_fea_edg$names$target
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_def_cre_edg_com,
      command = {
        tima:::parse_yaml_params(def = par_def_cre_com, usr = par_def_cre_com)
      },
      format = "rds"
    ),
    tar_target(
      name = benchmark_com_pos,
      command = {
        create_components(
          input = benchmark_edg_pre_pos,
          output = "data/interim/benchmark/benchmark_components_pos.tsv.gz"
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_com_neg,
      command = {
        create_components(
          input = benchmark_edg_pre_neg,
          output = "data/interim/benchmark/benchmark_components_neg.tsv.gz"
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_def_pre_fea_com,
      command = {
        tima:::parse_yaml_params(
          def = par_def_pre_fea_com,
          usr = par_def_pre_fea_com
        )
      },
      format = "rds"
    ),
    tar_target(
      name = benchmark_com_pre_pos,
      command = {
        prepare_features_components(
          input = benchmark_com_pos,
          output = "data/interim/benchmark/benchmark_com_pre_pos.tsv.gz"
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_com_pre_neg,
      command = {
        prepare_features_components(
          input = benchmark_com_neg,
          output = "data/interim/benchmark/benchmark_com_pre_neg.tsv.gz"
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_def_ann_spe,
      command = {
        tima:::parse_yaml_params(def = par_def_ann_spe, usr = par_def_ann_spe)
      },
      format = "rds"
    ),
    tar_target(
      name = benchmark_ann_spe_pos,
      command = {
        annotate_spectra(
          input = benchmark_pre_mgf_pos,
          libraries = c(
            lib_spe_is_nor_pre_pos,
            lib_spe_is_wik_pre_pos,
            lib_spe_exp_int_pre_pos,
            lib_spe_exp_mb_pre_pos,
            lib_spe_exp_mer_pre_pos
          ),
          polarity = "pos",
          output = "data/interim/benchmark/benchmark_ann_spe_pos.tsv.gz",
          method = benchmark_def_ann_spe$similarities$methods$annotations,
          threshold = benchmark_def_ann_spe$similarities$thresholds$annotations,
          ppm = benchmark_def_ann_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_ann_spe$ms$tolerances$mass$dalton$ms2,
          cutoff = 0,
          approx = benchmark_def_ann_spe$annotations$ms2approx
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_spe_neg,
      command = {
        annotate_spectra(
          input = benchmark_pre_mgf_neg,
          libraries = c(
            lib_spe_is_nor_pre_neg,
            lib_spe_is_wik_pre_neg,
            lib_spe_exp_int_pre_neg,
            lib_spe_exp_mb_pre_neg,
            lib_spe_exp_mer_pre_neg
          ),
          polarity = "neg",
          output = "data/interim/benchmark/benchmark_ann_spe_neg.tsv.gz",
          method = benchmark_def_ann_spe$similarities$methods$annotations,
          threshold = benchmark_def_ann_spe$similarities$thresholds$annotations,
          ppm = benchmark_def_ann_spe$ms$tolerances$mass$ppm$ms2,
          dalton = benchmark_def_ann_spe$ms$tolerances$mass$dalton$ms2,
          cutoff = 0,
          approx = benchmark_def_ann_spe$annotations$ms2approx
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_def_pre_ann_spe,
      command = {
        tima:::parse_yaml_params(
          def = par_def_pre_ann_spe,
          usr = par_def_pre_ann_spe
        )
      },
      format = "rds"
    ),
    tar_target(
      name = benchmark_ann_spe_pre_pos,
      command = {
        prepare_annotations_spectra(
          input = benchmark_ann_spe_pos,
          output = "data/interim/benchmark/benchmark_ann_spe_pre_pos.tsv.gz",
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_spe_pre_neg,
      command = {
        prepare_annotations_spectra(
          input = benchmark_ann_spe_neg,
          output = "data/interim/benchmark/benchmark_ann_spe_pre_neg.tsv.gz",
          str_stereo = lib_mer_str_stereo,
          str_met = lib_mer_str_met,
          str_tax_cla = lib_mer_str_tax_cla,
          str_tax_npc = lib_mer_str_tax_npc
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_def_pre_ann_sir,
      command = {
        tima:::parse_yaml_params(
          def = par_def_pre_ann_sir,
          usr = par_def_pre_ann_sir
        )
      },
      format = "rds"
    ),
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
    tar_target(
      name = benchmark_ann_sir_pre_can,
      command = {
        benchmark_ann_sir_pre[[1L]]
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_sir_pre_for,
      command = {
        benchmark_ann_sir_pre[[2L]]
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_sir_pre_str,
      command = {
        benchmark_ann_sir_pre[[3L]]
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_def_fil_ann,
      command = {
        tima:::parse_yaml_params(def = par_def_fil_ann, usr = par_def_fil_ann)
      },
      format = "rds"
    ),
    tar_target(
      name = benchmark_ann_fil_spe_neg,
      command = {
        filter_annotations(
          annotations = c(benchmark_ann_spe_pre_neg, benchmark_ann_sir_pre_str),
          features = benchmark_pre_meta_neg,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_spe_fil_neg.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_fil_spe_ms1_neg,
      command = {
        filter_annotations(
          annotations = c(
            benchmark_ann_spe_pre_neg,
            benchmark_ann_ms1_pre_neg[[1L]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_neg,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_spe_ms1_fil_neg.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_fil_ms1_neg,
      command = {
        filter_annotations(
          annotations = c(
            benchmark_ann_ms1_pre_neg[[1L]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_neg,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_ms1_fil_neg.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_fil_spe_pos,
      command = {
        filter_annotations(
          annotations = c(benchmark_ann_spe_pre_pos, benchmark_ann_sir_pre_str),
          features = benchmark_pre_meta_pos,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_spe_fil_pos.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_fil_spe_ms1_pos,
      command = {
        filter_annotations(
          annotations = c(
            benchmark_ann_spe_pre_pos,
            benchmark_ann_ms1_pre_pos[[1L]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_pos,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_spe_ms1_fil_pos.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_fil_ms1_pos,
      command = {
        filter_annotations(
          annotations = c(
            benchmark_ann_ms1_pre_pos[[1L]],
            benchmark_ann_sir_pre_str
          ),
          features = benchmark_pre_meta_pos,
          rts = list(),
          output = "data/interim/benchmark/benchmark_ann_ms1_fil_pos.tsv.gz",
          tolerance_rt = benchmark_def_fil_ann$ms$tolerances$rt$library
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_def_wei_ann,
      command = {
        tima:::parse_yaml_params(def = par_def_wei_ann, usr = par_def_wei_ann)
      },
      format = "rds"
    ),
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
        high_confidence = FALSE,
        remove_ties = benchmark_def_wei_ann$options$remove_ties,
        summarize = benchmark_def_wei_ann$options$summarize,
        pattern = benchmark_def_wei_ann$files$pattern,
        force = benchmark_def_wei_ann$options$force
      )
    }),
    tar_target(
      name = benchmark_files_pos,
      command = list(
        components = benchmark_com_pre_pos,
        edges = benchmark_edg_pre_pos,
        taxa = benchmark_taxed_pos
      )
    ),
    tar_target(
      name = benchmark_files_neg,
      command = list(
        components = benchmark_com_pre_neg,
        edges = benchmark_edg_pre_neg,
        taxa = benchmark_taxed_neg
      )
    ),
    tar_target(
      name = benchmark_ann_pre_ms2_b_pos,
      command = {
        do.call(
          what = weight_annotations,
          args = c(
            benchmark_wei_par,
            benchmark_files_pos,
            annotations = benchmark_ann_fil_spe_pos,
            weight_spectral = 0.333,
            weight_chemical = 0,
            weight_biological = 0.666,
            ms1_only = FALSE,
            output = "benchmark_lotus_ms2_bio_pos.tsv.gz"
          )
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_pre_ms1_ms2_b_pos,
      command = {
        do.call(
          what = weight_annotations,
          args = c(
            benchmark_wei_par,
            benchmark_files_pos,
            annotations = benchmark_ann_fil_spe_ms1_pos,
            ms1_only = FALSE,
            weight_spectral = 0.333,
            weight_chemical = 0,
            weight_biological = 0.666,
            output = "benchmark_lotus_ms1_ms2_bio_pos.tsv.gz"
          )
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_pre_ms2_b_c_pos,
      command = {
        do.call(
          what = weight_annotations,
          args = c(
            benchmark_wei_par,
            benchmark_files_pos,
            annotations = benchmark_ann_fil_spe_pos,
            ms1_only = FALSE,
            weight_spectral = 0.333,
            weight_chemical = 0.166,
            weight_biological = 0.5,
            output = "benchmark_lotus_ms2_bio_chemo_pos.tsv.gz"
          )
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_pre_ms1_ms2_b_c_pos,
      command = {
        do.call(
          what = weight_annotations,
          args = c(
            benchmark_wei_par,
            benchmark_files_pos,
            annotations = benchmark_ann_fil_spe_ms1_pos,
            ms1_only = FALSE,
            weight_spectral = 0.333,
            weight_chemical = 0.166,
            weight_biological = 0.5,
            output = "benchmark_lotus_ms1_ms2_bio_chemo_pos.tsv.gz"
          )
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_pre_ms2_b_neg,
      command = {
        do.call(
          what = weight_annotations,
          args = c(
            benchmark_wei_par,
            benchmark_files_neg,
            annotations = benchmark_ann_fil_spe_neg,
            ms1_only = FALSE,
            weight_spectral = 0.333,
            weight_chemical = 0,
            weight_biological = 0.666,
            output = "benchmark_lotus_ms2_bio_neg.tsv.gz"
          )
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_pre_ms1_ms2_b_neg,
      command = {
        do.call(
          what = weight_annotations,
          args = c(
            benchmark_wei_par,
            benchmark_files_neg,
            annotations = benchmark_ann_fil_spe_ms1_neg,
            ms1_only = FALSE,
            weight_spectral = 0.333,
            weight_chemical = 0,
            weight_biological = 0.666,
            output = "benchmark_lotus_ms1_ms2_bio_neg.tsv.gz"
          )
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_pre_ms2_b_c_neg,
      command = {
        do.call(
          what = weight_annotations,
          args = c(
            benchmark_wei_par,
            benchmark_files_neg,
            annotations = benchmark_ann_fil_spe_neg,
            ms1_only = FALSE,
            weight_spectral = 0.333,
            weight_chemical = 0.166,
            weight_biological = 0.5,
            output = "benchmark_lotus_ms2_bio_chemo_neg.tsv.gz"
          )
        )
      },
      format = "file"
    ),
    tar_target(
      name = benchmark_ann_pre_ms1_ms2_b_c_neg,
      command = {
        do.call(
          what = weight_annotations,
          args = c(
            benchmark_wei_par,
            benchmark_files_neg,
            annotations = benchmark_ann_fil_spe_ms1_neg,
            ms1_only = FALSE,
            weight_spectral = 0.333,
            weight_chemical = 0.166,
            weight_biological = 0.5,
            output = "benchmark_lotus_ms1_ms2_bio_chemo_neg.tsv.gz"
          )
        )
      },
      format = "file"
    )
  )
}
