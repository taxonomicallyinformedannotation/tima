# Libraries targets section.

targets_section_libraries <- function() {
  list(
    list(
      tar_target(
        name = lib_spe_is_nor_pre_pos,
        command = {
          get_file(
            url = paths$urls$spectra$pos$nor,
            export = paths$data$interim$libraries$spectra$is$pos$nor
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_is_nor_pre_neg,
        command = {
          get_file(
            url = paths$urls$spectra$neg$nor,
            export = paths$data$interim$libraries$spectra$is$neg$nor
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_is_nor_pre_sop,
        command = {
          get_file(
            url = paths$urls$sop$nor,
            export = paths$data$interim$libraries$sop$nor
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_is_wik_pre_pos,
        command = {
          get_file(
            url = paths$urls$spectra$pos$isdb,
            export = paths$data$interim$libraries$spectra$is$pos$isdb
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_is_wik_pre_neg,
        command = {
          get_file(
            url = paths$urls$spectra$neg$isdb,
            export = paths$data$interim$libraries$spectra$is$neg$isdb
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_is_wik_pre_sop,
        command = {
          get_file(
            url = paths$urls$sop$isdb,
            export = paths$data$interim$libraries$sop$isdb
          )
        },
        format = "file"
      )
    ),
    list(
      tar_target(
        name = lib_spe_exp_gnp_pre_pos,
        command = {
          get_file(
            url = paths$urls$spectra$pos$gnps,
            export = paths$data$interim$libraries$spectra$exp$pos$gnps
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_exp_gnp_pre_neg,
        command = {
          get_file(
            url = paths$urls$spectra$neg$gnps,
            export = paths$data$interim$libraries$spectra$exp$neg$gnps
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_exp_gnp_pre_sop,
        command = {
          get_file(
            url = paths$urls$sop$gnps,
            export = paths$data$interim$libraries$sop$gnps
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_exp_mb_pre_pos,
        command = {
          get_file(
            url = paths$urls$spectra$pos$massbank,
            export = paths$data$interim$libraries$spectra$exp$pos$massbank
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_exp_mb_pre_neg,
        command = {
          get_file(
            url = paths$urls$spectra$neg$massbank,
            export = paths$data$interim$libraries$spectra$exp$neg$massbank
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_exp_mb_pre_sop,
        command = {
          get_file(
            url = paths$urls$sop$massbank,
            export = paths$data$interim$libraries$sop$massbank
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_exp_mer_pre_pos,
        command = {
          get_file(
            url = paths$urls$spectra$pos$merlin,
            export = paths$data$interim$libraries$spectra$exp$pos$merlin
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_exp_mer_pre_neg,
        command = {
          get_file(
            url = paths$urls$spectra$neg$merlin,
            export = paths$data$interim$libraries$spectra$exp$neg$merlin
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_exp_mer_pre_sop,
        command = {
          get_file(
            url = paths$urls$sop$merlin,
            export = paths$data$interim$libraries$sop$merlin
          )
        },
        format = "file"
      )
    ),
    list(
      tar_target(
        name = lib_spe_exp_int_pre,
        command = {
          prepare_libraries_spectra(
            input = par_pre_lib_spe$files$libraries$spectral$raw,
            min_fragments = par_pre_lib_spe$ms$thresholds$ms2$min_fragments,
            nam_lib = par_pre_lib_spe$names$libraries,
            col_ad = par_pre_lib_spe$names$mgf$adduct,
            col_ce = par_pre_lib_spe$names$mgf$collision_energy,
            col_ci = par_pre_lib_spe$names$mgf$compound_id,
            col_in = par_pre_lib_spe$names$mgf$inchi,
            col_io = par_pre_lib_spe$names$mgf$inchi_no_stereo,
            col_ik = par_pre_lib_spe$names$mgf$inchikey,
            col_il = par_pre_lib_spe$names$mgf$inchikey_connectivity_layer,
            col_na = par_pre_lib_spe$names$mgf$name,
            col_po = par_pre_lib_spe$names$mgf$polarity,
            col_sm = par_pre_lib_spe$names$mgf$smiles,
            col_sn = par_pre_lib_spe$names$mgf$smiles_no_stereo,
            col_si = par_pre_lib_spe$names$mgf$spectrum_id,
            col_sp = par_pre_lib_spe$names$mgf$splash,
            col_sy = par_pre_lib_spe$names$mgf$synonyms
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_exp_int_pre_pos,
        command = {
          lib_spe_exp_int_pre[[1L]]
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_exp_int_pre_neg,
        command = {
          lib_spe_exp_int_pre[[2L]]
        },
        format = "file"
      ),
      tar_target(
        name = lib_spe_exp_int_pre_sop,
        command = {
          lib_spe_exp_int_pre[[3L]]
        },
        format = "file"
      ),
      tar_target(
        name = lib_rt,
        command = {
          prepare_libraries_rt(
            mgf_exp = par_pre_lib_rt$files$libraries$temporal$exp$mgf,
            mgf_is = par_pre_lib_rt$files$libraries$temporal$is$mgf,
            temp_exp = par_pre_lib_rt$files$libraries$temporal$exp$csv,
            temp_is = par_pre_lib_rt$files$libraries$temporal$is$csv,
            output_rt = par_pre_lib_rt$files$libraries$temporal$prepared,
            output_sop = par_pre_lib_rt$files$libraries$sop$prepared$rt,
            col_ik = par_pre_lib_rt$names$mgf$inchikey,
            col_rt = par_pre_lib_rt$names$mgf$retention_time,
            col_sm = par_pre_lib_rt$names$mgf$smiles,
            name_inchikey = par_pre_lib_rt$names$inchikey,
            name_rt = par_pre_lib_rt$names$rt$library,
            name_smiles = par_pre_lib_rt$names$smiles,
            unit_rt = par_pre_lib_rt$units$rt
          )
        },
        format = "file"
      ),
      tar_target(
        name = lib_rt_rts,
        command = {
          lib_rt[[1L]]
        },
        format = "file"
      ),
      tar_target(
        name = lib_rt_sop,
        command = {
          lib_rt[[2L]]
        },
        format = "file"
      ),
      list(
        tar_target(
          name = lib_sop_ecm,
          command = {
            lib_sop_ecm <- tryCatch(
              expr = {
                get_file(
                  url = paths$urls$ecmdb$metabolites,
                  export = paths$data$source$libraries$sop$ecmdb
                )
              },
              error = function(e) {
                getFromNamespace("log_warn", "tima")(
                  "ECMDB download failed: %s",
                  conditionMessage(e)
                )
                getFromNamespace("fake_ecmdb", "tima")(
                  export = paths$data$source$libraries$sop$ecmdb
                )
              },
              finally = {
                paths$data$source$libraries$sop$ecmdb
              }
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_hmd,
          command = {
            tryCatch(
              expr = {
                get_file(
                  url = paths$urls$hmdb$structures,
                  export = paths$data$source$libraries$sop$hmdb
                )
              },
              warning = function(w) {
                getFromNamespace("log_warn", "tima")(
                  "HMDB download warning: %s",
                  conditionMessage(w)
                )
                getFromNamespace("log_warn", "tima")(
                  "HMDB download failed partially, returning empty file instead"
                )
                unlink(paths$data$source$libraries$sop$hmdb)
                getFromNamespace("fake_hmdb", "tima")(
                  export = paths$data$source$libraries$sop$hmdb
                )
              },
              error = function(e) {
                getFromNamespace("log_warn", "tima")(
                  "HMDB download failed: %s",
                  conditionMessage(e)
                )
                getFromNamespace("fake_hmdb", "tima")(
                  export = paths$data$source$libraries$sop$hmdb
                )
              },
              finally = {
                paths$data$source$libraries$sop$hmdb
              }
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_pub,
          command = {
            pub_url <- paths$urls$pubchemlite$csv
            if (
              is.null(pub_url) || !is.character(pub_url) || !nzchar(pub_url)
            ) {
              pub_url <- paste0(
                "https://zenodo.org/records/20439802/files/",
                "PubChemLite_CCSbase_20260529.csv?download=1"
              )
            }

            pub_export <- paths$data$source$libraries$sop$pubchemlite
            if (
              is.null(pub_export) ||
                !is.character(pub_export) ||
                length(pub_export) != 1L ||
                !nzchar(pub_export)
            ) {
              pub_export <- "data/source/libraries/sop/pubchemlite.csv"
            }

            tryCatch(
              expr = {
                get_file(
                  url = pub_url,
                  export = pub_export
                )
              },
              error = function(e) {
                getFromNamespace("log_warn", "tima")(
                  "PubChem Lite download failed: %s",
                  conditionMessage(e)
                )
                unlink(pub_export)
                getFromNamespace("fake_pubchemlite", "tima")(
                  export = pub_export
                )
              },
              finally = {
                pub_export
              }
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_hmd_fam_raw,
          command = {
            hmdb_family_names <- names(paths$urls$hmdb_family)
            lib_sop_hmd_fam_raw <- purrr::map_chr(
              .x = hmdb_family_names,
              .f = function(lib_name) {
                output_file <- paths$data$source$libraries$sop[[lib_name]]
                tryCatch(
                  expr = {
                    get_file(
                      url = paths$urls$hmdb_family[[lib_name]],
                      export = output_file
                    )
                  },
                  error = function(e) {
                    getFromNamespace("log_warn", "tima")(
                      "HMDB family download failed: %s",
                      conditionMessage(e)
                    )
                    getFromNamespace("fake_hmdb", "tima")(export = output_file)
                  }
                )
                output_file
              }
            )
            names(lib_sop_hmd_fam_raw) <- hmdb_family_names
            lib_sop_hmd_fam_raw
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_lot,
          command = {
            tryCatch(
              expr = {
                get_last_version_from_zenodo(
                  doi = paths$urls$lotus$doi,
                  pattern = paths$urls$lotus$pattern,
                  path = paths$data$source$libraries$sop$lotus
                )
              },
              error = function(e) {
                getFromNamespace("log_warn", "tima")(
                  "LOTUS download failed: %s",
                  conditionMessage(e)
                )
                getFromNamespace("fake_lotus", "tima")(
                  export = paths$data$source$libraries$sop$lotus
                )
              },
              finally = {
                paths$data$source$libraries$sop$lotus
              }
            )
          },
          format = "file"
        )
      ),
      list(
        tar_target(
          name = lib_sop_big_pre,
          command = {
            prepare_libraries_sop_bigg(
              output = par_pre_lib_sop_big$files$libraries$sop$prepared$bigg
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_clo_pre,
          command = {
            prepare_libraries_sop_closed(
              input = par_pre_lib_sop_clo$files$libraries$sop$raw$closed,
              output = par_pre_lib_sop_clo$files$libraries$sop$prepared$closed
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_ecm_pre,
          command = {
            prepare_libraries_sop_ecmdb(
              input = lib_sop_ecm,
              output = par_pre_lib_sop_ecm$files$libraries$sop$prepared$ecmdb
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_hmd_pre,
          command = {
            prepare_libraries_sop_hmdb(
              input = lib_sop_hmd,
              output = par_pre_lib_sop_hmd$files$libraries$sop$prepared$hmdb
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_pub_pre,
          command = {
            pub_prepared <- tryCatch(
              par_pre_lib_sop_pub$files$libraries$sop$prepared$pubchemlite,
              error = function(e) {
                invisible(e)
                "data/interim/libraries/sop/pubchemlite_prepared.tsv.gz"
              }
            )
            getFromNamespace("prepare_libraries_sop_pubchemlite", "tima")(
              input = lib_sop_pub,
              output = pub_prepared
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_hmd_fam_pre,
          command = {
            hmdb_family_tags <- c(
              csfmetabolome = "csf",
              fecalmetabolome = "fecal",
              salivametabolome = "saliva",
              serummetabolome = "serum",
              sweatmetabolome = "sweat",
              urinemetabolome = "urine",
              mcdb = "milk",
              smpdb = "pathway",
              mimedb = "microbiome",
              t3db = "toxin",
              bovinedb = NA_character_,
              ymdb = NA_character_,
              cannabisdatabase = NA_character_
            )
            hmdb_family_organisms <- c(
              csfmetabolome = "Homo sapiens",
              fecalmetabolome = "Homo sapiens",
              salivametabolome = "Homo sapiens",
              serummetabolome = "Homo sapiens",
              sweatmetabolome = "Homo sapiens",
              urinemetabolome = "Homo sapiens",
              mcdb = "Bos taurus",
              smpdb = NA_character_,
              mimedb = NA_character_,
              t3db = NA_character_,
              bovinedb = "Bos taurus",
              ymdb = "Saccharomyces cerevisiae",
              cannabisdatabase = "Cannabis"
            )
            human_list <- list(
              organism_taxonomy_ottid = "770315",
              organism_taxonomy_01domain = "Eukaryota",
              organism_taxonomy_02kingdom = "Metazoa",
              organism_taxonomy_03phylum = "Chordata",
              organism_taxonomy_04class = "Mammalia",
              organism_taxonomy_05order = "Primates",
              organism_taxonomy_06family = "Hominidae",
              organism_taxonomy_07tribe = NA_character_,
              organism_taxonomy_08genus = "Homo",
              organism_taxonomy_09species = "Homo sapiens",
              organism_taxonomy_10varietas = NA_character_
            )
            cow_list <- list(
              organism_taxonomy_ottid = "490099",
              organism_taxonomy_01domain = "Eukaryota",
              organism_taxonomy_02kingdom = "Metazoa",
              organism_taxonomy_03phylum = "Chordata",
              organism_taxonomy_04class = "Mammalia",
              organism_taxonomy_05order = "Artiodactyla",
              organism_taxonomy_06family = "Bovidae",
              organism_taxonomy_07tribe = NA_character_,
              organism_taxonomy_08genus = "Bos",
              organism_taxonomy_09species = "Bos taurus",
              organism_taxonomy_10varietas = NA_character_
            )
            empty_list <- list(
              organism_taxonomy_ottid = "",
              organism_taxonomy_01domain = NA_character_,
              organism_taxonomy_02kingdom = NA_character_,
              organism_taxonomy_03phylum = NA_character_,
              organism_taxonomy_04class = NA_character_,
              organism_taxonomy_05order = NA_character_,
              organism_taxonomy_06family = NA_character_,
              organism_taxonomy_07tribe = NA_character_,
              organism_taxonomy_08genus = NA_character_,
              organism_taxonomy_09species = NA_character_,
              organism_taxonomy_10varietas = NA_character_
            )
            hmdb_family_taxonomy <- list(
              csfmetabolome = human_list,
              fecalmetabolome = human_list,
              salivametabolome = human_list,
              serummetabolome = human_list,
              sweatmetabolome = human_list,
              urinemetabolome = human_list,
              mcdb = cow_list,
              smpdb = empty_list,
              mimedb = empty_list,
              t3db = empty_list,
              bovinedb = cow_list,
              ymdb = list(
                organism_taxonomy_ottid = "356221",
                organism_taxonomy_01domain = "Eukaryota",
                organism_taxonomy_02kingdom = "Fungi",
                organism_taxonomy_03phylum = "Ascomycota",
                organism_taxonomy_04class = NA_character_,
                organism_taxonomy_05order = "Saccharomycetales",
                organism_taxonomy_06family = "Saccharomycetaceae",
                organism_taxonomy_07tribe = NA_character_,
                organism_taxonomy_08genus = "Saccharomyces",
                organism_taxonomy_09species = "Saccharomyces cerevisiae",
                organism_taxonomy_10varietas = NA_character_
              ),
              cannabisdatabase = list(
                organism_taxonomy_ottid = "84008",
                organism_taxonomy_01domain = "Eukaryota",
                organism_taxonomy_02kingdom = "Archaeplastida",
                organism_taxonomy_03phylum = "Streptophyta",
                organism_taxonomy_04class = "Magnoliopsida",
                organism_taxonomy_05order = "Rosales",
                organism_taxonomy_06family = "Cannabaceae",
                organism_taxonomy_07tribe = NA_character_,
                organism_taxonomy_08genus = "Cannabis",
                organism_taxonomy_09species = NA_character_,
                organism_taxonomy_10varietas = NA_character_
              )
            )
            hmdb_family_names <- names(hmdb_family_tags)
            raw_hmdb_family_files <- lib_sop_hmd_fam_raw
            if (
              is.null(names(raw_hmdb_family_files)) ||
                !all(hmdb_family_names %in% names(raw_hmdb_family_files))
            ) {
              raw_hmdb_family_files <- stats::setNames(
                as.character(raw_hmdb_family_files),
                names(paths$urls$hmdb_family)[seq_along(raw_hmdb_family_files)]
              )
            }
            hmdb_family_prepared <- purrr::map_chr(
              .x = hmdb_family_names,
              .f = function(lib_name) {
                input_file <- raw_hmdb_family_files[[lib_name]]
                if (
                  is.null(input_file) ||
                    is.na(input_file) ||
                    !nzchar(input_file)
                ) {
                  input_file <- paths$data$source$libraries$sop[[lib_name]]
                }
                getFromNamespace("prepare_libraries_sop_hmdb_like", "tima")(
                  input = input_file,
                  output = paths$data$interim$libraries$sop[[lib_name]],
                  source_name = toupper(lib_name),
                  organism_name = hmdb_family_organisms[[lib_name]],
                  organism_taxonomy = hmdb_family_taxonomy[[lib_name]],
                  tag = hmdb_family_tags[[lib_name]]
                )
              }
            )
            hmdb_family_prepared
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_lot_pre,
          command = {
            prepare_libraries_sop_lotus(
              input = if (!paths$test$mode) {
                lib_sop_lot
              } else {
                paths$data$source$libraries$sop$lotus
              },
              output = par_pre_lib_sop_lot$files$libraries$sop$prepared$lotus
            )
          },
          format = "file"
        )
      ),
      list(
        tar_target(
          name = lib_sop_mer_str_pro,
          command = {
            get_file(
              url = paths$urls$examples$structures_processed,
              export = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$processed
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_mer_npc_cache,
          command = {
            get_file(
              url = paths$urls$examples$npclassifier_cache,
              export = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$taxonomies$n
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_mer_cla_cache,
          command = {
            get_file(
              url = paths$urls$examples$classyfire_cache,
              export = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$taxonomies$c
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_mer,
          command = {
            prepare_libraries_sop_merged(
              files = c(
                lib_sop_big_pre,
                lib_sop_clo_pre,
                lib_sop_ecm_pre,
                lib_sop_hmd_pre,
                lib_sop_pub_pre,
                lib_sop_hmd_fam_pre,
                lib_sop_lot_pre,
                lib_rt_sop,
                lib_spe_exp_int_pre_sop,
                lib_spe_exp_gnp_pre_sop,
                lib_spe_exp_mb_pre_sop,
                lib_spe_exp_mer_pre_sop,
                lib_spe_is_nor_pre_sop,
                lib_spe_is_wik_pre_sop
              ),
              filter = par_pre_lib_sop_mer$organisms$filter$mode,
              level = par_pre_lib_sop_mer$organisms$filter$level,
              value = par_pre_lib_sop_mer$organisms$filter$value,
              cache = lib_sop_mer_str_pro,
              npc_cache = lib_sop_mer_npc_cache,
              cla_cache = lib_sop_mer_cla_cache,
              output_key = par_pre_lib_sop_mer$files$libraries$sop$merged$keys,
              output_org_tax_ott = par_pre_lib_sop_mer$files$libraries$sop$merged$organisms$taxonomies$ott,
              output_str_stereo = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$stereo,
              output_str_met = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$metadata,
              output_str_tax_cla = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$taxonomies$cla,
              output_str_tax_npc = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$taxonomies$npc
            )
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_key,
          command = {
            lib_sop_mer[[1L]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_org_tax_ott,
          command = {
            lib_sop_mer[[2L]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_str_can,
          command = {
            lib_sop_mer[[3L]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_str_stereo,
          command = {
            lib_sop_mer[[4L]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_str_met,
          command = {
            lib_sop_mer[[5L]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_str_tax_cla,
          command = {
            lib_sop_mer[[6L]]
          },
          format = "file"
        ),
        tar_target(
          name = lib_mer_str_tax_npc,
          command = {
            lib_sop_mer[[7L]]
          },
          format = "file"
        )
      )
    )
  )
}
