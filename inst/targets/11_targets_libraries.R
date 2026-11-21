# Libraries targets section.

targets_section_libraries <- function() {
  spe_download_specs <- list(
    list("is", "nor", "nor"),
    list("is", "wik", "isdb"),
    list("exp", "env", "enveda"),
    list("exp", "gnp", "gnps"),
    list("exp", "mb", "massbank"),
    list("exp", "mer", "merlin"),
    list("exp", "mul", "multims2")
  )
  list(
    lapply(spe_download_specs, function(s) {
      grp <- s[[1L]]
      abbr <- s[[2L]]
      key <- s[[3L]]
      prefix <- sprintf("lib_spe_%s_%s_pre", grp, abbr)
      c(
        lapply(c("pos", "neg"), function(pol) {
          tar_target_raw(
            name = paste0(prefix, "_", pol),
            command = substitute(
              get_file(
                url = paths$urls$spectra[[pol]][[key]],
                export = paths$data$interim$libraries$spectra[[grp]][[pol]][[
                  key
                ]]
              ),
              list(grp = grp, pol = pol, key = key)
            ),
            format = "file"
          )
        }),
        list(
          tar_target_raw(
            name = paste0(prefix, "_sop"),
            command = substitute(
              get_file(
                url = paths$urls$sop[[key]],
                export = paths$data$interim$libraries$sop[[key]]
              ),
              list(key = key)
            ),
            format = "file"
          )
        )
      )
    }),
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
      .tar_file_slice(
        "lib_spe_exp_int_pre",
        c(
          lib_spe_exp_int_pre_pos = 1L,
          lib_spe_exp_int_pre_neg = 2L,
          lib_spe_exp_int_pre_sop = 3L
        )
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
      .tar_file_slice("lib_rt", c(lib_rt_rts = 1L, lib_rt_sop = 2L)),
      list(
        tar_target(
          name = lib_sop_ecm,
          command = {
            tryCatch(
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
              }
            )
            paths$data$source$libraries$sop$ecmdb
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_hmd,
          command = {
            hmdb_path <- paths$data$source$libraries$sop$hmdb
            tryCatch(
              expr = {
                # A warning alone does not mean the download failed: log it
                # and keep the downloaded file
                withCallingHandlers(
                  get_file(
                    url = paths$urls$hmdb$structures,
                    export = hmdb_path
                  ),
                  warning = function(w) {
                    getFromNamespace("log_warn", "tima")(
                      "HMDB download warning: %s",
                      conditionMessage(w)
                    )
                    invokeRestart("muffleWarning")
                  }
                )
                if (!file.exists(hmdb_path) || file.size(hmdb_path) == 0) {
                  stop("downloaded file is missing or empty")
                }
              },
              error = function(e) {
                getFromNamespace("log_warn", "tima")(
                  "HMDB download failed: %s",
                  conditionMessage(e)
                )
                unlink(hmdb_path)
                getFromNamespace("fake_hmdb", "tima")(
                  export = hmdb_path
                )
              }
            )
            hmdb_path
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
              }
            )
            pub_export
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_hmd_fam_raw,
          command = {
            hmdb_family_names <- names(paths$urls$hmdb_family)
            lib_sop_hmd_fam_raw <- vapply(
              X = hmdb_family_names,
              FUN = function(lib_name) {
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
              },
              FUN.VALUE = character(1L)
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
                lotus_path <- paths$data$source$libraries$sop$lotus
                # Keep a pre-placed (e.g. offline) copy instead of replacing
                # it with an empty placeholder
                if (file.exists(lotus_path) && file.size(lotus_path) > 0) {
                  getFromNamespace("log_warn", "tima")(
                    "Using existing LOTUS file: %s",
                    lotus_path
                  )
                } else {
                  getFromNamespace("fake_lotus", "tima")(
                    export = lotus_path
                  )
                }
              }
            )
            paths$data$source$libraries$sop$lotus
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
            .make_taxonomy <- function(
              ottid = "",
              domain = NA_character_,
              kingdom = NA_character_,
              phylum = NA_character_,
              class = NA_character_,
              order = NA_character_,
              family = NA_character_,
              tribe = NA_character_,
              genus = NA_character_,
              species = NA_character_,
              varietas = NA_character_
            ) {
              list(
                organism_taxonomy_ottid = ottid,
                organism_taxonomy_01domain = domain,
                organism_taxonomy_02kingdom = kingdom,
                organism_taxonomy_03phylum = phylum,
                organism_taxonomy_04class = class,
                organism_taxonomy_05order = order,
                organism_taxonomy_06family = family,
                organism_taxonomy_07tribe = tribe,
                organism_taxonomy_08genus = genus,
                organism_taxonomy_09species = species,
                organism_taxonomy_10varietas = varietas
              )
            }
            human_list <- .make_taxonomy(
              ottid = "770315",
              domain = "Eukaryota",
              kingdom = "Metazoa",
              phylum = "Chordata",
              class = "Mammalia",
              order = "Primates",
              family = "Hominidae",
              genus = "Homo",
              species = "Homo sapiens"
            )
            cow_list <- .make_taxonomy(
              ottid = "490099",
              domain = "Eukaryota",
              kingdom = "Metazoa",
              phylum = "Chordata",
              class = "Mammalia",
              order = "Artiodactyla",
              family = "Bovidae",
              genus = "Bos",
              species = "Bos taurus"
            )
            empty_list <- .make_taxonomy()
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
              ymdb = .make_taxonomy(
                ottid = "356221",
                domain = "Eukaryota",
                kingdom = "Fungi",
                phylum = "Ascomycota",
                order = "Saccharomycetales",
                family = "Saccharomycetaceae",
                genus = "Saccharomyces",
                species = "Saccharomyces cerevisiae"
              ),
              cannabisdatabase = .make_taxonomy(
                ottid = "84008",
                domain = "Eukaryota",
                kingdom = "Archaeplastida",
                phylum = "Streptophyta",
                class = "Magnoliopsida",
                order = "Rosales",
                family = "Cannabaceae",
                genus = "Cannabis"
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
            hmdb_family_prepared <- vapply(
              X = hmdb_family_names,
              FUN = function(lib_name) {
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
              },
              FUN.VALUE = character(1L)
            )
            hmdb_family_prepared
          },
          format = "file"
        ),
        tar_target(
          name = lib_sop_lot_pre,
          command = {
            prepare_libraries_sop_lotus(
              input = if (!isTRUE(paths$tests$mode)) {
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
          }
        ),
        tar_target(
          name = lib_sop_mer_npc_cache,
          command = {
            get_file(
              url = paths$urls$examples$npclassifier_cache,
              export = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$taxonomies[[
                "n"
              ]]
            )
          }
        ),
        tar_target(
          name = lib_sop_mer_cla_cache,
          command = {
            get_file(
              url = paths$urls$examples$classyfire_cache,
              export = par_pre_lib_sop_mer$files$libraries$sop$merged$structures$taxonomies[[
                "c"
              ]]
            )
          }
        ),
        tar_target(
          name = lib_sop_mer,
          command = {
            if (!file.exists(lib_sop_mer_str_pro)) {
              get_file(
                url = paths$urls$examples$structures_processed,
                export = lib_sop_mer_str_pro
              )
            }
            if (!file.exists(lib_sop_mer_npc_cache)) {
              get_file(
                url = paths$urls$examples$npclassifier_cache,
                export = lib_sop_mer_npc_cache
              )
            }
            if (!file.exists(lib_sop_mer_cla_cache)) {
              get_file(
                url = paths$urls$examples$classyfire_cache,
                export = lib_sop_mer_cla_cache
              )
            }
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
                lib_spe_exp_env_pre_sop,
                lib_spe_exp_int_pre_sop,
                lib_spe_exp_gnp_pre_sop,
                lib_spe_exp_mb_pre_sop,
                lib_spe_exp_mer_pre_sop,
                lib_spe_exp_mul_pre_sop,
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
        .tar_file_slice(
          "lib_sop_mer",
          c(
            lib_mer_key = 1L,
            lib_mer_org_tax_ott = 2L,
            lib_mer_str_stereo = 4L,
            lib_mer_str_met = 5L,
            lib_mer_str_tax_cla = 6L,
            lib_mer_str_tax_npc = 7L
          )
        )
      )
    )
  )
}
