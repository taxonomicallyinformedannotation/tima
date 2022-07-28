#' Title
#'
#' @param library TODO
#' @param name TODO
#' @param gnps TODO
#' @param isdb TODO
#' @param sirius TODO
#' @param taxa TODO
#' @param edges TODO
#' @param output TODO
#' @param candidates_initial TODO
#' @param candidates_final TODO
#' @param weight_spectral TODO
#' @param weight_chemical TODO
#' @param weight_biological TODO
#' @param score_chemical_pathway TODO
#' @param score_chemical_superclass TODO
#' @param score_chemical_class TODO
#' @param score_biological_domain TODO
#' @param score_biological_kingdom TODO
#' @param score_biological_phylum TODO
#' @param score_biological_class TODO
#' @param score_biological_order TODO
#' @param score_biological_infraorder TODO
#' @param score_biological_family TODO
#' @param score_biological_subfamily TODO
#' @param score_biological_tribe TODO
#' @param score_biological_subtribe TODO
#' @param score_biological_genus TODO
#' @param score_biological_subgenus TODO
#' @param score_biological_species TODO
#' @param score_biological_subspecies TODO
#' @param score_biological_variety TODO
#' @param ms_mode TODO
#' @param ms_level TODO
#' @param annotate TODO
#' @param tolerance_ppm TODO
#' @param tolerance_rt TODO
#' @param force TODO
#' @param adducts_list TODO
#' @param minimal_ms1_bio TODO
#' @param minimal_ms1_chemo TODO
#' @param ms1_only TODO
#'
#' @return TODO
#' @export
#'
#' @importFrom crayon green
#' @importFrom dplyr across arrange bind_rows distinct filter select left_join
#' @importFrom dplyr mutate mutate_all mutate_if
#' @importFrom readr read_delim write_delim
#' @importFrom yaml write_yaml
#'
#' @examples
process_annotations <- function(library = params$library,
                                name = params$name,
                                gnps = params$annotation$gnps,
                                isdb = params$annotation$isdb,
                                sirius = params$annotation$sirius,
                                taxa = params$taxa,
                                edges = params$edges,
                                output = params$output,
                                candidates_initial = params$top_k$initial,
                                candidates_final = params$top_k$final,
                                weight_spectral = params$weights$spectral,
                                weight_chemical = params$weights$chemical,
                                weight_biological = params$weights$biological,
                                score_chemical_pathway = params$scores$chemical$pathway,
                                score_chemical_superclass = params$scores$chemical$superclass,
                                score_chemical_class = params$scores$chemical$class,
                                score_biological_domain = params$scores$biological$domain,
                                score_biological_kingdom = params$scores$biological$kingdom,
                                score_biological_phylum = params$scores$biological$phylum,
                                score_biological_class = params$scores$biological$class,
                                score_biological_order = params$scores$biological$order,
                                score_biological_infraorder = params$scores$biological$infraorder,
                                score_biological_family = params$scores$biological$family,
                                score_biological_subfamily = params$scores$biological$subfamily,
                                score_biological_tribe = params$scores$biological$tribe,
                                score_biological_subtribe = params$scores$biological$subtribe,
                                score_biological_genus = params$scores$biological$genus,
                                score_biological_subgenus = params$scores$biological$subgenus,
                                score_biological_species = params$scores$biological$species,
                                score_biological_subspecies = params$scores$biological$subspecies,
                                score_biological_variety = params$scores$biological$variety,
                                ms_mode = params$ms$mode,
                                ms_level = params$ms$level,
                                annotate = params$ms$annotate,
                                tolerance_ppm = params$ms$tolerance$ppm,
                                tolerance_rt = params$ms$tolerance$rt,
                                adducts_list = params$ms$adducts,
                                minimal_ms1_bio = params$scores$biological$minimal,
                                minimal_ms1_chemo = params$scores$chemical$minimal,
                                ms1_only = params$ms$ms1only,
                                force = params$force) {
  stopifnot("Your library file does not exist." = file.exists(library))
  ## TODO add name
  stopifnot("Your GNPS file does not exist." = if (length(gnps) == 0) {
    TRUE
  } else {
    file.exists(gnps)
  })
  stopifnot("Your ISDB file does not exist." = if (length(isdb) == 0) {
    TRUE
  } else {
    file.exists(isdb)
  })
  stopifnot("Your SIRIUS file does not exist." = if (length(sirius) == 0) {
    TRUE
  } else {
    file.exists(sirius)
  })
  stopifnot("Your taxa file does not exist." = file.exists(taxa))
  stopifnot("Your edges file does not exist." = file.exists(edges))
  stopifnot("Your ms_mode parameter must be 'pos' or 'neg'" = ms_mode %in% c("pos", "neg"))
  stopifnot("Your ms_level parameter must be 1" = ms_level == 1)
  stopifnot("Your ms_annotate parameter must be 'true' or 'false'" = annotate %in% c(TRUE, FALSE))
  if (force == FALSE) {
    stopifnot("Your ppm tolerance must be lower or equal to 20" = tolerance_ppm <=
      20)
    stopifnot("Your rt tolerance must be lower or equal to 0.1" = tolerance_rt <=
      0.1)
  }
  ## TODO for later on
  # stopifnot(
  #  adducts ...
  # )

  vars <- ls(all.names = TRUE)
  for (i in 1:length(vars)) {
    assign(vars[i], get(vars[i]), envir = .GlobalEnv)
  }

  log_debug(x = "... files ...")
  log_debug(x = "... annotations")
  metadata_table_spectral_annotation <<- lapply(
    X = c(gnps, isdb, sirius),
    FUN = function(x) {
      readr::read_delim(
        file = x,
        col_types = "c"
      ) |>
        dplyr::mutate_all(list(~ gsub(
          pattern = "\\|",
          replacement = " or ",
          x = .x
        ))) |>
        dplyr::mutate(dplyr::across(feature_id, as.numeric))
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    dplyr::arrange(feature_id)

  log_debug(x = "... metadata_table_biological_annotation")
  taxed_features_table <<- readr::read_delim(
    file = taxa,
    col_types = "c"
  ) |>
    dplyr::mutate(dplyr::across(feature_id, as.numeric)) |>
    dplyr::mutate_if(is.logical, as.character)

  taxed_features_table[is.na(taxed_features_table)] <- "ND"

  log_debug(x = "... edges table")
  edges_table <<- readr::read_delim(file = edges)

  log_debug(x = "... structure-organism pairs table")
  structure_organism_pairs_table <<-
    readr::read_delim(
      file = library,
      col_types = "c"
    ) |>
    dplyr::filter(!is.na(structure_exact_mass)) |>
    dplyr::mutate_all(list(~ gsub(
      pattern = "\\|",
      replacement = " or ",
      x = .x
    ))) |>
    dplyr::mutate(dplyr::across(structure_exact_mass, as.numeric)) |>
    dplyr::mutate_if(is.logical, as.character)

  structure_organism_pairs_table[is.na(structure_organism_pairs_table)] <-
    "notClassified"

  if (ms1_only == TRUE) {
    metadata_table_spectral_annotation <-
      metadata_table_spectral_annotation |>
      dplyr::mutate(
        inchikey_2D = NA,
        score_input = NA,
        library = NA,
        mz_error = NA
      ) |>
      dplyr::mutate(dplyr::across(
        c(inchikey_2D, score_input, library, mz_error),
        as.character
      ))
  }

  if (annotate == TRUE) {
    log_debug("... single charge adducts table")
    if (ms_mode == "pos") {
      adduct_file <- paths$data$interim$adducts$pos
    }
    if (ms_mode == "neg") {
      adduct_file <- paths$data$interim$adducts$neg
    }

    adductsTable <<- readr::read_delim(file = adduct_file)

    log_debug("... adducts masses for in source dimers and multicharged")
    adductsMassTable <<-
      readr::read_delim(file = paths$data$source$adducts)

    log_debug("... neutral lossses")
    neutral_losses_table <<-
      readr::read_delim(file = paths$data$source$neutral_losses)

    adductsM <<- adductsMassTable$mass
    names(adductsM) <<- adductsMassTable$adduct

    if (ms_mode == "pos") {
      adduct_db_file <<-
        file.path(
          paths$data$interim$adducts$path,
          paste0(name, "_pos.tsv.gz")
        )
    }
    if (ms_mode == "neg") {
      adduct_db_file <<-
        file.path(
          paths$data$interim$adducts$path,
          paste0(name, "_neg.tsv.gz")
        )
    }

    log_debug(x = "... exact masses for MS1 annotation")
    structure_exact_mass_table <<-
      readr::read_delim(file = adduct_db_file) |>
      dplyr::filter(exact_mass %in% structure_organism_pairs_table[["structure_exact_mass"]])

    log_debug(x = "performing MS1 annotation")
    annotation_table_ms1 <<- ms1_annotation()

    ms1_decoration()
  } else {
    annotation_table_ms1 <<-
      non_ms1_annotation(annotationTable = metadata_table_spectral_annotation)
  }

  log_debug(x = "adding biological organism metadata")
  annotation_table_ms1_taxed <<-
    dplyr::left_join(annotation_table_ms1, taxed_features_table) |>
    dplyr::left_join(
      metadata_table_spectral_annotation |>
        dplyr::distinct(
          inchikey_2D,
          smiles_2D,
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class
        )
    )

  log_debug(x = "performing taxonomically informed scoring")
  annotation_table_weighted_bio <<- biological_weighting()

  taxo_decoration()

  log_debug(x = "cleaning taxonomically informed results and preparing for chemically informed scoring")
  annotation_table_weighted_bio_cleaned <<- biological_cleaning()

  log_debug(x = "performing chemically informed scoring")
  annotation_table_weighted_chemo <<- chemical_weighting()

  chemical_decoration()

  log_debug(x = "cleaning for cytoscape export")
  results2cytoscape <<- chemical_cleaning()

  log_debug(x = "Exporting ...")
  ifelse(
    test = !dir.exists(paths$data$processed$path),
    yes = dir.create(paths$data$processed$path),
    no = paste(paths$data$processed$path, "exists")
  )

  time <- format(Sys.time(), "%y%m%d_%H%M%OS")
  dir_time <- file.path(paths$data$processed$path, time)

  ifelse(
    test = !dir.exists(dir_time),
    yes = dir.create(dir_time),
    no = paste(
      dir_time,
      "exists"
    )
  )
  log_debug(
    x = "... path to export is",
    crayon::green(file.path(
      dir_time,
      output
    ))
  )
  readr::write_delim(
    x = results2cytoscape,
    file = file.path(
      dir_time,
      output
    ),
    delim = "\t",
    na = ""
  )

  log_debug(
    x = "... path to used parameters is",
    crayon::green(file.path(
      dir_time,
      paste("tima",
        paths$version,
        "process_annotations.yaml",
        sep = "_"
      )
    ))
  )
  yaml::write_yaml(
    x = params,
    file = file.path(
      dir_time,
      paste("tima",
        paths$version,
        "process_annotations.yaml",
        sep = "_"
      )
    )
  )
}
