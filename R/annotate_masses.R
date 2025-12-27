#' @title Annotate masses
#'
#' @description This function annotates a feature table based on exact mass
#' match. It requires a structural library, its metadata, and lists of adducts,
#' clusters, and neutral losses to be considered. The polarity has to be `pos`
#' or `neg` and retention time and mass tolerances should be given. The feature
#' table is expected to be pre-formatted.
#'
#' @include adducts_utils.R
#' @include calculate_mass_of_m.R
#' @include decorate_masses.R
#' @include dists_utils.R
#' @include get_params.R
#' @include harmonize_adducts.R
#' @include round_reals.R
#' @include safe_fread.R
#' @include validations_utils.R
#'
#' @param features Table containing your previous annotation to complement
#' @param output_annotations Output for mass based structural annotations
#' @param output_edges Output for mass based edges
#' @param name_source Name of the source features column
#' @param name_target Name of the target features column
#' @param library Library containing the keys
#' @param str_stereo File containing structures stereo
#' @param str_met File containing structures metadata
#' @param str_nam File containing structures names
#' @param str_tax_cla File containing Classyfire taxonomy
#' @param str_tax_npc File containing NPClassifier taxonomy
#' @param adducts_list List of adducts to be used
#' @param clusters_list List of clusters to be used
#' @param neutral_losses_list List of neutral losses to be used
#' @param ms_mode Ionization mode. Must be 'pos' or 'neg'
#' @param tolerance_ppm Tolerance to perform annotation. Should be <= 20 ppm
#' @param tolerance_rt Tolerance to group adducts. Should be <= 0.05 minutes
#'
#' @return The path to the files containing MS1 annotations and edges
#'
#' @export
#'
#' @examples
#' \dontrun{
#' copy_backbone()
#' go_to_cache()
#' github <- "https://raw.githubusercontent.com/"
#' repo <- "taxonomicallyinformedannotation/tima-example-files/main/"
#' data_interim <- "data/interim/"
#' dir <- paste0(github, repo)
#' dir <- paste0(dir, data_interim)
#' dir_sop_mer <- paste0(dir, "libraries/sop/merged/")
#' dir_str <- paste0(dir_sop_mer, "structures/")
#' dir_tax <- paste0(dir_str, "taxonomies/")
#' annotate_masses(
#'   features = paste0(dir, "features/example_features.tsv"),
#'   library = paste0(dir_sop_mer, "keys.tsv"),
#'   str_stereo = paste0(dir_str, "stereo.tsv"),
#'   str_met = paste0(dir_str, "metadata.tsv"),
#'   str_nam = paste0(dir_str, "names.tsv"),
#'   str_tax_cla = paste0(dir_tax, "classyfire.tsv"),
#'   str_tax_npc = paste0(dir_tax, "npc.tsv")
#' )
#' unlink("data", recursive = TRUE)
#' }
annotate_masses <-
  function(
    features = get_params(step = "annotate_masses")$files$features$prepared,
    output_annotations = get_params(
      step = "annotate_masses"
    )$files$annotations$prepared$structural$ms1,
    output_edges = get_params(
      step = "annotate_masses"
    )$files$networks$spectral$edges$raw$ms1,
    name_source = get_params(step = "annotate_masses")$names$source,
    name_target = get_params(step = "annotate_masses")$names$target,
    library = get_params(
      step = "annotate_masses"
    )$files$libraries$sop$merged$keys,
    str_stereo = get_params(
      step = "annotate_masses"
    )$files$libraries$sop$merged$structures$stereo,
    str_met = get_params(
      step = "annotate_masses"
    )$files$libraries$sop$merged$structures$metadata,
    str_nam = get_params(
      step = "annotate_masses"
    )$files$libraries$sop$merged$structures$names,
    str_tax_cla = get_params(
      step = "annotate_masses"
    )$files$libraries$sop$merged$structures$taxonomies$cla,
    str_tax_npc = get_params(
      step = "annotate_masses"
    )$files$libraries$sop$merged$structures$taxonomies$npc,
    adducts_list = get_params(step = "annotate_masses")$ms$adducts,
    clusters_list = get_params(step = "annotate_masses")$ms$clusters,
    neutral_losses_list = get_params(
      step = "annotate_masses"
    )$ms$neutral_losses,
    ms_mode = get_params(step = "annotate_masses")$ms$polarity,
    tolerance_ppm = get_params(
      step = "annotate_masses"
    )$ms$tolerances$mass$ppm$ms1,
    tolerance_rt = get_params(
      step = "annotate_masses"
    )$ms$tolerances$rt$adducts
  ) {
    # Start operation logging
    ctx <- log_operation(
      "annotate_masses",
      ms_mode = ms_mode,
      tolerance_ppm = tolerance_ppm,
      tolerance_rt = tolerance_rt
    )

    # Input Validation ----
    log_info("Starting mass-based annotation")

    # Validate MS mode (cheapest check first)
    validate_ms_mode(ms_mode)

    # Validate tolerances
    validate_tolerances(
      tolerance_ppm = tolerance_ppm,
      tolerance_rt = tolerance_rt,
      max_ppm = 20,
      max_rt = 0.05,
      context = "mass annotation"
    )

    # Validate mode-specific lists
    validate_adduct_list(adducts_list, ms_mode, "adducts_list")
    validate_adduct_list(clusters_list, ms_mode, "clusters_list")

    # Validate file existence
    validate_file_existence(
      list(
        features = features,
        library = library,
        str_stereo = str_stereo,
        str_met = str_met,
        str_nam = str_nam,
        str_tax_cla = str_tax_cla,
        str_tax_npc = str_tax_npc
      )
    )

    log_debug(
      "Configuration: {ms_mode} mode, {tolerance_ppm} ppm, {tolerance_rt} min RT"
    )

    ## Load and Validate Features ----

    # log_trace("Loading features table from: %s", features)
    features_table <- safe_fread(
      file = features,
      file_type = "features table",
      required_cols = c("feature_id"),
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    n_features <- nrow(features_table)
    if (n_features == 0L) {
      log_warn(
        "Empty features table provided - no annotations to perform"
      )

      # Return empty results with proper structure
      ann_cols <- c(
        "feature_id",
        "candidate_structure_error_mz",
        "candidate_structure_name",
        "candidate_structure_inchikey_connectivity_layer",
        "candidate_structure_smiles_no_stereo",
        "candidate_structure_molecular_formula",
        "candidate_structure_exact_mass",
        "candidate_structure_xlogp",
        "candidate_library",
        "candidate_structure_tax_npc_01pat",
        "candidate_structure_tax_npc_02sup",
        "candidate_structure_tax_npc_03cla",
        "candidate_structure_tax_cla_chemontid",
        "candidate_structure_tax_cla_01kin",
        "candidate_structure_tax_cla_02sup",
        "candidate_structure_tax_cla_03cla",
        "candidate_structure_tax_cla_04dirpar",
        "candidate_adduct"
      )
      empty_annotations <- as.data.frame(matrix(
        ncol = length(ann_cols),
        nrow = 0
      ))
      colnames(empty_annotations) <- ann_cols

      edge_cols <- c("CLUSTERID1", "CLUSTERID2", "label")
      empty_edges <- as.data.frame(matrix(ncol = length(edge_cols), nrow = 0))
      colnames(empty_edges) <- edge_cols

      # Write empty outputs
      export_output(x = empty_annotations, file = output_annotations)
      export_output(x = empty_edges, file = output_edges)

      return(c(
        "annotations" = output_annotations[[1]],
        "edges" = output_edges[[1]]
      ))
    }

    log_info("Processing %d features for annotation", n_features)

    # Select Mode-Specific Adducts and Clusters ----

    # Direct assignment is faster than conditional blocks
    adducts <- adducts_list[[ms_mode]]
    clusters <- clusters_list[[ms_mode]]

    # Load and Join Library Tables ----

    # log_trace("Loading library and supplementary data")

    library_table <- safe_fread(
      file = library,
      file_type = "structure library",
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    # Load all supplementary files
    supp_files <- list(str_stereo, str_met, str_nam, str_tax_cla, str_tax_npc)
    supp_names <- c(
      "stereochemistry",
      "metadata",
      "names",
      "ClassyFire taxonomy",
      "NPClassifier taxonomy"
    )

    supp_tables <- purrr::map2(
      .x = supp_files,
      .y = supp_names,
      .f = ~ safe_fread(
        file = .x,
        file_type = .y,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    )

    # Single reduce operation for all joins
    joined_table <- purrr::reduce(
      .x = c(list(library_table), supp_tables),
      .f = tidytable::left_join
    )

    # Filter and prepare structure-organism pairs
    structure_organism_pairs_table <- joined_table |>
      tidytable::filter(!is.na(structure_exact_mass)) |>
      tidytable::mutate(tidytable::across(
        .cols = c("structure_exact_mass"),
        .fns = as.numeric
      ))

    # Add structure_inchikey_connectivity_layer if not present
    if (
      !"structure_inchikey_connectivity_layer" %in%
        colnames(structure_organism_pairs_table)
    ) {
      structure_organism_pairs_table <- structure_organism_pairs_table |>
        tidytable::mutate(
          structure_inchikey_connectivity_layer = stringi::stri_sub(
            str = structure_inchikey,
            from = 1L,
            to = 14L
          )
        )
    }

    structure_organism_pairs_table <- round_reals(
      structure_organism_pairs_table
    )

    # log_trace("Filtering desired adducts and adding mz tolerance")
    df_add_em <- structure_organism_pairs_table |>
      tidytable::filter(!is.na(structure_exact_mass)) |>
      tidytable::distinct(exact_mass = structure_exact_mass) |>
      tidytable::mutate(tidytable::across(
        .cols = c("exact_mass"),
        .fns = as.numeric
      )) |>
      tidytable::mutate(
        value_min = exact_mass - (1E-6 * tolerance_ppm * exact_mass),
        value_max = exact_mass + (1E-6 * tolerance_ppm * exact_mass)
      ) |>
      tidytable::filter(!is.na(value_min)) |>
      tidytable::filter(value_min > 0)

    if (!"adduct" %in% colnames(features_table)) {
      features_table$adduct <- NA_character_
    }
    already_assigned <- features_table |>
      tidytable::distinct(feature_id, adduct) |>
      tidytable::filter(!is.na(adduct))
    log_info(
      "Already %d adducts previously detected",
      nrow(already_assigned)
    )
    features_table <- features_table |>
      harmonize_adducts(adducts_translations = adducts_translations)

    df_fea_min <- features_table |>
      tidytable::mutate(tidytable::across(
        .cols = c("mz"),
        .fns = as.numeric
      )) |>
      tidytable::distinct(feature_id, sample, .keep_all = TRUE)

    if (any(names(features_table) == "rt")) {
      df_fea_min <- df_fea_min |>
        tidytable::mutate(tidytable::across(
          .cols = c("rt"),
          .fns = as.numeric
        ))
    } else {
      log_warn(
        "No 'rt' column found, using sequential numbering as RT proxy"
      )
      df_fea_min$rt <- seq_len(nrow(df_fea_min))
    }

    # log_trace("Calculating rt tolerance")
    df_rt_tol <- df_fea_min |>
      tidytable::mutate(
        rt_min = as.numeric(rt - tolerance_rt),
        rt_max = as.numeric(rt + tolerance_rt)
      )

    # log_trace("Joining within given rt tolerance")
    df_couples_diff <- df_rt_tol |>
      dplyr::inner_join(
        y = df_fea_min,
        by = dplyr::join_by(rt_min <= rt, rt_max >= rt, sample == sample)
      ) |>
      tidytable::distinct(
        feature_id = feature_id.x,
        rt = rt.x,
        mz = mz.x,
        adduct = adduct.x,
        feature_id_dest = feature_id.y,
        mz_dest = mz.y,
        adduct_dest = adduct.y
      ) |>
      tidytable::select(
        tidyselect::everything(),
        feature_id_dest,
        mz_dest,
        adduct_dest
      ) |>
      tidytable::filter(feature_id != feature_id_dest) |>
      tidytable::filter(mz_dest >= mz) |>
      tidytable::mutate(
        delta = mz_dest - mz,
        delta_min = (mz_dest -
          (1E-6 * tolerance_ppm * (mz + mz_dest) / 2) -
          mz),
        delta_max = (mz_dest + (1E-6 * tolerance_ppm * (mz + mz_dest) / 2) - mz)
      )

    if (nrow(df_couples_diff) > 0) {
      bins <- df_couples_diff[,
        .N,
        by = .(bin = cut(delta, breaks = 10000L))
      ] |>
        tidytable::arrange(
          N |>
            tidytable::desc()
        ) |>
        tidytable::slice_head(n = 10L)
      log_info(
        "Here are the top 10 observed m/z differences inside the RT windows:"
      )
      log_info(
        "\n%s",
        paste(
          utils::capture.output(print.data.frame(x = bins, row.names = FALSE)),
          collapse = "\n"
        )
      )
      log_info(
        "These differences may help identify potential preprocessing issues"
      )
      rm(bins)
    }
    df_couples_diff <- df_couples_diff |>
      tidytable::select(-delta)
    rm(df_rt_tol, features_table)

    adducts_table <- adducts |>
      tidytable::tidytable() |>
      tidytable::rename(adduct = 1)
    clusters_table <- clusters |>
      tidytable::tidytable() |>
      tidytable::rename(cluster = 1)

    # log_trace("Forming adducts and clusters")
    add_clu_table <- adducts_table |>
      tidytable::mutate(join = "x") |>
      tidytable::left_join(
        y = clusters_table |>
          tidytable::mutate(join = "x")
      ) |>
      tidytable::bind_rows(adducts_table) |>
      tidytable::mutate(
        adduct = tidytable::if_else(
          condition = !is.na(cluster),
          true = paste0(
            adduct |>
              gsub(
                pattern = "M(?![a-z]).*",
                replacement = "M",
                perl = TRUE
              ),
            "+",
            cluster,
            adduct |>
              gsub(
                pattern = ".*M(?![a-z])",
                replacement = "",
                perl = TRUE
              )
          ),
          false = adduct
        )
      ) |>
      # Single charge monomers only
      tidytable::filter(grepl(
        pattern = "[M",
        x = adduct,
        fixed = TRUE
      )) |>
      tidytable::filter(grepl(pattern = "](\\+|\\-)", x = adduct)) |>
      tidytable::mutate_rowwise(
        adduct_mass = -1 * calculate_mass_of_m(adduct_string = adduct, mz = 0)
      )

    rm(clusters_table)

    # Validate that we have monocharged adducts/clusters to work with
    if (nrow(add_clu_table) == 0L) {
      log_error(
        "No valid monocharged adducts or clusters found for mode %s.
        Cannot proceed with mass annotation.
        Please check your adducts and clusters configuration.",
        ms_mode
      )
      stop(
        "No valid monocharged adducts or clusters found for mode %s.
        Cannot proceed with mass annotation.
        Please check your adducts and clusters configuration.",
        ms_mode
      )
    }

    log_debug(
      "Found %d monocharged adducts/clusters for processing",
      nrow(add_clu_table)
    )

    # log_trace(
    #  "Calculating delta mz for single charge adducts and clusters"
    # )
    differences <-
      dist_groups(
        d = stats::dist(x = add_clu_table$adduct_mass),
        g = add_clu_table$adduct
      ) |>
      tidytable::mutate(
        Group1 = Label |>
          gsub(
            pattern = "Between ",
            replacement = "",
            fixed = TRUE
          ) |>
          gsub(pattern = " and .*", replacement = ""),
        Group2 = Label |>
          gsub(pattern = ".* and ", replacement = "")
      ) |>
      tidytable::select(-Item1, -Item2, -Label) |>
      ## remove redundancy among clusters
      ## Keep proton first
      tidytable::mutate(l = stringi::stri_length(str = Group1)) |>
      tidytable::arrange(l) |>
      tidytable::select(-l) |>
      tidytable::distinct(Distance, .keep_all = TRUE) |>
      ## Using max because ppm tolerance tends to be overconfident
      tidytable::filter(
        Distance >= tolerance_ppm * 1E-6 * max(df_fea_min$mz)
      )

    neutral_losses <- neutral_losses_list |>
      tidytable::tidytable() |>
      tidytable::rename(loss = 1) |>
      tidytable::mutate_rowwise(
        mass = loss |>
          gsub(pattern = " .*", replacement = "") |>
          MetaboCoreUtils::calculateMass()
      )

    # log_trace(
    #  "Joining within given delta mz tolerance (neutral losses)"
    # )
    df_nl <- df_couples_diff |>
      dplyr::inner_join(
        y = neutral_losses,
        by = dplyr::join_by(delta_min <= mass, delta_max >= mass)
      ) |>
      tidytable::filter(!is.na(loss)) |>
      tidytable::distinct(
        feature_id,
        adduct,
        loss,
        mass,
        feature_id_dest,
        adduct_dest
      )

    # log_trace("Joining within given delta mz tolerance (adducts)")
    df_add <- df_couples_diff |>
      dplyr::inner_join(
        y = differences,
        by = dplyr::join_by(delta_min <= Distance, delta_max >= Distance)
      ) |>
      tidytable::filter(!is.na(Group1)) |>
      tidytable::mutate(tidytable::across(
        .cols = c("rt"),
        .fns = as.character
      )) |>
      tidytable::mutate(
        adduct = tidytable::if_else(
          condition = is.na(adduct),
          true = as.character(Group1),
          false = adduct
        ),
        adduct_dest = tidytable::if_else(
          condition = is.na(adduct_dest),
          true = as.character(Group2),
          false = adduct_dest
        )
      ) |>
      tidytable::distinct(
        feature_id,
        adduct,
        adduct_dest,
        !!as.name(paste("feature_id", "dest", sep = "_"))
      )
    rm(df_couples_diff, differences)

    df_nl_min <- df_nl |>
      tidytable::distinct(feature_id, loss, mass)

    # log_trace("Keeping initial and destination feature")
    df_add_a <- df_add |>
      tidytable::distinct(feature_id, adduct)

    df_add_b <- df_add |>
      tidytable::distinct(
        !!as.name(paste("feature_id", "dest", sep = "_")),
        adduct_dest
      ) |>
      tidytable::select(
        feature_id := !!as.name(paste("feature_id", "dest", sep = "_")),
        adduct = adduct_dest
      )

    ## Always considering [M+H]+ and [M-H]- ions for unassigned features
    df_add_enforced <- df_fea_min |>
      tidytable::distinct(feature_id) |>
      tidytable::anti_join(y = already_assigned) |>
      tidytable::mutate(
        adduct = switch(ms_mode, "pos" = "[M+H]+", "neg" = "[M-H]-")
      )
    rm(already_assigned)

    df_add_full <- tidytable::bind_rows(df_add_a, df_add_b, df_add_enforced) |>
      tidytable::distinct()
    rm(df_add_a, df_add_b, df_add_enforced)

    # log_trace("Joining with initial results (adducts)")
    df_adducted <- df_fea_min |>
      tidytable::distinct(feature_id, rt, mz) |>
      tidytable::left_join(y = df_add_full)
    rm(df_add_full)

    # log_trace("Joining with initial results (neutral losses)")
    df_addlossed <- df_adducted |>
      tidytable::left_join(y = df_nl_min) |>
      tidytable::bind_rows(df_adducted) |>
      tidytable::filter(!is.na(adduct)) |>
      tidytable::distinct() |>
      tidytable::mutate(
        adduct = tidytable::if_else(
          condition = !is.na(loss),
          true = paste0(
            adduct |>
              gsub(
                pattern = "M(?![a-z]).*",
                replacement = "M",
                perl = TRUE
              ),
            "-",
            loss,
            adduct |>
              gsub(
                pattern = ".*M(?![a-z])",
                replacement = "",
                perl = TRUE
              )
          ),
          false = adduct
        )
      )
    rm(df_adducted, df_nl_min)

    df_addlossed_min <- df_addlossed |>
      tidytable::mutate_rowwise(
        mass = calculate_mass_of_m(adduct_string = adduct, mz = mz)
      )

    ## Safety
    df_addlossed_min_1 <- df_addlossed_min |>
      tidytable::filter(mass != mz) |>
      tidytable::filter(mass != 0)

    df_addlossed_min_2 <- df_addlossed_min |>
      tidytable::filter(mass == mz | mass == 0)

    if (nrow(df_addlossed_min_2) != 0) {
      log_warn(
        "Some adducts were unproperly detected, defaulting to (de)protonated"
      )
      df_addlossed_min_2 <- df_addlossed_min_2 |>
        tidytable::mutate(
          adduct = switch(ms_mode, "pos" = "[M+H]+", "neg" = "[M-H]-")
        ) |>
        tidytable::mutate_rowwise(
          mass = calculate_mass_of_m(adduct_string = adduct, mz = mz)
        )
    }

    df_addlossed_rdy <- df_addlossed_min_1 |>
      tidytable::bind_rows(df_addlossed_min_2) |>
      tidytable::select(-loss) |>
      tidytable::distinct()
    rm(
      df_addlossed,
      df_addlossed_min,
      df_addlossed_min_1,
      df_addlossed_min_2
    )

    # log_trace("Joining within given mz tol to exact mass library")
    df_addlossed_em <- df_addlossed_rdy |>
      dplyr::inner_join(
        y = df_add_em,
        by = dplyr::join_by(mass >= value_min, mass <= value_max)
      ) |>
      tidytable::mutate(error_mz = exact_mass - mass) |>
      tidytable::mutate(library = adduct) |>
      tidytable::select(
        feature_id,
        rt,
        mz,
        library,
        error_mz,
        exact_mass
      ) |>
      tidytable::distinct()

    # log_trace("Keeping unique exact masses and molecular formulas")
    df_em_mf <- structure_organism_pairs_table |>
      tidytable::distinct(
        tidyselect::any_of(
          x = c(
            "structure_exact_mass",
            "structure_molecular_formula"
          )
        )
      )

    # Select only columns that exist in the table
    available_cols <- colnames(structure_organism_pairs_table)
    required_cols <- c(
      "structure_name",
      "structure_inchikey_connectivity_layer",
      "structure_smiles_no_stereo",
      "structure_molecular_formula",
      "structure_exact_mass",
      "structure_xlogp"
    )
    cols_to_select <- intersect(required_cols, available_cols)

    df_str_unique <- structure_organism_pairs_table |>
      tidytable::distinct(tidyselect::any_of(x = cols_to_select)) |>
      ## Avoid SMILES redundancy
      tidytable::distinct(
        tidyselect::any_of(
          x = c(
            "structure_inchikey_connectivity_layer",
            "structure_molecular_formula",
            "structure_exact_mass",
            "structure_xlogp"
          )
        ),
        .keep_all = TRUE
      ) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(fn = is.numeric),
        .fns = as.character
      ))

    # log_trace("Joining exact masses with single charge adducts")
    # log_trace("Getting back to M")
    df_annotated_1 <- tidytable::left_join(
      x = df_addlossed_em,
      y = df_em_mf,
      by = stats::setNames(object = "structure_exact_mass", nm = "exact_mass")
    ) |>
      tidytable::filter(!(library %in% adducts_forbidden)) |>
      tidytable::distinct()
    rm(df_addlossed_em)

    # log_trace("Calculating multicharged and in source dimers")
    adducts_table_multi <- adducts_table |>
      tidytable::filter(!adduct %in% add_clu_table$adduct) |>
      tidytable::mutate(join = "x")
    rm(adducts_table, add_clu_table)

    if (nrow(adducts_table_multi) != 0) {
      df_multi <- df_fea_min |>
        tidytable::select(-adduct) |>
        tidytable::mutate(join = "x") |>
        tidytable::left_join(y = adducts_table_multi) |>
        tidytable::mutate_rowwise(
          value = calculate_mass_of_m(adduct_string = adduct, mz = mz)
        ) |>
        tidytable::mutate(
          mass_min = value - (1E-6 * tolerance_ppm * value),
          mass_max = value + (1E-6 * tolerance_ppm * value),
          rt_min = rt - tolerance_rt,
          rt_max = rt + tolerance_rt
        ) |>
        tidytable::distinct()
    } else {
      df_multi <- tidytable::tidytable(
        "feature_id" = NA_real_,
        "adduct" = NA_character_,
        "rt" = NA_real_,
        "rt_min" = NA_real_,
        "rt_max" = NA_real_,
        "mass_min" = NA_real_,
        "mass_max" = NA_real_,
        "mz" = NA_real_
      )
    }

    rm(adducts_table_multi)

    # log_trace("Joining within given rt tolerance")
    df_multi_nl <- df_multi |>
      dplyr::inner_join(
        y = df_addlossed_rdy,
        by = dplyr::join_by(
          rt_min <= rt,
          rt_max >= rt,
          mass_min <= mass,
          mass_max >= mass
        )
      )
    rm(df_fea_min, df_multi, neutral_losses, df_addlossed_rdy)

    # log_trace(
    #   "Joining within given mz tol and filtering possible adducts"
    # )
    df_multi_nl_em <- df_multi_nl |>
      dplyr::inner_join(
        y = df_add_em,
        by = dplyr::join_by(mass >= value_min, mass <= value_max)
      ) |>
      tidytable::as_tidytable() |>
      tidytable::mutate(error_mz = exact_mass - mass) |>
      tidytable::rename(
        feature_id = feature_id.x,
        rt = rt.x,
        mz = mz.x,
        library_name = adduct.x
      ) |>
      tidytable::distinct(
        feature_id,
        rt,
        mz,
        exact_mass,
        library_name,
        error_mz
      )
    rm(df_add_em, df_multi_nl)

    df_annotated_2 <-
      tidytable::left_join(
        x = df_multi_nl_em,
        y = df_em_mf,
        by = stats::setNames(object = "structure_exact_mass", nm = "exact_mass")
      ) |>
      tidytable::select(
        tidyselect::any_of(x = "structure_molecular_formula"),
        library = library_name,
        tidyselect::everything(),
        -exact_mass,
      ) |>
      tidytable::filter(!(library %in% adducts_forbidden)) |>
      tidytable::mutate(library = as.character(library)) |>
      tidytable::distinct()
    rm(df_em_mf, df_multi_nl_em)

    # log_trace(
    #   "Joining single adducts, in source dimers, and multicharged"
    # )
    df_annotated_combined <- tidytable::bind_rows(
      df_annotated_1,
      df_annotated_2
    )

    # Find common columns for join
    common_cols <- intersect(
      colnames(df_annotated_combined),
      colnames(df_str_unique)
    )

    # Only join if there are common columns
    if (length(common_cols) > 0) {
      df_annotated_final <- df_annotated_combined |>
        tidytable::left_join(y = df_str_unique, by = common_cols) |>
        tidytable::filter(!is.na(structure_inchikey_connectivity_layer))
    } else {
      # No common columns, just use combined data
      df_annotated_final <- df_annotated_combined
    }

    df_annotated_final <- df_annotated_final |>
      tidytable::select(
        tidyselect::any_of(
          x = c(
            "feature_id",
            "error_mz",
            "structure_name",
            "structure_inchikey_connectivity_layer",
            "structure_smiles_no_stereo",
            "structure_molecular_formula",
            "structure_exact_mass",
            "structure_xlogp",
            "library"
          )
        )
      ) |>
      tidytable::rename(
        candidate_structure_error_mz = tidyselect::any_of(x = "error_mz"),
        candidate_structure_name = tidyselect::any_of(x = "structure_name"),
        candidate_structure_inchikey_connectivity_layer = tidyselect::any_of(
          x = "structure_inchikey_connectivity_layer"
        ),
        candidate_structure_smiles_no_stereo = tidyselect::any_of(
          x = "structure_smiles_no_stereo"
        ),
        candidate_structure_molecular_formula = tidyselect::any_of(
          x = "structure_molecular_formula"
        ),
        candidate_structure_exact_mass = tidyselect::any_of(
          x = "structure_exact_mass"
        ),
        candidate_structure_xlogp = tidyselect::any_of(x = "structure_xlogp"),
        candidate_library = tidyselect::any_of(x = "library")
      ) |>
      tidytable::distinct()

    rm(df_annotated_1, df_annotated_2, df_str_unique)

    # log_trace("Adding chemical classification")
    taxonomy_table <- structure_organism_pairs_table |>
      tidytable::distinct(
        tidyselect::any_of(
          x = c(
            "structure_inchikey_connectivity_layer",
            "structure_smiles_no_stereo",
            "structure_tax_npc_01pat",
            "structure_tax_npc_02sup",
            "structure_tax_npc_03cla",
            "structure_tax_cla_chemontid",
            "structure_tax_cla_01kin",
            "structure_tax_cla_02sup",
            "structure_tax_cla_03cla",
            "structure_tax_cla_04dirpar"
          )
        )
      ) |>
      tidytable::rename(
        candidate_structure_inchikey_connectivity_layer = tidyselect::any_of(
          x = "structure_inchikey_connectivity_layer"
        ),
        candidate_structure_smiles_no_stereo = tidyselect::any_of(
          x = "structure_smiles_no_stereo"
        ),
        candidate_structure_tax_npc_01pat = tidyselect::any_of(
          x = "structure_tax_npc_01pat"
        ),
        candidate_structure_tax_npc_02sup = tidyselect::any_of(
          x = "structure_tax_npc_02sup"
        ),
        candidate_structure_tax_npc_03cla = tidyselect::any_of(
          x = "structure_tax_npc_03cla"
        ),
        candidate_structure_tax_cla_chemontid = tidyselect::any_of(
          x = "structure_tax_cla_chemontid"
        ),
        candidate_structure_tax_cla_01kin = tidyselect::any_of(
          x = "structure_tax_cla_01kin"
        ),
        candidate_structure_tax_cla_02sup = tidyselect::any_of(
          x = "structure_tax_cla_02sup"
        ),
        candidate_structure_tax_cla_03cla = tidyselect::any_of(
          x = "structure_tax_cla_03cla"
        ),
        candidate_structure_tax_cla_04dirpar = tidyselect::any_of(
          x = "structure_tax_cla_04dirpar"
        )
      )

    # Find common columns for taxonomy join
    tax_common_cols <- intersect(
      colnames(df_annotated_final),
      colnames(taxonomy_table)
    )

    # Only join if there are common columns
    if (length(tax_common_cols) > 0) {
      df_final <- tidytable::left_join(
        x = df_annotated_final,
        y = taxonomy_table,
        by = tax_common_cols
      )
    } else {
      df_final <- df_annotated_final
    }

    df_final <- df_final |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(fn = is.character),
        .fns = function(x) {
          tidytable::na_if(x = x, y = "")
        }
      ))

    # Add candidate_adduct and update candidate_library if they exist
    if ("candidate_library" %in% colnames(df_final)) {
      df_final <- df_final |>
        tidytable::mutate(candidate_adduct = candidate_library) |>
        tidytable::mutate(candidate_library = "TIMA MS1")
    }

    rm(structure_organism_pairs_table, df_annotated_final)

    df_final |>
      decorate_masses()

    edges <- tidytable::bind_rows(
      df_add |>
        tidytable::mutate(label = paste0(adduct, " _ ", adduct_dest)) |>
        tidytable::select(
          !!as.name(name_source) := feature_id,
          !!as.name(name_target) := feature_id_dest,
          label
        ) |>
        tidytable::distinct(),
      df_nl |>
        tidytable::mutate(label = paste0(loss, " loss")) |>
        tidytable::select(
          !!as.name(name_source) := feature_id,
          !!as.name(name_target) := feature_id_dest,
          label
        ) |>
        tidytable::distinct()
    )
    rm(df_nl, df_add)

    export_params(
      parameters = get_params(step = "annotate_masses"),
      step = "annotate_masses"
    )
    export_output(x = edges, file = output_edges[[1]])
    export_output(x = df_final, file = output_annotations[[1]])

    log_complete(ctx, n_annotations = nrow(df_final), n_edges = nrow(edges))

    rm(edges, df_final)
    return(
      c(
        "annotations" = output_annotations[[1]],
        "edges" = output_edges[[1]]
      )
    )
  }
