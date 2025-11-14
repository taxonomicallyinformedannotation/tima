#' @title Annotate masses
#'
#' @description This function annotates a feature table based on exact mass
#' match. It requires a structural library, its metadata, and lists of adducts,
#' clusters, and neutral losses to be considered. The polarity has to be `pos`
#' or `neg` and retention time and mass tolerances should be given. The feature
#' table is expected to be pre-formatted.
#'
#' @include adducts_forbidden.R
#' @include adducts_translations.R
#' @include calculate_mass_of_m.R
#' @include decorate_masses.R
#' @include dist_groups.R
#' @include get_params.R
#' @include harmonize_adducts.R
#' @include round_reals.R
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
    # ============================================================================
    # Input Validation
    # ============================================================================

    # Validate MS mode first (cheapest check)
    if (!ms_mode %in% c("pos", "neg")) {
      stop("ms_mode must be either 'pos' or 'neg', got: ", ms_mode)
    }

    # Validate tolerances (numeric checks)
    if (
      !is.numeric(tolerance_ppm) || tolerance_ppm <= 0 || tolerance_ppm > 20
    ) {
      stop(
        "tolerance_ppm must be a positive number <= 20, got: ",
        tolerance_ppm
      )
    }

    if (!is.numeric(tolerance_rt) || tolerance_rt <= 0 || tolerance_rt > 0.05) {
      stop(
        "tolerance_rt must be a positive number <= 0.05, got: ",
        tolerance_rt
      )
    }

    # Validate adducts, clusters, and neutral losses lists
    if (!is.list(adducts_list) || is.null(adducts_list[[ms_mode]])) {
      stop("adducts_list must contain '", ms_mode, "' mode adducts")
    }

    if (!is.list(clusters_list) || is.null(clusters_list[[ms_mode]])) {
      stop("clusters_list must contain '", ms_mode, "' mode clusters")
    }

    # File existence check
    required_files <- c(
      features = features,
      library = library,
      str_stereo = str_stereo,
      str_met = str_met,
      str_nam = str_nam,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    )

    file_exists_check <- file.exists(required_files)
    if (!all(file_exists_check)) {
      missing_files <- names(required_files)[!file_exists_check]
      stop(
        "Required file(s) not found: ",
        paste(missing_files, collapse = ", "),
        "\nPaths: ",
        paste(required_files[!file_exists_check], collapse = ", ")
      )
    }

    logger::log_info("Starting mass-based annotation in {ms_mode} mode")
    logger::log_debug("Tolerances: {tolerance_ppm} ppm, {tolerance_rt} min RT")

    # ============================================================================
    # Load and Validate Features
    # ============================================================================

    logger::log_trace("Loading features table from: {features}")
    features_table <- tidytable::fread(
      file = features,
      na.strings = c("", "NA"),
      colClasses = "character"
    )

    n_features <- nrow(features_table)
    if (n_features == 0L) {
      logger::log_warn(
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

      return(list(annotations = output_annotations, edges = output_edges))
    }

    logger::log_info("Processing {n_features} features for annotation")

    # ============================================================================
    # Select Mode-Specific Adducts and Clusters
    # ============================================================================

    # Direct assignment is faster than conditional blocks
    adducts <- adducts_list[[ms_mode]]
    clusters <- clusters_list[[ms_mode]]

    # ============================================================================
    # Load and Join Library Tables
    # ============================================================================

    logger::log_trace("Loading library and supplementary data")

    library_table <-
      tidytable::fread(
        file = library,
        na.strings = c("", "NA"),
        colClasses = "character"
      )

    # Load all supplementary files
    supp_files <- list(str_stereo, str_met, str_nam, str_tax_cla, str_tax_npc)

    supp_tables <- purrr::map(.x = supp_files, .f = function(file_path) {
      tidytable::fread(
        file = file_path,
        na.strings = c("", "NA"),
        colClasses = "character"
      )
    })

    # Single reduce operation for all joins
    joined_table <- purrr::reduce(
      .x = c(list(library_table), supp_tables),
      .f = function(x, y) {
        tidytable::left_join(x, y)
      }
    )

    # Filter and prepare structure-organism pairs
    structure_organism_pairs_table <- joined_table |>
      tidytable::filter(!is.na(structure_exact_mass)) |>
      tidytable::mutate(tidytable::across(
        .cols = c("structure_exact_mass"),
        .fns = as.numeric
      )) |>
      round_reals()

    logger::log_trace("Filtering desired adducts and adding mz tolerance")
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
    logger::log_info(
      "Already {
      nrow(already_assigned)
      } adducts previously detected"
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
        tidytable::mutate(tidytable::across(.cols = c("rt"), .fns = as.numeric))
    } else {
      df_fea_min[, "rt"] <- df_fea_min[, "feature_id"] |>
        as.numeric()
    }

    logger::log_trace("Calculating rt tolerance")
    df_rt_tol <- df_fea_min |>
      tidytable::mutate(
        rt_min = as.numeric(rt - tolerance_rt),
        rt_max = as.numeric(rt + tolerance_rt)
      )

    logger::log_trace("Joining within given rt tolerance")
    df_couples_diff <- df_rt_tol |>
      dplyr::inner_join(
        df_fea_min,
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
      logger::log_info(
        "Here are the top 10 observed m/z differences inside the RT windows:"
      )
      logger::log_info(
        "\n{paste(capture.output(print.data.frame(bins, row.names = FALSE)), collapse = '\n')}"
      )
      logger::log_info(
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

    logger::log_trace("Forming adducts and clusters")
    add_clu_table <- adducts_table |>
      tidytable::mutate(join = "x") |>
      tidytable::left_join(
        clusters_table |>
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

    # TODO add safety if no monocharged?

    logger::log_trace(
      "Calculating delta mz for single charge adducts and clusters"
    )
    differences <-
      dist_groups(
        d = stats::dist(add_clu_table$adduct_mass),
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
      tidytable::mutate(l = stringi::stri_length(Group1)) |>
      tidytable::arrange(l) |>
      tidytable::select(-l) |>
      tidytable::distinct(Distance, .keep_all = TRUE) |>
      ## Using max because ppm tolerance tends to be overconfident
      tidytable::filter(Distance >= tolerance_ppm * 1E-6 * max(df_fea_min$mz))

    neutral_losses <- neutral_losses_list |>
      tidytable::tidytable() |>
      tidytable::rename(loss = 1) |>
      tidytable::mutate_rowwise(
        mass = loss |>
          gsub(pattern = " .*", replacement = "") |>
          MetaboCoreUtils::calculateMass()
      )

    logger::log_trace(
      "Joining within given delta mz tolerance (neutral losses)"
    )
    df_nl <- df_couples_diff |>
      dplyr::inner_join(
        neutral_losses,
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

    logger::log_trace("Joining within given delta mz tolerance (adducts)")
    df_add <- df_couples_diff |>
      dplyr::inner_join(
        differences,
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

    logger::log_trace("Keeping initial and destination feature")
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
      tidytable::anti_join(already_assigned) |>
      tidytable::mutate(
        adduct = switch(ms_mode, "pos" = "[M+H]+", "neg" = "[M-H]-")
      )
    rm(already_assigned)

    df_add_full <- tidytable::bind_rows(df_add_a, df_add_b, df_add_enforced) |>
      tidytable::distinct()
    rm(df_add_a, df_add_b, df_add_enforced)

    logger::log_trace("Joining with initial results (adducts)")
    df_adducted <- df_fea_min |>
      tidytable::distinct(feature_id, rt, mz) |>
      tidytable::left_join(df_add_full)
    rm(df_add_full)

    logger::log_trace("Joining with initial results (neutral losses)")
    df_addlossed <- df_adducted |>
      tidytable::left_join(df_nl_min) |>
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
      logger::log_warn(
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

    logger::log_trace("Joining within given mz tol to exact mass library")
    df_addlossed_em <- df_addlossed_rdy |>
      dplyr::inner_join(
        df_add_em,
        by = dplyr::join_by(mass >= value_min, mass <= value_max)
      ) |>
      tidytable::mutate(error_mz = exact_mass - mass) |>
      tidytable::mutate(library = adduct) |>
      tidytable::select(feature_id, rt, mz, library, error_mz, exact_mass) |>
      tidytable::distinct()

    logger::log_trace("Keeping unique exact masses and molecular formulas")
    df_em_mf <- structure_organism_pairs_table |>
      tidytable::distinct(structure_exact_mass, structure_molecular_formula)
    df_str_unique <- structure_organism_pairs_table |>
      tidytable::distinct(
        structure_name,
        structure_inchikey_connectivity_layer,
        structure_smiles_no_stereo,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp
      ) |>
      ## Avoid SMILES redundancy
      tidytable::distinct(
        structure_inchikey_connectivity_layer,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(is.numeric),
        .fns = as.character
      ))

    logger::log_trace("Joining exact masses with single charge adducts")
    logger::log_trace("Getting back to M")
    df_annotated_1 <- tidytable::left_join(
      x = df_addlossed_em,
      y = df_em_mf,
      by = stats::setNames("structure_exact_mass", "exact_mass")
    ) |>
      tidytable::filter(!(library %in% adducts_forbidden)) |>
      tidytable::distinct()
    rm(df_addlossed_em)

    logger::log_trace("Calculating multicharged and in source dimers")
    adducts_table_multi <- adducts_table |>
      tidytable::filter(!adduct %in% add_clu_table$adduct) |>
      tidytable::mutate(join = "x")
    rm(adducts_table, add_clu_table)

    if (nrow(adducts_table_multi) != 0) {
      df_multi <- df_fea_min |>
        tidytable::select(-adduct) |>
        tidytable::mutate(join = "x") |>
        tidytable::left_join(adducts_table_multi) |>
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

    logger::log_trace("Joining within given rt tolerance")
    df_multi_nl <- df_multi |>
      dplyr::inner_join(
        df_addlossed_rdy,
        by = dplyr::join_by(
          rt_min <= rt,
          rt_max >= rt,
          mass_min <= mass,
          mass_max >= mass
        )
      )
    rm(df_fea_min, df_multi, neutral_losses, df_addlossed_rdy)

    logger::log_trace(
      "Joining within given mz tol and filtering possible adducts"
    )
    df_multi_nl_em <- df_multi_nl |>
      dplyr::inner_join(
        df_add_em,
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
        df_multi_nl_em,
        df_em_mf,
        by = stats::setNames("structure_exact_mass", "exact_mass")
      ) |>
      tidytable::select(
        structure_molecular_formula,
        library = library_name,
        tidyselect::everything(),
        -exact_mass,
      ) |>
      tidytable::filter(!(library %in% adducts_forbidden)) |>
      tidytable::mutate(library = as.character(library)) |>
      tidytable::distinct()
    rm(df_em_mf, df_multi_nl_em)

    logger::log_trace(
      "Joining single adducts, in source dimers, and multicharged"
    )
    df_annotated_final <- tidytable::bind_rows(
      df_annotated_1,
      df_annotated_2
    ) |>
      tidytable::left_join(df_str_unique) |>
      tidytable::filter(!is.na(structure_inchikey_connectivity_layer)) |>
      tidytable::select(
        feature_id,
        candidate_structure_error_mz = error_mz,
        candidate_structure_name = structure_name,
        candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
        candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
        candidate_structure_molecular_formula = structure_molecular_formula,
        candidate_structure_exact_mass = structure_exact_mass,
        candidate_structure_xlogp = structure_xlogp,
        candidate_library = library
      ) |>
      tidytable::distinct()

    rm(df_annotated_1, df_annotated_2, df_str_unique)

    logger::log_trace("Adding chemical classification")
    df_final <- tidytable::left_join(
      df_annotated_final,
      structure_organism_pairs_table |>
        tidytable::distinct(
          candidate_structure_inchikey_connectivity_layer = structure_inchikey_connectivity_layer,
          candidate_structure_smiles_no_stereo = structure_smiles_no_stereo,
          candidate_structure_tax_npc_01pat = structure_tax_npc_01pat,
          candidate_structure_tax_npc_02sup = structure_tax_npc_02sup,
          candidate_structure_tax_npc_03cla = structure_tax_npc_03cla,
          ## TODO until better
          candidate_structure_tax_cla_chemontid = structure_tax_cla_chemontid,
          candidate_structure_tax_cla_01kin = structure_tax_cla_01kin,
          candidate_structure_tax_cla_02sup = structure_tax_cla_02sup,
          candidate_structure_tax_cla_03cla = structure_tax_cla_03cla,
          candidate_structure_tax_cla_04dirpar = structure_tax_cla_04dirpar
        )
    ) |>
      tidytable::mutate(tidytable::across(
        .cols = tidyselect::where(is.character),
        .fns = function(x) {
          tidytable::na_if(x, "")
        }
      )) |>
      tidytable::mutate(candidate_adduct = candidate_library) |>
      tidytable::mutate(candidate_library = "TIMA MS1")
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

    rm(edges, df_final)
    return(
      c(
        "annotations" = output_annotations[[1]],
        "edges" = output_edges[[1]]
      )
    )
  }
