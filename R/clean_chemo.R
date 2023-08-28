utils::globalVariables(
  c(
    "annot_table_wei_chemo",
    "best_candidate",
    "best_candidate_organism",
    "best_candidate_structure",
    "candidates_final",
    "component_id",
    "components_table",
    "consensus_structure_cla_cla",
    "consensus_structure_cla_kin",
    "consensus_structure_cla_par",
    "consensus_structure_cla_sup",
    "consensus_structure_npc_cla",
    "consensus_structure_npc_pat",
    "consensus_structure_npc_sup",
    "consistency_structure_cla_cla",
    "consistency_structure_cla_kin",
    "consistency_structure_cla_par",
    "consistency_structure_cla_sup",
    "consistency_structure_npc_cla",
    "consistency_structure_npc_pat",
    "consistency_structure_npc_sup",
    "count_peaks_explained",
    "count_peaks_matched",
    "error_mz",
    "error_rt",
    "feature_id",
    "features_table",
    "minimal_ms1_bio",
    "minimal_ms1_chemo",
    "mz",
    "organism_name",
    "rank_final",
    "rank_initial",
    "reference_doi",
    "rt",
    "score_biological",
    "score_chemical",
    "score_final",
    "score_input",
    "score_interim",
    "score_pondered_bio",
    "score_pondered_chemo",
    "structure_01_cla_kingdom",
    "structure_01_npc_pathway",
    "structure_02_cla_superclass",
    "structure_02_npc_superclass",
    "structure_03_cla_class",
    "structure_03_npc_class",
    "structure_04_cla_parent",
    "structure_exact_mass",
    "structure_inchikey_2D",
    "structure_molecular_formula",
    "structure_name",
    "structure_organism_pairs_table",
    "structure_smiles_2D",
    "structure_taxonomy_classyfire_01kingdom",
    "structure_taxonomy_classyfire_02superclass",
    "structure_taxonomy_classyfire_03class",
    "structure_taxonomy_classyfire_04directparent",
    "structure_taxonomy_npclassifier_01pathway",
    "structure_taxonomy_npclassifier_02superclass",
    "structure_taxonomy_npclassifier_03class",
    "structure_xlogp",
    "summarise",
    "value"
  )
)

#' @title Clean chemo
#'
#' @description This function cleans the results
#'    obtained after chemical weighting
#'
#' @include clean_collapse.R
#'
#' @param annot_table_wei_chemo Table containing your
#'    chemically weighted annotation
#' @param components_table Prepared components file
#' @param features_table Prepared features file
#' @param structure_organism_pairs_table Table containing the
#'    structure - organism pairs
#' @param candidates_final Number of final candidates to keep
#' @param minimal_ms1_bio Minimal biological score to keep MS1 based annotation
#' @param minimal_ms1_chemo Minimal chemical score to keep MS1 based annotation
#' @param minimal_ms1_condition Condition to be used. Must be "OR" or "AND".
#' @param summarise Boolean. summarise results (1 row per feature)
#'
#' @return A table containing the chemically weighted annotation
#'    where only a given number of initial candidates are kept
#'
#' @export
#'
#' @seealso weight_chemo
#'
#' @examples NULL
clean_chemo <-
  function(annot_table_wei_chemo = get("annot_table_wei_chemo",
             envir = parent.frame()
           ),
           components_table = get("components_table",
             envir = parent.frame()
           ),
           features_table = get("features_table",
             envir = parent.frame()
           ),
           structure_organism_pairs_table = get(
             "structure_organism_pairs_table",
             envir = parent.frame()
           ),
           candidates_final = get("candidates_final",
             envir = parent.frame()
           ),
           minimal_ms1_bio = get("minimal_ms1_bio",
             envir = parent.frame()
           ),
           minimal_ms1_chemo = get("minimal_ms1_chemo",
             envir = parent.frame()
           ),
           minimal_ms1_condition = get("minimal_ms1_condition",
             envir = parent.frame()
           ),
           summarise = get("summarise",
             envir = parent.frame()
           )) {
    log_debug(
      "filtering top ",
      candidates_final,
      " candidates and keeping only MS1 candidates with minimum \n",
      minimal_ms1_bio,
      " biological score \n",
      minimal_ms1_condition,
      minimal_ms1_chemo,
      "chemical score \n"
    )
    if (minimal_ms1_condition == "OR") {
      df1 <- annot_table_wei_chemo |>
        tidytable::filter(
          score_input > 0 | (
            ## Those lines are to keep ms1 annotation
            score_biological >= minimal_ms1_bio |
              ## Only if a good biological
              score_chemical >= minimal_ms1_chemo
          )
          ## Or chemical consistency score is obtained
        )
    }
    if (minimal_ms1_condition == "AND") {
      df1 <- annot_table_wei_chemo |>
        tidytable::filter(
          score_input > 0 | (
            ## Those lines are to keep ms1 annotation
            score_biological >= minimal_ms1_bio &
              ## Only if a good biological
              score_chemical >= minimal_ms1_chemo
          )
          ## Or chemical consistency score is obtained
        )
    }

    df1 <- df1 |>
      tidytable::distinct(feature_id,
        structure_inchikey_2D,
        .keep_all = TRUE
      ) |>
      tidytable::mutate(
        rank_final = (tidytable::dense_rank(-score_pondered_chemo)),
        .by = c(feature_id)
      ) |>
      tidytable::filter(rank_final <= candidates_final, .by = c(feature_id))

    log_debug("adding initial metadata (RT, etc.) and simplifying columns \n")
    df2 <- features_table |>
      tidytable::left_join(df1) |>
      tidytable::left_join(components_table) |>
      tidytable::mutate(
        best_candidate_structure = paste(
          structure_taxonomy_npclassifier_01pathway,
          structure_taxonomy_npclassifier_02superclass,
          structure_taxonomy_npclassifier_03class,
          sep = "\u00a7"
        )
      ) |>
      tidytable::distinct(
        feature_id,
        component_id,
        rt,
        mz,
        structure_molecular_formula,
        structure_exact_mass,
        structure_xlogp,
        structure_inchikey_2D,
        structure_smiles_2D,
        structure_name,
        structure_01_cla_kingdom =
          structure_taxonomy_classyfire_01kingdom,
        structure_01_npc_pathway =
          structure_taxonomy_npclassifier_01pathway,
        structure_02_cla_superclass =
          structure_taxonomy_classyfire_02superclass,
        structure_02_npc_superclass =
          structure_taxonomy_npclassifier_02superclass,
        structure_03_cla_class =
          structure_taxonomy_classyfire_03class,
        structure_03_npc_class =
          structure_taxonomy_npclassifier_03class,
        structure_04_cla_parent =
          structure_taxonomy_classyfire_04directparent,
        library,
        error_mz,
        error_rt,
        rank_initial,
        rank_final,
        score_input,
        score_biological,
        score_interim = score_pondered_bio,
        score_chemical,
        score_final = score_pondered_chemo,
        count_peaks_explained,
        count_peaks_matched,
        best_candidate_organism = best_candidate,
        best_candidate_structure,
        consensus_structure_cla_kin,
        consistency_structure_cla_kin,
        consensus_structure_npc_pat,
        consistency_structure_npc_pat,
        consensus_structure_cla_sup,
        consistency_structure_cla_sup,
        consensus_structure_npc_sup,
        consistency_structure_npc_sup,
        consensus_structure_cla_cla,
        consistency_structure_cla_cla,
        consensus_structure_npc_cla,
        consistency_structure_npc_cla,
        consensus_structure_cla_par,
        consistency_structure_cla_par
      )

    references <- structure_organism_pairs_table |>
      tidytable::select(
        structure_inchikey_2D,
        reference_doi,
        organism_name,
        tidytable::contains("organism_taxonomy_")
      ) |>
      tidytable::distinct() |>
      tidytable::pivot_longer(tidytable::contains("organism_taxonomy_")) |>
      tidytable::filter(!is.na(value)) |>
      tidytable::filter(value != "notClassified") |>
      tidytable::distinct(structure_inchikey_2D,
        best_candidate_organism = value,
        reference_doi
      )

    log_debug("adding references \n")
    df3 <- df2 |>
      tidytable::left_join(references) |>
      tidytable::group_by(c(-reference_doi)) |>
      clean_collapse(cols = c("reference_doi")) |>
      tidytable::select(
        feature_id,
        component_id,
        rt,
        mz,
        structure_molecular_formula,
        structure_inchikey_2D,
        structure_smiles_2D,
        structure_name,
        structure_exact_mass,
        structure_xlogp,
        structure_01_cla_kingdom,
        structure_01_npc_pathway,
        structure_02_cla_superclass,
        structure_02_npc_superclass,
        structure_03_cla_class,
        structure_03_npc_class,
        structure_04_cla_parent,
        score_input,
        score_biological,
        score_chemical,
        count_peaks_matched,
        count_peaks_explained,
        library,
        error_mz,
        error_rt,
        score_interim,
        score_final,
        rank_initial,
        rank_final,
        best_candidate_organism,
        best_candidate_structure,
        reference_doi,
        tidytable::everything()
      ) |>
      tidytable::arrange(rank_final)

    if (summarise == TRUE) {
      log_debug("Collecting garbage ...")
      gc()
      log_debug("summarizing results \n")
      df4 <- df3 |>
        tidytable::group_by(feature_id) |>
        tidytable::reframe(tidytable::across(
          .cols = colnames(df3)[5:32],
          .fns = function(x) {
            gsub(
              pattern = "\\bNA\\b",
              replacement = "",
              x = paste(x, collapse = "|")
            )
          }
        )) |>
        tidytable::ungroup()

      df5 <- df4 |>
        tidytable::left_join(df3 |>
          tidytable::select(
            "feature_id",
            !colnames(df4)
          ) |>
          tidytable::distinct())
    } else {
      df5 <- df3
    }

    log_debug("selecting columns to export \n")
    df6 <- df5 |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::everything(),
          .fns = as.character
        )
      ) |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::where(is.character),
          .fns = trimws
        )
      ) |>
      tidytable::mutate(
        tidytable::across(
          .cols = tidytable::where(is.character),
          .fns = function(x) {
            tidytable::na_if(x, "")
          }
        )
      ) |>
      tidytable::select(tidytable::any_of(
        c(
          "feature_id",
          "component_id",
          "mz",
          "rt",
          "structure_name",
          "structure_molecular_formula",
          "structure_xlogp",
          "structure_smiles_2D",
          "structure_inchikey_2D",
          "library",
          "error_mz",
          "error_rt",
          "score_input",
          "score_biological",
          "score_chemical",
          "score_final",
          "count_peaks_matched",
          "count_peaks_explained",
          "rank_initial",
          "rank_final",
          "best_candidate_organism",
          "best_candidate_structure",
          "consensus_structure_cla_kin",
          "consistency_structure_cla_kin",
          "consensus_structure_npc_pat",
          "consistency_structure_npc_pat",
          "consensus_structure_cla_sup",
          "consistency_structure_cla_sup",
          "consensus_structure_npc_sup",
          "consistency_structure_npc_sup",
          "consensus_structure_cla_cla",
          "consistency_structure_cla_cla",
          "consensus_structure_npc_cla",
          "consistency_structure_npc_cla",
          "consensus_structure_cla_par",
          "consistency_structure_cla_par",
          "reference_doi"
        )
      ))

    log_debug("adding consensus again to droped candidates \n")
    df8 <- df6 |>
      tidytable::filter(!is.na(structure_inchikey_2D))

    df9 <- df6 |>
      tidytable::filter(is.na(structure_inchikey_2D))

    df10 <- tidytable::left_join(
      df9,
      annot_table_wei_chemo |>
        tidytable::mutate(tidytable::across(
          .cols = tidytable::everything(),
          .fns = as.character
        ))
    ) |>
      tidytable::select(tidytable::any_of(
        c(
          "feature_id",
          "component_id",
          "mz",
          "rt",
          "consensus_structure_cla_kin",
          "consistency_structure_cla_kin",
          "consensus_structure_npc_pat",
          "consistency_structure_npc_pat",
          "consensus_structure_cla_sup",
          "consistency_structure_cla_sup",
          "consensus_structure_npc_sup",
          "consistency_structure_npc_sup",
          "consensus_structure_cla_cla",
          "consistency_structure_cla_cla",
          "consensus_structure_npc_cla",
          "consistency_structure_npc_cla",
          "consensus_structure_cla_par",
          "consistency_structure_cla_par"
        )
      )) |>
      tidytable::distinct()

    df11 <- tidytable::bind_rows(df8, df10) |>
      tidytable::arrange(as.numeric(feature_id))

    ## Because cytoscape import fails otherwise
    colnames(df11) <-
      stringi::stri_replace_all_fixed(
        str = colnames(df11),
        pattern = "_structure",
        replacement = "",
        vectorize_all = FALSE
      )

    return(df11)
  }
