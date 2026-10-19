#' @title Prepare annotations from mzTab-M
#'
#' @description Extracts structural annotations from mzTab-M tables and
#'   standardizes them for TIMA weighting and filtering steps.
#'
#' @details Annotation source priority:
#' \enumerate{
#'   \item SME (evidence) rows — highest specificity; mapped back to SML IDs
#'     via the `SME_ID_REFS` column when an SML section is present.
#'   \item SML (small molecule summary) rows — used when no SME section exists.
#'   \item SMF (feature) rows — fallback for feature-only files.
#' }
#' When no `input` is provided (or the file does not exist) an empty annotation
#' table is written and the function returns silently.
#'
#' @include get_params.R
#' @include read_mztab.R
#' @include export_output.R
#' @include select_annotations_columns.R
#' @include validations_utils.R
#' @include logs_utils.R
#' @include columns_utils.R
#' @include assert_utils.R
#' @include mztab_parser.R
#' @include mztab_validate.R
#' @include validations_params.R
#'
#' @param input `character(1)` Path to an mzTab-M file (`.mztab` or `.json`).
#' @param output `character(1)` Output path for the prepared annotation table.
#' @param str_stereo `character(1)` Path to the structure stereo lookup table.
#' @param str_met `character(1)` Path to the structure metadata lookup table.
#' @param str_tax_cla `character(1)` Path to the ClassyFire taxonomy table.
#' @param str_tax_npc `character(1)` Path to the NPClassifier taxonomy table.
#' @param strict `logical(1)` If `TRUE`, apply strict SME required-column
#'   validation during parsing.
#'
#' @return Character path to the prepared annotation file (invisibly when the
#'   empty-annotation fallback is used).
#'
#' @family preparation
#' @export
#'
#' @examples
#' \dontrun{
#' # Prepare annotations from an mzTab-M file
#' prepare_annotations_mztab(
#'   input = "annotations.mztab",
#'   output = "prepared_annotations.tsv"
#' )
#' }
prepare_annotations_mztab <- function(
  input = get_params(step = "prepare_annotations_mztab")$files$mztab$raw,
  output = get_params(
    step = "prepare_annotations_mztab"
  )$files$annotations$prepared$structural$mztab,
  str_stereo = get_params(
    step = "prepare_annotations_mztab"
  )$files$libraries$sop$merged$structures$stereo,
  str_met = get_params(
    step = "prepare_annotations_mztab"
  )$files$libraries$sop$merged$structures$metadata,
  str_tax_cla = get_params(
    step = "prepare_annotations_mztab"
  )$files$libraries$sop$merged$structures$taxonomies$cla,
  str_tax_npc = get_params(
    step = "prepare_annotations_mztab"
  )$files$libraries$sop$merged$structures$taxonomies$npc,
  strict = FALSE
) {
  validate_character(output, param_name = "output")
  assert_flag(strict, "strict")

  validate_file_existence(
    list(
      str_stereo = str_stereo,
      str_met = str_met,
      str_tax_cla = str_tax_cla,
      str_tax_npc = str_tax_npc
    ),
    allow_null = FALSE
  )

  if (
    is.null(input) ||
      !is.character(input) ||
      !nzchar(input) ||
      !file.exists(input)
  ) {
    log_warn(
      "No mzTab input provided for prepare_annotations_mztab, exporting empty annotations"
    )
    export_output(
      x = fake_annotations_columns(),
      file = output
    )
    return(output)
  }

  tabs <- read_mztab_tables(input)
  validate_mztab_tables(tabs, strict = strict)

  src <- if (nrow(tabs$sme) > 0L) {
    tabs$sme
  } else if (nrow(tabs$sml) > 0L) {
    tabs$sml
  } else {
    tabs$smf
  }

  id_col <- if ("SML_ID" %in% names(src)) {
    "SML_ID"
  } else if ("evidence_input_id" %in% names(src)) {
    "evidence_input_id"
  } else {
    "SMF_ID"
  }

  # Map SME evidence rows back to SML IDs when summary rows are present.
  if (
    identical(id_col, "evidence_input_id") &&
      nrow(tabs$sml) > 0L &&
      "SME_ID_REFS" %in% names(tabs$sml) &&
      "SME_ID" %in% names(src)
  ) {
    refs_split <- strsplit(
      as.character(tabs$sml$SME_ID_REFS),
      "|",
      fixed = TRUE
    )
    ref_lengths <- lengths(refs_split)
    refs_flat <- unlist(refs_split, use.names = FALSE)
    sml_flat <- rep(as.character(tabs$sml$SML_ID), ref_lengths)

    keep <- !is.na(refs_flat) & nzchar(refs_flat) & refs_flat != "null"
    refs_flat <- refs_flat[keep]
    sml_flat <- sml_flat[keep]

    if (length(refs_flat) > 0L) {
      first_ref <- !duplicated(refs_flat)
      sme_keys <- refs_flat[first_ref]
      sml_vals <- sml_flat[first_ref]
      mapped <- sml_vals[match(as.character(src$SME_ID), sme_keys)]
    } else {
      mapped <- rep(NA_character_, nrow(src))
    }

    fallback <- as.character(src[[id_col]])
    fallback[fallback %in% c("", "null", "NA")] <- NA_character_
    mapped[is.na(mapped)] <- fallback[is.na(mapped)]
  } else {
    mapped <- as.character(src[[id_col]])
  }

  lib_col <- .mztab_pick_col(
    src,
    c("identification_method", "candidate_library")
  )

  # Spectral similarity is only meaningful for MS2 / spectral-library evidence.
  # TIMA writes its spectral similarity to id_confidence_measure[4] (present
  # only for spectral evidence); standard mzTab exposes it via
  # best_id_confidence_value. MS1 evidence (exact-mass database search) carries
  # no spectral match, so the similarity is left NA — this also keeps the
  # downstream ms1_only weighting filter correct. The combined confidence
  # (id_confidence_measure[1] / best_id_evidence_value / reliability) is NOT a
  # spectral similarity and must not masquerade as one.
  spectral_sources <- c(
    "id_confidence_measure[4]",
    "candidate_score_similarity",
    "best_id_confidence_value",
    "best_id_evidence_value",
    "best_id_score"
  )
  spectral_raw <- .mztab_pick_col(src, spectral_sources)
  candidate_score_similarity <- suppressWarnings(as.numeric(spectral_raw))
  candidate_score_similarity[!is.finite(candidate_score_similarity)] <-
    NA_real_

  is_ms1 <- !is.na(lib_col) &
    grepl("exact mass database search", lib_col, ignore.case = TRUE)
  candidate_score_similarity[is_ms1] <- NA_real_

  ann <- tidytable::tidytable(
    feature_id = mapped,
    candidate_library = ifelse(
      is.na(lib_col) | !nzchar(lib_col),
      "mztab",
      lib_col
    ),
    candidate_structure_name = .mztab_pick_col(
      src,
      c("chemical_name", "description")
    ),
    candidate_structure_smiles_no_stereo = .mztab_pick_col(
      src,
      c("smiles", "SMILES")
    ),
    candidate_structure_inchi = .mztab_pick_col(src, c("inchi", "InChI")),
    candidate_structure_inchikey_connectivity_layer = .mztab_pick_col(
      src,
      c("database_identifier", "database_id")
    ),
    candidate_structure_molecular_formula = .mztab_pick_col(
      src,
      c("chemical_formula", "formula")
    ),
    candidate_structure_exact_mass = suppressWarnings(as.numeric(.mztab_pick_col(
      src,
      c(
        "theoretical_neutral_mass",
        "theoretical_mass_to_charge",
        "calc_mass_to_charge",
        "exp_mass_to_charge"
      )
    ))),
    candidate_adduct = .mztab_pick_col(
      src,
      c("adduct_ion", "adduct_ions", "adduct")
    ),
    candidate_score_similarity = candidate_score_similarity,
    candidate_structure_error_mz = NA_real_,
    candidate_structure_error_rt = NA_real_,
    candidate_count_similarity_peaks_matched = NA_integer_,
    rank_final = .mztab_pick_col(src, c("rank"))
  )

  ann <- ann |>
    tidytable::mutate(
      candidate_structure_tax_npc_01pat = NA_character_,
      candidate_structure_tax_npc_02sup = NA_character_,
      candidate_structure_tax_npc_03cla = NA_character_,
      candidate_structure_tax_cla_chemontid = NA_character_,
      candidate_structure_tax_cla_01kin = NA_character_,
      candidate_structure_tax_cla_02sup = NA_character_,
      candidate_structure_tax_cla_03cla = NA_character_,
      candidate_structure_tax_cla_04dirpar = NA_character_
    )

  ann <- select_annotations_columns(
    df = ann,
    str_stereo = str_stereo,
    str_met = str_met,
    str_tax_cla = str_tax_cla,
    str_tax_npc = str_tax_npc
  )

  export_output(
    x = ann,
    file = output
  )
  rm(ann)
  output
}

#' @keywords internal
.mztab_pick_col <- function(df, candidates) {
  col <- candidates[candidates %in% names(df)]
  if (length(col) == 0L) {
    return(rep(NA_character_, nrow(df)))
  }
  as.character(df[[col[[1L]]]])
}
