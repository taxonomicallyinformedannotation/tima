# mzTab Export targets section.

targets_section_export_mztab <- function() {
  tar_target(
    name = exp_mzt,
    command = {
      .scalar_chr_or <- function(x, default = NULL) {
        if (is.null(x) || length(x) == 0L) {
          return(default)
        }
        if (is.character(x)) {
          x <- x[[1L]]
          if (is.na(x) || !nzchar(x) || identical(x, "null")) {
            return(default)
          }
          return(x)
        }
        x
      }
      .get_cfg <- function(cfg, ..., default = NULL) {
        for (k in list(...)) {
          if (!is.list(cfg)) {
            return(default)
          }
          cfg <- cfg[[k]]
        }
        .scalar_chr_or(cfg, default = default)
      }
      .output_in_input_dir <- function(input_path, configured_output) {
        if (is.null(input_path) || !nzchar(input_path)) {
          return(configured_output)
        }
        out_name <- basename(configured_output)
        if (
          is.null(out_name) || !nzchar(out_name) || identical(out_name, "null")
        ) {
          out_name <- paste0(
            tools::file_path_sans_ext(basename(input_path)),
            ".mztab"
          )
        }
        file.path(dirname(input_path), out_name)
      }
      mzt_xrefs <- .get_cfg(par_exp_mzt, "files", "xrefs", default = lib_xrefs)
      mzt_contact <- if (is.list(par_exp_mzt)) par_exp_mzt$contact
      if (
        !is.null(mzt_contact) &&
          all(vapply(
            mzt_contact,
            function(x) !nzchar(.scalar_chr_or(x, "")),
            logical(1L)
          ))
      ) {
        mzt_contact <- NULL
      }
      raw_path <- .get_cfg(par_exp_mzt, "files", "mztab", "raw")
      base_mzt <- if (!is.null(raw_path) && file.exists(raw_path)) raw_path
      mzt_input <- .scalar_chr_or(ann_wei, default = ann_wei)
      mzt_output_cfg <- .scalar_chr_or(
        par_exp_mzt$files$output$mztab,
        default = par_exp_mzt$files$output$mztab
      )
      write_mztab(
        input = mzt_input,
        output = .output_in_input_dir(
          input_path = mzt_input,
          configured_output = mzt_output_cfg
        ),
        ms_run_location = .get_cfg(
          par_exp_mzt,
          "ms",
          "run_location",
          default = "null"
        ),
        ms_run_format = .get_cfg(
          par_exp_mzt,
          "ms",
          "run_format",
          default = "null"
        ),
        ms_run_id_format = .get_cfg(
          par_exp_mzt,
          "ms",
          "run_id_format",
          default = "null"
        ),
        polarity = if (is.list(par_exp_mzt) && is.list(par_exp_mzt$ms)) {
          par_exp_mzt$ms$polarity
        },
        instrument = .get_cfg(par_exp_mzt, "ms", "instrument"),
        title = .get_cfg(
          par_exp_mzt,
          "study",
          "title",
          default = "TIMA annotation results"
        ),
        description = .get_cfg(
          par_exp_mzt,
          "study",
          "description",
          default = paste0(
            "Annotation results produced by Taxonomically Informed ",
            "Metabolomics Annotation (TIMA)."
          )
        ),
        sample_name = .get_cfg(par_exp_mzt, "study", "sample_name"),
        publication = .get_cfg(par_exp_mzt, "study", "publication"),
        contact = mzt_contact,
        xrefs_file = mzt_xrefs,
        edges_file = .get_cfg(par_exp_mzt, "files", "edges"),
        base_mztab = base_mzt
      )
    },
    format = "file",
    garbage_collection = TRUE
  )
}
