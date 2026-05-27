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
      .null_or <- function(x, default) {
        .scalar_chr_or(x, default = default)
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
      mzt_xrefs <- tryCatch(par_exp_mzt$files$xrefs, error = function(e) NULL)
      if (is.null(mzt_xrefs) || !nzchar(.null_or(mzt_xrefs, ""))) {
        mzt_xrefs <- lib_xrefs
      }
      mzt_contact <- tryCatch(par_exp_mzt$contact, error = function(e) NULL)
      if (
        !is.null(mzt_contact) &&
          all(vapply(
            mzt_contact,
            function(x) !nzchar(.null_or(x, "")),
            logical(1L)
          ))
      ) {
        mzt_contact <- NULL
      }
      base_mzt <- if (!is.null(par_exp_mzt$files$mztab$raw)) {
        raw_path <- par_exp_mzt$files$mztab$raw
        raw_path <- .scalar_chr_or(raw_path, default = NULL)
        if (!is.null(raw_path) && file.exists(raw_path)) {
          raw_path
        } else {
          NULL
        }
      } else {
        NULL
      }
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
        ms_run_location = .null_or(
          tryCatch(par_exp_mzt$ms$run_location, error = function(e) NULL),
          "null"
        ),
        ms_run_format = .null_or(
          tryCatch(par_exp_mzt$ms$run_format, error = function(e) NULL),
          "null"
        ),
        ms_run_id_format = .null_or(
          tryCatch(par_exp_mzt$ms$run_id_format, error = function(e) NULL),
          "null"
        ),
        polarity = tryCatch(par_exp_mzt$ms$polarity, error = function(e) NULL),
        instrument = .null_or(
          tryCatch(par_exp_mzt$ms$instrument, error = function(e) NULL),
          NULL
        ),
        title = .null_or(
          tryCatch(par_exp_mzt$study$title, error = function(e) NULL),
          "TIMA annotation results"
        ),
        description = .null_or(
          tryCatch(par_exp_mzt$study$description, error = function(e) NULL),
          paste0(
            "Annotation results produced by Taxonomically Informed ",
            "Metabolomics Annotation (TIMA)."
          )
        ),
        sample_name = .null_or(
          tryCatch(par_exp_mzt$study$sample_name, error = function(e) NULL),
          NULL
        ),
        publication = .null_or(
          tryCatch(par_exp_mzt$study$publication, error = function(e) NULL),
          NULL
        ),
        contact = mzt_contact,
        xrefs_file = mzt_xrefs,
        edges_file = .null_or(
          tryCatch(par_exp_mzt$files$edges, error = function(e) NULL),
          NULL
        ),
        base_mztab = base_mzt
      )
    },
    format = "file"
  )
}
