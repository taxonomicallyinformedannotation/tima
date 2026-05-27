# Inputs targets section.

targets_section_inputs <- function() {
  list(
    tar_target(
      name = input_features,
      command = par_pre_fea_tab$files$features$raw,
      format = "file"
    ),
    tar_target(
      name = test_spectra_mini,
      command = {
        get_file(
          url = paths$urls$examples$spectra_mini,
          export = paths$data$source$spectra
        )
      },
      format = "file"
    ),
    tar_target(
      name = input_spectra,
      command = {
        if (!paths$test$mode) {
          par_ann_spe$files$spectral$raw
        } else {
          test_spectra_mini
        }
      },
      format = "file"
    )
  )
}
