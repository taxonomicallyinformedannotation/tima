paths <- yaml::read_yaml(file = system.file("extdata", "paths.yaml", package = "tima"))

paths$tests$mode <- "yes"
paths$urls$examples$spectra <- paths$urls$examples$spectra_mini

yaml::write_yaml(
  x = paths,
  file = system.file("extdata", "paths.yaml", package = "tima")
)
