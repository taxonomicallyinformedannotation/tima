paths <- yaml::read_yaml(file = "inst/paths.yaml")

paths$tests$mode <- "yes"
paths$urls$examples$spectra <- paths$urls$examples$spectra_mini

yaml::write_yaml(x = paths, file = "inst/paths.yaml")
