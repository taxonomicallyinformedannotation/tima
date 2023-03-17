paths <- yaml::read_yaml(file = "paths.yaml")

paths$tests$mode <- "yes"
paths$urls$examples$spectra <- paths$urls$examples$spectra_mini

yaml::write_yaml(x = paths, file = "paths.yaml")
