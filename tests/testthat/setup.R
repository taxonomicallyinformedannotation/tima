system(
  command =
    "
    mkdir inst
    cp -R params inst/params
    cp -R scripts inst/scripts ## because of docopt steps
    cp paths.yaml inst/paths.yaml
    "
)
