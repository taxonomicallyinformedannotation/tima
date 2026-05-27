# Architecture targets section.

targets_section_architecture <- function() {
  list(list(
    tar_target(
      name = yaml_paths,
      command = {
        system.file("paths.yaml", package = "tima")
      },
      format = "file"
    ),
    tar_target(
      name = paths,
      command = {
        tima:::get_default_paths(yaml = yaml_paths)
      },
      format = "rds"
    )
  ))
}
