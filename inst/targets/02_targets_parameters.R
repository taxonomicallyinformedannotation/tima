# Parameters targets section.

targets_section_parameters <- function() {
  list(
    lapply(names(PARAM_STEPS), function(abbrev) {
      tar_target_raw(
        name = paste0("par_def_", abbrev),
        command = substitute(
          system.file(path, package = "tima"),
          list(path = paste0("params/default/", PARAM_STEPS[[abbrev]], ".yaml"))
        ),
        format = "file"
      )
    }),
    list(
      tar_target(
        name = par_pre_par,
        command = paths$params$prepare_params,
        format = "file"
      ),
      tar_target(
        name = par_pre_par2,
        command = paths$params$prepare_params_advanced,
        format = "file"
      ),
      tar_target(
        name = par_fin_par,
        command = getFromNamespace("parse_yaml_params", "tima")(
          def = par_pre_par,
          usr = par_pre_par
        ),
        format = "rds"
      ),
      tar_target(
        name = par_fin_par2,
        command = getFromNamespace("parse_yaml_params", "tima")(
          def = par_pre_par2,
          usr = par_pre_par2
        ),
        format = "rds"
      )
    ),
    lapply(names(PARAM_STEPS), function(abbrev) {
      tar_target_raw(
        name = paste0("par_usr_", abbrev),
        command = substitute(
          prepare_params(
            params_small = par_fin_par,
            params_advanced = par_fin_par2,
            step = step_name
          ),
          list(step_name = PARAM_STEPS[[abbrev]])
        ),
        format = "file"
      )
    }),
    lapply(names(PARAM_STEPS), function(abbrev) {
      tar_target_raw(
        name = paste0("par_", abbrev),
        command = substitute(
          getFromNamespace("parse_yaml_params", "tima")(
            def = def_sym,
            usr = usr_sym[[1L]]
          ),
          list(
            def_sym = as.symbol(paste0("par_def_", abbrev)),
            usr_sym = as.symbol(paste0("par_usr_", abbrev))
          )
        ),
        format = "rds"
      )
    })
  )
}
