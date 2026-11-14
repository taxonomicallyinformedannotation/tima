start <- Sys.time()

library(tima)

.log_trace <- getFromNamespace("log_trace", "tima")
.log_success <- getFromNamespace("log_success", "tima")

.log_trace(
  "This script gets gnps tables"
)
.log_trace("Authors: AR")
.log_trace("Contributors: ...")

params <- getFromNamespace("get_params", "tima")(
  step = "prepare_params_advanced"
)
get_gnps_tables(
  gnps_job_id = params$gnps$id,
  filename = params$files$pattern,
  workflow = params$gnps$workflow,
  path_features = params$files$features$raw,
  path_metadata = params$files$metadata$raw,
  path_spectra = params$files$spectral$raw
)

end <- Sys.time()

.log_success("Script finished in %s", format(end - start))
