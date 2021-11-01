#!/usr/bin/env bash

cp -R config/default config/params &&
bash inst/scripts/get_lotus.sh &&
conda run -n tima Rscript inst/scripts/prepare_lotus.R &&
conda run -n tima Rscript inst/scripts/prepare_library.R &&
conda run -n tima Rscript inst/scripts/prepare_adducts.R &&
bash inst/scripts/get_example_isdb.sh &&
bash inst/scripts/get_gnverifier.sh &&
conda run -n tima Rscript inst/scripts/prepare_gnps.R &&
conda run -n tima Rscript inst/scripts/prepare_isdb.R &&
conda run -n tima Rscript inst/scripts/prepare_edges.R &&
conda run -n tima Rscript inst/scripts/prepare_features_components.R &&
conda run -n tima Rscript inst/scripts/prepare_features_classification.R &&
conda run -n tima Rscript inst/scripts/prepare_taxa.R &&
conda run -n tima Rscript inst/scripts/process_annotations.R
