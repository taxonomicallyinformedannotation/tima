#!/usr/bin/env bash

cp -R config/default config/params &&
bash scripts/get_lotus.sh &&
conda run -n tima Rscript scripts/prepare_lotus.R &&
conda run -n tima Rscript scripts/prepare_library.R &&
conda run -n tima Rscript scripts/prepare_adducts.R &&
bash scripts/get_example_isdb.sh &&
bash scripts/get_gnverifier.sh &&
conda run -n tima Rscript scripts/prepare_gnps.R &&
conda run -n tima Rscript scripts/prepare_isdb.R &&
conda run -n tima Rscript scripts/prepare_edges.R &&
conda run -n tima Rscript scripts/prepare_features_components.R &&
conda run -n tima Rscript scripts/prepare_features_classification.R &&
conda run -n tima Rscript scripts/prepare_taxa.R &&
conda run -n tima Rscript scripts/process_annotations.R
