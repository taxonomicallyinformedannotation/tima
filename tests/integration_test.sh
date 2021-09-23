#!/usr/bin/env bash

cp -R config/default config/params &&
bash src/get_lotus.sh &&
conda run -n tima Rscript src/prepare_lotus.R &&
conda run -n tima Rscript src/prepare_library.R &&
conda run -n tima Rscript src/prepare_adducts.R &&
bash src/get_example_isdb.sh &&
bash src/get_gnverifier.sh &&
conda run -n tima Rscript src/prepare_gnps.R &&
conda run -n tima Rscript src/prepare_isdb.R &&
conda run -n tima Rscript src/prepare_edges.R &&
conda run -n tima Rscript src/prepare_features_components.R &&
conda run -n tima Rscript src/prepare_features_classification.R &&
conda run -n tima Rscript src/prepare_taxa.R &&
conda run -n tima Rscript src/process_annotations.R
