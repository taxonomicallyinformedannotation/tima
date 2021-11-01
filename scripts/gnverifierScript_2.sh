#!/usr/bin/env bash

source scripts/parse_yaml.sh
source scripts/warning_gnverifier.sh

eval $(parse_yaml paths.yaml)

${GNVERIFIER} $data_interim_taxa_original_2 -s 179 -j 200 -f compact >$data_interim_taxa_verified_2

