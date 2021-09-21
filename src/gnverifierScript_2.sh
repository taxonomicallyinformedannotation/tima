#!/usr/bin/env bash

source src/parse_yaml.sh
source src/warning_gnverifier.sh

parse_yaml src/paths.yaml

${GNVERIFIER} $data_interim_taxa_original_2 -s 179 -j 200 -f compact >$data_interim_taxa_verified_2

