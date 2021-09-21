#!/usr/bin/env bash
# -*- coding: utf-8 -*-

. src/parse_yaml.sh
. src/warning_gnverifier.sh

eval $(parse_yaml src/paths.yaml)

${GNVERIFIER} $data_interim_taxa_original_2 -s 179 -j 200 -f compact >$data_interim_taxa_verified_2
