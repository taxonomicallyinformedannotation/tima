#!/usr/bin/env bash
# -*- coding: utf-8 -*-

. src/parse_yaml.sh
. src/warning.sh

eval $(parse_yaml paths.yaml)

mkdir -p $data_source_libraries_path

wget "https://osf.io/rheq5/download" -O $data_source_libraries_lotus
