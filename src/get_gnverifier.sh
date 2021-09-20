#!/usr/bin/env bash
# -*- coding: utf-8 -*-

source src/get_platform.sh
source src/parse_yaml.sh
source src/warning.sh

eval $(parse_yaml paths.yaml)
eval $(parse_yaml config/versions.yaml)

mkdir -p $bin_path
wget "https://github.com/gnames/gnverifier/releases/download/$gnverifier/gnverifier-$gnverifier-$OS.tar.gz" -P bin/
tar -xzf bin/gnverifier-$gnverifier-$OS.tar.gz -C bin/
rm -r bin/gnverifier-$gnverifier-$OS.tar.gz
chmod +x bin/gnverifier
