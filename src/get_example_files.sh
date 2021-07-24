#!/usr/bin/env bash
# -*- coding: utf-8 -*-

source src/parse_yaml.sh
source src/warning.sh

eval $(parse_yaml src/paths.yaml)

mkdir -p $data_interim_path
mkdir -p $data_interim_adducts_path
mkdir -p $data_interim_annotations_path
mkdir -p $data_interim_edges_path
mkdir -p $data_interim_libraries_path
mkdir -p $data_interim_taxa_path

wget "https://metabo-store.nprod.net/tima_example_files/interim/adducts_neg.tsv.gz" -O $data_interim_adducts_neg
wget "https://metabo-store.nprod.net/tima_example_files/interim/adducts_pos.tsv.gz" -O $data_interim_adducts_pos
wget "https://metabo-store.nprod.net/tima_example_files/interim/b988e66d1542430cbc6e703be781e49c_edges.tsv.gz" -O data/interim/edges/b988e66d1542430cbc6e703be781e49c_edges.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/b988e66d1542430cbc6e703be781e49c_isdb_treated.tsv.gz" -O data/interim/annotations/b988e66d1542430cbc6e703be781e49c_isdb_treated.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/b988e66d1542430cbc6e703be781e49c_taxed.tsv.gz" -O data/interim/taxa/b988e66d1542430cbc6e703be781e49c_taxed.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/db_prepared_neg.tsv.gz" -O data/interim/adducts/library_neg.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/db_prepared_pos.tsv.gz" -O data/interim/adducts/library_pos.tsv.gz
wget "https://metabo-store.nprod.net/tima_example_files/interim/db_prepared.tsv.gz" -O data/interim/libraries/library.tsv.gz
