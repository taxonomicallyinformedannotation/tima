## tl;dr:

```
# repo preparation

docker build -t tima . # optional
docker run -it --rm -v $PWD:/app tima bash # optional
conda env create -f environment.yml &&
conda activate tima &&


# get a working structure-organism pairs library

bash src/get-lotus.sh &&
cd src &&
Rscript prepare_lotus.R &&
# Rscript prepare_dnp.R && # only if you have access to it
Rscript prepare_library.R &&
Rscript prepare_adducts.R &&


# get spectral matches
# (spectral-lib-matcher, which is only in python. see related repo)
cd .. &&
bash src/get_example_isdb.sh && # get an example result from new isdb without python


# prepare all files for weighting

bash src/get_gnverifier.sh &&
cd src &&
Rscript prepare_gnps.R && # optional
Rscript prepare_isdb.R &&
Rscript prepare_features_components.R &&
Rscript prepare_features_classification.R &&
Rscript prepare_edges.R && 
Rscript prepare_taxa.R 

# And finally the graal!
Rscript process_annotations.R

# NOTE: you can use --help or -h argument for all .R steps to get more info
```