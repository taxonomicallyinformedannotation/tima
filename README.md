## Repo preparation

### To run locally:

```shell
conda env create -f environment.yml &&
conda activate tima-python
```

## Structure-organism pairs library

```shell
Rscript src/prepare_lotus.R &&
# Rscript src/prepare_dnp.R && # only if you have access to it
Rscript src/prepare_library.R &&
Rscript src/prepare_adducts.R &&
```

## Annotations

### Get MS2 annotations

```shell
# (spectral-lib-matcher, which is only in python. see related repo)
# instead we provide an example file coming from the new ISDB.
# It also works with annotations coming from GNPS (see next steps)
./src/get_example_isdb.sh
```

### Format MS2 annotations

```shell
# depending on the annotation tool you used

Rscript prepare_gnps.R && # optional
Rscript prepare_isdb.R &&
```

### Complement MS2 annotations (with spectral clusters and chemical taxonomy of annotations)

```shell
Rscript src/prepare_edges.R && 
Rscript src/prepare_features_components.R &&
Rscript src/prepare_features_classification.R &&
```

### Get biological taxonomy information

```shell
./src/get_gnverifier.sh && 
Rscript src/prepare_taxa.R 
```

## And finally the graal!

```shell
Rscript src/process_annotations.R
```

NOTE: you can use --help or -h argument for all .R steps to get more info
