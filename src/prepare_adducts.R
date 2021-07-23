#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""This script prepares adducts from the \n,
   structure-organism pairs for further processing"""

import os

import pandas
import yaml


def form_adducts_pos(dataframe, adducts):
    dataframe_pos = dataframe.assign(
        pos_3_3proton=(dataframe['exact_mass'] +
                       3 * dataframe['proton']) / 3,
        pos_3_2proton1sodium=(dataframe['exact_mass'] +
                              2 * dataframe['proton'] +
                              dataframe[
                                  'sodium']) / 3,
        pos_3_1proton2sodium=(dataframe['exact_mass'] +
                              dataframe['proton'] +
                              2 * dataframe[
                                  'sodium']) / 3,
        pos_3_3sodium=(dataframe['exact_mass'] +
                       3 * dataframe['sodium']) / 3,
        pos_2_2proton=(dataframe['exact_mass'] +
                       2 * dataframe['proton']) / 2,
        pos_2_2proton1ammonium=
        (dataframe['exact_mass'] +
         2 * dataframe['proton'] +
         dataframe['ammonium']) / 2,
        pos_2_1proton1sodium=(dataframe['exact_mass'] +
                              dataframe['proton'] +
                              dataframe['sodium']) / 2,
        pos_2_1proton1potassium=(
                                        dataframe['exact_mass'] +
                                        dataframe['proton'] +
                                        dataframe['potassium']) / 2,
        pos_2_2proton1acetonitrile=(
                                           dataframe['exact_mass'] +
                                           2 * dataframe['proton'] +
                                           dataframe['acetonitrile']) / 2,
        pos_2_2sodium=(dataframe['exact_mass'] +
                       2 * dataframe['sodium']) / 2,
        pos_2_2proton2acetonitrile=(dataframe['exact_mass'] +
                                    2 * dataframe['proton'] +
                                    2 * dataframe[
                                        'acetonitrile']) / 2,
        pos_2_2proton3acetonitrile=(dataframe['exact_mass'] +
                                    2 * dataframe['proton'] +
                                    3 * dataframe[
                                        'acetonitrile']) / 2,
        # pos_1_minus2water1proton = dataframe['exact_mass'] - 2 * dataframe['water'] + dataframe['proton'],
        # pos_1_minus1water1proton = dataframe['exact_mass'] - dataframe['water'] + dataframe['proton'],
        pos_1_1proton=dataframe['exact_mass'] +
                      dataframe['proton'],
        # pos_1_minus1water1sodium=dataframe['exact_mass'] - dataframe['water'] + dataframe['sodium'],
        pos_1_1proton1ammonium=dataframe['exact_mass'] +
                               dataframe['proton'] +
                               dataframe['ammonium'],
        pos_1_sodium=dataframe['exact_mass'] +
                     dataframe['sodium'],
        pos_1_1proton1methanol=dataframe['exact_mass'] +
                               dataframe['proton'] +
                               dataframe['methanol'],
        pos_1_1potassium=dataframe['exact_mass'] +
                         dataframe['potassium'],
        pos_1_1proton1acetonitrile=dataframe['exact_mass'] +
                                   dataframe['proton'] +
                                   dataframe[
                                       'acetonitrile'],
        pos_1_minus1proton2sodium=dataframe['exact_mass'] -
                                  dataframe['proton'] +
                                  2 * dataframe[
                                      'sodium'],
        pos_1_1proton1ethylamine=dataframe['exact_mass'] +
                                 dataframe['proton'] +
                                 dataframe['ethylamine'],
        pos_1_1proton1isopropanol=dataframe['exact_mass'] +
                                  dataframe['proton'] +
                                  dataframe[
                                      'isopropanol'],
        pos_1_1sodium1acetonitrile=dataframe['exact_mass'] +
                                   dataframe['sodium'] +
                                   dataframe[
                                       'acetonitrile'],
        pos_1_minus1proton2potassium=dataframe['exact_mass'] -
                                     dataframe['proton'] +
                                     2 * dataframe[
                                         'potassium'],
        pos_1_1proton1dmso=dataframe['exact_mass'] +
                           dataframe['proton'] +
                           dataframe['dmso'],
        pos_1_1proton2acetonitrile=dataframe['exact_mass'] +
                                   dataframe['proton'] +
                                   2 * dataframe[
                                       'acetonitrile'],
        # pos_IsoPNa-H = dataframe['exact_mass'] - dataframe['proton'] + dataframe['isopropanol'] + dataframe['sodium)
        pos_2MH=2 * dataframe['exact_mass'] +
                dataframe['proton'],
        pos_2MHNH3=2 * dataframe['exact_mass'] +
                   dataframe['proton'] +
                   dataframe['ammonium'],
        pos_2MNa=2 * dataframe['exact_mass'] +
                 dataframe['sodium'],
        pos_2MK=2 * dataframe['exact_mass'] +
                dataframe['potassium'],
        pos_2MHCH3CN=2 * dataframe['exact_mass'] +
                     dataframe['proton'] +
                     dataframe['acetonitrile'],
        pos_2MCH3CNNa=2 * dataframe['exact_mass'] +
                      dataframe['acetonitrile'] +
                      dataframe['sodium']
    ).drop(
        list(adducts.columns.values),
        axis=1
    )

    dataframe_pos = pandas.melt(
        dataframe_pos,
        id_vars=['exact_mass'],
        value_vars=list(dataframe_pos.columns.values)[1:],
        var_name='adduct',
        value_name='adduct_mass')
    return dataframe_pos


def form_adducts_neg(dataframe, adducts):
    dataframe_neg = dataframe.assign(
        neg_3_3proton=(dataframe['exact_mass'] -
                       3 * dataframe['proton']) / 3,
        neg_2_2proton=(dataframe['exact_mass'] -
                       2 * dataframe['proton']) / 2,
        # neg_1_minus2waterminus1proton = dataframe['exact_mass'] - 2 * dataframe['water'] + dataframe['proton'],
        # neg_1_minus1waterminus1proton = dataframe['exact_mass'] - 2 * dataframe['water'] + dataframe['proton'],
        neg_1_minus1proton=dataframe['exact_mass'] -
                           dataframe['proton'],
        neg_1_minus2proton1sodium=(dataframe['exact_mass'] -
                                   2 * dataframe['proton'] +
                                   dataframe['sodium']),
        neg_1_1chlorine=dataframe['exact_mass'] +
                        dataframe['chlorine'],
        neg_1_minus2proton1potassium=
        dataframe['exact_mass'] -
        2 * dataframe['proton'] +
        dataframe['potassium'],
        neg_1_minus1proton1formic=dataframe['exact_mass'] -
                                  dataframe['proton'] +
                                  dataframe['formic'],
        neg_1_minus1proton1acetic=dataframe['exact_mass'] -
                                  dataframe['proton'] +
                                  dataframe['acetic'],
        neg_1_minus2proton1sodium1formic=dataframe['exact_mass'] -
                                         2 * dataframe['proton'] +
                                         dataframe['sodium'] +
                                         dataframe['formic'],
        neg_1_1bromine=(dataframe['exact_mass'] +
                        dataframe['bromine']),
        neg_1_minus1proton1tfa=(dataframe['exact_mass'] -
                                dataframe['proton'] +
                                dataframe[
                                    'tfa']),
        neg_2MH=2 * dataframe['exact_mass'] -
                dataframe['proton'],
        neg_2MFAH=2 * dataframe['exact_mass'] -
                  dataframe['proton'] +
                  dataframe['formic'],
        neg_2MACH=2 * dataframe['exact_mass'] -
                  dataframe['proton'] +
                  dataframe['acetic'],
        neg_3MH=3 * dataframe['exact_mass'] -
                dataframe['proton']
    ).drop(
        list(adducts.columns.values),
        axis=1
    )
    dataframe_neg = pandas.melt(
        dataframe_neg,
        id_vars=['exact_mass'],
        value_vars=list(dataframe_neg.columns.values)[1:],
        var_name='adduct',
        value_name='adduct_mass')
    return dataframe_neg


with open("paths.yaml", 'r') as stream:
    try:
        paths = yaml.safe_load(stream)
    except yaml.YAMLError as exc:
        print(exc)

os.chdir(paths["base_dir"])

with open("config/default/prepare_adducts.yaml", 'r') as stream:
    try:
        params = yaml.safe_load(stream)
    except yaml.YAMLError as exc:
        print(exc)

with open("config/params/prepare_adducts.yaml", 'r') as stream:
    try:
        params = yaml.safe_load(stream)
    except yaml.YAMLError as exc:
        print(exc)

col_list = ["structure_exact_mass"]

if os.path.isfile(params["input"]):
    masses = pandas.read_csv(
        filepath_or_buffer=params["input"],
        usecols=col_list
    ).rename(
        columns={"structure_exact_mass": "exact_mass"}
    ).drop_duplicates(
        
    )

    adducts = pandas.read_csv(
        filepath_or_buffer=paths["data"]["source"]["adducts"], sep='\t'
    ).transpose(

    )

    adducts.columns = adducts.iloc[0]

    adducts = adducts.filter(
        like='mass', axis=0)

    masses_adducts = pandas.concat(
        objs=[masses.reset_index(drop=True),
              adducts.reset_index(drop=True)],
        ignore_index=True,
        axis=1
    ).ffill(
        axis=0
    )

    masses_null = pandas.concat(
        objs=[pandas.DataFrame({'exact_mass': 0}, index=[0]).reset_index(drop=True),
              adducts.reset_index(drop=True)],
        ignore_index=True,
        axis=1
    ).ffill(
        axis=0
    )

    masses_adducts.columns = list(masses.columns.values) + list(adducts.columns.values)

    masses_null.columns = list(masses.columns.values) + list(adducts.columns.values)

    adducts_pos = form_adducts_pos(dataframe=masses_adducts, adducts=adducts).drop_duplicates()

    adducts_neg = form_adducts_neg(dataframe=masses_adducts, adducts=adducts).drop_duplicates()

    pure_pos = form_adducts_pos(dataframe=masses_null, adducts=adducts)

    pure_pos = pure_pos[pure_pos['adduct'].str.contains(pat="pos_1")]

    pure_neg = form_adducts_neg(dataframe=masses_null, adducts=adducts)

    pure_neg = pure_neg[pure_neg['adduct'].str.contains(pat="neg_1")]

    adducts_pos.to_csv(
        path_or_buf=os.path.join(
            paths["data"]["interim"]["adducts"]["path"],
            params["output"] +
            '_pos.tsv.gz'
        ),
        index=False
    )

    adducts_neg.to_csv(
        path_or_buf=os.path.join(
            paths["data"]["interim"]["adducts"]["path"],
            params["output"] +
            '_neg.tsv.gz'
        ),
        index=False
    )

    pure_pos.to_csv(
        path_or_buf=paths["data"]["interim"]["adducts"]["pos"],
        index=False
    )

    pure_neg.to_csv(
        path_or_buf=paths["data"]["interim"]["adducts"]["neg"],
        index=False
    )

else:
    print("""Sorry, your path does not match any file""")
