# Import libraries.
import json
import os
import sys
import warnings
from os.path import join
from pathlib import Path
from typing import Dict, List

import geopandas as gpd
import numpy as np
import pandas as pd

sys.path.append(join(os.getcwd(), 'src'))

from logger import logger
from utils import worksheet_reader

warnings.filterwarnings("ignore")


class Data_Preprocessing_LMIC:
    GADM_LEVEL = "GADM_1"
    CRS = "EPSG:4326"

    def __init__(self, config_json: Dict) -> None:
        self.config = config_json

    @staticmethod
    def shapefile_reader(shapefile_path: str):
        shapefile_data = gpd.read_file(shapefile_path)
        return shapefile_data

    @classmethod
    def combine_shapefiles_single_gadm_level(cls, shapefilepaths: List) -> pd.DataFrame:
        """
        Combines all shapefiles for a single GADM level
        :param shapefilepaths:
        :return combined shapefiles:
        """
        all_shapefiles_ls = []
        logger.info("combining shapefiles for single level")
        for single_shapefilepath in shapefilepaths:
            try:
                all_shapefiles_ls.append(gpd.read_file(single_shapefilepath))
            except Exception as e:
                logger.info(e)
        combined_shapefile = pd.concat(all_shapefiles_ls)
        logger.info(" all shapefiles combined for single level")
        return gpd.GeoDataFrame(combined_shapefile, crs=cls.CRS)

    @staticmethod
    def correction_of_GID_for_special_case(row: pd.Series):
        if row['GID_1'][:3] == 'COM':
            return 'COM'
        elif row['GID_1'][:3] == 'CPV':
            return 'CPV'
        else:
            return row['GID_1']

    @staticmethod
    def correct_HASC_1(row: pd.Series):
        if (row['HASC_1'] == np.nan) or (row['HASC_1'] is None):
            return row['HASC_x']
        else:
            return row['HASC_1']

    @staticmethod
    def refactor_GHA_GID_1(row: pd.Series):
        GID_1 = row['GID_1']
        if row['COUNTRY'].strip() == "Ghana":
            GID_1 = f"{GID_1[:3]}.{GID_1[4:-1]}1"
            return GID_1
        else:
            return GID_1

    def main(self):
        logger.info(f"combining dataset for Gadm {self.GADM_LEVEL}")
        level_config = self.config.pop(self.GADM_LEVEL)
        level_shapefiles = level_config["shapefiles_path"]
        africa_dataset_path = level_config["africa_dataset_path"]
        combined_shapefile = self.combine_shapefiles_single_gadm_level(list(level_shapefiles.values()))

        # save combined shapefiles
        africa_dataset = worksheet_reader(africa_dataset_path)

        africa_dataset = africa_dataset.drop(
            columns=['GID_0', 'fbkey', 'FB_key', 'NAME_0', 'NAME_1', 'VARNAME_1',
                     'NL_NAME_1', 'TYPE_1', 'ENGTYPE_1', 'CC_1'])

        combined_shapefile.rename(columns={"HASC_1": "HASC_x"}, inplace=True)
        gadm1_dhs_dataset = pd.merge(combined_shapefile, africa_dataset, on="GID_1", how="inner")

        # change GID_1 for Ghana
        gadm1_dhs_dataset['GID_1'] = gadm1_dhs_dataset.apply(lambda row: self.refactor_GHA_GID_1(row), axis=1)
        gadm1_dhs_dataset['GID_1'] = gadm1_dhs_dataset.GID_1.apply(lambda x:
                                                                   x.replace('.', '').replace('_1', ''))
        # change GID_1 of Comoros and Cape Verde
        gadm1_dhs_dataset['GID_1_1'] = gadm1_dhs_dataset['GID_1']
        gadm1_dhs_dataset['GID_1'] = gadm1_dhs_dataset.apply(lambda x: self.correction_of_GID_for_special_case(x),
                                                             axis=1)

        # resolving issues with HASC_1 naming
        gadm1_dhs_dataset['HASC_1'] = gadm1_dhs_dataset.apply(lambda row: self.correct_HASC_1(row), axis=1)
        gadm1_dhs_dataset.drop(columns=['HASC_x'], inplace=True)
        gadm1_dhs_dataset = gpd.GeoDataFrame(gadm1_dhs_dataset, crs=self.CRS)

        save_path = "external_dataset/lmic_shapefile.gpkg"
        gadm1_dhs_dataset.to_file(f"{save_path}", driver="GPKG")
        logger.info(f"lmic shapefile saved in {save_path}")


if __name__ == '__main__':
    config_path = "config_scripts/lmic_shapefiles_config.json"
    with open(config_path) as pth:
        config = json.load(pth)

    obj = Data_Preprocessing_LMIC(config)
    obj.main()
