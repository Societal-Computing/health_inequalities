# Import libraries.
import json
import os
import sys
import warnings
from os.path import join
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
        logger.info("combining all shapefiles")
        for single_shapefilepath in shapefilepaths:
            try:
                all_shapefiles_ls.append(gpd.read_file(single_shapefilepath))
            except Exception as e:
                logger.info(e)
        combined_shapefile = pd.concat(all_shapefiles_ls)
        logger.info("all shapefiles combined !")
        return gpd.GeoDataFrame(combined_shapefile, crs=cls.CRS)


    def main(self):
        logger.info(f"combining dataset for Gadm {self.GADM_LEVEL}")
        level_config = self.config.pop(self.GADM_LEVEL)
        level_shapefiles = level_config["shapefiles_path"]
        africa_dataset_path = level_config["africa_dataset_path"]
        combined_shapefile = self.combine_shapefiles_single_gadm_level(list(level_shapefiles.values()))
        combined_shapefile['GID_1'] = combined_shapefile['ISO'].astype(str)+combined_shapefile['ID_1'].astype(str)

        combined_shapefile.drop_duplicates(inplace=True)
        save_path = "combined_dataset/GADM_1_geometries.gpkg"
        combined_shapefile.to_file(f"{save_path}", driver="GPKG")

        # save combined shapefiles
        africa_dataset = worksheet_reader(africa_dataset_path)
        africa_dataset = africa_dataset.drop(
            columns=['GID_0', 'fbkey', 'FB_key', 'NAME_0', 'NAME_1', 'VARNAME_1',
                     'NL_NAME_1', 'TYPE_1', 'ENGTYPE_1', 'CC_1','HASC_1'])
        
        africa_dataset['GID_1'] = africa_dataset.GID_1.apply(lambda x:
                                                                   x.replace('.', '').replace('_1', ''))
        gadm1_dhs_dataset = pd.merge(combined_shapefile, africa_dataset, on="GID_1", how="inner")

        
        
        # computing weighted averages for fb data
        fb_pntr = {"FB_pntr_13p_all":"wpop_2020_age_13plus_all",
            "FB_pntr_60p_all":"wpop_2020_age_60plus_all",
            "FB_pntr_15p_all":"wpop_2020_age_15plus_all",
            "FB_pntr_18p_all":"wpop_2020_age_18plus_all",
            "FB_pntr_15to24_all":"wpop_2020_age_15_to_24_all",
            "FB_pntr_25to59_all":"wpop_2020_age_25_to_59_all",
            "FB_pntr_15to49_all":"wpop_2020_age_15_to_49_all",
            "FB_pntr_18to49_all":"wpop_2020_age_18_to_49_all",
            "FB_pntr_18to24_all":"wpop_2020_age_18_to_24_all",
            "FB_pntr_13p_female":"wpop_2020_age_13plus_female",
            "FB_pntr_13p_male":"wpop_2020_age_13plus_male",
            "FB_pntr_15p_female":"wpop_2020_age_15plus_female",
            "FB_pntr_15p_male":"wpop_2020_age_15plus_male",
            "FB_pntr_18p_female":"wpop_2020_age_18plus_female",
            "FB_pntr_18p_male":"wpop_2020_age_18plus_male",
            "FB_pntr_60p_male":"wpop_2020_age_60plus_male",
            "FB_pntr_15to24_female":"wpop_2020_age_18plus_female",
            "FB_pntr_15to24_male":"wpop_2020_age_15_to_24_male",
            "FB_pntr_25to59_female":"wpop_2020_age_25_to_59_female",
            "FB_pntr_25to59_male":"wpop_2020_age_25_to_59_male",
            "FB_pntr_60p_female":"wpop_2020_age_60plus_female", 
            "FB_pntr_15to49_female":"wpop_2020_age_15_to_49_female",
            "FB_pntr_15to49_male":"wpop_2020_age_15_to_49_male",
            "FB_pntr_18to24_female":"wpop_2020_age_18_to_24_female",
            "FB_pntr_18to24_male":"wpop_2020_age_18_to_24_male",
            "FB_pntr_18to49_female":"wpop_2020_age_18_to_49_female",
            "FB_pntr_18to49_male":"wpop_2020_age_18_to_49_male"
            }
        
        for fb,pop in fb_pntr.items():
            gadm1_dhs_dataset[f"weighted_{fb}"] = gadm1_dhs_dataset.apply(lambda x: x[fb] * x[pop],axis=1)

        fb_headers = ['GID_1'] + [i for i in gadm1_dhs_dataset.columns.tolist() if "FB" in i or 'fb' in i]
        other_headers = [i for i in gadm1_dhs_dataset.columns.tolist() if "FB" not in i and 'fb' not in i]
        fb_data = gadm1_dhs_dataset.loc[:,gadm1_dhs_dataset.columns.isin(fb_headers)]
        gadm1_dhs_dataset = gadm1_dhs_dataset.loc[:,gadm1_dhs_dataset.columns.isin(other_headers)]
        gadm1_dhs_dataset = gpd.GeoDataFrame(gadm1_dhs_dataset, crs=self.CRS)
        
        save_path = "external_dataset/lmic_shapefile.gpkg"
        gadm1_dhs_dataset.to_file(f"{save_path}", driver="GPKG")
        logger.info(f"lmic shapefile saved in {save_path}")

        save_path = "external_dataset/fb.csv"
        fb_data.to_csv(save_path)
        logger.info(f"fb_data saved in {save_path}")



if __name__ == '__main__':
    config_path = "config_scripts/lmic_shapefiles_config.json"
    with open(config_path) as pth:
        config = json.load(pth)

    obj = Data_Preprocessing_LMIC(config)
    obj.main()
