import json
import os
import sys
from os.path import join
from pathlib import Path

import geopandas as gpd
import pandas as pd

sys.path.append(join(os.getcwd(), 'src'))
from logger import logger
from utils.helper_functions import worksheet_reader


class Health_Inequalities_DataCreator:
    GADM_LEVEL = "GADM_1"
    CRS = "4326"

    def __init__(self, wp_data_path, dhs_dataset_path, lmic_shapefile_path, sci_dataset_path,
                 afrobarometer_dataset_path, hdi_dataset_path, saving_path):
        self.lmic_shapefile_path = lmic_shapefile_path
        self.wp_data = worksheet_reader(wp_data_path)
        logger.info(f"wp_data data read with size {len(self.wp_data)}")
        self.lmic_data = gpd.read_file(lmic_shapefile_path)
        logger.info(f"lmic data read with size {len(self.lmic_data)}")
        self.dhs_data = worksheet_reader(dhs_dataset_path)
        logger.info(f"dhs_data data read with size {len(self.dhs_data)}")
        self.sci_data = worksheet_reader(sci_dataset_path)
        logger.info(f"sci_data data read with size {len(self.sci_data)}")
        self.afrobarometer_data = worksheet_reader(afrobarometer_dataset_path)
        logger.info(f"afrobarometer_data data read with size {len(self.afrobarometer_data)}")
        self.hdi = worksheet_reader(hdi_dataset_path)
        logger.info(f"sub_national_data data read with size {len(self.hdi)}")
        self.saving_path = saving_path

    def main(self):
        lmic_sci = pd.merge(self.lmic_data, self.sci_data, right_on="user_loc", left_on="GID_1", how='inner')
        logger.info(f"combined lmic and sci with length {len(lmic_sci)}")
        lmic_sci.drop(columns=['user_loc'], inplace=True)
        lmic_sci_wp = lmic_sci.merge(self.wp_data, on="GID_1", how="inner")
        logger.info(f"combined lmic_sci and w_pop data with length {len(lmic_sci_wp)}")
        lmic_sci_wp_dhs = lmic_sci_wp.merge(self.dhs_data, on="GID_1", how="inner")
        logger.info(f"combined lmic_sci_w_pop and dhs data with length {len(lmic_sci_wp_dhs)}")
        #lmic_sci_covariates_dhs_afro = lmic_sci_covariates_dhs.merge(self.afrobarometer_data, on="GID_1", how="inner")
        #logger.info(f"combined lmic_sci_w_pop_dhs and afrobarometer data with length {len(lmic_sci_covariates_dhs_afro)}")
        lmic_sci_wp_dhs_afro_hdi = lmic_sci_wp_dhs.merge(self.hdi, on="GID_1", how="inner")
        lmic_sci_wp_dhs_afro_hdi = lmic_sci_wp_dhs_afro_hdi.drop_duplicates()

        geometries_cols = ['GID_1', 'geometry']
        geometries = lmic_sci_wp_dhs_afro_hdi[geometries_cols]
        variables = lmic_sci_wp_dhs_afro_hdi.loc[:,
                    ~lmic_sci_wp_dhs_afro_hdi.columns.isin(['geometry'])]

        #variables = variables.T.drop_duplicates().T
        geometries = gpd.GeoDataFrame(geometries, crs=self.CRS)
        saving_path_variables, saving_path_geometries = self.saving_path_for_gadm_file(self.GADM_LEVEL,
                                                                                       self.saving_path)
        # saving data
        variables.to_csv(saving_path_variables)
        geometries.to_file(saving_path_geometries)
        # if os.path.isfile(self.lmic_shapefile_path):
        #     os.remove(self.lmic_shapefile_path)
        # logger.info(f"{self.lmic_shapefile_path} has been deleted after final data creation.")

        logger.info(
            f"dataset for Gadm {self.GADM_LEVEL} completed, geometries are saved in {saving_path_geometries}"
            f" and variables are saved in {saving_path_variables}")

    @staticmethod
    def saving_path_for_gadm_file(gadm_level, saving_path):
        if not os.path.exists(saving_path):
            os.makedirs(saving_path)
        return Path(saving_path).joinpath(f"{gadm_level}_variables_updated.csv").as_posix(), Path(saving_path).joinpath(
            f"{gadm_level}_geometries_updated.gpkg").as_posix()


if __name__ == "__main__":
    config_path = "config_scripts/dataset_creator.json"
    with open(config_path) as pth:
        config = json.load(pth)

    covariates_dataset_path = config['wp_data_path']
    dhs_dataset_path = config['dhs_health_data']
    lmic_shapefile_path = config['lmic_shapefile']
    sci_dataset_path = config['sci_data_path']
    afrobarometer_dataset_path = config['afrobarometer_dataset_path']
    hdi_dataset_path = config['hdi_data_path']
    saving_path = config['saving_path']
    obj = Health_Inequalities_DataCreator(covariates_dataset_path, dhs_dataset_path, lmic_shapefile_path,
                                          sci_dataset_path, afrobarometer_dataset_path, hdi_dataset_path, saving_path)
    obj.main()
