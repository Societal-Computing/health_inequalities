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
    def __init__(self, covariates_dataset_path, dhs_dataset_path, lmic_shapefile_path, sci_dataset_path, afrobarometer_dataset_path, hdi_dataset_path, saving_path):
        self.covariates_data = worksheet_reader(covariates_dataset_path)
        logger.info(f"covariates_data data read with size {len(self.covariates_data)}")
        self.lmic_data = gpd.read_file(lmic_shapefile_path)
        logger.info(f"lmic data read with size {len(self.lmic_data)}")
        self.dhs_data = worksheet_reader(dhs_dataset_path)
        logger.info(f"dhs_data data read with size {len(self.dhs_data)}")
        self.sci_data = worksheet_reader(sci_dataset_path)
        logger.info(f"sci_data data read with size {len(self.sci_data)}")
        self.afrobarometer_data = worksheet_reader(afrobarometer_dataset_path)
        logger.info(f"afrobarometer_data data read with size {len(self.afrobarometer_data)}")
        self.sub_national_data = worksheet_reader(hdi_dataset_path)
        logger.info(f"sub_national_data data read with size {len(self.sub_national_data)}")
        self.saving_path = saving_path

    def main(self):
        lmic_sci = pd.merge(self.lmic_data, self.sci_data, right_on="user_loc", left_on="GID_1", how='right')
        lmic_sci.drop(columns=['user_loc'], inplace=True)
        lmic_sci_covariates = lmic_sci.merge(self.covariates_data, on="GID_1", how="inner")
        lmic_sci_covariates_dhs = lmic_sci_covariates.merge(self.dhs_data, on="GID_1", how="inner")
        lmic_sci_covariates_dhs_afro = lmic_sci_covariates_dhs.merge(self.afrobarometer_data, on="GID_1", how="inner")
        lmic_sci_covariates_dhs_afro_hdi = lmic_sci_covariates_dhs_afro.merge(self.sub_national_data, on="GID_1", how="inner")
        lmic_sci_covariates_dhs_afro_hdi = lmic_sci_covariates_dhs_afro_hdi.drop_duplicates()

        geometries_cols = ['GID_1', 'geometry']
        geometries = lmic_sci_covariates_dhs_afro_hdi[geometries_cols]
        variables = lmic_sci_covariates_dhs_afro_hdi.loc[:, ~lmic_sci_covariates_dhs_afro_hdi.columns.isin(['geometry'])]

        variables = variables.T.drop_duplicates().T
        #variables.drop(columns=['GID_1'])
        variables = variables.drop(columns=['GID_1'])
        variables.rename(columns={'GID_1_1': 'GID_1'}, inplace=True)

        geometries = gpd.GeoDataFrame(geometries, crs='EPSG:4326')
        saving_path_variables, saving_path_geometries = self.saving_path_for_gadm_file(self.GADM_LEVEL, self.saving_path)
        # saving data
        variables.to_csv(saving_path_variables)
        geometries.to_file(saving_path_geometries)

        logger.info(
            f"dataset for Gadm {self.GADM_LEVEL} completed, geometries are saved in {saving_path_geometries}"
            f" and variables are saved in {saving_path_variables}")

    @classmethod
    def dhsData_sciData_with_healthInequalities(cls, dhs_data_path: str or Path,
                                                Gadm_data: pd.DataFrame) -> pd.DataFrame:
            health_inequalities = worksheet_reader(dhs_data_path)
            Gadm_data = Gadm_data.copy()
            sci_health_data = health_inequalities.merge(Gadm_data, on='GID_1', how="left")
            return sci_health_data


    def combine_dhs_health_ineq(self, dhs_sci_dataset, dhs_dataset_path):
            # combination with health inequalities data
            dhs_sci_dataset = dhs_sci_dataset[
                    ['GID_1', 'COUNTRY', 'Mean_SCI_with_Self', 'Median_SCI_with_Self', 'Std_SCI_with_Self', 'SCI',
                     'Mean_SCI_without_Self', 'Median_SCI_without_Self', 'Std_SCI_without_Self',
                     'Intraconnection_index', 'LMIC_interconnection_index', 'geometry']]
            dhs_sci_health = self.dhsData_sciData_with_healthInequalities(dhs_dataset_path, dhs_sci_dataset)
            # remove duplicates in data
            dhs_sci_health = dhs_sci_health.T.drop_duplicates().T
            dhs_sci_health = gpd.GeoDataFrame(dhs_sci_health, crs=self.CRS)
            # dhs_sci_health.to_file(saving_path, driver="GPKG")
            return dhs_sci_health

    @staticmethod
    def saving_path_for_gadm_file(gadm_level, saving_path):
            if not os.path.exists(saving_path):
                os.makedirs(saving_path)
            return Path(saving_path).joinpath(f"{gadm_level}_variables_updated.csv").as_posix(), Path(saving_path).joinpath(f"{gadm_level}_geometries_updated.gpkg").as_posix()

if __name__ == "__main__":
    config_path = "config_scripts/dataset_creator.json"
    with open(config_path) as pth:
        config = json.load(pth)

    covariates_dataset_path = config['covariates_data_path']
    dhs_dataset_path= config['dhs_health_data']
    lmic_shapefile_path = config['lmic_shapefile']
    sci_dataset_path = config['sci_data_path']
    afrobarometer_dataset_path = config['afrobarometer_dataset_path']
    hdi_dataset_path = config['sub_national_data_path']
    saving_path = config['saving_path']
    obj = Health_Inequalities_DataCreator(covariates_dataset_path, dhs_dataset_path, lmic_shapefile_path, sci_dataset_path, afrobarometer_dataset_path, hdi_dataset_path, saving_path)
    obj.main()