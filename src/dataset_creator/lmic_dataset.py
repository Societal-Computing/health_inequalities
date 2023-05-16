# Import libraries.
import json
import logging.handlers
import os
import warnings
from pathlib import Path
from typing import Dict, List

import geopandas as gpd
import numpy as np
import pandas as pd

warnings.filterwarnings("ignore")

# TODO move logging to file

logger = logging.getLogger('health_inequalities')
formatter = logging.Formatter(
    fmt='%(filename)s | %(lineno)d | %(funcName)s | %(asctime)s | %(levelname)s: %(message)s',
    datefmt='%H:%M:%S'
)
logger.setLevel(logging.INFO)

consoleHandler = logging.StreamHandler()
consoleHandler.setLevel(logging.INFO)
consoleHandler.setFormatter(formatter)
logger.addHandler(consoleHandler)


class DataPreprocessing:
    GADM_LEVELS = ["GADM_0", "GADM_1"]

    def __init__(self, config_json: Dict) -> None:
        self.config = config_json

    @staticmethod
    def worksheet_reader(data_path: Path) -> pd.DataFrame:
        """
        For parsing excel and csv dataset
        :return:
        """
        data = None
        data_path = Path(data_path)
        if data_path.suffix == '.xlsx':
            data = pd.read_excel(data_path, encoding='ISO-8859-1')
        elif data_path.suffix == '.csv':
            data = pd.read_csv(data_path, encoding='ISO-8859-1')
        else:
            logger.error("Use a worksheet file of either csv or xlsx format")
        return data

    @staticmethod
    def shapefile_reader(shapefile_path: str):
        shapefile_data = gpd.read_file(shapefile_path)
        return shapefile_data

    @staticmethod
    def combine_shapefiles_single_gadm_level(shapefilepaths: List) -> pd.DataFrame:
        """
        Combines all shapefiles for a single GADM level
        :param shapefilepaths:
        :return combined shapefiles:
        """
        all_shapefiles_ls = []
        logger.info("combing shapefiles for single level")
        for single_shapefilepath in shapefilepaths:
            try:
                all_shapefiles_ls.append(gpd.read_file(single_shapefilepath))
            except Exception as e:
                logger.info(e)
        combined_shapefile = pd.concat(all_shapefiles_ls)
        logger.info(" all shapefiles combined for single level")
        return combined_shapefile

    @staticmethod
    def calculate_intra_inter_connection_indices(sci_dataset: pd.DataFrame,
                                                 lmic_gadm_level_names: List) -> pd.DataFrame:
        """
        :param sci_dataset:
        :param lmic_gadm_level_names:
        calculates the following indices:
            Intraconnection_index = Self_SCI / Total_SCI , this is a measure of how connected each LMIC is connected
            with itself. Where Self_SCI is the SCI within a particular LMIC. Total_SCI is sum of all SCIs for a
            particular LMIC.
            LMIC_interconnection_index = Sum_LMIC_SCI / Total_SCI, this is a measure of how connected a specific LMIC
            is connected to other LMICs.

        :return:
        """
        # filters all sci from LMICs
        meta_data = sci_dataset[sci_dataset.user_loc.isin(lmic_gadm_level_names)].copy()
        sum_sci = meta_data.groupby('user_loc').agg(Total_SCI=('scaled_sci', 'sum')).reset_index()

        # filters all sci to fellow LMICs except sci within country
        inter_lmic_data = meta_data[
            ~(meta_data.user_loc == meta_data.fr_loc) & meta_data.fr_loc.isin(lmic_gadm_level_names)]
        lmic_sci = inter_lmic_data.groupby('user_loc').agg(Sum_LMIC_SCI=('scaled_sci', 'sum')).reset_index()

        # filters all sci within each LMIC
        intra_lmic_data = meta_data[(meta_data.user_loc == meta_data.fr_loc)]
        self_sci = intra_lmic_data.groupby('user_loc').agg(Self_SCI=('scaled_sci', 'sum')).reset_index()
        all_sum_sci = sum_sci.merge(self_sci, on='user_loc', how='inner').merge(lmic_sci, on='user_loc', how='inner')
        all_sum_sci['Intraconnection_index'] = all_sum_sci.apply(lambda x: x['Self_SCI'] / x['Total_SCI'], axis=1)
        all_sum_sci['LMIC_interconnection_index'] = all_sum_sci.apply(lambda x: x['Sum_LMIC_SCI'] / x['Total_SCI'],
                                                                      axis=1)
        all_sum_sci = all_sum_sci[['user_loc', 'Intraconnection_index', 'LMIC_interconnection_index']]
        return all_sum_sci

    @staticmethod
    def calculate_avg_median_std_SCI(sci_dataset: pd.DataFrame, lmic_gadm_level_names: List) -> pd.DataFrame:
        """
        Calculates avg and standard deviation with and without scaled_index from same country.
        The output data has the following columns:
            Mean_SCI_with_Self : Mean of sci_index including connections within LMIC country
            Std_SCI_with_Self: Standard deviation of sci_index including connections within LMIC country
            Median_SCI_with_Self : Median of sci_index including connections within LMIC country
            Mean_SCI_without_Self: Mean of sci_index  NOT including connections within LMIC country
            Std_SCI_without_Self: Standard deviation of sci_index NOT including connections within LMIC country
            Median_SCI_without_Self : Median of sci_index NOT including connections within LMIC country
        :param sci_dataset:
        :param lmic_gadm_level_names:
        :return: avg_std
        """
        meta_data = sci_dataset[sci_dataset.user_loc.isin(lmic_gadm_level_names)].copy()
        avg_median_std_with_self = meta_data.groupby('user_loc').agg(Mean_SCI_with_Self=('scaled_sci', np.mean),
                                                                     Median_SCI_with_Self=('scaled_sci', np.median),
                                                                     Std_SCI_with_Self=('scaled_sci', np.std),
                                                                     SCI=('scaled_sci', np.sum)).reset_index()

        meta_data = meta_data[~(meta_data.user_loc == meta_data.fr_loc)]
        avg_median_std_without_self = meta_data.groupby('user_loc').agg(Mean_SCI_without_Self=('scaled_sci', np.mean),
                                                                        Median_SCI_without_Self=(
                                                                            'scaled_sci', np.median),
                                                                        Std_SCI_without_Self=(
                                                                            'scaled_sci', np.std)).reset_index()
        avg_std = avg_median_std_with_self.merge(avg_median_std_without_self, on='user_loc', how='inner')
        return avg_std

    @classmethod
    def dhsData_sciData_with_healthInequalities(cls, dhs_data_path: str or Path,
                                                Gadm_data: pd.DataFrame) -> pd.DataFrame:
        health_inequalities = cls.worksheet_reader(dhs_data_path)
        Gadm_data = Gadm_data.copy()
        sci_health_data = health_inequalities.merge(Gadm_data, on='GID_1', how="left")
        return sci_health_data

    def combine_dhs_health_ineq(self, dhs_sci_dataset, dhs_dataset_path):
        # combination with health inequalites data
        dhs_sci_dataset = dhs_sci_dataset[
            ['GID_1', 'COUNTRY', 'Mean_SCI_with_Self', 'Median_SCI_with_Self', 'Std_SCI_with_Self', 'SCI',
             'Mean_SCI_without_Self', 'Median_SCI_without_Self', 'Std_SCI_without_Self',
             'Intraconnection_index', 'LMIC_interconnection_index', 'geometry']]
        dhs_sci_health = self.dhsData_sciData_with_healthInequalities(dhs_dataset_path, dhs_sci_dataset)
        # remove duplicates in data
        dhs_sci_health = dhs_sci_health.T.drop_duplicates().T
        dhs_sci_health = gpd.GeoDataFrame(dhs_sci_health, crs="EPSG:4326")
        # dhs_sci_health.to_file(saving_path, driver="GPKG")
        return dhs_sci_health

    def main(self):
        dhs_dataset_path = self.config['dhs_dataset_path']
        for level in self.GADM_LEVELS:
            logger.info(f"combining dataset for Gadm {level}")
            level_config = self.config.pop(level)
            level_shapefiles = level_config["shapefiles_path"]
            africa_dataset_path = level_config["africa_dataset_path"]
            sci_dataset = pd.read_csv(level_config["sci_dataset_path"], delimiter="\t")
            combined_shapefile = self.combine_shapefiles_single_gadm_level(list(level_shapefiles.values()))
            africa_dataset = self.worksheet_reader(africa_dataset_path)
            if level == "GADM_0":
                gadm_dhs_dataset = pd.merge(combined_shapefile, africa_dataset, on="GID_0", how="inner")
                lmic_iso2_names = gadm_dhs_dataset.iso2.unique().tolist()
                avg_median_std_sci = self.calculate_avg_median_std_SCI(sci_dataset, lmic_iso2_names)
                intra_inter_connection_indices = self.calculate_intra_inter_connection_indices(sci_dataset,
                                                                                               lmic_iso2_names)
                generated_sci_indices = pd.merge(avg_median_std_sci, intra_inter_connection_indices, on='user_loc',
                                                 how='inner')
                dhs_sci_dataset = pd.merge(gadm_dhs_dataset, generated_sci_indices, left_on='iso2',
                                           right_on='user_loc', how='inner')
                dhs_sci_dataset = dhs_sci_dataset.drop(columns=["user_loc"])
                saving_path_variables, saving_path_geometries = self.saving_path_for_gadm_file(level)
                geometries_cols = ['GID_0', 'geometry']

                dhs_sci_geometries = dhs_sci_dataset[geometries_cols]
                dhs_variables = dhs_sci_dataset.loc[:, ~dhs_sci_dataset.columns.isin(['geometry'])]

                dhs_variables = dhs_variables.T.drop_duplicates().T

                dhs_variables.to_csv(saving_path_variables)

                dhs_sci_geometries = gpd.GeoDataFrame(dhs_sci_geometries, crs="EPSG:4326")
                dhs_sci_geometries.to_file(saving_path_geometries)

                logger.info(
                    f"dataset for Gadm {level} completed, geometries are saved in {saving_path_geometries}"
                    f" and variables are saved in {saving_path_variables}")
            if level == "GADM_1":
                africa_dataset = africa_dataset.drop(
                    columns=['GID_0', 'fbkey', 'FB_key', 'NAME_0', 'NAME_1', 'VARNAME_1',
                             'NL_NAME_1', 'TYPE_1', 'ENGTYPE_1', 'CC_1', 'HASC_1'])
                combined_shapefile['GID_1'] = combined_shapefile.apply(lambda row: self.refactor_GHA_GID_1(row), axis=1)
                gadm1_dhs_dataset = pd.merge(combined_shapefile, africa_dataset, on="GID_1", how="inner")
                gadm1_dhs_dataset['GID_1'] = gadm1_dhs_dataset.GID_1.apply(lambda x:
                                                                           x.replace('.', '').replace('_1', ''))
                lmic_gid1_names = gadm1_dhs_dataset.GID_1.unique().tolist()
                intra_inter_connection_indices = self.calculate_intra_inter_connection_indices(sci_dataset,
                                                                                               lmic_gid1_names)
                avg_median_std_sci = self.calculate_avg_median_std_SCI(sci_dataset, lmic_gid1_names)
                generated_sci_indices = pd.merge(avg_median_std_sci, intra_inter_connection_indices, on='user_loc',
                                                 how='inner')
                dhs_sci_dataset = pd.merge(gadm1_dhs_dataset, generated_sci_indices, left_on='GID_1',
                                           right_on='user_loc', how='inner')
                saving_path_variables, saving_path_geometries = self.saving_path_for_gadm_file(level)
                geometries_cols = ['GID_1', 'geometry']
                dhs_sci_geometries = dhs_sci_dataset[geometries_cols]
                dhs_variables = dhs_sci_dataset.loc[:, ~dhs_sci_dataset.columns.isin(['geometry'])]
                dhs_variables = dhs_variables.T.drop_duplicates().T
                dhs_sci_geometries = gpd.GeoDataFrame(dhs_sci_geometries, crs="EPSG:4326")

                dhs_variables.to_csv(saving_path_variables)
                dhs_sci_geometries.to_file(saving_path_geometries)

                logger.info(
                    f"dataset for Gadm {level} completed, geometries are saved in {saving_path_geometries}"
                    f" and variables are saved in {saving_path_variables}")

    @staticmethod
    def refactor_GHA_GID_1(row: pd.Series):
        GID_1 = row['GID_1']
        if row['COUNTRY'].strip() == "Ghana":
            GID_1 = f"{GID_1[:3]}.{GID_1[4:-1]}1"
        return GID_1

    def saving_path_for_gadm_file(self, level):
        if not os.path.exists(self.config['saving_path']):
            os.makedirs(self.config['saving_path'])
        return Path(self.config['saving_path']).joinpath(f"{level}_variables.csv").as_posix(), Path(
            self.config['saving_path']).joinpath(f"{level}_geometries.gpkg").as_posix()


if __name__ == '__main__':
    config_path = "config_scripts/dataset_config.json"
    with open(config_path) as pth:
        config = json.load(pth)

    obj = DataPreprocessing(config)
    obj.main()
