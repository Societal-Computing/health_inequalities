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
    GADM_LEVEL = "GADM_1"
<<<<<<< HEAD
    CRS = "4326"
=======
>>>>>>> main

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
    def calculate_country_constrained_features(sci_dataset: pd.DataFrame):
        """
        Calculate country constrained mean, median and std of sci
        :param sci_dataset:
        :return:
        """
        sci_dataset = sci_dataset.copy()
        sci_dataset['fr_GID_0'] = sci_dataset['fr_loc'].str.strip().str[:2]
        sci_dataset['user_GID_0'] = sci_dataset['user_loc'].str.strip().str[:2]
        # Get only countrywide information
        sci_dataset = sci_dataset[sci_dataset['user_GID_0'] == sci_dataset['fr_GID_0']]
        sci_dataset = sci_dataset.groupby(['user_loc']).agg(Local_sum_SCI=('scaled_sci', np.sum),
                                                            Local_mean_SCI=('scaled_sci', np.mean),
                                                            Local_std_SCI=('scaled_sci', np.std)).reset_index()

        return sci_dataset

    @classmethod
    def compute_distance_indices(cls, geometries: gpd.GeoDataFrame, sci_dataset: pd.DataFrame):
        """
        Computes the mean, median and std of distances between all regions
        :param geometries:
        :param sci_dataset:
        :return:
        """
        # compute centroids
        sci_dataset = sci_dataset.copy()
        geometries = geometries.copy()
        geometries = geometries.to_crs(cls.CRS)
        geometries['Centroids'] = geometries.geometry.centroid
        sci_dataset = sci_dataset.merge(geometries, left_on='user_loc', right_on='GID_1', how='inner')
        sci_dataset = sci_dataset.rename(columns={"Centroids": "user_loc_centroid"})
        sci_dataset.drop(columns=["GID_1", "geometry"], inplace=True)

        sci_dataset = sci_dataset.merge(geometries, left_on='fr_loc', right_on='GID_1', how='inner')
        sci_dataset = sci_dataset.rename(columns={"Centroids": "fr_loc_centroid"})
        sci_dataset.drop(columns=["GID_1"], inplace=True)
        sci_dataset = gpd.GeoDataFrame(sci_dataset)
        sci_dataset = sci_dataset.to_crs(cls.CRS)
        sci_dataset['distance'] = sci_dataset['user_loc_centroid'].distance(sci_dataset['fr_loc_centroid'])
        sci_dataset['distance'] = sci_dataset['distance'].abs()
        sci_dataset.drop(columns=["fr_loc_centroid", "user_loc_centroid", "geometry"], inplace=True)
        avg_median_std_distance = sci_dataset.groupby('user_loc').agg(Mean_dist_to_SCI=('distance', np.mean),
                                                                      Median_dist_to_SCI=('distance', np.median),
                                                                      Std_dist_to_SCI=('distance', np.std),
                                                                      Total_dist_to_SCI=('distance', np.sum)
                                                                      ).reset_index()
        return avg_median_std_distance

    @staticmethod
    def calculate_country_constrained_features(sci_dataset: pd.DataFrame):
        """
        Calculate country constrained mean, median and std of sci
        :param sci_dataset:
        :return:
        """
        sci_dataset = sci_dataset.copy()
        sci_dataset['fr_GID_0'] = sci_dataset['fr_loc'].str.strip().str[:2]
        sci_dataset['user_GID_0'] = sci_dataset['user_loc'].str.strip().str[:2]
        # Get only countrywide information
        sci_dataset = sci_dataset[sci_dataset['user_GID_0'] == sci_dataset['fr_GID_0']]
        sci_dataset = sci_dataset.groupby(['user_loc']).agg(Local_sum_SCI=('scaled_sci', np.sum),
                                                            Local_mean_SCI=('scaled_sci', np.mean),
                                                            Local_std_SCI=('scaled_sci', np.std)).reset_index()

        return sci_dataset

    @staticmethod
    def compute_distance_indices(geometries: gpd.GeoDataFrame, sci_dataset: pd.DataFrame):
        """
        Computes the mean, median and std of distances between all regions
        :param geometries:
        :param sci_dataset:
        :return:
        """
        # compute centroids
        sci_dataset = sci_dataset.copy()
        geometries = geometries.copy()
        geometries = geometries.to_crs("4326")
        geometries['Centroids'] = geometries.geometry.centroid
        sci_dataset = sci_dataset.merge(geometries, left_on='user_loc', right_on='GID_1', how='inner')
        sci_dataset = sci_dataset.rename(columns={"Centroids": "user_loc_centroid"})
        sci_dataset.drop(columns=["GID_1", "geometry"], inplace=True)

        sci_dataset = sci_dataset.merge(geometries, left_on='fr_loc', right_on='GID_1', how='inner')
        sci_dataset = sci_dataset.rename(columns={"Centroids": "fr_loc_centroid"})
        sci_dataset.drop(columns=["GID_1"], inplace=True)
        sci_dataset = gpd.GeoDataFrame(sci_dataset)
        sci_dataset = sci_dataset.to_crs('4326')
        sci_dataset['distance'] = sci_dataset['user_loc_centroid'].distance(sci_dataset['fr_loc_centroid'])
        sci_dataset['distance'] = sci_dataset['distance'].abs()
        sci_dataset.drop(columns=["fr_loc_centroid", "user_loc_centroid", "geometry"], inplace=True)
        avg_median_std_distance = sci_dataset.groupby('user_loc').agg(Mean_dist_to_SCI=('distance', np.mean),
                                                                      Median_dist_to_SCI=('distance', np.median),
                                                                      Std_dist_to_SCI=('distance', np.std),
                                                                      Total_dist=('distance', np.sum)
                                                                      ).reset_index()
        return avg_median_std_distance

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
<<<<<<< HEAD
        # combination with health inequalities data
=======
        # combination with health inequalites data
>>>>>>> main
        dhs_sci_dataset = dhs_sci_dataset[
            ['GID_1', 'COUNTRY', 'Mean_SCI_with_Self', 'Median_SCI_with_Self', 'Std_SCI_with_Self', 'SCI',
             'Mean_SCI_without_Self', 'Median_SCI_without_Self', 'Std_SCI_without_Self',
             'Intraconnection_index', 'LMIC_interconnection_index', 'geometry']]
        dhs_sci_health = self.dhsData_sciData_with_healthInequalities(dhs_dataset_path, dhs_sci_dataset)
        # remove duplicates in data
        dhs_sci_health = dhs_sci_health.T.drop_duplicates().T
<<<<<<< HEAD
        dhs_sci_health = gpd.GeoDataFrame(dhs_sci_health, crs=self.CRS)
        # dhs_sci_health.to_file(saving_path, driver="GPKG")
        return dhs_sci_health

    @staticmethod
    def correction_of_GID_for_special_case(row: pd.Series):
        if row['GID_1'][:3] == 'COM':
            return 'COM'
        elif row['GID_1'][:3] == 'CPV':
            return 'CPV'
        else:
            return row['GID_1']

    def main(self):
=======
        dhs_sci_health = gpd.GeoDataFrame(dhs_sci_health, crs="EPSG:4326")
        # dhs_sci_health.to_file(saving_path, driver="GPKG")
        return dhs_sci_health

    def main(self):
        dhs_dataset_path = self.config['dhs_dataset_path']
>>>>>>> main
        logger.info(f"combining dataset for Gadm {self.GADM_LEVEL}")
        level_config = self.config.pop(self.GADM_LEVEL)
        level_shapefiles = level_config["shapefiles_path"]
        africa_dataset_path = level_config["africa_dataset_path"]
<<<<<<< HEAD
        health_indicators_path = level_config['health_indicators_dataset_path']
        sci_dataset = pd.read_csv(level_config["sci_dataset_path"], delimiter="\t")
        combined_shapefile = self.combine_shapefiles_single_gadm_level(list(level_shapefiles.values()))
        health_indicators = pd.read_csv(health_indicators_path)

        # save combined shapefiles
        combined_shapefile.to_file("external_dataset/all_shapefiles.gpkg", driver="GPKG")

=======
        sci_dataset = pd.read_csv(level_config["sci_dataset_path"], delimiter="\t")
        combined_shapefile = self.combine_shapefiles_single_gadm_level(list(level_shapefiles.values()))
>>>>>>> main
        africa_dataset = self.worksheet_reader(africa_dataset_path)

        africa_dataset = africa_dataset.drop(
            columns=['GID_0', 'fbkey', 'FB_key', 'NAME_0', 'NAME_1', 'VARNAME_1',
<<<<<<< HEAD
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

=======
                     'NL_NAME_1', 'TYPE_1', 'ENGTYPE_1', 'CC_1', 'HASC_1'])
        combined_shapefile['GID_1'] = combined_shapefile.apply(lambda row: self.refactor_GHA_GID_1(row), axis=1)
        gadm1_dhs_dataset = pd.merge(combined_shapefile, africa_dataset, on="GID_1", how="inner")
        gadm1_dhs_dataset['GID_1'] = gadm1_dhs_dataset.GID_1.apply(lambda x:
                                                                   x.replace('.', '').replace('_1', ''))
>>>>>>> main
        lmic_gid1_names = gadm1_dhs_dataset.GID_1.unique().tolist()

        # merge all sci related features for only LMICs
        intra_inter_connection_indices = self.calculate_intra_inter_connection_indices(sci_dataset, lmic_gid1_names)
        distance_between_sci = self.compute_distance_indices(gadm1_dhs_dataset[['GID_1', 'geometry']], sci_dataset)
        intra_inter_connection_dist_indices = intra_inter_connection_indices.merge(distance_between_sci,
<<<<<<< HEAD
                                                                                   on='user_loc', how='inner')

        avg_median_std_sci = self.calculate_avg_median_std_SCI(sci_dataset, lmic_gid1_names)

        generated_sci_indices = pd.merge(avg_median_std_sci, intra_inter_connection_dist_indices, on='user_loc',
                                         how='inner')

        local_sci_indices = self.calculate_country_constrained_features(sci_dataset)
        sci_features = generated_sci_indices.merge(local_sci_indices, on="user_loc", how="inner")

        dhs_sci_dataset = pd.merge(gadm1_dhs_dataset, sci_features, left_on='GID_1', right_on='user_loc', how='inner')

=======
                                                                                   on='user_loc', how='left')
        avg_median_std_sci = self.calculate_avg_median_std_SCI(sci_dataset, lmic_gid1_names)
        generated_sci_indices = pd.merge(avg_median_std_sci, intra_inter_connection_dist_indices, on='user_loc',
                                         how='inner')
        local_sci_indices = self.calculate_country_constrained_features(sci_dataset)
        sci_features = generated_sci_indices.merge(local_sci_indices, on="user_loc", how="left")
        dhs_sci_dataset = pd.merge(gadm1_dhs_dataset, sci_features, left_on='GID_1', right_on='user_loc', how='inner')
>>>>>>> main
        # end of sci features addition

        saving_path_variables, saving_path_geometries = self.saving_path_for_gadm_file(self.GADM_LEVEL)
        geometries_cols = ['GID_1', 'geometry']
        dhs_sci_geometries = dhs_sci_dataset[geometries_cols]
        dhs_variables = dhs_sci_dataset.loc[:, ~dhs_sci_dataset.columns.isin(['geometry'])]
        dhs_variables = dhs_variables.T.drop_duplicates().T
<<<<<<< HEAD
        dhs_variables.drop(columns=['GID_1'])
        dhs_variables = dhs_variables.drop(columns=['GID_1'])
        dhs_variables.rename(columns={'GID_1_1': 'GID_1'}, inplace=True)

        dhs_sci_geometries = gpd.GeoDataFrame(dhs_sci_geometries, crs='EPSG:4326')

        # Combining dataset with health indicators
        dhs_variables = dhs_variables.merge(health_indicators, on='GID_1', how='inner')

        # saving data
=======
        dhs_sci_geometries = gpd.GeoDataFrame(dhs_sci_geometries, crs="EPSG:4326")

>>>>>>> main
        dhs_variables.to_csv(saving_path_variables)
        dhs_sci_geometries.to_file(saving_path_geometries)

        logger.info(
            f"dataset for Gadm {self.GADM_LEVEL} completed, geometries are saved in {saving_path_geometries}"
            f" and variables are saved in {saving_path_variables}")
<<<<<<< HEAD

    @staticmethod
    def correct_HASC_1(row: pd.Series):
        if (row['HASC_1'] == np.nan) or (row['HASC_1'] is None):
            return row['HASC_x']
        else:
            return row['HASC_1']
=======
>>>>>>> main

    @staticmethod
    def refactor_GHA_GID_1(row: pd.Series):
        GID_1 = row['GID_1']
        if row['COUNTRY'].strip() == "Ghana":
            GID_1 = f"{GID_1[:3]}.{GID_1[4:-1]}1"
            return GID_1
        else:
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
