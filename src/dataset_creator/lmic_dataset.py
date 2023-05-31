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
from dataset_creator import Dataset_Creator_World_Covariates

warnings.filterwarnings("ignore")


class DataPreprocessing:
    GADM_LEVEL = "GADM_1"
    CRS = "4326"

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
    def calculate_regional_ratios(sci_dataset: pd.DataFrame, agg_sci_from_region_to_africa: pd.DataFrame,
                                  agg_sci_from_region_to_world: pd.DataFrame) -> pd.DataFrame:
        """
        Calculate ratio of self_loop to al sci in country,
        ratio of self_sci to all regions in Africa,
        ratio of self_sci to all regions,
        ratio of sum of sci in country to all regions
        :param sci_dataset , agg_sci_from_region_to_africa, agg_sci_from_region_to_world
        :return:
        """
        sci_dataset = sci_dataset.copy()
        # get self_loop sci
        self_sci_dataset = sci_dataset[sci_dataset['user_loc'] == sci_dataset['fr_loc']][['user_loc', 'scaled_sci']]
        self_sci_dataset = self_sci_dataset.merge(agg_sci_from_region_to_africa, on='user_loc',
                                                  how='inner').reset_index()
        self_sci_dataset = self_sci_dataset.merge(agg_sci_from_region_to_world, on='user_loc',
                                                  how='inner').reset_index()
        self_sci_dataset['ratio_selfloop_to_africa'] = self_sci_dataset.apply(
            lambda x: x['scaled_sci'] / x['Total_SCI_in_Africa'], axis=1)
        self_sci_dataset['ratio_selfloop_to_all_sci'] = self_sci_dataset.apply(
            lambda x: x['scaled_sci'] / x['Total_SCI_in_World'], axis=1)

        sci_dataset['fr_GID_0'] = sci_dataset['fr_loc'].str.strip().str[:2]
        sci_dataset['user_GID_0'] = sci_dataset['user_loc'].str.strip().str[:2]
        # Get only countrywide information
        country_sci_dataset = sci_dataset[sci_dataset['user_GID_0'] == sci_dataset['fr_GID_0']]
        country_sci_dataset = country_sci_dataset.groupby('user_loc').agg(
            Total_SCI_in_Country=('scaled_sci', 'sum')).reset_index()
        self_sci_dataset = self_sci_dataset[
            ['user_loc', 'scaled_sci', 'ratio_selfloop_to_africa', 'ratio_selfloop_to_all_sci']]
        self_sci_dataset = self_sci_dataset.merge(country_sci_dataset, on='user_loc', how='inner')

        self_sci_dataset['ratio_selfloop_to_country'] = self_sci_dataset.apply(
            lambda x: x['scaled_sci'] / x['Total_SCI_in_Country'], axis=1)

        return self_sci_dataset[
            ['user_loc', 'ratio_selfloop_to_country', 'ratio_selfloop_to_africa', 'ratio_selfloop_to_all_sci']]

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
    def compute_sci_share_in_destination_region(gadm1_dataset: pd.DataFrame, sci_dataset: pd.DataFrame) -> pd.DataFrame:
        gadm1_dataset = gadm1_dataset[['GID_1', 'All_devices_age_13_plus_all_genders']].copy()
        sci_dataset = sci_dataset.copy()
        sci_dataset = gadm1_dataset.merge(sci_dataset, left_on='GID_1', right_on='fr_loc', how='inner')
        sci_dataset['new_sci'] = sci_dataset['All_devices_age_13_plus_all_genders'] * sci_dataset['scaled_sci']
        agg_destination_sci_data = sci_dataset.groupby('user_loc').agg(Mean_friendship=('scaled_sci', np.mean),
                                                                       Median_friendship=('scaled_sci', np.median),
                                                                       Std_friendship=('scaled_sci', np.std),
                                                                       Total_friendship=(
                                                                           'scaled_sci', np.sum)).reset_index()
        return agg_destination_sci_data

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
    def correction_of_GID_for_special_case(row: pd.Series):
        if row['GID_1'][:3] == 'COM':
            return 'COM'
        elif row['GID_1'][:3] == 'CPV':
            return 'CPV'
        else:
            return row['GID_1']

    def main(self):
        logger.info(f"combining dataset for Gadm {self.GADM_LEVEL}")
        wpop_source_base_url = self.config['wpop_source_base_url']
        level_config = self.config.pop(self.GADM_LEVEL)
        level_shapefiles = level_config["shapefiles_path"]
        africa_dataset_path = level_config["africa_dataset_path"]
        health_indicators_path = level_config['health_indicators_dataset_path']
        sci_dataset = pd.read_csv(level_config["sci_dataset_path"], delimiter="\t")
        combined_shapefile = self.combine_shapefiles_single_gadm_level(list(level_shapefiles.values()))
        health_indicators = pd.read_csv(health_indicators_path)

        # save combined shapefiles
        combined_shapefile.to_file("external_dataset/all_shapefiles.gpkg", driver="GPKG")
        africa_dataset = self.worksheet_reader(africa_dataset_path)

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
        lmic_gid1_names = gadm1_dhs_dataset.GID_1.unique().tolist()

        agg_data_on_sci_of_fr_loc = self.compute_sci_share_in_destination_region(gadm1_dhs_dataset, sci_dataset)

        # aggregated sci from African countries to all other countries
        agg_sci_from_region_to_all_sci = sci_dataset[sci_dataset.user_loc.isin(lmic_gid1_names)].copy()
        agg_sci_from_region_to_all_sci = agg_sci_from_region_to_all_sci.groupby('user_loc').agg(
            Total_SCI_in_World=('scaled_sci', 'sum')).reset_index()

        # aggregated sci from Africa to Africa
        agg_sci_from_region_to_africa = sci_dataset[
            sci_dataset.user_loc.isin(lmic_gid1_names) & sci_dataset.fr_loc.isin(lmic_gid1_names)].copy()
        agg_sci_from_region_to_africa = agg_sci_from_region_to_africa.groupby('user_loc').agg(
            Total_SCI_in_Africa=('scaled_sci', 'sum')).reset_index()

        # merge all sci related features for only African Countries
        distance_between_sci = self.compute_distance_indices(gadm1_dhs_dataset[['GID_1', 'geometry']], sci_dataset)
        avg_median_std_sci = self.calculate_avg_median_std_SCI(sci_dataset, lmic_gid1_names)
        regional_ratios = self.calculate_regional_ratios(sci_dataset, agg_sci_from_region_to_africa,
                                                         agg_sci_from_region_to_all_sci)
        avg_median_std__with_agg_sci = agg_data_on_sci_of_fr_loc.merge(avg_median_std_sci, on='user_loc',
                                                                       how='inner').reset_index()
        distance_nd_avg_sc = avg_median_std__with_agg_sci.merge(distance_between_sci, on='user_loc',
                                                                how='inner').reset_index()
        sci_features = distance_nd_avg_sc.merge(regional_ratios, on="user_loc", how="inner")
        dhs_sci_dataset = pd.merge(gadm1_dhs_dataset, sci_features, left_on='GID_1', right_on='user_loc', how='inner')
        # end of sci features addition
        obj = Dataset_Creator_World_Covariates()
        covariate_data = obj.calculate_all_covariates(dhs_sci_dataset, level_config, wpop_source_base_url)

        saving_path_variables, saving_path_geometries = self.saving_path_for_gadm_file(self.GADM_LEVEL)
        geometries_cols = ['GID_1', 'geometry']
        dhs_sci_geometries = covariate_data[geometries_cols]
        dhs_variables = covariate_data.loc[:,
                        ~covariate_data.columns.isin(['geometry'])]

        dhs_variables = dhs_variables.T.drop_duplicates().T
        dhs_variables.drop(columns=['GID_1'])
        dhs_variables = dhs_variables.drop(columns=['GID_1'])
        dhs_variables.rename(columns={'GID_1_1': 'GID_1'}, inplace=True)

        dhs_sci_geometries = gpd.GeoDataFrame(dhs_sci_geometries, crs='EPSG:4326')

        # Combining dataset with health indicators
        dhs_variables = dhs_variables.merge(health_indicators, on='GID_1', how='inner')

        # saving data
        dhs_variables.to_csv(saving_path_variables)
        dhs_sci_geometries.to_file(saving_path_geometries)

        logger.info(
            f"dataset for Gadm {self.GADM_LEVEL} completed, geometries are saved in {saving_path_geometries}"
            f" and variables are saved in {saving_path_variables}")

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
