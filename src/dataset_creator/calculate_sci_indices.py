import json
import os
import sys
from os.path import join
from pathlib import Path
from typing import List

import geopandas as gpd
import numpy as np
import pandas as pd

sys.path.append(join(os.getcwd(), 'src'))
from logger import logger
from utils.helper_functions import worksheet_reader


class SCI_Indices_Calculator:
    CRS = "EPSG:4326"
    QUANTILES = ["Ratio_SCI_low_hi_africa", "Ratio_SCI_middle_hi_africa", "Ratio_SCI_high_hi_africa"]

    def __init__(self, sci_data_path: str, all_shapefile_path: str, health_index_dataset_path: str):
        self.sci_dataset = worksheet_reader(sci_data_path)
        self.gadm1_dhs_dataset = gpd.read_file(all_shapefile_path)[
            ["GID_1", "geometry", "All_devices_age_13_plus_all_genders"]]
        self.health_index_dataset_path = health_index_dataset_path

    @staticmethod
    def calculate_regional_ratios(sci_dataset: pd.DataFrame, agg_sci_from_region_to_africa: pd.DataFrame,
                                  agg_sci_from_region_to_world: pd.DataFrame) -> pd.DataFrame:
        """
        Calculate ratio of self_loop to al sci in country,
        ratio of self_sci to all regions in Africa,
        ratio of self_sci to all regions,
        ratio of sum of sci in country to all regions
        :param agg_sci_from_region_to_world:
        :param agg_sci_from_region_to_africa:
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
        logger.info("Computing distance indices ....")
        sci_dataset = sci_dataset.copy()
        geometries = geometries.copy()
        geometries = geometries.to_crs(cls.CRS)
        geometries['Centroids'] = geometries.geometry.centroid
        geometries['Centroids'] = geometries['Centroids'].to_crs(cls.CRS)
        sci_dataset = sci_dataset.merge(geometries, left_on='user_loc', right_on='GID_1', how='inner')
        sci_dataset = sci_dataset.rename(columns={"Centroids": "user_loc_centroid"})
        sci_dataset.drop(columns=["GID_1", "geometry"], inplace=True)

        sci_dataset = sci_dataset.merge(geometries, left_on='fr_loc', right_on='GID_1', how='inner')
        sci_dataset = sci_dataset.rename(columns={"Centroids": "fr_loc_centroid"})
        sci_dataset.drop(columns=["GID_1"], inplace=True)
        sci_dataset = gpd.GeoDataFrame(sci_dataset)
        sci_dataset['distance'] = sci_dataset['user_loc_centroid'].distance(sci_dataset['fr_loc_centroid'])
        sci_dataset = sci_dataset.to_crs(cls.CRS)
        sci_dataset['distance'] = sci_dataset['distance'].abs()
        sci_dataset.drop(columns=["fr_loc_centroid", "user_loc_centroid", "geometry"], inplace=True)
        avg_median_std_distance = sci_dataset.groupby('user_loc').agg(Mean_dist_to_SCI=('distance', np.mean),
                                                                      Median_dist_to_SCI=('distance', np.median),
                                                                      Std_dist_to_SCI=('distance', np.std),
                                                                      Total_dist_to_SCI=('distance', np.sum)
                                                                      ).reset_index()
        logger.info("Distance computations completed!")
        return avg_median_std_distance

    @staticmethod
    def calculate_country_constrained_features(sci_dataset: pd.DataFrame):
        """
        Calculate country constrained mean, median and std of sci
        :param sci_dataset:
        :return:
        """
        logger.info("Computing mean , std and median SCI of a region constrained by country")
        sci_dataset = sci_dataset.copy()
        sci_dataset['fr_GID_0'] = sci_dataset['fr_loc'].str.strip().str[:2]
        sci_dataset['user_GID_0'] = sci_dataset['user_loc'].str.strip().str[:2]
        # Get only countrywide information
        sci_dataset = sci_dataset[sci_dataset['user_GID_0'] == sci_dataset['fr_GID_0']]
        sci_dataset = sci_dataset.groupby(['user_loc']).agg(Local_sum_SCI=('scaled_sci', np.sum),
                                                            Local_mean_SCI=('scaled_sci', np.mean),
                                                            Local_std_SCI=('scaled_sci', np.std)).reset_index()

        logger.info("Computation of country constrained features completed!")
        return sci_dataset

    @staticmethod
    def compute_sci_share_in_destination_region(gadm1_dataset: pd.DataFrame, sci_dataset: pd.DataFrame) -> pd.DataFrame:
        logger.info("Calculating friendship share in destination regions")
        gadm1_dataset = gadm1_dataset[['GID_1', 'All_devices_age_13_plus_all_genders']].copy()
        sci_dataset = sci_dataset.copy()
        sci_dataset = gadm1_dataset.merge(sci_dataset, left_on='GID_1', right_on='fr_loc', how='inner')
        sci_dataset['new_sci'] = sci_dataset['All_devices_age_13_plus_all_genders'] * sci_dataset['scaled_sci']
        agg_destination_sci_data = sci_dataset.groupby('user_loc').agg(Mean_friendship=('scaled_sci', np.mean),
                                                                       Median_friendship=('scaled_sci', np.median),
                                                                       Std_friendship=('scaled_sci', np.std),
                                                                       Total_friendship=(
                                                                           'scaled_sci', np.sum)).reset_index()
        logger.info("Freindhip share calculation completed !")
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
        logger.info("Calculating indices based on raw SCI")
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
        logger.info("Computations on raw SCI completed!")
        return avg_std

    @classmethod
    def get_quantile(cls, min_value, max_value):
        quantiles = {}
        lower_bound = min_value
        tol_value = (max_value - min_value) / len(cls.QUANTILES)
        upper_bound = lower_bound + tol_value
        for i in range(len(cls.QUANTILES)):
            # upper_bound = lower_bound + tol_value
            quantiles[cls.QUANTILES[i]] = [lower_bound, upper_bound]
            lower_bound = upper_bound
            upper_bound += tol_value
        return quantiles

    @staticmethod
    def assign_quantile(value, quantiles):
        for i, j in quantiles.items():
            if j[1] > value >= j[0]:
                return i

    def calculate_SCI_share_based_HI_quantiles(self, health_index_data, raw_sci, lmic_countries):
        raw_sci = raw_sci.copy()
        total_sci = raw_sci.groupby('user_loc').agg(Total_SCI=('scaled_sci', 'sum')).reset_index()
        quantiles = self.get_quantile(health_index_data['2021'].min(), health_index_data['2021'].max())
        health_index_data['quantiles'] = health_index_data.apply(
            lambda x: self.assign_quantile(x['2021'], quantiles), axis=1)

        raw_sci = raw_sci[raw_sci.user_loc.isin(lmic_countries)]
        raw_sci['ISO_Code'] = raw_sci['fr_loc'].str[:3]
        raw_sci = raw_sci.merge(health_index_data, on='ISO_Code', how='inner')
        raw_sci = raw_sci.groupby(['user_loc', 'quantiles']).agg(SCI_dist=('scaled_sci', 'sum')).reset_index()
        agg_raw_sci = raw_sci.merge(total_sci, on='user_loc', how='inner')
        agg_raw_sci['HI_ratio'] = agg_raw_sci['SCI_dist'] / agg_raw_sci['Total_SCI']
        agg_raw_sci = agg_raw_sci.pivot(index='user_loc', columns='quantiles', values='HI_ratio').reset_index()
        return agg_raw_sci.reset_index(drop=True)

    @classmethod
    def dhsData_sciData_with_healthInequalities(cls, dhs_data_path: str or Path,
                                                gadm_data: pd.DataFrame) -> pd.DataFrame:
        health_inequalities = worksheet_reader(dhs_data_path)
        gadm_data = gadm_data.copy()
        sci_health_data = health_inequalities.merge(gadm_data, on='GID_1', how="left")
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
        sci_dataset = self.sci_dataset.copy()
        gadm1_dataset = self.gadm1_dhs_dataset.copy()
        lmic_gid1 = gadm1_dataset.GID_1.unique().tolist()
        health_index_data = worksheet_reader(self.health_index_dataset_path)

        agg_data_on_sci_of_fr_loc = self.compute_sci_share_in_destination_region(gadm1_dataset, sci_dataset)

        # aggregated sci from African countries to all other countries
        agg_sci_from_region_to_all_sci = sci_dataset[sci_dataset.user_loc.isin(lmic_gid1)].copy()
        agg_sci_from_region_to_all_sci = agg_sci_from_region_to_all_sci.groupby('user_loc').agg(
            Total_SCI_in_World=('scaled_sci', 'sum')).reset_index()

        # aggregated sci from Africa to Africa
        agg_sci_from_region_to_africa = sci_dataset[
            sci_dataset.user_loc.isin(lmic_gid1) & sci_dataset.fr_loc.isin(lmic_gid1)].copy()
        agg_sci_from_region_to_africa = agg_sci_from_region_to_africa.groupby('user_loc').agg(
            Total_SCI_in_Africa=('scaled_sci', 'sum')).reset_index()

        # merge all sci related features for only African Countries
        distance_between_sci = self.compute_distance_indices(gadm1_dataset[['GID_1', 'geometry']], sci_dataset)
        avg_median_std_sci = self.calculate_avg_median_std_SCI(sci_dataset, lmic_gid1)
        regional_ratios = self.calculate_regional_ratios(sci_dataset, agg_sci_from_region_to_africa,
                                                         agg_sci_from_region_to_all_sci)
        avg_median_std__with_agg_sci = agg_data_on_sci_of_fr_loc.merge(avg_median_std_sci, on='user_loc',
                                                                       how='inner').reset_index()
        distance_nd_avg_sc = avg_median_std__with_agg_sci.merge(distance_between_sci, on='user_loc',
                                                                how='inner').reset_index()
        sci_features = distance_nd_avg_sc.merge(regional_ratios, on="user_loc", how="inner")
        sci_features = sci_features[['user_loc', 'Mean_friendship', 'Median_friendship', 'Std_friendship',
                                     'Total_friendship', 'Mean_SCI_with_Self', 'Median_SCI_with_Self',
                                     'Std_SCI_with_Self', 'SCI', 'Mean_SCI_without_Self', 'Median_SCI_without_Self',
                                     'Std_SCI_without_Self', 'Mean_dist_to_SCI', 'Median_dist_to_SCI',
                                     'Std_dist_to_SCI', 'Total_dist_to_SCI', 'ratio_selfloop_to_country',
                                     'ratio_selfloop_to_africa', 'ratio_selfloop_to_all_sci']]
        quantiles_hi = self.calculate_SCI_share_based_HI_quantiles(health_index_data, sci_dataset, lmic_gid1)
        sci_features = sci_features.merge(quantiles_hi, on='user_loc', how='inner')
        save_path = "external_dataset/sci_indices.csv"
        # save sci aggregated dataset
        sci_features.to_csv(save_path)
        logger.info(f" Generated indices on SCI has been saved in {save_path}")


if __name__ == "__main__":
    config_path = "config_scripts/lmic_shapefiles_config.json"
    with open(config_path) as pth:
        config = json.load(pth)
    obj = SCI_Indices_Calculator(config["GADM_1"]["sci_dataset_path"], config["GADM_1"]["lmic_shapefile"], config["world_health_index_dataset_path"])
    obj.main()
