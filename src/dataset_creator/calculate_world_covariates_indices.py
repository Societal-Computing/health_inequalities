import json
import os
import sys
from os.path import join

import geopandas as gpd
import numpy as np
import rasterio
from rasterio.mask import mask

sys.path.append(join(os.getcwd(), 'src'))
from logger import logger
from utils.helper_functions import covariance_indices_downloader


class Dataset_Creator_World_Covariates:
    CRS = "4326"

    def calculate_single_covariant_index(self, gadm_1_dataset, config, source_base_url, mean_label, std_label):
        gadm_1_dataset = gadm_1_dataset.to_crs(self.CRS)

        destination_path = config['destination_path']
        extended_url = config['extended_url']
        data_type = config['data_type']
        variables_list = gadm_1_dataset.GID_0.unique().tolist()
        covariance_indices_downloader(source_base_url, destination_path, variables_list, data_type,
                                      extended_url)
        gadm_1_dataset[mean_label] = None
        gadm_1_dataset[std_label] = None

        for index, row in gadm_1_dataset.iterrows():
            src = rasterio.open(config[row['GID_0']], crs=self.CRS)
            try:
                out_image, _ = mask(src, [row['geometry']], nodata=-10000, invert=False)
                out_image = np.where(out_image > 0, out_image, 0)
                gadm_1_dataset.at[index, mean_label] = np.mean(out_image[out_image > 0], dtype=np.float64)
                gadm_1_dataset.at[index, std_label] = np.std(out_image, dtype=np.float64)
            except Exception as e:
                logger.error(f"Error due to {e}")

        return gadm_1_dataset

    def calculate_all_covariates(self, gadm_1_dataset, config, source_base_url):
        config_night_light = config['night_lights_shapefiles']
        out_data = self.calculate_single_covariant_index(gadm_1_dataset, config_night_light, source_base_url,
                                                         "Mean_of_Night_Light", "Std_of_Night_Light")

        config_dist_major_rd_intersection = config['distance_to_mjr_rd_intersection_shapefiles']
        out_data = self.calculate_single_covariant_index(out_data, config_dist_major_rd_intersection, source_base_url,
                                                         "Mean_distance_to_major_rd_intersection",
                                                         "Std_distance_to_major_rd_intersection")

        config_distance_to_mjr_rd_shapefiles = config['distance_to_mjr_rd_shapefiles']
        out_data = self.calculate_single_covariant_index(out_data, config_distance_to_mjr_rd_shapefiles,
                                                         source_base_url, "Mean_distance_to_major_rd",
                                                         "Std_distance_to_major_rd")

        config_distance_to_inland_water = config['distance_to_inland_water']
        out_data = self.calculate_single_covariant_index(out_data, config_distance_to_inland_water,
                                                         source_base_url, "Mean_distance_to_inland_water",
                                                         "Std_distance_to_inland_water")

        config_built_settlement_growth = config['built_settlement_growth']
        out_data = self.calculate_single_covariant_index(out_data, config_built_settlement_growth,
                                                         config_built_settlement_growth['wpop_source_base_url'],
                                                         "Mean_built_settlement_growth",
                                                         "Std_built_settlement_growth")

        return out_data[["GID_1", "Mean_of_Night_Light", "Std_of_Night_Light", "Mean_distance_to_major_rd_intersection",
                         "Std_distance_to_major_rd_intersection", "Mean_distance_to_major_rd",
                         "Std_distance_to_major_rd", "Mean_distance_to_inland_water", "Std_distance_to_inland_water",
                         "Mean_built_settlement_growth",
                         "Std_built_settlement_growth"]]


if __name__ == "__main__":
    config_path = "config_scripts/covariates_config.json"
    with open(config_path) as pth:
        config = json.load(pth)

    all_shape_files = gpd.read_file(config['lmic_shapefile'])[["GID_0", "GID_1", "geometry"]]
    wpop_source_base_url = config['wpop_source_base_url']
    obj = Dataset_Creator_World_Covariates()
    covariate_data = obj.calculate_all_covariates(all_shape_files, config, wpop_source_base_url)
    covariate_data.to_csv(config['saving_path'])
