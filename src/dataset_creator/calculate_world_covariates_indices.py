import os
import sys
from os.path import join

import numpy as np
import rasterio
from rasterio.mask import mask

sys.path.append(join(os.getcwd(), 'src'))
from logger import logger
from utils.helper_functions import covariance_indices_downloader


class Dataset_Creator_World_Covariates:

    def calculate_single_covariant_index(self, gadm_1_dataset, config, source_base_url, mean_label, std_label):
        gadm_1_dataset = gadm_1_dataset.to_crs('4326')
        destination_path = config['destination_path']
        extended_url = config['extended_url']
        data_type = config['data_type']
        variables_list = gadm_1_dataset.GID_0.unique().tolist()
        covariance_indices_downloader(source_base_url, destination_path, variables_list, data_type,
                                      extended_url)
        gadm_1_dataset[mean_label] = None
        gadm_1_dataset[std_label] = None

        for index, row in gadm_1_dataset.iterrows():
            try:
                with rasterio.open(config[row['GID_0']]) as src:
                    out_image, out_transform = mask(src, [row['geometry']], crop=True)

                out_image = np.where(out_image > 0, out_image, 0)
                gadm_1_dataset.at[index, mean_label] = np.mean(out_image[out_image>0], dtype=np.float64)
                gadm_1_dataset.at[index, std_label] = np.std(out_image, dtype=np.float64)
            except Exception as e:
                logger.error(e)
        return gadm_1_dataset

    def calculate_all_covariates(self, gadm_1_dataset, config, source_base_url):
        config_night_light = config['night_lights_shapefiles']
        out_data = self.calculate_single_covariant_index(gadm_1_dataset, config_night_light, source_base_url,
                                                         "Mean_of_Night_Light", "Std_of_Night_Light")
        config_dist_major_rd_intersection = config['distance_to_mjr_rd_intersection_shapefiles']
        out_data = self.calculate_single_covariant_index(out_data, config_dist_major_rd_intersection, source_base_url,
                                                         "Mean_distance_to_major_rd_intesection",
                                                         "Std_distance_to_major_rd_intesection")

        config_distance_to_mjr_rd_shapefiles = config['distance_to_mjr_rd_shapefiles']
        out_data = self.calculate_single_covariant_index(out_data, config_distance_to_mjr_rd_shapefiles,
                                                         source_base_url, "Mean_distance_to_major_rd",
                                                         "Std_distance_to_major_rd")

        return out_data
