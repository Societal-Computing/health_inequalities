import json
import os
import sys
from os.path import join

import geopandas as gpd

sys.path.append(join(os.getcwd(), 'src'))
from logger import logger
from utils.helper_functions import worksheet_reader, generate_hash_value

CRS = "4326"


def combine_hdi_lmic(gdl_hdi_dataset_path, gdl_shapefile_path, lmic_shapefile_path, save_path):
    gdl_hdi_data = worksheet_reader(gdl_hdi_dataset_path)
    logger.info(f"Sub-national data read with {len(gdl_hdi_data)} records")
    gdl_geometries = gpd.read_file(gdl_shapefile_path)
    lmic_geometries = gpd.read_file(lmic_shapefile_path)[["GID_1", "geometry"]]
    logger.info(f"LMIC geometries data read with {len(lmic_geometries)} records")
    lmic_geometries = lmic_geometries.to_crs(CRS)
    gdl_with_geom = gpd.GeoDataFrame(
        gdl_hdi_data.merge(gdl_geometries, left_on='GDLCODE', right_on='gdlcode', how='inner')).to_crs(CRS)

    lmic_geometries['ID'] = lmic_geometries.apply(
        lambda x: generate_hash_value(x['geometry']), axis=1)

    gdl_with_geom['ID'] =gdl_with_geom.apply(
        lambda x: generate_hash_value(x['geometry']), axis=1)

    #sub_nat_ind = lmic_geometries.sjoin(gdl_with_geom, how="left")
    sub_nat_ind = lmic_geometries.merge(gdl_with_geom, on='ID', how='inner')
    logger.info(f"Common maps are filtered from LMIC geometries and Sub-national data with {len(sub_nat_ind)} records")
    sub_nat_ind = sub_nat_ind[["GID_1", "2021"]]
    sub_nat_ind.rename(columns={'2021': 'HDI'}, inplace=True)
    sub_nat_ind.to_csv(save_path)


if __name__ == "__main__":
    config_path = "config_scripts/gdl_processing_config.json"
    with open(config_path) as pth:
        config = json.load(pth)
    gdl_hdi_dataset_path = config['gdl_hdi_dataset_path']
    gdl_shapefile_path = config['gdl_shapefile_path']
    lmic_shapefile_path = config['lmic_shapefile_path']
    save_path = config['save_path']
    combine_hdi_lmic(gdl_hdi_dataset_path, gdl_shapefile_path, lmic_shapefile_path, save_path)
