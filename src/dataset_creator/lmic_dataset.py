# Import libraries.
import json
import logging.handlers
import os
from pathlib import Path
from typing import Dict, List

import geopandas as gpd
import pandas as pd

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

    GADM_LEVELS = ["GADM_0", "GADM_1", "GADM_2"]

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
            data = pd.read_csv(data_path, encoding='iso-8859-9')
        else:
            logger.error("Use a worksheet file of either csv or xlsx format")
        return data.head()

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
    def main(self):
        for level in self.GADM_LEVELS:
            logger.info(f"combining dataset for Gadm {level}")
            level_config = self.config.pop(level)
            level_shapefiles = level_config["shapefiles_path"]
            africa_dataset_path = level_config["africa_dataset_path"]
            combined_shapefile = self.combine_shapefiles_single_gadm_level(list(level_shapefiles.values()))
            africa_dataset = self.worksheet_reader(africa_dataset_path)
            if level == "GADM_0":
                combined_dataset = pd.merge(combined_shapefile, africa_dataset,on="GID_0", how="inner")
                saving_path = self.saving_path_for_gadm_file(level)
                combined_dataset.to_file(saving_path, driver="GPKG")
                logger.info(f"dataset for Gadm {level} complete and saved in {saving_path}")
            if level == "GADM_1":
                # to drop duplicate columns
                # https://stackoverflow.com/questions/14984119/python-pandas-remove-duplicate-columns
                africa_dataset = africa_dataset.T.drop_duplicates().T
                combined_dataset = pd.merge(combined_shapefile, africa_dataset, on="GID_1", how="inner")
                saving_path = self.saving_path_for_gadm_file(level)
                combined_dataset.to_file(saving_path, driver="GPKG")
                logger.info(f"dataset for Gadm {level} complete and saved in {saving_path}")
            if level == "GADM_2":
                combined_dataset = pd.merge(combined_shapefile, africa_dataset, on="GID_2", how="inner")
                saving_path = self.saving_path_for_gadm_file(level)
                combined_dataset.to_file(saving_path, driver="GPKG")

    def saving_path_for_gadm_file(self, level):
        if not os.path.exists(self.config['saving_path']):
            os.makedirs(self.config['saving_path'])
        return Path(self.config['saving_path']).joinpath(f"{level}.gpkg").as_posix()


if __name__ == '__main__':
    config_path = "config_scripts/dataset_config.json"
    with open(config_path) as pth:
        config = json.load(pth)

    obj = DataPreprocessing(config)
    obj.main()
