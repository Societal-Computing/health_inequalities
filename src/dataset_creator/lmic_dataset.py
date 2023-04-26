# Import libraries.
import os
import json
from pathlib import Path
from typing import Dict, List

import geopandas as gpd
import pandas as pd


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
            data = pd.read_excel(data_path)
        elif data_path.suffix == '.csv':
            data = pd.read_csv(data_path, encoding='ISO-8859-1')

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
        for single_shapefilepath in shapefilepaths:
            all_shapefiles_ls.append(gpd.read_file(single_shapefilepath))
        combined_shapefile = pd.concat(all_shapefiles_ls)
        return combined_shapefile
    def main(self):
        for level in self.GADM_LEVELS:
            level_config = self.config.pop(level)
            level_shapefiles = level_config["shapefiles_path"]
            africa_dataset_path = level_config["africa_dataset_path"]
            combined_shapefile = self.combine_shapefiles_single_gadm_level(list(level_shapefiles.values()))
            africa_dataset = self.worksheet_reader(africa_dataset_path)
            if level == "GADM_0":
                combined_dataset = pd.merge(combined_shapefile, africa_dataset,on="GID_0", how="inner")
                saving_path = self.saving_path_for_gadm_file(level)
                combined_dataset.to_csv(saving_path)
            if level == "GADM_1":
                combined_dataset = pd.merge(combined_shapefile, africa_dataset, on="GID_1", how="inner")
                saving_path = self.saving_path_for_gadm_file(level)
                combined_dataset.to_csv(saving_path)

    def saving_path_for_gadm_file(self,level):
        if not os.path.exists(self.config['saving_path']):
            os.makedirs(self.config['saving_path'])
        return f"{Path(self.config['saving_path']).joinpath(level)}.csv"


if __name__ == '__main__':
    config_path = "/home/theo/Documents/Societal_Computing/health_inequalities/config_scripts/dataset_config.json"
    with open(config_path) as pth:
        config = json.load(pth)

    obj = DataPreprocessing(config)
    obj.main()
