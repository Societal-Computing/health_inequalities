import os
import wget
import time
import sys
from pathlib import Path
from os.path import join
import pandas as pd
import hashlib
import shapely

sys.path.append(join(os.getcwd(), 'src'))
from logger import logger


def covariance_indices_downloader(source_base_url, destination_path, variables_list, type_data, extended_url,
                                  task=None, ):
    if task == 'night_light':
        split_value = '//'
    else:
        split_value = '/'
    for country in variables_list:
        logger.info(f"Downloading {type_data} data for {country}")
        xtd_url = extended_url.replace('#', country)
        xtd_url = xtd_url.replace('$', country.lower())
        url = source_base_url + xtd_url

        if os.path.isfile(f"{destination_path}{xtd_url.split(split_value)[-1]}"):
            logger.info(f"{country} file has already been downloaded")
        else:
            wget.download(url, destination_path)
            time.sleep(10)

    logger.info("Download completed !!!!!!")


def worksheet_reader(data_path: str or Path) -> pd.DataFrame:
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
    elif data_path.suffix == '.tsv':
        data = pd.read_csv(data_path, delimiter="\t",  encoding='ISO-8859-1')
    else:
        logger.error("Use a worksheet file of either csv, tsv or xlsx format")
    return data

def generate_hash_value(geometry):
    return hashlib.md5(bytes(shapely.wkb.dumps(geometry, hex=True), encoding="utf8")).hexdigest()