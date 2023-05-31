import os
import wget
import time
import sys
from os.path import join
sys.path.append(join(os.getcwd(), 'src'))
from logger import logger


def covariance_indices_downloader(source_base_url, destination_path, variables_list, type_data, extended_url, task=None,):
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