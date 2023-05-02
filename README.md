# Social connectedness and health inequalities
Git repository for the corresponding paper with the working title 'Social connectedness and health inequalities'

#### Environment setup
``` pip install -r requirements.txt ```

``` export PYTHONPATH="src" ```

#### Dataset creation
``` python src/dataset_creator/lmic_dataset.py ```
When code is executed, the program does the following:

For each GADM level: 
1. combines all shapefiles into a single shapefile 
2. combines single shapefile with Africa_dataset 
3. calculate average and standard deviation for LMICs in meta sci 
4. combines result from above with meta sci data 
5. save result as a shapefile ``` /combined_dataset ```


GADM levels used in this project are ``GADM0`` and ```GADM1```