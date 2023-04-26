# Social connectedness and health inequalities
Git repository for the corresponding paper with the working title 'Social connectedness and health inequalities'

#### Environment setup
1.``` pip install -r requirements.txt ```

2.``` export PYTHONPATH="src" ```

#### Dataset creation
3 ``` python src/dataset_creator/lmic_dataset.py ```
When code is executed, the program combines the shapefiles with the African dataset on Gadm 0 and subsequently on Gadm 1 
for all the selected countries. The result is saved as a csv file in ``` /combined_dataset ```