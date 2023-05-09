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
2. combines single shapefile with Africa_dataset for that level 
3. calculate average and standard deviation for LMICs in meta sci 
4. combines result from above with meta sci data 
5. save result as a shapefile ``` /combined_dataset ```


GADM levels used in this project are ``GADM0`` and ```GADM1```

Generated columns from SCI, kindly note that each indices calculated for each LMIC

```Mean_SCI_with_Self```: Mean of SCI with SCI within the country.

```Median_SCI_with_Self``` : Median of SCI with SCI within the country.

```Std_SCI_with_Self``` : Standard Deviation of SCI with SCI within the country.

```Mean_SCI_without_Self```: Mean of SCI without SCI within the country.

```Median_SCI_without_Self``` : Median of SCI without SCI within the country.

```Std_SCI_without_Self``` : Standard Deviation of SCI without SCI within the country.

```Intraconnection_index``` = Self_SCI / Total_SCI , this is a measure of how connected each LMIC is connected
with itself. Where Self_SCI is the SCI within a particular LMIC. Total_SCI is sum of all SCIs for a
particular LMIC.

```LMIC_interconnection_index``` = Sum_LMIC_SCI / Total_SCI, this is a measure of how connected a specific LMIC
is connected to other LMICs.

NOTE: Countries such as `DJIBOUTI`, `COMOROS` and `CAPE VERDE` only have SCI at `GADM_0` level so were not included in
 ```GADM_1.gpkg```.