library(haven)          # Load stata files
library(tidyverse)      # Method chaining
library(survey)         # Survey aggregation
library(labelled)       # Set variable labels
library(matrixStats)    # 
library(sf)             # read shape files

options(dplyr.summarise.inform = FALSE)
select <- dplyr::select

# Functions and directories -----------------------------------------------

codewd <- "/Users/tillkoebe/Documents/GitHub/health_inequalities/health_indicators"
datawd <- "/Users/tillkoebe/Documents/Data/Afrobarometer/"

# Load data ---------------------------------------------------------------

setwd(datawd)

afro <- paste0(datawd,'/afro_regional_update.csv') %>% 
  read_csv %>% 
  select(region:mp_service, lon = longitude, lat = latitude)


AFRdata <- st_read(dsn = "/Users/tillkoebe/Documents/GitHub/health_inequalities/combined_dataset/GADM_1_geometries.gpkg") %>% 
  st_make_valid() %>% 
  st_transform(4326) %>% 
  rowid_to_column(var = "rowid")

afro <- afro %>%
  st_as_sf(., coords = c("lon", "lat"), 
           crs = 4326, agr = "identity") %>% 
  mutate(nearest_index = st_nearest_feature(.,AFRdata)) %>% 
  rename(rowid = nearest_index) %>% 
  left_join(AFRdata %>% 
              as.data.frame %>% 
              select(rowid, GID_1),
            by = 'rowid') %>% 
  as.data.frame() %>% 
  select(-geometry, -rowid) %>% 
  group_by(GID_1) %>%
  summarise(across(member_community:mp_service, ~mean(.x, na.rm = T))) %>%
  ungroup %>% 
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))

write.csv(afro,
          file='/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/afrobarometer.csv', 
          row.names=FALSE)
