library(haven)          # Load stata files
library(tidyverse)      # Method chaining
library(survey)         # Survey aggregation
library(naniar)         # replace_with_na function
library(labelled)       # Set variable labels
library(matrixStats)    # Set variable labels
library(sf)             # read shape files

# Functions and directories -----------------------------------------------
codewd <- "/Users/tillkoebe/Documents/GitHub/health_inequalities/src/dataset_creator/health_indicators"
datawd <- "/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/DHS_data/"

setwd(datawd)


# Get survey list ---------------------------------------------------------
# For variable coding DHS7 see: https://www.dhsprogram.com/pubs/pdf/DHSG4/Recode7_Map_31Aug2018_DHSG4.pdf

### Get all datasets in directory
dataset_ls <- list.files(pattern = "*.DTA", 
                         recursive = TRUE, 
                         all.files = T, 
                         full.names = F) %>% 
  append(list.files(pattern = "*.shp", 
                    recursive = TRUE, 
                    all.files = T, 
                    full.names = F)) %>% 
  str_subset(pattern = ".xml", negate = T)

### Extract countries covered
country_ls <- substr(dataset_ls, 1, 2) %>% 
  unique()

#**Starting point for parallelization*

for(i in country_ls){
  ### Check if DHS or MIS
  MIS <- !is_empty(dataset_ls %>% 
                     str_subset(pattern = paste0(country_ls[1],"FW")))
  
  ### Geographic locations of survey clusters
  GEOdata <- st_read(dsn = dataset_ls %>% 
                       str_subset(pattern = paste0(country_ls[1],"GE")) %>% 
                       paste0(datawd,.))
  
  ### Births
  if(MIS == FALSE){
    
    BRdata <- dataset_ls %>% 
      str_subset(pattern = paste0(country_ls[1],"BR")) %>% 
      paste0(datawd,.) %>% 
      read_dta %>% 
      mutate(wt = v005/1000000,
             hhid = paste(v001, v002, sep='.'))
  }
  
  ### Children
  KRdata <-  dataset_ls %>% 
    str_subset(pattern = paste0(country_ls[1],"KR")) %>% 
    paste0(datawd,.) %>% 
    read_dta %>% 
    mutate(wt = v005/1000000,
           hhid = paste(v001, v002, sep='.'))
  
  ### Couple
  if(MIS == FALSE){
    
    CRdata <-  dataset_ls %>% 
      str_subset(pattern = paste0(country_ls[1],"CR")) %>% 
      paste0(datawd,.) %>% 
      read_dta %>% 
      mutate(wt = v005/1000000,
             hhid = paste(v001, v002, sep='.'))
    
  }
  
  
  ### Households
  HRdata <-  dataset_ls %>% 
    str_subset(pattern = paste0(country_ls[1],"HR")) %>% 
    paste0(datawd,.) %>% 
    read_dta %>% 
    mutate(wt = hv005/1000000,
           hhid = paste(hv001, hv002, sep='.'))
  
  ### Household members
  PRdata <-  dataset_ls %>% 
    str_subset(pattern = paste0(country_ls[1],"PR")) %>% 
    paste0(datawd,.) %>% 
    read_dta %>% 
    mutate(wt = hv005/1000000,
           hhid = paste(hv001, hv002, sep='.'))
  
  ### Men
  if(MIS == FALSE){
    MRdata <-  dataset_ls %>% 
      str_subset(pattern = paste0(country_ls[1],"MR")) %>% 
      paste0(datawd,.) %>% 
      read_dta %>% 
      mutate(wt = mv005/1000000,
             hhid = paste(mv001, mv002, sep='.'))
  }
  
  ### Individual (Women)
  IRdata <-  dataset_ls %>% 
    str_subset(pattern = paste0(country_ls[1],"IR")) %>% 
    paste0(datawd,.) %>% 
    read_dta %>% 
    mutate(wt = v005/1000000,
           hhid = paste(v001, v002, sep='.'))
  
  
  # Indicators --------------------------------------------------------------
  # Much indicator code taken from: https://github.com/DHSProgram/DHS-Indicators-R
  
  ### Multidimensional poverty
  
  # source(paste0(codewd,"/multi_poverty.R"))
  
  # Vaccination coverage ----------------------------------------------------
  
  if(MIS == FALSE){
    source(paste0(codewd,"/child_health.R"))
  }
  
  
  # Healthcare access -------------------------------------------------------
  
  if(MIS == FALSE){
    source(paste0(codewd,"/healthcare_access.R"))
  }
  
  # Women empowerment -------------------------------------------------------
  
  if(MIS == FALSE){
    source(paste0(codewd,"/women_empowerment.R"))
  }
  
  # Antenatal care ----------------------------------------------------------
  
  if(MIS == FALSE){
    source(paste0(codewd,"/antenatal_care.R"))
  }
  
  # Domestic violence -------------------------------------------------------
  
  if(MIS == FALSE){
    source(paste0(codewd,"/domestic_violence.R"))
  }
  
  # Breastfeeding -----------------------------------------------------------
  
  if(MIS == FALSE){
    source(paste0(codewd,"/breastfeeding.R"))
  }
  
  # Contraceptive Knowledge -------------------------------------------------
  
  if(MIS == FALSE){
    source(paste0(codewd,"/contraceptive_knowledge.R"))
  }
  
  # Contraceptive preferences -----------------------------------------------
  
  if(MIS == FALSE){
    source(paste0(codewd,"/contraceptive_preferences.R"))
  }
  
  
  # Add geographic identifier -----------------------------------------------
  
  if(MIS == FALSE){
    
    BFdata <- BFdata %>% 
      left_join(GEOdata %>% 
                  select(DHSCLUST, 
                         ADM1DHS, ADM1NAME, 
                         URBAN_RURA, 
                         LATNUM, LONGNUM), 
                by = join_by(v001 == DHSCLUST))
    
    CHdata <- CHdata %>% 
      left_join(GEOdata %>% 
                  select(DHSCLUST, 
                         ADM1DHS, ADM1NAME, 
                         URBAN_RURA, 
                         LATNUM, LONGNUM), 
                by = join_by(v001 == DHSCLUST))
    
    CKdata <- CKdata %>% 
      left_join(GEOdata %>% 
                  select(DHSCLUST, 
                         ADM1DHS, ADM1NAME, 
                         URBAN_RURA, 
                         LATNUM, LONGNUM), 
                by = join_by(v001 == DHSCLUST))
    
    CKmdata <- CKmdata %>% 
      left_join(GEOdata %>% 
                  select(DHSCLUST, 
                         ADM1DHS, ADM1NAME, 
                         URBAN_RURA, 
                         LATNUM, LONGNUM), 
                by = join_by(mv001 == DHSCLUST))
    
    CPdata <- CPdata %>% 
      left_join(GEOdata %>% 
                  select(DHSCLUST, 
                         ADM1DHS, ADM1NAME, 
                         URBAN_RURA, 
                         LATNUM, LONGNUM), 
                by = join_by(v001 == DHSCLUST))
    
    CPmdata <- CPmdata %>% 
      left_join(GEOdata %>% 
                  select(DHSCLUST, 
                         ADM1DHS, ADM1NAME, 
                         URBAN_RURA, 
                         LATNUM, LONGNUM), 
                by = join_by(mv001 == DHSCLUST))
    
    DVdata <- DVdata %>% 
      left_join(GEOdata %>% 
                  select(DHSCLUST, 
                         ADM1DHS, ADM1NAME, 
                         URBAN_RURA, 
                         LATNUM, LONGNUM), 
                by = join_by(v001 == DHSCLUST))
    
    HAdata <- HAdata %>% 
      left_join(GEOdata %>% 
                  select(DHSCLUST, 
                         ADM1DHS, ADM1NAME, 
                         URBAN_RURA, 
                         LATNUM, LONGNUM), 
                by = join_by(v001 == DHSCLUST))
    
    WEdata <- WEdata %>% 
      left_join(GEOdata %>% 
                  select(DHSCLUST, 
                         ADM1DHS, ADM1NAME, 
                         URBAN_RURA, 
                         LATNUM, LONGNUM), 
                by = join_by(v001 == DHSCLUST))
    
    WEmdata <- WEmdata %>% 
      left_join(GEOdata %>% 
                  select(DHSCLUST, 
                         ADM1DHS, ADM1NAME, 
                         URBAN_RURA, 
                         LATNUM, LONGNUM), 
                by = join_by(mv001 == DHSCLUST))
  }
  
  
  # Append and Save ---------------------------------------------------------
  
  if(MIS == FALSE){
    saveRDS(BFdata, file = paste0(datawd, country_ls[1], "BFdata.rds"))
    saveRDS(CHdata, file = paste0(datawd, country_ls[1], "CHdata.rds"))
    saveRDS(CKdata, file = paste0(datawd, country_ls[1], "CKdata.rds"))
    saveRDS(CKmdata, file = paste0(datawd, country_ls[1], "CKmdata.rds"))
    saveRDS(CPdata, file = paste0(datawd, country_ls[1], "CPdata.rds"))
    saveRDS(CPmdata, file = paste0(datawd, country_ls[1], "CPmdata.rds"))
    saveRDS(DVdata, file = paste0(datawd, country_ls[1], "DVdata.rds"))
    saveRDS(HAdata, file = paste0(datawd, country_ls[1], "HAdata.rds"))
    saveRDS(WEdata, file = paste0(datawd, country_ls[1], "WEdata.rds"))
    saveRDS(WEmdata, file = paste0(datawd, country_ls[1], "WEmdata.rds"))
  }
  
  print(paste(i,'out of',length(country_ls),'countries processed.'))
}




#**End point for parallelization*
