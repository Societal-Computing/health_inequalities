library(haven)          # Load stata files
library(tidyverse)      # Method chaining
library(labelled)       # Set variable labels
library(naniar)         # replace_with_na function
library(sf)             # read shape files
library(data.table)     # for inrange function

options(dplyr.summarise.inform = FALSE)
select <- dplyr::select

# Functions and directories -----------------------------------------------
codewd <- "/Users/tillkoebe/Documents/GitHub/health_inequalities/health_indicators"
datawd <- "/Users/tillkoebe/Documents/Data/DHS Africa/"

setwd(datawd)


# Define output -----------------------------------------------------------

dhs_health <- data.frame()

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

### Allocate clusters to ADM1 regions
AFRdata <- st_read(dsn = "/Users/tillkoebe/Documents/GitHub/health_inequalities/combined_dataset/GADM_1_geometries.gpkg") %>% 
  st_make_valid() %>% 
  st_transform(4326) %>% 
  # left_join(read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/combined_dataset/GADM_1_variables.csv") %>% 
  #             select(GID_1, HASC_1), by = 'GID_1') %>% 
  select(GID_1, HASC_1, geom) %>% 
  mutate(
    HASC_1 = if_else(
      grepl('GIN', GID_1, fixed = TRUE),'GN', HASC_1), # fix ISO of Guinea
    HASC_1 = if_else(
      grepl('UGA', GID_1, fixed = TRUE),'UG', HASC_1), # fix ISO of Uganda
    HASC_1 = if_else(
      grepl('NER', GID_1, fixed = TRUE),'NI', HASC_1), # fix ISO of Niger
    HASC_1 = if_else(
      grepl('GHA', GID_1, fixed = TRUE),'GH', HASC_1), # fix ISO of Ghana
    HASC_1 = if_else(
      grepl('NAM', GID_1, fixed = TRUE),'NM', HASC_1), # fix ISO of Namibia
    HASC_1 = if_else(
      grepl('BDI', GID_1, fixed = TRUE),'BU', HASC_1), # fix ISO of Burundi
    HASC_1 = if_else(
      grepl('LBR', GID_1, fixed = TRUE),'LB', HASC_1), # fix ISO of Liberia
    HASC_1 = if_else(
      grepl('MDG', GID_1, fixed = TRUE),'MD', HASC_1), # fix ISO of Madagascar
    HASC_1 = if_else(
      grepl('TCD', GID_1, fixed = TRUE),'TD', HASC_1), # fix ISO of Chad
    HASC_1 = if_else(
      grepl('COD', GID_1, fixed = TRUE),'CD', HASC_1), # fix ISO of DR Congo
    HASC_1 = if_else(
      grepl('EGY', GID_1, fixed = TRUE),'EG', HASC_1), # fix ISO of Egypt
    HASC_1 = if_else(
      grepl('BFA', GID_1, fixed = TRUE),'BF', HASC_1), # fix ISO of Burkina Faso
    HASC_1 = if_else(
      grepl('COM', GID_1, fixed = TRUE),'KM', HASC_1), # fix ISO of Comoros
  )

# Start loop across countries ---------------------------------------------

for(i in country_ls){
  
  tryCatch({
    ### Check if DHS or MIS
    MIS <- is_empty(dataset_ls %>% 
                      str_subset(pattern = paste0(i,"BR")))
    
    if(MIS == FALSE){
      
      
      # Allocate clusters to GADM regions ---------------------------------------
      
      GEOdata <- AFRdata %>% 
        filter(substr(HASC_1, 1, 2) == i) %>% 
        select(GID_1, geom) %>% 
        rowid_to_column(var = 'nearest_index')
      
      '
      We use st_nearest_feature instead of st_intersection as some cluster 
      locations (e.g. on islands, very close to the national border)
      might turn NA due to spatial inaccuracies.
      '
      
      GEOdata <- st_read(dsn = dataset_ls %>% 
                           str_subset(pattern = paste0(i,"GE")) %>% 
                           paste0(datawd,.)) %>% 
        mutate(nearest_index = st_nearest_feature(.,GEOdata)) %>% 
        left_join(GEOdata %>% 
                    as.data.frame() %>% 
                    select(nearest_index, GID_1),
                  by = 'nearest_index') %>% 
        select(DHSCLUST, GID_1)
      
      
      # Load survey data --------------------------------------------------------
      
      ### Births
      BRdata <- dataset_ls %>% 
        str_subset(pattern = paste0(i,"BR")) %>% 
        paste0(datawd,.) %>% 
        read_dta %>% 
        mutate(wt = v005/1000000,
               hhid = paste(v001, v002, sep='.'))
      
      ### Children
      KRdata <-  dataset_ls %>% 
        str_subset(pattern = paste0(i,"KR")) %>% 
        paste0(datawd,.) %>% 
        read_dta %>% 
        mutate(wt = v005/1000000,
               hhid = paste(v001, v002, sep='.'))
      
      ### Couple
      CRdata <-  dataset_ls %>% 
        str_subset(pattern = paste0(i,"CR")) %>% 
        paste0(datawd,.) %>% 
        read_dta %>% 
        mutate(wt = v005/1000000,
               hhid = paste(v001, v002, sep='.'))
      
      ### Households
      HRdata <-  dataset_ls %>% 
        str_subset(pattern = paste0(i,"HR")) %>% 
        paste0(datawd,.) %>% 
        read_dta %>% 
        mutate(wt = hv005/1000000,
               hhid = paste(hv001, hv002, sep='.'))
      
      ### Household members
      PRdata <-  dataset_ls %>% 
        str_subset(pattern = paste0(i,"PR")) %>% 
        paste0(datawd,.) %>% 
        read_dta %>% 
        mutate(wt = hv005/1000000,
               hhid = paste(hv001, hv002, sep='.'))
      
      ### Men
      MRdata <-  dataset_ls %>% 
        str_subset(pattern = paste0(i,"MR")) %>% 
        paste0(datawd,.) %>% 
        read_dta %>% 
        mutate(wt = mv005/1000000,
               hhid = paste(mv001, mv002, sep='.'))
      
      ### Individual (Women)
      IRdata <-  dataset_ls %>% 
        str_subset(pattern = paste0(i,"IR")) %>% 
        paste0(datawd,.) %>% 
        read_dta %>% 
        mutate(wt = v005/1000000,
               hhid = paste(v001, v002, sep='.'))
      
      ### Check availability of sparse modules
      no_DV <- is_empty(IRdata$d105a)
      no_FG <- ifelse(is_empty(IRdata$g100) == T, TRUE, is_empty(MRdata$mg100))
      
      # Indicators --------------------------------------------------------------
      '
     Much indicator code taken from: https://github.com/DHSProgram/DHS-Indicators-R
    '
      
      ### Multidimensional poverty
      
      # source(paste0(codewd,"/multi_poverty.R"))
      
      ### Vaccination coverage
      
      source(paste0(codewd,"/child_health.R"), local = T)
      
      ### Healthcare access
      
      source(paste0(codewd,"/healthcare_access.R"), local = T)
      
      ### Women empowerment
      
      source(paste0(codewd,"/women_empowerment.R"), local = T)
      
      ### Antenatal care
      
      source(paste0(codewd,"/antenatal_care.R"), local = T)
      
      ### Domestic violence
      
      if(no_DV){
        DVdata <- data.frame(caseid = unique(IRdata$caseid))
      } else{
        source(paste0(codewd,"/domestic_violence.R"), local = T)
      }
      
      ### Breastfeeding
      
      source(paste0(codewd,"/breastfeeding.R"), local = T)
      
      ### Contraceptive Knowledge
      
      source(paste0(codewd,"/contraceptive_knowledge.R"), local = T)
      
      ### Contraceptive preferences
      
      source(paste0(codewd,"/contraceptive_preferences.R"), local = T)
      
      ### HIV knowledge
      
      source(paste0(codewd,"/hiv_knowledge.R"), local = T)
      
      ### HIV behaviour
      
      source(paste0(codewd,"/hiv_behaviour.R"), local = T)
      
      ### Female genital mutilation
      
      if(no_FG){
        FGdata <- data.frame(caseid = unique(IRdata$caseid))
      } else{
        source(paste0(codewd,"/genital_mutilation.R"), local = T)
      }
      
      
      
      # Aggregate to geography of interest --------------------------------------
      
      ACdata <- ACdata %>% 
        mutate(across(c(rh_anc_pv, rh_anc_numvs, rh_anc_moprg), ~as.character(as_factor(.x))),
               rh_anc_pv = ifelse(rh_anc_pv == 'No ANC', 0, 
                                     ifelse(is.na(rh_anc_pv), NA, 1)),
               rh_anc_numvs = ifelse(rh_anc_numvs == '2-3' | rh_anc_numvs == '4+', 1, 
                                     ifelse(rh_anc_numvs == '1', 0, NA)),
               rh_anc_moprg = ifelse(rh_anc_moprg == '<4', 0, 
                                         ifelse(rh_anc_moprg == '4-5' | rh_anc_moprg == '6-7' | rh_anc_moprg == '8+', 1, NA))) %>% 
        select(caseid, rh_anc_pv:rh_anc_moprg)
      
      BFdata <- BFdata %>% 
        select(caseid, nt_bf_ever:nt_bottle)
      
      CHdata <- CHdata %>% 
        select(caseid, ch_allvac_either, ch_novac_either) %>% 
        distinct(caseid, .keep_all = T)
      
      CKdata <- CKdata %>% 
        select(caseid, fp_know_any:fp_know_trad) %>% 
        bind_rows(
          CKmdata %>% 
            rename_with(., ~ sub(".", "", .x), .cols = c(mcaseid)) %>% 
            select(caseid, fp_know_any:fp_know_trad)
        )
      
      
      CPdata <- CPdata %>% 
        select(caseid, 
               fp_use_mod:fp_message_noneof3) %>% 
        bind_rows(
          CPmdata %>% 
            rename_with(., ~ sub(".", "", .x), .cols = c(mcaseid)) %>% 
            select(caseid, 
                   fp_use_mod:fp_message_noneof3)
        )
      
      if(!no_DV){
        DVdata <- DVdata %>% 
          select(caseid, 
                 dv_sex_12m, dv_phy_12m)
      }
      
      HAdata <- HAdata %>% 
        select(caseid, 
               rh_prob_permit:rh_prob_minone)
      
      WEdata <- WEdata %>% 
        select(caseid, we_decide_all:we_justify_cond, we_num_justifydv) %>% 
        bind_rows(
          WEmdata %>%
            rename_with(., ~ sub(".", "", .x), .cols = c(mcaseid)) %>%
            select(caseid, we_decide_all:we_justify_cond, we_num_justifydv)
        ) %>%
        mutate(across(we_num_justifydv, ~as.character(as_factor(.x))),
               we_num_justifydv = ifelse(we_num_justifydv == '0', 0, 
                                         ifelse(is.na(we_num_justifydv), NA, 1)))
        
      HBdata <- HBdata %>% 
        select(caseid, hk_sex_2plus:hk_test_ever) %>% 
        bind_rows(
          HBmdata %>% 
            rename_with(., ~ sub(".", "", .x), .cols = c(mcaseid)) %>% 
            select(caseid,  hk_sex_2plus:hk_test_ever)
        )
      
      HKdata <- HKdata %>% 
        select(caseid, hk_ever_heard:hk_atd_shop_notbuy) %>% 
        bind_rows(
          HKmdata %>% 
            rename_with(., ~ sub(".", "", .x), .cols = c(mcaseid)) %>% 
            select(caseid, hk_ever_heard:hk_atd_shop_notbuy)
        )
      
      if(!no_FG){
        FGdata <- FGdata %>% 
        select(caseid, fg_relig:fg_cont) %>% 
        bind_rows(
          FGmdata %>% 
            rename_with(., ~ sub(".", "", .x), .cols = c(mcaseid)) %>% 
            select(caseid, fg_relig:fg_cont)
        )
      }
      
      control_vars <- c('v021', # PSU
                        'v024', # region
                        'v104', # gender
                        'hid', # household id
                        'vidx', # line number
                        'v243a', # mobile phone ownership
                        'v270') # wealth index in quintiles
      
      temp <- IRdata %>% 
        select(caseid,
               wt,
               v000,
               v001,
               v012,
               v025,
               v106,
               hid = hhid,
               v003) %>% 
        bind_rows(
          MRdata %>% 
        rename_with(., ~ sub(".", "", .x), .cols = c(mcaseid, mv000:mv190)) %>% 
        select(caseid,
               wt,
               v000,
               v001,
               v012,
               v025,
               v106,
               hid = hhid,
               v003)) %>% 
        left_join(PRdata %>% 
                    select(hhid, paste0("h", control_vars)) %>% 
                    rename_with(., ~ sub(".", "", .x)) %>% 
                    rename(v003 = vidx),
                  by = c('hid', 'v003')) %>% 
        set_value_labels(v270 = c("poorest" = 1, # Some surveys have messed up labelling
                                  "poorer" = 2,
                                  "middle" = 3, 
                                  "richer" = 4,
                                  "richest" = 5)) %>% 
        mutate(#v012 = as.integer(as_factor(v012)),
               v012 = replace(v012, v012 == 98, NA),
               v012 = cut(v012, 
                          breaks=c(0, 15, 20, 25, 30, 35, 40, 45, 50, Inf),
                          include.lowest = TRUE, right = FALSE),
               across(where(is.labelled), as_factor),
               across(where(is.factor), as.character),
               # across(where(is.numeric), as.integer),
               v106 = case_when(
                 v106 %in% c('secondary', 'higher') ~ 'secondary_or_higher',
                 v106 %in% c('no education', 'primary') ~ 'primary_or_no',
                 is.na(v106) ~ NA),
               v104 = replace(v104, v104 == 'Male', 'male'),
               v104 = replace(v104, v104 == 'Female', 'female'),
               v243a = replace(v243a, v243a == 'Yes', 'yes'),
               v243a = replace(v243a, v243a == 'No', 'no'),
               v243a = replace(v243a, v243a == '9', NA),
               v025 = replace(v025, v025 == 'Rural', 'rural'),
               v025 = replace(v025, v025 == 'Urban', 'urban'),
               strata = paste0(v024, '_', v025)) %>% 
        left_join(ACdata, by = 'caseid') %>% 
        # left_join(BFdata, by = 'caseid') %>% 
        left_join(CHdata, by = 'caseid') %>% 
        left_join(CKdata, by = 'caseid') %>% 
        left_join(CPdata, by = 'caseid') %>% 
        left_join(DVdata, by = 'caseid') %>% 
        left_join(FGdata, by = 'caseid') %>% 
        left_join(HAdata, by = 'caseid') %>% 
        left_join(HBdata, by = 'caseid') %>% 
        left_join(HKdata, by = 'caseid') %>% 
        left_join(WEdata, by = 'caseid') %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        mutate(pid = paste0(substr(v000, 1, 2), caseid)) %>% 
        select(-caseid, -v000, -v024, -geometry)
      
      # temp <- IRdata %>% 
      #   select(caseid, 
      #          wt, # survey weight
      #          v000, # country code
      #          v001, # cluster id
      #          v013, # age group
      #          v025, # urban/rural
      #          v106, # highest educational level
      #          v130, # religion
      #          v169a, # mobile phone ownership
      #          v190) %>%   # wealth index in quintiles
      #   mutate(female = 1) %>%
      #   bind_rows(
      #     MRdata %>% 
      #       rename_with(., ~ sub(".", "", .x), .cols = c(mcaseid, mv000:mv190)) %>% 
      #       select(caseid, 
      #              wt, # survey weight
      #              v000, # country code
      #              v001, # cluster id
      #              v013, # age group
      #              v025, # urban/rural
      #              v106, # highest educational level
      #              v130, # religion
      #              v169a, # mobile phone ownership
      #              v190) %>%   # wealth index in quintiles
      #       mutate(female = 0)
      #   ) %>% 
      #   # left_join(BFdata, by = 'caseid') %>% 
      #   left_join(CHdata, by = 'caseid') %>% 
      #   left_join(CKdata, by = 'caseid') %>% 
      #   left_join(CPdata, by = 'caseid') %>% 
      #   left_join(DVdata, by = 'caseid') %>% 
      #   left_join(FGdata, by = 'caseid') %>% 
      #   left_join(HAdata, by = 'caseid') %>% 
      #   left_join(HBdata, by = 'caseid') %>% 
      #   left_join(HKdata, by = 'caseid') %>% 
      #   left_join(WEdata, by = 'caseid') %>% 
      #   left_join(GEOdata, 
      #             by = join_by(v001 == DHSCLUST)) %>% 
      #   mutate(pid = paste0(substr(v000, 1, 2), caseid)) %>% 
      #   select(-caseid, -v000, -v001, -geometry)

    }
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
  
  if(exists('temp')){
    
    names(temp) <- tolower(names(temp))
    
    dhs_health <- dhs_health %>% 
      bind_rows(temp  %>% 
                  mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))
      )
    
    rm(temp)
    
  }
  
}

# Clean up final dataset --------------------------------------------------

write.csv(dhs_health,
          file='/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/dhs_health_individual.csv', 
          row.names=FALSE)
