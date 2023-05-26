library(haven)          # Load stata files
library(tidyverse)      # Method chaining
library(survey)         # Survey aggregation
library(naniar)         # replace_with_na function
library(labelled)       # Set variable labels
library(matrixStats)    # Set variable labels
library(sf)             # read shape files

options(dplyr.summarise.inform = FALSE)

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
  left_join(read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/combined_dataset/GADM_1_variables.csv") %>% 
              select(GID_1, HASC_1), by = 'GID_1') %>% 
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
      grepl('TCD', GID_1, fixed = TRUE),'CD', HASC_1), # fix ISO of Chad
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
        select(GID_1, geom)
      
      '
      We use st_nearest_feature instead of st_intersection as some cluster 
      locations (e.g. on islands, very close to the national border)
      might turn NA due to spatial inaccuracies.
      '
      
      GEOdata <- st_read(dsn = dataset_ls %>% 
                           str_subset(pattern = paste0(i,"GE")) %>% 
                           paste0(datawd,.)) %>% 
        mutate(nearest_index = st_nearest_feature(.,GEOdata),
               GID_1 = paste0(substr(GEOdata$GID_1, 1, 3), nearest_index)) %>% 
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
        DVdata <- data.frame(GID_1 = unique(GEOdata$GID_1))
      } else{
        source(paste0(codewd,"/domestic_violence.R"), local = T)
      }
      
      ### Breastfeeding
      
      source(paste0(codewd,"/breastfeeding.R"), local = T)
      
      ### Contraceptive Knowledge
      
      source(paste0(codewd,"/contraceptive_knowledge.R"), local = T)
      
      ### Contraceptive preferences
      
      source(paste0(codewd,"/contraceptive_preferences.R"), local = T)
      
      
      # Aggregate to geography of interest --------------------------------------
      
      ACdata <- ACdata %>% 
        mutate(across(v013:v190, as_factor),
               across(where(is.factor), as.character),
               sex = 'female') %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(GID_1, 
               sex, 
               v169a, # owns mobile phone
               rh_anc_pv:rh_anc_moprg) %>% 
        pivot_wider(
          names_from = c(sex, v169a),
          names_sep = "_",
          values_from = c(rh_anc_pv:rh_anc_moprg),
          values_fn = ~ weighted.mean(.x, wt = wt, na.rm = TRUE)
        )
      
      BFdata <- BFdata %>% 
        mutate(across(v013:v190, as_factor),
               across(where(is.factor), as.character)) %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(GID_1, 
               # v013, # age groups
               v169a, # owns mobile phone
               # v171b, # frequency of internet use
               # v190, # wealth index quintile
               nt_bf_ever:nt_bottle) %>% 
        pivot_wider(
          names_from = c(v169a),
          names_sep = "_",
          values_from = c(nt_bf_ever:nt_bottle),
          values_fn = ~ weighted.mean(.x, wt = wt, na.rm = TRUE)
        )
      
      CHdata <- CHdata %>% 
        mutate(across(b4:v190, as_factor),
               across(where(is.factor), as.character)) %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(GID_1, 
               # b4, # child's sex
               # b8, # child's age
               v169a, # mother owns mobile phone
               # v171b, # frequency of internet use
               # v190, # wealth index quintile
               ch_allvac_either, ch_novac_either) %>% 
        pivot_wider(
          names_from = c(v169a),
          names_sep = "_",
          values_from = c(ch_allvac_either, ch_novac_either),
          values_fn = ~ weighted.mean(.x, wt = wt, na.rm = TRUE)
        )
      
      CKdata <- CKdata %>% 
        mutate(across(v013:v190, as_factor),
               across(where(is.factor), as.character),
               sex = 'female') %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(GID_1, 
               sex, 
               # v013, # age groups
               v169a, # owns mobile phone
               # v171b, # frequency of internet use
               # v190, # wealth index quintile
               fp_know_any:fp_know_trad) %>% 
        pivot_wider(
          names_from = c(sex, v169a),
          names_sep = "_",
          values_from = c(fp_know_any:fp_know_trad),
          values_fn = ~ weighted.mean(.x, wt = wt, na.rm = TRUE)
        )
      
      CKmdata <- CKmdata %>% 
        mutate(across(mv013:mv190, as_factor),
               across(where(is.factor), as.character),
               sex = 'male') %>% 
        left_join(GEOdata, 
                  by = join_by(mv001 == DHSCLUST)) %>% 
        select(GID_1, 
               sex, 
               mv169a, # owns mobile phone
               fp_know_any:fp_know_trad) %>% 
        pivot_wider(
          names_from = c(sex, mv169a),
          names_sep = "_",
          values_from = c(fp_know_any:fp_know_trad),
          values_fn = ~ weighted.mean(.x, wt = wt, na.rm = TRUE)
        )
      
      CPdata <- CPdata %>% 
        mutate(across(v013:v190, as_factor),
               across(where(is.factor), as.character),
               sex = 'female') %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(GID_1, 
               sex, 
               v169a, # owns mobile phone
               fp_message_noneof4:fp_message_noneof3) %>% 
        pivot_wider(
          names_from = c(sex, v169a),
          names_sep = "_",
          values_from = c(fp_message_noneof4:fp_message_noneof3),
          values_fn = ~ weighted.mean(.x, wt = wt, na.rm = TRUE)
        )
      
      CPmdata <- CPmdata %>% 
        mutate(across(mv013:mv190, as_factor),
               across(where(is.factor), as.character),
               sex = 'male') %>% 
        left_join(GEOdata, 
                  by = join_by(mv001 == DHSCLUST)) %>% 
        select(GID_1, 
               sex, 
               mv169a, # owns mobile phone
               fp_message_noneof4:fp_message_noneof3) %>% 
        pivot_wider(
          names_from = c(sex, mv169a),
          names_sep = "_",
          values_from = c(fp_message_noneof4:fp_message_noneof3),
          values_fn = ~ weighted.mean(.x, wt = wt, na.rm = TRUE)
        )
      
      if(!no_DV){
        DVdata <- DVdata %>% 
          mutate(across(v013:v190, as_factor),
                 across(where(is.factor), as.character),
                 sex = 'female') %>% 
          left_join(GEOdata, 
                    by = join_by(v001 == DHSCLUST)) %>% 
          select(GID_1, 
                 sex, 
                 v169a, # owns mobile phone
                 dv_sex_12m, dv_phy_12m) %>% 
          pivot_wider(
            names_from = c(sex, v169a),
            names_sep = "_",
            values_from = c(dv_sex_12m, dv_phy_12m),
            values_fn = ~ weighted.mean(.x, wt = wt, na.rm = TRUE)
          )
      }
      
      HAdata <- HAdata %>% 
        mutate(across(v013:v190, as_factor),
               across(where(is.factor), as.character),
               sex = 'female') %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(GID_1, 
               sex, 
               v169a, # owns mobile phone
               rh_prob_permit:rh_prob_minone) %>% 
        pivot_wider(
          names_from = c(sex, v169a),
          names_sep = "_",
          values_from = c(rh_prob_permit:rh_prob_minone),
          values_fn = ~ weighted.mean(.x, wt = wt, na.rm = TRUE)
        )
      
      WEdata <- WEdata %>% 
        mutate(across(v013:v190, as_factor),
               across(where(is.factor), as.character),
               sex = 'female') %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(GID_1, 
               sex, 
               v169a, # owns mobile phone
               we_num_decide:we_num_justifydv) %>% 
        pivot_wider(
          names_from = c(sex, v169a),
          names_sep = "_",
          values_from = c(we_num_decide:we_num_justifydv),
          values_fn = ~ weighted.mean(.x, wt = wt, na.rm = TRUE)
        )
      
      WEmdata <- WEmdata %>% 
        mutate(across(mv013:mv190, as_factor),
               across(where(is.factor), as.character),
               sex = 'male') %>% 
        left_join(GEOdata, 
                  by = join_by(mv001 == DHSCLUST)) %>% 
        select(GID_1, 
               sex, 
               mv169a, # owns mobile phone
               we_num_decide:we_num_justifydv) %>% 
        pivot_wider(
          names_from = c(sex, mv169a),
          names_sep = "_",
          values_from = c(we_num_decide:we_num_justifydv),
          values_fn = ~ weighted.mean(.x, wt = wt, na.rm = TRUE)
        )
      
      temp <- ACdata %>% 
        left_join(BFdata, by = 'GID_1') %>% 
        left_join(CHdata, by = 'GID_1') %>% 
        left_join(CKdata, by = 'GID_1') %>% 
        left_join(CKmdata, by = 'GID_1') %>% 
        left_join(CPdata, by = 'GID_1') %>% 
        left_join(CPmdata, by = 'GID_1') %>% 
        left_join(DVdata, by = 'GID_1') %>% 
        left_join(HAdata, by = 'GID_1') %>% 
        left_join(WEdata, by = 'GID_1') %>%
        left_join(WEmdata, by = 'GID_1')
      
      # Add control variables ---------------------------------------------------
      
      control_vars <- c('sex', # gender,
                        'v001', # cluster
                        'v013', # age group
                        'v025',
                        'v045b', # interview language
                        'v130', # religion
                        'v169a', # mobile phone ownership
                        'v190') # wealth index in quintiles
      
      if(is.null(IRdata$v045b)){
        
        IRdata <- IRdata %>% 
          mutate(v045b = NA) 
        
        if(!is.null(IRdata$sinterv)){
          IRdata <- IRdata %>% 
            mutate(v045b = sinterv) 
        }
        if(!is.null(IRdata$slangint)){
          IRdata <- IRdata %>% 
            mutate(v045b = slangint) 
        }
        if(!is.null(IRdata$slang)){
          IRdata <- IRdata %>% 
            mutate(v045b = slang) 
        }
        if(!is.null(IRdata$slinterview)){
          IRdata <- IRdata %>% 
            mutate(v045b = slinterview)
        }
      }
      
      if(is.null(MRdata$mv045b)){
        
        MRdata <- MRdata %>% 
          mutate(mv045b = NA) 
        
        if(!is.null(MRdata$sminterv)){
          MRdata <- MRdata %>% 
            mutate(mv045b = sminterv) 
        }
        if(!is.null(IRdata$smlangint)){
          MRdata <- MRdata %>% 
            mutate(mv045b = smlangint) 
        }
        if(!is.null(MRdata$smlang)){
          MRdata <- MRdata %>% 
            mutate(mv045b = smlang) 
        }
        if(!is.null(MRdata$smlinterview)){
          MRdata <- MRdata %>% 
            mutate(mv045b = smlinterview) 
        }
      }
      
      Control <- IRdata %>% 
        mutate(sex = 'female') %>% 
        select(control_vars) %>% 
        bind_rows(
          MRdata %>% 
            select(paste0("m", control_vars[!control_vars == 'sex'])) %>% 
            rename_with(., ~ sub(".", "", .x)) %>% 
            mutate(sex = 'male') %>% 
            select(control_vars)
        ) %>% 
        set_value_labels(v190 = c("poorest" = 1, # Some surveys have messed up labelling
                                  "poorer" = 2,
                                  "middle" = 3, 
                                  "richer" = 4,
                                  "richest" = 5)) %>%
        mutate(across(control_vars[!control_vars == 'v001'], as_factor),
               across(where(is.factor), as.character)
               ) %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        mutate(maj_lang = 'lang_minor', # determine majority language
               maj_lang = replace(maj_lang, 
                                  is.na(v045b), 'lang_NA'),
               maj_lang = replace(maj_lang, 
                                  v045b == names(which.max(table(v045b))), 'lang_major')) %>% 
        mutate(maj_rel = 'rel_minor', # determine majority religion
               maj_rel = replace(maj_rel, 
                                  is.na(v130), 'rel_NA'),
               maj_rel = replace(maj_rel, 
                                 v130 == names(which.max(table(v130))), 'rel_major')) %>% 
        select(-v001, -v045b, -geometry)
      
      control_vars <- c(control_vars[!control_vars == 'v045b'],
                        "maj_lang",
                        "maj_rel")
      
      for(j in control_vars[!control_vars == 'v001']){
        temp <- Control %>% 
          group_by(.data[[j]], GID_1) %>%
          summarise(n = n()) %>%
          group_by(GID_1) %>%
          mutate(freq = n / sum(n, na.rm = T), n = sum(n, na.rm = T)) %>% 
          select(-n) %>% 
          pivot_wider(names_from = {{j}}, 
                      values_from = freq, 
                      values_fill = list(freq = 0)) %>%
          right_join(temp, by = 'GID_1')
      }
      
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

dhs_health <- 
  dhs_health %>% 
  rename(GID_1 = gid_1) %>% 
  filter(GID_1 != 'NA') %>% 
  rename(other_religion = other,
         na_religion = .data[['na']],
    has_mobile_phone_yes = yes, has_mobile_phone_no = no) %>% 
  select(where(~sum(!is.na(.x)) > 0),
         -.data[['9']], -.data[['99']])
  

write.csv(dhs_health,
          file='/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/dhs_health.csv', 
          row.names=FALSE)
