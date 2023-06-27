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
      no_FG <- is_empty(MRdata$mg100)
      
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
      
      ### HIV knowledge
      
      source(paste0(codewd,"/hiv_knowledge.R"), local = T)
      
      ### HIV behaviour
      
      source(paste0(codewd,"/hiv_behaviour.R"), local = T)
      
      ### Female genital mutilation
      
      if(no_FG){
        FGdata <- data.frame(GID_1 = unique(GEOdata$GID_1))
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
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(GID_1, 
               wt,
               rh_anc_pv:rh_anc_moprg) %>% 
        group_by(GID_1) %>% 
        summarise(across(rh_anc_pv:rh_anc_moprg,  ~ weighted.mean(.x, wt = wt, na.rm = TRUE))
        ) %>% 
        ungroup
      
      BFdata <- BFdata %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(GID_1, 
               wt,nt_bf_ever:nt_bottle) %>% 
        group_by(GID_1) %>% 
        summarise(across(nt_bf_ever:nt_bottle,  ~ weighted.mean(.x, wt = wt, na.rm = TRUE))
        ) %>% 
        ungroup
      
      CHdata <- CHdata %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(GID_1, 
               wt,
               ch_allvac_either, ch_novac_either) %>% 
        group_by(GID_1) %>% 
        summarise(across(ch_allvac_either:ch_novac_either,  ~ weighted.mean(.x, wt = wt, na.rm = TRUE))
        ) %>% 
        ungroup
      
      CKdata <- CKdata %>% 
        select(v001, 
               fp_know_any:fp_know_trad) %>% 
        bind_rows(
          CKmdata %>% 
            rename_with(., ~ sub(".", "", .x), .cols = c(mv000:mv190)) %>% 
            select(v001, 
                   fp_know_any:fp_know_trad)
        ) %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(-v001, -geometry) %>% 
        group_by(GID_1) %>% 
        summarise(across(fp_know_any:fp_know_trad,  ~ weighted.mean(.x, wt = wt, na.rm = TRUE))
        ) %>% 
        ungroup
      
      
      CPdata <- CPdata %>% 
        select(v001, 
               fp_message_noneof4:fp_message_noneof3) %>% 
        bind_rows(
          CPmdata %>% 
            rename_with(., ~ sub(".", "", .x), .cols = c(mv000:mv190)) %>% 
            select(v001, 
                   fp_message_noneof4:fp_message_noneof3)
        ) %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(-v001, -geometry) %>% 
        group_by(GID_1) %>% 
        summarise(across(fp_message_noneof4:fp_message_noneof3,  ~ weighted.mean(.x, wt = wt, na.rm = TRUE))
        ) %>% 
        ungroup
      
      if(!no_DV){
        DVdata <- DVdata %>% 
          left_join(GEOdata, 
                    by = join_by(v001 == DHSCLUST)) %>% 
          select(GID_1, 
                 dv_sex_12m, dv_phy_12m) %>% 
          group_by(GID_1) %>% 
          summarise(across(dv_sex_12m:dv_phy_12m,  ~ weighted.mean(.x, wt = wt, na.rm = TRUE))
          ) %>% 
          ungroup
      }
      
      HAdata <- HAdata %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(GID_1, 
               rh_prob_permit:rh_prob_minone) %>% 
        group_by(GID_1) %>% 
        summarise(across(rh_prob_permit:rh_prob_minone,  ~ weighted.mean(.x, wt = wt, na.rm = TRUE))
        ) %>% 
        ungroup
      
      WEdata <- WEdata %>% 
        select(v001, 
               we_decide_all:we_justify_cond,
               we_num_justifydv) %>% 
        bind_rows(
          WEmdata %>%
            rename_with(., ~ sub(".", "", .x), .cols = c(mv000:mv190)) %>%
            select(v001,
                   we_decide_all:we_justify_cond,
                   we_num_justifydv)
        ) %>%
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(-v001, -geometry) %>% 
        mutate(across(we_num_justifydv, ~as.character(as_factor(.x))),
               we_num_justifydv = ifelse(we_num_justifydv == '0', 0, 
                                         ifelse(is.na(we_num_justifydv), NA, 1))) %>% 
        group_by(GID_1) %>% 
        summarise(across(c(we_decide_all:we_justify_cond, we_num_justifydv),
                         ~ weighted.mean(.x, wt = wt, na.rm = TRUE))
        ) %>% 
        ungroup
        
      HBdata <- HBdata %>% 
        select(v001, 
               hk_sex_2plus:hk_sexprtnr_num) %>% 
        bind_rows(
          HBmdata %>% 
            rename_with(., ~ sub(".", "", .x), .cols = c(mv000:mv190)) %>% 
            select(v001, 
                   hk_sex_2plus:hk_sexprtnr_num)
        ) %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(-v001, -geometry) %>% 
        group_by(GID_1) %>% 
        summarise(across(hk_sex_2plus:hk_sexprtnr_num,  ~ weighted.mean(.x, wt = wt, na.rm = TRUE))
        ) %>% 
        ungroup
      
      HKdata <- HKdata %>% 
        select(v001, 
               hk_ever_heard:hk_atd_shop_notbuy) %>% 
        bind_rows(
          HKmdata %>% 
            rename_with(., ~ sub(".", "", .x), .cols = c(mv000:mv190)) %>% 
            select(v001, 
                   hk_ever_heard:hk_atd_shop_notbuy)
        ) %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(-v001, -geometry) %>% 
        group_by(GID_1) %>% 
        summarise(across(hk_ever_heard:hk_atd_shop_notbuy,  ~ weighted.mean(.x, wt = wt, na.rm = TRUE))
        ) %>% 
        ungroup
      
      if(!no_FG){
        FGdata <- FGdata %>% 
        select(v001, 
               fg_relig:fg_cont) %>% 
        bind_rows(
          FGmdata %>% 
            rename_with(., ~ sub(".", "", .x), .cols = c(mv000:mv190)) %>% 
            select(v001, 
                   fg_relig:fg_cont)
        ) %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(-v001, -geometry) %>% 
        group_by(GID_1) %>% 
        summarise(across(fg_relig:fg_cont,  ~ weighted.mean(.x, wt = wt, na.rm = TRUE))
        ) %>% 
        ungroup
      }
      
      temp <- ACdata %>% 
        left_join(BFdata, by = 'GID_1') %>% 
        left_join(CHdata, by = 'GID_1') %>% 
        left_join(CKdata, by = 'GID_1') %>% 
        left_join(CPdata, by = 'GID_1') %>% 
        left_join(DVdata, by = 'GID_1') %>% 
        left_join(FGdata, by = 'GID_1') %>% 
        left_join(HAdata, by = 'GID_1') %>% 
        left_join(HBdata, by = 'GID_1') %>% 
        left_join(HKdata, by = 'GID_1') %>% 
        left_join(WEdata, by = 'GID_1')
      
      # Add control variables ---------------------------------------------------
      
      # control_vars <- c('sex', # gender #hv104
      #                   'v001', # cluster #hv001
      #                   'v013', # age group #hv105
      #                   'v025', # urban/rural #hv025
      #                   'v045b', # interview language #hv045b
      #                   'v106', # highest educational level #hv122
      #                   'v130', # religion
      #                   'v169a', # mobile phone ownership #hv243a
      #                   'v190') # wealth index in quintiles #hv270
      
      control_vars <- c('v104', # gender #hv104
                        'v001', # cluster #hv001
                        'v105', # age #hv105
                        'v025', # urban/rural #hv025
                        # 'hv045b', # interview language #hv045b
                        'v122', # highest educational level #hv122
                        # 'v130', # religion
                        'v243a', # mobile phone ownership #hv243a
                        'v270') # wealth index in quintiles #hv270
      
      # if(is.null(IRdata$v045b)){
      #   
      #   IRdata <- IRdata %>% 
      #     mutate(v045b = NA) 
      #   
      #   if(!is.null(IRdata$sinterv)){
      #     IRdata <- IRdata %>% 
      #       mutate(v045b = sinterv) 
      #   }
      #   if(!is.null(IRdata$slangint)){
      #     IRdata <- IRdata %>% 
      #       mutate(v045b = slangint) 
      #   }
      #   if(!is.null(IRdata$slang)){
      #     IRdata <- IRdata %>% 
      #       mutate(v045b = slang) 
      #   }
      #   if(!is.null(IRdata$slinterview)){
      #     IRdata <- IRdata %>% 
      #       mutate(v045b = slinterview)
      #   }
      # }
      # 
      # if(is.null(MRdata$mv045b)){
      #   
      #   MRdata <- MRdata %>% 
      #     mutate(mv045b = NA) 
      #   
      #   if(!is.null(MRdata$sminterv)){
      #     MRdata <- MRdata %>% 
      #       mutate(mv045b = sminterv) 
      #   }
      #   if(!is.null(IRdata$smlangint)){
      #     MRdata <- MRdata %>% 
      #       mutate(mv045b = smlangint) 
      #   }
      #   if(!is.null(MRdata$smlang)){
      #     MRdata <- MRdata %>% 
      #       mutate(mv045b = smlang) 
      #   }
      #   if(!is.null(MRdata$smlinterview)){
      #     MRdata <- MRdata %>% 
      #       mutate(mv045b = smlinterview) 
      #   }
      # }
      
      Control <- PRdata %>% 
        select(paste0("h", control_vars)) %>% 
        rename_with(., ~ sub(".", "", .x)) %>%
        set_value_labels(v270 = c("poorest" = 1, # Some surveys have messed up labelling
                                  "poorer" = 2,
                                  "middle" = 3, 
                                  "richer" = 4,
                                  "richest" = 5)) %>% 
        mutate(v105 = as.integer(as_factor(v105)),
               v105 = replace(v105, v105 == 98, NA),
               v105 = cut(v105, 
                           breaks=c(0, 15, 20, 25, 30, 35, 40, 45, 50, Inf),
                           include.lowest = FALSE, right = TRUE),
               across(where(is.labelled), as_factor),
               across(where(is.factor), as.character),
               across(where(is.numeric), as.integer),
               v122 = case_when(
                 v122 %in% c('secondary', 'higher') ~ 'secondary_or_higher',
                 v122 %in% c('no education', 'primary') ~ 'primary_or_no',
                 is.na(v122) ~ NA)
               ) %>% 
        left_join(GEOdata, 
                  by = join_by(v001 == DHSCLUST)) %>% 
        select(-v001, -geometry)
        
      
      # Control <- IRdata %>% 
      #   mutate(sex = 'female') %>% 
      #   select(all_of(control_vars)) %>% 
      #   bind_rows(
      #     MRdata %>% 
      #       select(paste0("m", control_vars[!control_vars == 'sex'])) %>% 
      #       rename_with(., ~ sub(".", "", .x)) %>% 
      #       mutate(sex = 'male') %>% 
      #       select(all_of(control_vars))
      #   ) %>% 
      #   set_value_labels(v190 = c("poorest" = 1, # Some surveys have messed up labelling
      #                             "poorer" = 2,
      #                             "middle" = 3, 
      #                             "richer" = 4,
      #                             "richest" = 5)) %>%
      #   mutate(across(control_vars[!control_vars == 'v001'], as_factor),
      #          across(where(is.factor), as.character)
      #   ) %>% 
      #   mutate(maj_lang = 'lang_minor', # determine majority language
      #          maj_lang = replace(maj_lang, 
      #                             is.na(v045b), 'lang_NA'),
      #          maj_lang = replace(maj_lang, 
      #                             v045b == names(which.max(table(v045b))), 'lang_major'),
      #          maj_rel = 'rel_minor', # determine majority religion
      #          maj_rel = replace(maj_rel, 
      #                            is.na(v130), 'rel_NA'),
      #          maj_rel = replace(maj_rel, 
      #                            v130 == names(which.max(table(v130))), 'rel_major'),
      #          v106 = case_when(
      #            v106 %in% c('secondary', 'higher') ~ 'secondary_or_higher',
      #            v106 %in% c('no education', 'primary') ~ 'primary_or_no',
      #            is.na(v106) ~ NA
      #          )) %>% 
      #   left_join(GEOdata, 
      #             by = join_by(v001 == DHSCLUST)) %>% 
      #   select(-v001, -v045b, -geometry)
      # 
      # control_vars <- c(control_vars[!control_vars == 'v045b'],
      #                   "maj_lang",
      #                   "maj_rel")
      
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
  rename(has_mobile_phone_yes = yes, has_mobile_phone_no = no) %>% 
  select(where(~sum(!is.na(.x)) > 0),
         -.data[['9']], -.data[['na']], -na.x, -na.y, -na.x.x, -na.y.y)


write.csv(dhs_health,
          file='/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/dhs_health.csv', 
          row.names=FALSE)
