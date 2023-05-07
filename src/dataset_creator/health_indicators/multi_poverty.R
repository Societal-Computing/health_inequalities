

# Health
### Indicator 1: Child mortality - Deprived if any child has died in the family
if(MIS == FALSE){
  ind1 <- BRdata %>% 
    mutate(
      across(where(is.character), ~ na_if(.x, "97")),
      across(where(is.character), ~ na_if(.x, "98")),
      across(where(is.character), ~ na_if(.x, "99")),
      across(where(is.numeric), ~ na_if(.x, 97)),
      across(where(is.numeric), ~ na_if(.x, 98)),
      across(where(is.numeric), ~ na_if(.x, 99))) %>% 
    select(hhid, v206, v207) %>% 
    drop_na %>% 
    mutate(n.child=ifelse(v206>0 | v207>0, 1, 0)) %>% # At least one child has died
    group_by(hhid) %>% 
    summarise(n.child.hh=sum(n.child, na.rm = T)) %>% 
    mutate(ind1=factor(x = ifelse(n.child.hh>0, 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
    select(hhid, ind1)
} else{
  ind1 <- data.frame(hhid = 'hhid')
}


### Indicator 2: Nutrition - Deprived if any adult or child, for whom there is nutritional information, is underweight
### Defined as BMI below 18.5 for individuals aged 20 and above (http://hdr.undp.org/en/mpi-2019-faq)
if(MIS == FALSE){
  ind2 <- PRdata %>% 
    mutate(
      across(where(is.character), ~ na_if(.x, "97")),
      across(where(is.character), ~ na_if(.x, "98")),
      across(where(is.character), ~ na_if(.x, "99")),
      across(where(is.numeric), ~ na_if(.x, 97)),
      across(where(is.numeric), ~ na_if(.x, 98)),
      across(where(is.numeric), ~ na_if(.x, 99))) %>% 
    select(hhid, hv105, hc2, hc3) %>% 
    drop_na %>% 
    mutate(bmi = (hc2/10)/((hc3/1000)**2)*100,
      n.malnourished = ifelse(hv105 > 19 & bmi < 1850, 1, 0)) %>% 
    group_by(hhid) %>% 
    summarise(n.malnourished.hh = sum(n.malnourished, na.rm = T)) %>% 
    mutate(ind2 = factor(x = ifelse(n.malnourished.hh > 0, 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
    select(hhid, ind2)
} else{
  ind2 <- data.frame(hhid = 'hhid')
}


# Education
### Indicator 3: Years of schooling - Deprived if no household member has completed six years of schooling
# remaining indicators which can be extracted from the DHS data
if(MIS == FALSE){
  ind3 <- HRdata %>% 
    mutate(
      across(where(is.character), ~ na_if(.x, "97")),
      across(where(is.character), ~ na_if(.x, "98")),
      across(where(is.character), ~ na_if(.x, "99")),
      across(where(is.numeric), ~ na_if(.x, 97)),
      across(where(is.numeric), ~ na_if(.x, 98)),
      across(where(is.numeric), ~ na_if(.x, 99))) %>% 
    select(hhid, contains("hv108_")) %>% 
    mutate(ind3.sum = rowSums(across(hv108_01:ncol(.)), na.rm = T),
           ind3 = factor(x = ifelse(ind3.sum == 0, 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
    select(hhid, ind3) %>% 
    distinct(hhid, .keep_all = TRUE)
} else{
  ind3 <- data.frame(hhid = 'hhid')
}

### Indicator 4: School attendance - Deprived if any school-aged child is not attending school up to class 8
if(MIS == FALSE){
  ind4 <- PRdata %>% 
    mutate(
      across(where(is.character), ~ na_if(.x, "97")),
      across(where(is.character), ~ na_if(.x, "98")),
      across(where(is.character), ~ na_if(.x, "99")),
      across(where(is.numeric), ~ na_if(.x, 97)),
      across(where(is.numeric), ~ na_if(.x, 98)),
      across(where(is.numeric), ~ na_if(.x, 99))) %>% 
    select(hhid, hv129, hv105, hv124) %>% 
    drop_na(any_of(c('hv105', 'hv124'))) %>% 
    filter((hv105 >= 7) & (hv105 <= 16)) %>% 
    mutate(
      ind4 = case_when(
        (hv129 == 0 & hv105 >= 7)
        | (hv105 == 9 & hv124  <= 1)
        | (hv105 == 10 & hv124 <= 2)
        | (hv105 == 11 & hv124 <= 3)
        | (hv105 == 12 & hv124 <= 4)
        | (hv105 == 13 & hv124 <= 5)
        | (hv105 == 14 & hv124 <= 6)
        | (hv105 == 15 & hv124 <= 7)
        | (hv105 == 16 & hv124 <= 8)
        ~ 1,
        TRUE ~ 0)
    ) %>% 
    group_by(hhid) %>% 
    summarize(ind4 = factor(x = max(ind4, na.rm = T), levels = c(0,1), labels = c('not_deprived', 'deprived')))
} else{
  ind4 <- data.frame(hhid = 'hhid')
}

# Living Standards
### Indicator 5: Cooking fuel - Deprived if the household cooks with dung, wood or charcoal
ind5 <- HRdata %>% 
  mutate(
    across(where(is.character), ~ na_if(.x, "97")),
    across(where(is.character), ~ na_if(.x, "98")),
    across(where(is.character), ~ na_if(.x, "99")),
    across(where(is.numeric), ~ na_if(.x, 97)),
    across(where(is.numeric), ~ na_if(.x, 98)),
    across(where(is.numeric), ~ na_if(.x, 99))) %>% 
  select(hhid, hv226) %>% 
  drop_na %>% 
  mutate(ind5 = factor(x = ifelse(hv226 %in% c(7, 8, 9, 10, 11), 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind5) %>% 
  distinct(hhid, .keep_all = TRUE) 

### Indicator 6: Sanitation - Deprived if the household's sanitation facility is not improved (according to MDG guidelines), or it is improved but shared with other households
ind6 <- HRdata %>% 
  mutate(
    across(where(is.character), ~ na_if(.x, "97")),
    across(where(is.character), ~ na_if(.x, "98")),
    across(where(is.character), ~ na_if(.x, "99")),
    across(where(is.numeric), ~ na_if(.x, 97)),
    across(where(is.numeric), ~ na_if(.x, 98)),
    across(where(is.numeric), ~ na_if(.x, 99))) %>% 
  select(hhid, hv205, hv225) %>% 
  drop_na %>% 
  mutate(ind6 = factor(x = ifelse((hv205 %in% c(23, 31, 41, 42, 43) | hv225 == 1), 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind6) %>% 
  distinct(hhid, .keep_all = TRUE)

### Indicator 7: Drinking Water - Deprived if the household does not have access to safe drinking water (according to MDG guidelines) or safe drinking water is more than a 30-minute walk from home roundtrip
ind7 <- HRdata %>% 
  mutate(
    across(where(is.character), ~ na_if(.x, "99")),
    across(where(is.character), ~ na_if(.x, "999")),
    across(where(is.numeric), ~ na_if(.x, 99)),
    across(where(is.numeric), ~ na_if(.x, 999))) %>% 
  select(hhid, hv201, hv204) %>% 
  drop_na %>% 
  mutate(ind7 = factor(x = ifelse((hv201 %in% c(32, 40, 42, 43, 51, 61, 62, 71)), 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% # | v204 %>% replace(996,0) %>% replace_na(0) > 30
  select(hhid, ind7) %>% 
  distinct(hhid, .keep_all = TRUE) 

### Indicator 8: Electricity - Deprived if the household has no electricity
ind8 <- HRdata %>% 
  mutate(
    across(where(is.character), ~ na_if(.x, "9")),
    across(where(is.numeric), ~ na_if(.x, 9))) %>% 
  select(hhid, hv206) %>% 
  drop_na %>% 
  mutate(ind8 = factor(x = ifelse(hv206 == 0, 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind8) %>% 
  distinct(hhid, .keep_all = TRUE) 

### Indicator 9: Housing - Deprived if the household has a dirt floor
ind9 <- HRdata %>% 
  mutate(
    across(where(is.character), ~ na_if(.x, "99")),
    across(where(is.numeric), ~ na_if(.x, 99))) %>% 
  select(hhid, hv213) %>% 
  drop_na %>% 
  mutate(ind9 = factor(x = ifelse(hv213 %in% c(11, 12), 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind9) %>% 
  distinct(hhid, .keep_all = TRUE) 

### Indicator 10: Assets - Deprived if the household does not own more than one of these assets: radio, TV, telephone, computer, Animal cart, bicycle, motorbike or refrigerator and does not own a car or truck
ind10 <- HRdata %>% 
  mutate(
    across(where(is.character), ~ na_if(.x, "9")),
    across(where(is.numeric), ~ na_if(.x, 9))) %>% 
  select(hhid, hv207, hv208, hv209, hv210, hv211, hv221, hv212) %>% 
  drop_na %>% 
  mutate(ind10 = factor(x = ifelse(
    ((hv207+hv208+hv209+hv210+hv211+hv221) == 0 & hv212 == 0), 1, 0), levels = c(0,1), labels = c('not_deprived', 'deprived'))) %>% 
  select(hhid, ind10) %>% 
  distinct(hhid, .keep_all = TRUE)

# Other indicators --------------------------------------------------------

# Female-headed households (male 0, female 1)
hh_head <- 
  HRdata %>% 
  select(hhid, hv219) %>% 
  drop_na() %>% 
  transmute(hhid = hhid,
            hh_head = factor(x = (hv219 - 1), levels = c(0,1), labels = c('male', 'female'))) %>% 
  distinct()

# Age of head of household
age <- 
  HRdata %>% 
  select(hhid, hv220) %>% 
  drop_na() %>% 
  transmute(hhid = hhid,
            age_head = cut(hv220, c(seq(0, 100, 5), Inf), right = FALSE)) %>% 
  distinct()

# Urban status (rural 0, urban 1)
urban <- 
  HRdata %>% 
  select(hhid, hv025) %>% 
  drop_na() %>% 
  transmute(hhid = hhid,
            urban_head = factor(x = (hv025 - 1), levels = c(0,1), labels = c('urban', 'rural'))) %>%
  distinct()

# Marital status of head of household
marital <- 
  PRdata %>% 
  select(hhid, hv101, hv115) %>% 
  drop_na() %>% 
  filter(hv101 == 1) %>% 
  transmute(hhid = hhid,
            marital_head = as_factor(hv115)) %>%
  distinct()

# Educational attainment of head of household
if(MIS == FALSE){
  education <- 
    PRdata %>% 
    select(hhid, hv101, hv107) %>% #we could also use v106 here, less non-response
    drop_na() %>% 
    filter(hv101 == 1) %>% 
    transmute(hhid = hhid,
              education_head = as_factor(hv107)) %>%
    distinct()
} else{
  education <- data.frame(hhid = 'hhid')
}

# Number of children
children <- 
  PRdata %>% 
  select(hhid, hv105) %>% 
  drop_na() %>% 
  group_by(hhid) %>% 
  summarise(children_n = sum(ifelse(hv105 <= 16, 1, 0))) %>% 
  ungroup()

# Create area-level output -------------------------------------------

POVdata <- HRdata %>% 
  select(hhid, hv001, hv021, hv023, hv024, wt, hhmembers_n = hv009) %>% 
  mutate(hv024 = as_factor(hv024, levels = "labels")) %>% 
  distinct() %>% 
  left_join(ind1, by = 'hhid') %>% 
  left_join(ind2, by = 'hhid') %>% 
  left_join(ind3, by = 'hhid') %>% 
  left_join(ind4, by = 'hhid') %>% 
  left_join(ind5, by = 'hhid') %>% 
  left_join(ind6, by = 'hhid') %>% 
  left_join(ind7, by = 'hhid') %>% 
  left_join(ind8, by = 'hhid') %>% 
  left_join(ind9, by = 'hhid') %>% 
  left_join(ind10, by = 'hhid') %>% 
  left_join(hh_head, by = 'hhid') %>% 
  left_join(age, by = 'hhid') %>% 
  left_join(urban, by = 'hhid') %>% 
  left_join(marital, by = 'hhid') %>% 
  left_join(education, by = 'hhid') %>% 
  left_join(children, by = 'hhid')

if(MIS == FALSE){
  POVdata <- POVdata %>% 
    mutate(health = rowMeans(select(.,ind1, ind2) %>% 
                               transmute_if(is.factor, as.integer), 
                             na.rm = T) - 1,
           edu = rowMeans(select(.,ind3, ind4) %>% 
                            transmute_if(is.factor, as.integer), 
                          na.rm = T) - 1,
           liv = rowMeans(select(.,ind5, ind6, ind7, ind8, ind9, ind10) %>% 
                            transmute_if(is.factor, as.integer), 
                          na.rm = T) - 1) %>% 
    mutate_at(vars(health, edu, liv), list(~na_if(., NaN))) %>% 
    mutate(score = rowMeans(select(.,health, edu, liv), na.rm = T),
           h_ind = factor(x = ifelse(score >= 1/3, 1, 0), levels = c(0,1), labels = c('non-poor', 'poor')),
           v_ind = factor(x = ifelse((score < 1/3) & (score >= 1/5), 1, 0), levels = c(0,1), labels = c('no', 'yes')),
           s_ind = factor(x = ifelse(score >= 1/2, 1, 0), levels = c(0,1), labels = c('no', 'yes')))
} else{
  POVdata <- POVdata %>% 
    mutate(liv = rowMeans(select(.,ind5, ind6, ind7, ind8, ind9, ind10) %>% 
                            transmute_if(is.factor, as.integer), 
                          na.rm = T) - 1) %>% 
    mutate_at(vars(liv), list(~na_if(., NaN))) %>% 
    mutate(score = rowMeans(select(.,liv), na.rm = T),
           h_ind = factor(x = ifelse(score >= 1/3, 1, 0), levels = c(0,1), labels = c('non-poor', 'poor')),
           v_ind = factor(x = ifelse((score < 1/3) & (score >= 1/5), 1, 0), levels = c(0,1), labels = c('no', 'yes')),
           s_ind = factor(x = ifelse(score >= 1/2, 1, 0), levels = c(0,1), labels = c('no', 'yes')))
}

rm(ind1, ind2, ind3, ind4, ind5, ind6, ind7, ind8, ind9, ind10, 
   age, children, education, hh_head, urban, marital)
