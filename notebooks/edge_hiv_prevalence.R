library(tidyverse)    # Data wrangling
library(sf)           # Spatial analysis
library(caret)        # For machine learning and preprocessing
library(lme4)         # For random effects models
library(sandwich)     # For clustered se
library(lmtest)     # For clustered se

library(sjPlot)
library(sjmisc)
library(stargazer)
library(scales)      # for rescale function

# Loading relevant data ---------------------------------------------------
# Map data
afr_dat <- st_read(dsn = "/Users/tillkoebe/Documents/GitHub/health_inequalities/combined_dataset/GADM_1_geometries.gpkg") %>% 
  st_make_valid() %>% 
  st_transform(4326)

# Social Connectedness Index
sci <- read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/sci_indices.csv") %>% 
  distinct(user_loc, .keep_all = T)

# Worldpop covariates
wp <- read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/wp.csv") %>% 
  distinct(GID_1, .keep_all = T)

# Human Development Index
hdi <- read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/hdi.csv") %>% 
  distinct(GID_1, .keep_all = T)

# Facebook covariates
fb <- read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/fb.csv") %>% 
  distinct(GID_1, .keep_all = T) %>%
  mutate(iso3 = substr(GID_1, 1, 3)) %>% 
  group_by(iso3) %>% 
  mutate(iso3_pntr_13p_all = mean(FB_pntr_13p_all, na.rm = T)) %>% 
  ungroup() %>% 
  filter(iso3_pntr_13p_all >= 0)

# DHS data
dhs_raw <- read.csv('/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/dhs_health_individual.csv') %>% 
  rename(GID_1 = gid_1) %>% 
  filter(GID_1 != 'NA')
# %>% 
#   filter(GID_1 != 'TCD1')
# %>%
#   filter(substr(GID_1, 1, 3) != 'TCD')

# Afrobarometer
afr <- read.csv('/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/afrobarometer.csv') %>% 
  distinct(GID_1, .keep_all = T)

# Raw SCI
sci_raw <- read.csv("/Users/tillkoebe/Documents/Data/SCI/sci_gadm1.tsv", sep = "\t") %>% 
  filter(user_loc %in% unique(dhs_raw$GID_1),
         fr_loc %in% unique(dhs_raw$GID_1))
# %>%
#   filter(scaled_sci > 1)

# Centrality measures
sci_cen <- read.csv('/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/all_centrality_measures.csv') %>% 
  rename(GID_1 = user_loc)


# Data preparation --------------------------------------------------------

### Get DHS on regional-level
dhs <- dhs_raw %>% 
  select(-hid, -strata, -v003, -v021, -pid) %>% 
  group_by(GID_1) %>% 
  summarise(across(where(is.numeric), ~ weighted.mean(.x, wt = wt, na.rm = TRUE))) %>% 
  ungroup

for(j in c('v012', 'v025', 'v104', 'v106', 'v243a', 'v270', 'hv_hiv_pos')){
  dhs <- dhs_raw %>% 
    mutate(hv_hiv_pos = replace(hv_hiv_pos, hv_hiv_pos == 1, 'hiv_pos'),
           hv_hiv_pos = replace(hv_hiv_pos, hv_hiv_pos == 0, 'hiv_neg')) %>% 
    drop_na(.data[[j]]) %>% 
    group_by(.data[[j]], GID_1) %>%
    summarise(n = sum(wt, na.rm = T)) %>%
    group_by(GID_1) %>%
    mutate(freq = n / sum(n, na.rm = T), n = sum(n, na.rm = T)) %>% 
    ungroup %>% 
    select(-n) %>% 
    pivot_wider(names_from = {{j}}, 
                values_from = freq, 
                values_fill = list(freq = 0)) %>%
    right_join(dhs, by = 'GID_1')
}

### Define targets
dhs_targets <- dhs %>% 
  select(fp_use_mod, hk_test_ever, fp_know_mod, 
         hk_knw_linear_index, hk_knw_any, hk_knw_comphsv) %>% 
  names()

### Define controls
dhs_controls <- c(
  'poorest',
  'poorer',
  'middle',
  'richer',
  'richest',
  'female',
  'male',
  'secondary_or_higher',
  'primary_or_no',
  'yes',
  'no',
  'rural',
  'urban',
  "[15,20)",
  "[20,25)",
  "[25,30)",
  "[30,35)",
  "[35,40)",
  "[40,45)",
  "[45,50)",
  "[50,Inf]",
  "hiv_pos"
)

sci_controls <- c('Mean_dist_to_SCI_km', # Average distance of friendships
                  'Median_dist_to_SCI_km', # Average distance of friendships
                  'Std_dist_to_SCI_km', # Clustering measure of friendships across distances
                  'Total_dist_to_SCI_km', # Total distance of friendships
                  'Ratio_selfloop_to_country', # Share of local friendships within the same country
                  'Ratio_selfloop_to_africa', # Share of local friendships that stay within Africa
                  'Ratio_selfloop_to_all_sci', # Share of local friendships of all friendships
                  'Average_distance_of_friendships_km', 
                  'Mean_SCI_without_Self', # Average probability of friendships with other regions
                  'Median_SCI_without_Self', # Average probability of friendships with other regions
                  'Std_SCI_without_Self', # Clustering measure for the probability of friendships with other regions
                  'Mean_SCI_with_Self', # Average probability of friendships across all regions
                  'Median_SCI_with_Self', # Average probability of friendships across all regions
                  'Std_SCI_with_Self', # Average probability of friendships across all regions
                  'Mean_friendship', # Average friendships per FB users
                  'Median_friendship', # Average friendships per FB users
                  'Std_friendship', # Clustering measure for friendships per FB user
                  'Total_friendship', # Sum of friendships per FB user 
                  'Ratio_SCI_low_hi_africa', # Share of friendships to regions with a low health index
                  'Ratio_SCI_middle_hi_africa', # Share of friendships to regions with a medium health index
                  'Ratio_SCI_high_hi_africa' # Share of friendships to regions with a high health index
)

hdi_controls <- c('HDI')

wp_controls <- c(
  'Mean_of_Night_Light',
  # 'Std_of_Night_Light',
  # 'Mean_distance_to_major_rd_intersection',
  # 'Std_distance_to_major_rd_intersection',
  'Mean_distance_to_major_rd',
  # 'Std_distance_to_major_rd',
  # "Mean_distance_to_inland_water",
  # "Std_distance_to_inland_water",
  "Mean_built_settlement_growth"
  # "Std_built_settlement_growth"
  )

fb_controls <- c('fb_rwi_mean',
                 'fb_rwi_mean_pop_wght',
                 'FB_pntr_15to49_female',
                 'FB_pntr_15to49_all')

afr_controls <- c('member_community',
                  'trpar',
                  'trtax',
                  'trgov',
                  'trlaw',
                  'trgen',
                  'trrel',
                  'trnei',
                  'tracq',
                  'railway_contact',
                  'explorer_contact',
                  'district_ethnic_frac',
                  'loc_ln_export_area')

### Prepare dataset
dat <- dhs %>% 
  select(GID_1,
         all_of(dhs_targets),
         all_of(dhs_controls)) %>% 
  left_join(sci %>% 
              select(GID_1 = user_loc,
                     all_of(sci_controls)),
            by = 'GID_1') %>% 
  left_join(hdi %>%
              select(GID_1,
                     all_of(hdi_controls)),
            by = 'GID_1') %>%
  left_join(wp %>%
              drop_na(GID_1) %>%
              select(GID_1,
                     all_of(wp_controls)),
            by = 'GID_1') %>%
  left_join(fb %>% 
              select(GID_1,
                     all_of(fb_controls)),
            by = 'GID_1') %>% 
  left_join(afr %>% 
              select(GID_1,
                     all_of(afr_controls)),
            by = 'GID_1') %>% 
  filter(hiv_pos < median(dhs$hiv_pos, na.rm = TRUE))  
# %>% 
#   mutate(fp_know_mod_cat = ntile(fp_know_mod, 4),
#          hk_knw_linear_index_cat = ntile(hk_knw_linear_index, 4))
# %>%
#   filter(hk_knw_linear_index_cat == 4)

# Preprocess --------------------------------------------------------------

### Center and scale SCI variables
dat <- dat %>% 
  mutate(across(all_of(sci_controls), ~ scale(.x, scale = T)),
         across(all_of(wp_controls), ~ scale(.x, scale = T)),
         across(all_of(afr_controls), ~ scale(.x, scale = T)))

# Get inter-regional differences ------------------------------------------

long <- sci_raw %>% 
  rename(GID_1 = user_loc) %>%
  # filter(GID_1 != fr_loc) %>%  # remove self-connections
  mutate(scaled_sci = (scaled_sci/1000000000))

for(i in c(dhs_targets, dhs_controls, sci_controls, wp_controls, fb_controls, hdi_controls, afr_controls)){
  long <- long %>% 
    left_join(
      sci_raw %>% 
        rename(GID_1 = user_loc) %>% 
        select(GID_1, fr_loc) %>% 
        left_join(dat %>% 
                    select(GID_1,
                           x = i),
                  by = 'GID_1') %>% 
        left_join(dat %>% 
                    select(GID_1,
                           y = i),
                  by = join_by(fr_loc == GID_1)) %>% 
        mutate(!!sym(i) := x - y) %>% 
        select(-x, -y),
      by = join_by(GID_1, fr_loc))
}

### Reducing it to the unique pairwise connections (e.g. )
long <- long %>%
  # distinct(scaled_sci, abs(fp_use_mod), abs(hk_test_ever), .keep_all = T) %>%
  mutate(iso3_user = substr(GID_1, 1, 3),
         iso3_fr = substr(fr_loc, 1, 3))

# Add product of penetration rates
long <- long %>% 
  left_join(dat %>% 
              select(GID_1,
                     x = FB_pntr_15to49_all),
            by = 'GID_1') %>% 
  left_join(dat %>% 
              select(GID_1,
                     y = FB_pntr_15to49_all),
            by = join_by(fr_loc == GID_1)) %>% 
  mutate(FB_pntr_product = x*y)

# Add use levels
long <- long %>% 
  left_join(dat %>% 
              mutate(poor = poorest + poorer) %>% 
              select(GID_1,
                     fp_use_mod_gid = fp_use_mod,
                     fp_know_mod_gid = fp_know_mod,
                     hk_test_ever_gid = hk_test_ever,
                     hk_knw_linear_index_gid = hk_knw_linear_index,
                     urban_gid = urban,
                     secondary_or_higher_gid = secondary_or_higher,
                     poor_gid = poor
                     ),
            by = 'GID_1') %>% 
  left_join(dat %>% 
              select(GID_1,
                     fp_use_mod_fr = fp_use_mod,
                     fp_know_mod_fr = fp_know_mod,
                     hk_test_ever_fr = hk_test_ever,
                     hk_knw_linear_index_fr = hk_knw_linear_index),
            by = join_by(fr_loc == GID_1))

long <- long %>%
  mutate(fp_know_mod_cat = ntile(fp_know_mod_gid, 2) %>% as.character(),
         hk_knw_linear_index_cat = ntile(hk_knw_linear_index_gid, 2) %>% as.character(),
         urban_cat = ntile(urban_gid, 2) %>% as.character(),
         secondary_or_higher_cat = ntile(secondary_or_higher_gid, 2) %>% as.character(),
         poor_cat = ntile(poor_gid, 2) %>% as.character(),
         fb_pntr_cat = ntile(FB_pntr_product, 2) %>% as.character()
  )
# %>%
#   filter(fp_know_mod_cat == 4)

# Linear regression -------------------------------------------------------

key_control <- c('scaled_sci')

basic_controls <- c("poorest +
                poorer +
                richer +
                richest +
                male +
                secondary_or_higher + 
                yes +
                urban +
                `[15,20)` +
                `[20,25)` +
                `[25,30)` +
                `[30,35)` +
                `[35,40)` +
                `[40,45)` +
                `[45,50)` +
                `[50,Inf]` +
                `hiv_pos`")

country_controls <- c("iso3_user + iso3_fr")

additional_controls <- c("Mean_of_Night_Light +
                Mean_distance_to_major_rd +
                Mean_built_settlement_growth +
                FB_pntr_15to49_all + 
                         FB_pntr_15to49_all:scaled_sci")

### Use of modern contraception <> Knowledge about modern contraception

i <- 'fp_use_mod'
j <- 'fp_know_mod'
interactions <- c(paste0(j,":",key_control))

temp <- long %>% 
  filter(fp_use_mod >= 0) %>%
  distinct(scaled_sci, abs(fp_use_mod), abs(hk_test_ever), abs(fp_know_mod), abs(hk_knw_linear_index), .keep_all = T) %>%
  select(GID_1,
         fr_loc,
         fp_use_mod_gid,
         fp_use_mod_fr,
         fp_know_mod_gid,
         fp_know_mod_fr,
         # fp_know_mod_cat,
         # hk_knw_linear_index_cat,
         # urban_cat,
         # secondary_or_higher_cat,
         # poor_cat,
         # fb_pntr_cat,
         all_of(i),
         all_of(j),
         all_of(dhs_controls), 
         all_of(key_control), 
         all_of(fb_controls), 
         all_of(wp_controls),
         iso3_user, iso3_fr) %>% 
  drop_na

fp_use_mod_basic_fit <- lm(paste(i,' ~',j,'+',
                      key_control,'+',
                      basic_controls,'+',
                      interactions), temp)
fp_use_mod_additional_fit <- lm(paste(i,' ~',j,'+',
                           key_control,'+',
                           basic_controls,'+',
                           additional_controls,'+',
                           interactions), temp)
fp_use_mod_interaction_fit <- lmer(paste(i,' ~',j,'+',
                            key_control,'+',
                            basic_controls,'+',
                            additional_controls,
                            '+ (1 | iso3_user)+ (1 | iso3_fr) +',
                            interactions), temp)
fp_use_mod_interaction_fit_fe <- lm(paste(i,' ~',j,'+',
                                         key_control,'+',
                                         basic_controls,'+',
                                         additional_controls,
                                         '+ iso3_user + iso3_fr +',
                                         interactions), temp)

# fp_use_mod_interaction_fit_rs <- lmer(paste(i,' ~ ',basic_controls,'+',
#                                             additional_controls,
#                                             '+ (1 | iso3_user)+ (1 | iso3_fr) + 
#                                             (scaled_sci|fp_know_mod_cat)'), temp)

# test <- data.frame()
# for (r in c('urban_cat',
#             'secondary_or_higher_cat',
#             'poor_cat',
#             'fb_pntr_cat')){
#   fp_use_mod_interaction_fit_rs <- lmer(paste(i,' ~',j,'+',
#                                               # key_control,'+',
#                                               basic_controls,'+',
#                                               additional_controls,
#                                               paste0('+ (1 | iso3_user)+ (1 | iso3_fr) + 
#                                             (scaled_sci|',r,') + (fp_know_mod:scaled_sci|',r,')')), temp)
#                                         
#   test <- test %>% 
#     bind_rows(coef(fp_use_mod_interaction_fit_rs)[r] %>% 
#     as.data.frame() %>% 
#     select(main = 1, interaction = 2) %>% 
#     mutate(ranef = r))                
# }


# tab_model(fp_use_mod_interaction_fit, fp_use_mod_interaction_fit_rs, fp_use_mod_interaction_fit_fe)
tab_model(fp_use_mod_basic_fit, fp_use_mod_additional_fit, fp_use_mod_interaction_fit)

### Get predictions
fp_use_mod_interaction_fit_fe_base_zero <- fp_use_mod_interaction_fit_fe
fp_use_mod_interaction_fit_fe_base_zero$coefficients['scaled_sci'] <- 0
fp_use_mod_interaction_fit_fe_base_zero$coefficients['fp_know_mod:scaled_sci'] <- 0

fp_use_mod_pred <- temp %>% 
  mutate(fp_main = scaled_sci*fp_use_mod_interaction_fit_fe$coefficients['scaled_sci']*100,
         fp_know = fp_know_mod*fp_use_mod_interaction_fit_fe$coefficients['fp_know_mod']*100,
         fp_inter = scaled_sci*fp_know_mod*fp_use_mod_interaction_fit_fe$coefficients['fp_know_mod:scaled_sci']*100,
         fp_pred = predict(fp_use_mod_interaction_fit_fe, newdata = temp),
         fp_base = predict(fp_use_mod_interaction_fit_fe_base_zero, newdata = temp))

#Clustered Standard Errors
fp_use_mod_basic_fit_cse <- coeftest(fp_use_mod_basic_fit, vcov = vcovCL, cluster = ~GID_1)
fp_use_mod_additional_fit_cse <- coeftest(fp_use_mod_additional_fit, vcov = vcovCL, cluster = ~GID_1)
fp_use_mod_interaction_fit_fe_cse <- coeftest(fp_use_mod_interaction_fit_fe, vcov = vcovCL, cluster = ~GID_1)

### Latex output

class(fp_use_mod_interaction_fit) <- "lmerMod"

# Get stats
stargazer(fp_use_mod_basic_fit, fp_use_mod_additional_fit, fp_use_mod_interaction_fit_fe, 
          title="Results")

# Get coefficients, se, t, p and asterisks
stargazer(fp_use_mod_basic_fit_cse,fp_use_mod_additional_fit_cse,fp_use_mod_interaction_fit_fe_cse,
          title="Results")

# ### Using condom with non-partner <> Knowledge about HIV transmission
# 
# i <- 'hk_cond_notprtnr'
# j <- 'hk_knw_linear_index'
# interactions <- c(paste0(j,":",key_control))
# 
# temp <- long %>% 
#   select(GID_1,
#          all_of(i),
#          all_of(j),
#          all_of(dhs_controls), 
#          all_of(sci_controls), 
#          all_of(fb_controls), 
#          all_of(wp_controls),
#          iso3_user, iso3_fr) %>% 
#   drop_na
# 
# hk_cond_notprtnr_basic_fit <- lm(paste(i,' ~',
#                       key_control,'+',
#                       basic_controls,'+',
#                       j,'+',
#                       interactions), temp)
# hk_cond_notprtnr_additional_fit <- lm(paste(i,' ~',
#                            key_control,'+',
#                            basic_controls,'+',
#                            additional_controls,'+',
#                            j,'+',
#                            interactions), temp)
# hk_cond_notprtnr_interaction_fit <- lmer(paste(i,' ~',
#                             key_control,'+',
#                             basic_controls,'+',
#                             j,'+',
#                             additional_controls,
#                             '+ (1 | iso3_user) + (1 | iso3_fr) +',
#                             interactions), temp)
# 
# tab_model(hk_cond_notprtnr_basic_fit, hk_cond_notprtnr_additional_fit, hk_cond_notprtnr_interaction_fit)

### Ever tested on HIV <> Knowledge about HIV transmission

i <- 'hk_test_ever'
j <- 'hk_knw_linear_index'
interactions <- c(paste0(j,":",key_control))

temp <- long %>% 
  filter(hk_test_ever >= 0) %>%
  distinct(scaled_sci, abs(fp_use_mod), abs(hk_test_ever), abs(fp_know_mod), abs(hk_knw_linear_index), .keep_all = T) %>%
  select(GID_1,
         fr_loc,
         hk_test_ever_gid,
         hk_test_ever_fr,
         hk_test_ever_gid,
         hk_test_ever_fr,
         all_of(i),
         all_of(j),
         all_of(dhs_controls), 
         all_of(key_control), 
         all_of(fb_controls), 
         all_of(wp_controls),
         iso3_user, iso3_fr) %>% 
  drop_na

hk_test_ever_basic_fit <- lm(paste(i,' ~',
                                       key_control,'+',
                                       basic_controls,'+',
                                       j,'+',
                                       interactions), temp)
hk_test_ever_additional_fit <- lm(paste(i,' ~',
                                            key_control,'+',
                                            basic_controls,'+',
                                            additional_controls,'+',
                                            j,'+',
                                            interactions), temp)
hk_test_ever_interaction_fit <- lmer(paste(i,' ~',
                                               key_control,'+',
                                               basic_controls,'+',
                                               j,'+',
                                               additional_controls,
                                               '+ (1 | iso3_user) + (1 | iso3_fr) +',
                                               interactions), temp)
hk_test_ever_interaction_fit_fe <- lm(paste(i,' ~',
                                           key_control,'+',
                                           basic_controls,'+',
                                           j,'+',
                                           additional_controls,
                                           '+ iso3_user + iso3_fr +',
                                           interactions), temp)

tab_model(hk_test_ever_basic_fit, hk_test_ever_additional_fit, hk_test_ever_interaction_fit)

### Get predictions
hk_test_ever_pred <- temp %>% 
  mutate(hk_main = scaled_sci*hk_test_ever_interaction_fit_fe$coefficients['scaled_sci']*100,
         hk_know = hk_knw_linear_index*hk_test_ever_interaction_fit_fe$coefficients['hk_knw_linear_index']*100,
         hk_inter = scaled_sci*hk_knw_linear_index*hk_test_ever_interaction_fit_fe$coefficients['scaled_sci:hk_knw_linear_index']*100)

#Clustered Standard Errors
hk_test_ever_basic_fit_cse <- coeftest(hk_test_ever_basic_fit, vcov = vcovCL, cluster = ~GID_1)
hk_test_ever_additional_fit_cse <- coeftest(hk_test_ever_additional_fit, vcov = vcovCL, cluster = ~GID_1)
hk_test_ever_interaction_fit_fe_cse <- coeftest(hk_test_ever_interaction_fit_fe, vcov = vcovCL, cluster = ~GID_1)

### Latex output
class(hk_test_ever_interaction_fit) <- "lmerMod"

stargazer(hk_test_ever_basic_fit, hk_test_ever_additional_fit, hk_test_ever_interaction_fit_fe,
          title="Results")

stargazer(hk_test_ever_basic_fit_cse, hk_test_ever_additional_fit_cse, hk_test_ever_interaction_fit_fe_cse,
          title="Results")

### Knowledge about modern contraception <> SCI

i <- 'fp_know_mod'

temp <- long %>% 
  filter(fp_know_mod >= 0) %>%
  distinct(scaled_sci, abs(fp_use_mod), abs(hk_test_ever), abs(fp_know_mod), abs(hk_knw_linear_index), .keep_all = T) %>%
  select(GID_1,
         fr_loc,
         all_of(i),
         all_of(dhs_controls), 
         all_of(key_control), 
         all_of(fb_controls), 
         all_of(wp_controls),
         iso3_user, iso3_fr) %>% 
  drop_na

fp_know_mod_basic_fit <- lm(paste(i,' ~',
                      key_control,'+',
                      basic_controls), temp)
fp_know_mod_additional_fit <- lm(paste(i,' ~',
                           key_control,'+',
                           basic_controls,'+',
                           additional_controls), temp)
fp_know_mod_interaction_fit <- lmer(paste(i,' ~',
                              key_control,'+',
                              basic_controls,'+',
                            additional_controls,
                            '+ (1 | iso3_user) + (1 | iso3_fr)'), temp)
fp_know_mod_interaction_fit_fe <- lm(paste(i,' ~',
                                          key_control,'+',
                                          basic_controls,'+',
                                          additional_controls,
                                          '+ iso3_user + iso3_fr'), temp)

tab_model(fp_know_mod_basic_fit, fp_know_mod_additional_fit, fp_know_mod_interaction_fit)

#Clustered Standard Errors
fp_know_mod_basic_fit_cse <- coeftest(fp_know_mod_basic_fit, vcov = vcovCL, cluster = ~GID_1)
fp_know_mod_additional_fit_cse <- coeftest(fp_know_mod_additional_fit, vcov = vcovCL, cluster = ~GID_1)
fp_know_mod_interaction_fit_fe_cse <- coeftest(fp_know_mod_interaction_fit_fe, vcov = vcovCL, cluster = ~GID_1)

# Latex output
class(fp_know_mod_interaction_fit) <- "lmerMod"

stargazer(fp_know_mod_basic_fit, fp_know_mod_additional_fit, fp_know_mod_interaction_fit_fe,
          title="Results")

stargazer(fp_know_mod_basic_fit_cse, fp_know_mod_additional_fit_cse, fp_know_mod_interaction_fit_fe_cse,
          title="Results")

### Knowledge about HIV transmission <> SCI

i <- 'hk_knw_linear_index'

temp <- long %>% 
  filter(hk_knw_linear_index >= 0) %>%
  distinct(scaled_sci, abs(fp_use_mod), abs(hk_test_ever), abs(fp_know_mod), abs(hk_knw_linear_index), .keep_all = T) %>%
  select(GID_1,
         fr_loc,
         all_of(i),
         all_of(dhs_controls), 
         all_of(key_control), 
         all_of(fb_controls), 
         all_of(wp_controls),
         iso3_user, iso3_fr) %>% 
  drop_na

hk_knw_linear_index_basic_fit <- lm(paste(i,' ~',
                      key_control,'+',
                      basic_controls), temp)
hk_knw_linear_index_additional_fit <- lm(paste(i,' ~',
                           key_control,'+',
                           basic_controls,'+',
                           additional_controls), temp)
hk_knw_linear_index_interaction_fit <- lmer(paste(i,' ~',
                              key_control,'+',
                              basic_controls,'+',
                            additional_controls,
                            '+ (1 | iso3_user) + (1 | iso3_fr)'), temp)
hk_knw_linear_index_interaction_fit_fe <- lm(paste(i,' ~',
                                                  key_control,'+',
                                                  basic_controls,'+',
                                                  additional_controls,
                                                  '+ iso3_user + iso3_fr'), temp)

tab_model(hk_knw_linear_index_basic_fit, hk_knw_linear_index_additional_fit, hk_knw_linear_index_interaction_fit)

#Clustered Standard Errors
hk_knw_linear_index_basic_fit_cse <- coeftest(hk_knw_linear_index_basic_fit, vcov = vcovCL, cluster = ~GID_1)
hk_knw_linear_index_additional_fit_cse <- coeftest(hk_knw_linear_index_additional_fit, vcov = vcovCL, cluster = ~GID_1)
hk_knw_linear_index_interaction_fit_fe_cse <- coeftest(hk_knw_linear_index_interaction_fit_fe, vcov = vcovCL, cluster = ~GID_1)

# Latex output
class(hk_knw_linear_index_interaction_fit) <- "lmerMod"

stargazer(hk_knw_linear_index_basic_fit, hk_knw_linear_index_additional_fit, hk_knw_linear_index_interaction_fit_fe,
          title="Results")

stargazer(hk_knw_linear_index_basic_fit_cse, hk_knw_linear_index_additional_fit_cse, hk_knw_linear_index_interaction_fit_fe_cse,
          title="Results")


# Get final tables --------------------------------------------------------

tab_model(fp_use_mod_interaction_fit_fe, 
          hk_test_ever_interaction_fit_fe,
          fp_know_mod_interaction_fit_fe, 
          hk_knw_linear_index_interaction_fit_fe)

stargazer(fp_use_mod_interaction_fit_fe, 
          hk_test_ever_interaction_fit_fe, 
          fp_know_mod_interaction_fit_fe, 
          hk_knw_linear_index_interaction_fit_fe, 
          title="Results")

stargazer(fp_use_mod_interaction_fit_fe_cse, 
          hk_test_ever_interaction_fit_fe_cse, 
          fp_know_mod_interaction_fit_fe_cse, 
          hk_knw_linear_index_interaction_fit_fe_cse, 
          title="Results")
