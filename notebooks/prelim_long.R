library(tidyverse)    # Data wrangling
library(sf)           # Spatial analysis
library(caret)        # For machine learning and preprocessing
library(MASS)         # For stepwise feature selection

library(sjPlot)
library(sjmisc)



select <- dplyr::select

# Loading relevant data ---------------------------------------------------

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
  distinct(GID_1, .keep_all = T)

# DHS data
dhs <- read.csv('/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/dhs_health.csv')

# Afrobarometer
afr <- read.csv('/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/afrobarometer.csv') %>% 
  distinct(GID_1, .keep_all = T)

# Raw SCI
sci_raw <- read.csv("/Users/tillkoebe/Documents/Data/SCI/sci_gadm1.tsv", sep = "\t") %>% 
  filter(user_loc %in% unique(dhs$GID_1),
         fr_loc %in% unique(dhs$GID_1))


# Data preparation --------------------------------------------------------

### Define targets
dhs_targets <- dhs %>% 
  select(rh_anc_pv:we_num_justifydv) %>% 
  select(starts_with(c('rh', 'ch', 'nt'))) %>% 
  names()

### Define controls
dhs_controls <- c(
  'poorest',
  'poorer',
  'middle',
  'richer',
  'richest',
  'female',
  'secondary_or_higher',
  'has_mobile_phone_yes',
  'rural',
  "X15.19",
  "X20.24",
  "X25.29",
  "X30.34",
  "X35.39",
  "X40.44",
  "X45.49"    
  # 'rel_minor',
  # 'lang_minor'
  # 'muslim',
  # 'protestant',
  # 'catholic'
)

sci_controls <- c('Mean_dist_to_SCI', # Average distance of friendships
                  'Median_dist_to_SCI', # Average distance of friendships
                  'Std_dist_to_SCI', # Clustering measure of friendships across distances
                  'Total_dist_to_SCI', # Total distance of friendships
                  'Ratio_selfloop_to_country', # Share of local friendships within the same country
                  'Ratio_selfloop_to_africa', # Share of local friendships that stay within Africa
                  'Ratio_selfloop_to_all_sci', # Share of local friendships of all friendships
                  'Average_distance_of_friendships', 
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
  'Std_of_Night_Light',
  'Mean_distance_to_major_rd_intersection',
  'Std_distance_to_major_rd_intersection',
  'Mean_distance_to_major_rd',
  'Std_distance_to_major_rd')

fb_controls <- c('fb_rwi_mean',
                 'fb_rwi_mean_pop_wght',
                 'FB_pntr_15to49_female',
                 'FB_pntr_15to49_all'
)

afr_controls <- c('trpar',
                  'trtax',
                  'trgov',
                  'trlaw',
                  'trgen',
                  'trrel',
                  'trnei',
                  'tracq')

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
            by = 'GID_1')

# Preprocess --------------------------------------------------------------

### Center and scale SCI variables
dat <- dat %>% 
  mutate(across(all_of(sci_controls), ~ scale(.x, scale = T)),
         across(all_of(wp_controls), ~ scale(.x, scale = T)),
         across(all_of(afr_controls), ~ scale(.x, scale = T)))

### Define interaction effects

sci_dhs_interaction <- crossing(sci_controls, dhs_controls) %>% 
  mutate(interaction = paste0(sci_controls,':',dhs_controls)) %>% 
  select(interaction) %>% 
  t %>% 
  as.vector

sci_wp_interaction <- crossing(sci_controls, wp_controls) %>% 
  mutate(interaction = paste0(dhs_controls,':',wp_controls)) %>% 
  select(interaction) %>% 
  t %>% 
  as.vector

# Get inter-regional differences ------------------------------------------

long <- sci_raw %>% 
  rename(GID_1 = user_loc) %>% 
  mutate(scaled_sci = scaled_sci / 1000000000,
         tertiale_sci = factor(ntile(scaled_sci, 3), levels = c(1,2,3), labels = c('weak', 'medium', 'strong')),
         log_sci = scaled_sci %>% log %>% scale %>% as.vector,
         sqrt_sci = sqrt(scaled_sci)) %>% 
  filter(GID_1 != fr_loc)

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
  distinct(log_sci, abs(rh_anc_pv), .keep_all = T)


# Add raw SCI as control --------------------------------------------------

sci_controls <- c('scaled_sci', 'tertiale_sci', 'log_sci', 'sqrt_sci', sci_controls)

# Define interaction effects ----------------------------------------------

# dhs_controls_interaction <- crossing(dhs_controls, dhs_controls) %>% 
#   mutate(interaction = paste0(dhs_controls...1,':',dhs_controls...2)) %>% 
#   select(interaction) %>% 
#   t %>% 
#   as.vector

# Linear regression -------------------------------------------------------

overview <- data.frame()

for(i in dhs_targets){ #dhs_targets[grepl("rh_", dhs_targets)]
  
  temp <- long %>% 
    select(i, dhs_controls, 
           sci_controls, fb_controls, wp_controls, hdi_controls) %>% #, afr_controls
    drop_na
  
  ### Scaled SCI only
  results_scaled_sci <- lm(paste(i, '~ log_sci'), temp)
  
  ### SCI only
  results_only_sci <- lm(paste(i, '~', 
                               paste(sci_controls, collapse = '+')), temp)
  
  ### With SCI
  results_all <- lm(paste(i, '~', 
                          paste(dhs_controls, collapse = '+'), '+',
                          paste(sci_controls, collapse = '+'), '+',
                          paste(fb_controls, collapse = '+'), '+',
                          paste(wp_controls, collapse = '+'), '+',
                          # paste(afr_controls, collapse = '+'), '+',
                          paste(sci_dhs_interaction, collapse = '+')), temp)
  
  ### Without interactions
  results_no_interactions <- lm(paste(i, '~', 
                                      paste(dhs_controls, collapse = '+'), '+',
                                      paste(sci_controls, collapse = '+'), '+',
                                      paste(fb_controls, collapse = '+'), '+',
                                      # paste(afr_controls, collapse = '+'), '+',
                                      paste(wp_controls, collapse = '+')), temp)
  
  ### Without SCI
  results_no_sci <- lm(paste(i, '~', 
                             paste(dhs_controls, collapse = '+'), '+',
                             paste(fb_controls, collapse = '+'), '+',
                             # paste(afr_controls, collapse = '+'), '+',
                             paste(wp_controls, collapse = '+')), temp)
  
  ### Without WP
  results_no_wp <- lm(paste(i, '~', 
                            paste(dhs_controls, collapse = '+'), '+',
                            paste(sci_controls, collapse = '+'), '+',
                            paste(fb_controls, collapse = '+'), '+',
                            # paste(afr_controls, collapse = '+'), '+',
                            paste(sci_dhs_interaction, collapse = '+')), temp)
  
  ### Without FB
  results_no_fb <- lm(paste(i, '~', 
                            paste(dhs_controls, collapse = '+'), '+',
                            paste(sci_controls, collapse = '+'), '+',
                            paste(wp_controls, collapse = '+'), '+',
                            # paste(afr_controls, collapse = '+'), '+',
                            paste(sci_dhs_interaction, collapse = '+')), temp)
  
  ### Without Afrobarometer
  results_no_afr <- lm(paste(i, '~', 
                             paste(dhs_controls, collapse = '+'), '+',
                             paste(sci_controls, collapse = '+'), '+',
                             paste(fb_controls, collapse = '+'), '+',
                             paste(wp_controls, collapse = '+'), '+',
                             paste(sci_dhs_interaction, collapse = '+')), temp)
  
  ### Lucky shot
  results_lucky <- lm(paste(i, '~', 
                            paste(dhs_controls, collapse = '+'), '+',
                            paste(sci_controls, collapse = '+'), '+',
                            paste(fb_controls, collapse = '+'), '+',
                            paste(wp_controls, collapse = '+'), '+',
                            # paste(afr_controls, collapse = '+'), '+',
                            paste(sci_dhs_interaction, collapse = '+')), 
                      temp %>% 
                        mutate(across(c(dhs_controls, sci_controls, fb_controls, wp_controls), #, afr_controls
                                      ~ sample(.x, length(.x), replace = T)))
  )
  
  temp <- data.frame(
    target = i,
    adj_r_scaled_sci = round(summary(results_scaled_sci)$adj.r.squared, digits = 2),
    adj_r_only_sci = round(summary(results_only_sci)$adj.r.squared, digits = 2),
    adj_r_all = round(summary(results_all)$adj.r.squared, digits = 2),
    adj_r_no_interactions = round(summary(results_no_interactions)$adj.r.squared, digits = 2),
    adj_r_no_sci = round(summary(results_no_sci)$adj.r.squared, digits = 2),
    adj_r_no_wp = round(summary(results_no_wp)$adj.r.squared, digits = 2),
    adj_r_no_fb = round(summary(results_no_fb)$adj.r.squared, digits = 2),
    adj_r_no_afr = round(summary(results_no_afr)$adj.r.squared, digits = 2),
    adj_r_lucky = round(summary(results_lucky)$adj.r.squared, digits = 2),
    n_obs = nobs(results_all)
  )
  
  overview <- overview %>% 
    bind_rows(temp)
  
}

overview <- overview %>% 
  mutate(diff = adj_r_all - adj_r_no_sci)


# Linear regression hand-picked -------------------------------------------

i <- 'rh_anc_pv'

temp <- long %>% 
  select(all_of(i), dhs_controls, 
         sci_controls, 
         fb_controls, 
         wp_controls,
         # afr_controls,
         # hdi_controls, 
         wp_controls) %>% 
  drop_na

results <- lm(paste(i, '~
  scaled_sci +
  tertiale_sci +
  log_sci +
  sqrt_sci +
  Mean_dist_to_SCI +
  Mean_SCI_with_Self +
  Mean_SCI_without_Self +
  Mean_friendship +
  Ratio_selfloop_to_africa + 
  Ratio_SCI_low_hi_africa + 
  Ratio_SCI_high_hi_africa +
  poorest +
  poorer +
  middle +
  richer +
  richest +
  female +
  secondary_or_higher + 
  has_mobile_phone_yes +
  rural +
  X15.19 +
  X20.24 +
  X25.29 +
  X30.34 +
  X35.39 +
  X40.44 +
  X45.49 +
  Mean_of_Night_Light +
  Mean_distance_to_major_rd +
  FB_pntr_15to49_all +
  female:has_mobile_phone_yes +
  female:Mean_SCI_with_Self +
  has_mobile_phone_yes:Mean_SCI_with_Self'), temp) 

summary(results)

tab_model(results)

results_only_sci <- lm(paste(i, '~', 
                             paste(sci_controls, collapse = '+')), temp)

results_hand <- lm(paste(i, '~', 
                         paste(dhs_controls, collapse = '+'), '+',
                         paste(sci_controls, collapse = '+'), '+',
                         paste(fb_controls, collapse = '+'), '+',
                         paste(hdi_controls, collapse = '+'), '+',
                         paste(wp_controls, collapse = '+')),
                   temp)

results_hand_select <- stepAIC(results_hand, direction = "both", trace=FALSE)

results_hand_no_sci <- lm(paste(i, '~', 
                                paste(dhs_controls, collapse = '+'), '+',
                                paste(fb_controls, collapse = '+')),
                          temp)

results_hand_select_no_sci <- stepAIC(results_hand_no_sci, direction = "both", trace=FALSE)


### Lucky shot
results_hand_lucky <- lm(paste(i, '~', 
                          paste(dhs_controls, collapse = '+'), '+',
                          paste(fb_controls, collapse = '+')), 
                    temp %>% 
                      mutate(across(c(dhs_controls, sci_controls, fb_controls), 
                                    ~ sample(.x, length(.x), replace = T)))
)

results_hand_select_lucky <- stepAIC(results_hand_lucky, direction = "both", trace=FALSE)

tab_model(results_hand_select, results_hand_select_no_sci, results_hand_select_lucky)

# Linear regression with model selection ----------------------------------

overview_select <- data.frame()

for(i in dhs_targets[!grepl("_male_", dhs_targets)]){
  
  temp <- dat %>% 
    select(i, dhs_controls, sci_controls, fb_controls) %>% 
    drop_na
  
  results_select_all <- lm(paste(i, '~', 
                                 paste(dhs_controls, collapse = '+'), '+',
                                 paste(sci_controls, collapse = '+'), '+',
                                 paste(fb_controls, collapse = '+')),
                           temp)
  
  results_select <- stepAIC(results_select_all, direction = "both", trace=FALSE)
  
  results_select_all_no_sci <- lm(paste(i, '~', 
                                        paste(dhs_controls, collapse = '+'), '+',
                                        paste(fb_controls, collapse = '+')),
                                  temp)
  
  results_select_no_sci <- stepAIC(results_select_all_no_sci, direction = "both", trace=FALSE)
  
  temp <- data.frame(
    target = i,
    adj_r_sci = round(summary(results_select)$adj.r.squared, digits = 2),
    adj_r_no_sci = round(summary(results_select_no_sci)$adj.r.squared, digits = 2),
    n_obs = nobs(results_select),
    n_pred_sci = summary(results_select)$df[1] - 1,
    n_pred_no_sci = summary(results_select_no_sci)$df[1] - 1
  )
  
  overview_select <- overview_select %>% 
    bind_rows(temp)
  
}

overview_select <- overview_select %>% 
  mutate(diff = adj_r_sci - adj_r_no_sci)



# Cross-Validation --------------------------------------------------------

for(i in dhs_targets){
  
  temp <- long %>% 
    select(all_of(i), dhs_controls, sci_controls, fb_controls, wp_controls) %>% 
    drop_na
  
  fitControl <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10)
  
  fit <- train(formula(paste0(i, '~',  
                              paste(sci_controls, collapse = '+'), '+',
                              paste(dhs_controls, collapse = '+'), '+',
                       paste(fb_controls, collapse = '+'), '+',
               paste(wp_controls, collapse = '+'))), 
               data = temp, 
               method = "lm", #svmRadial #gbm #rf #gaussprRadial #qrf
               trControl = fitControl,
               verbose = FALSE)
  
  fit
  
  }
