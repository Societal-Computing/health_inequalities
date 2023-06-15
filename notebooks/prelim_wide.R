library(tidyverse)    # Data wrangling
library(sf)           # Spatial analysis
library(caret)        # For machine learning and preprocessing
library(MASS)         # For stepwise feature selection

library(sjPlot)

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


# Data preparation --------------------------------------------------------

### Define targets
dhs_targets <- dhs %>% 
  select(rh_anc_pv:we_num_justifydv) %>% 
  select(starts_with(c('rh', 'ch', 'nt', 'fp'))) %>% 
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
  'Std_distance_to_major_rd',
  "Mean_distance_to_inland_water",
  "Std_distance_to_inland_water",
  "Mean_built_settlement_growth",
  "Std_built_settlement_growth")

fb_controls <- c('fb_rwi_mean',
                 'fb_rwi_mean_pop_wght',
                 'FB_pntr_15to49_female',
                 'FB_pntr_15to49_all')

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

# Center and scale SCI variables
dat <- dat %>% 
  mutate(across(all_of(sci_controls), ~ scale(.x, scale = T)),
         across(all_of(wp_controls), ~ scale(.x, scale = T)),
         across(all_of(afr_controls), ~ scale(.x, scale = T)))

# Define interaction effects
sci_dhs_interaction <- crossing(sci_controls, dhs_controls) %>% 
  mutate(interaction = paste0(sci_controls,':',dhs_controls)) %>% 
  select(interaction) %>% 
  t %>% 
  as.vector

# Linear regression -------------------------------------------------------

overview <- data.frame()
coeff <- data.frame()

for(i in dhs_targets){ #dhs_targets[grepl("rh_", dhs_targets)]
  
  temp <- dat %>% 
    select(i, dhs_controls, 
           sci_controls, fb_controls, wp_controls) %>% #, afr_controls
    drop_na
  
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
  
  coeff <- summary(results_all)$coefficients %>% 
    as.data.frame %>% 
    mutate(dependent = i) %>% 
    rownames_to_column('feature') %>% 
    filter(feature %in% sci_controls) %>% 
    mutate(dependent = i) %>% 
    bind_rows(coeff)
  
}

overview <- overview %>% 
  mutate(diff = adj_r_all - adj_r_no_sci)

coeff_summary <- coeff %>% 
  rename(p_value = .data[['Pr(>|t|)']]) %>% 
  filter(p_value <= 0.1) %>% 
  group_by(feature) %>% 
  summarise(min = min(Estimate, na.rm = T),
            q5 = quantile(Estimate, probs = 0.05, na.rm = T),
            median = median(Estimate, na.rm = T),
            mean = mean(Estimate, na.rm = T),
            # sd = sd(Estimate, na.rm = T),
            q95 = quantile(Estimate, probs = 0.95, na.rm = T),
            max = max(Estimate, na.rm = T),
            p_value = mean(p_value, na.rm = T))
            
  
# Linear regression hand-picked -------------------------------------------

# Prepare data
temp <- dat %>% 
  select(dhs_targets, dhs_controls, 
         sci_controls, 
         fb_controls, 
         # afr_controls,
         # hdi_controls, 
         wp_controls) %>% 
  drop_na

# Define controls ---------------------------------------------------------

controls <- c('Mean_dist_to_SCI +
                Std_dist_to_SCI +
                Mean_SCI_with_Self +
                Average_distance_of_friendships +
                Ratio_selfloop_to_africa + 
                Ratio_SCI_low_hi_africa + 
                Ratio_SCI_high_hi_africa +
                poorest +
                poorer +
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
                has_mobile_phone_yes:Mean_SCI_with_Self')

# Child vaccination rate
fit_ch_allvac_either <- lm(paste('ch_allvac_either ~',controls), temp)
fit_rh_anc_pv <- lm(paste('rh_anc_pv ~',controls), temp)
fit_rh_anc_pvskill <- lm(paste('rh_anc_pvskill ~',controls), temp)
fit_nt_bf_ever <- lm(paste('nt_bf_ever ~',controls), temp)


# Diagnostics -------------------------------------------------------------

# Regression output
tab_model(fit_ch_allvac_either, fit_rh_anc_pv, fit_rh_anc_pvskill, fit_nt_bf_ever)

# Histogram of dependent variable
dat %>%
  ggplot() +
  geom_histogram(aes(x=(ch_allvac_either))) +
  xlab("Share of children with full vaccination records")

# QQ-Plot of residuals
qqPlot(fit_ch_allvac_either)

# Plot of residuals
plot(resid(fit_ch_allvac_either))

# Linear regression with model selection ----------------------------------

overview_select <- data.frame()

for(i in dhs_targets){ #dhs_targets[grepl("rh_", dhs_targets)]
  
  temp <- dat %>% 
    select(i, dhs_controls, sci_controls, fb_controls) %>% 
    drop_na
  
  ### With SCI
  results_select_all <- lm(paste(i, '~', 
                                 paste(dhs_controls, collapse = '+'), '+',
                                 paste(sci_controls, collapse = '+'), '+',
                                 paste(fb_controls, collapse = '+')),
                           temp)
  
  results_select <- stepAIC(results_select_all, direction = "both", trace=FALSE)
  
  ### Without SCI
  results_select_all_no_sci <- lm(paste(i, '~', 
                                        paste(dhs_controls, collapse = '+'), '+',
                                        paste(fb_controls, collapse = '+')),
                                  temp)
  
  results_select_no_sci <- stepAIC(results_select_all_no_sci, direction = "both", trace=FALSE)
  
  ### Lucky shot
  results_select_all_lucky <- lm(paste(i, '~', 
                                 paste(dhs_controls, collapse = '+'), '+',
                                 paste(fb_controls, collapse = '+')), 
                           temp %>% 
                             mutate(across(c(dhs_controls, sci_controls, fb_controls), 
                                           ~ sample(.x, length(.x), replace = T)))
  )
  
  results_select_lucky <- stepAIC(results_select_all_lucky, direction = "both", trace=FALSE)
  
  
  temp <- data.frame(
    target = i,
    adj_r_sci = round(summary(results_select)$adj.r.squared, digits = 2),
    adj_r_no_sci = round(summary(results_select_no_sci)$adj.r.squared, digits = 2),
    adj_r_lucky = round(summary(results_select_lucky)$adj.r.squared, digits = 2),
    n_obs = nobs(results_select),
    n_pred_sci = summary(results_select)$df[1] - 1,
    n_pred_no_sci = summary(results_select_no_sci)$df[1] - 1
  )
  
  overview_select <- overview_select %>% 
    bind_rows(temp)
  
}

overview_select <- overview_select %>% 
  mutate(diff = adj_r_sci - adj_r_no_sci)


# Summary table -----------------------------------------------------------

test <- overview %>% 
  select(target,
         adj_r_all = adj_r_sci,
         adj_r_all_no_sci = adj_r_no_sci,
         diff_all = diff) %>% 
  left_join(overview_select %>% 
              select(
                target,
                adj_r_select = adj_r_sci,
                adj_r_select_no_sci = adj_r_no_sci,
                diff_select = diff),
            by = 'target') %>% 
  filter(!str_detect(target, '_male_'),
         str_detect(target, 'fp_'))

kable(head(test),caption='Normal `kable` usage.')


# Non-linear modeling -----------------------------------------------------



for(i in dhs_targets){
  
  i <- 'ch_allvac_either'
  
  temp <- dat %>% 
    select(all_of(i), 
           dhs_controls, 
           sci_controls, 
           fb_controls, 
           # hdi_controls,
           wp_controls) %>% 
    drop_na

  temp_index <- createDataPartition(temp[[i]], p = .75, list = FALSE)
  temp_train <- temp[ temp_index, ]
  temp_test <- temp[-temp_index, ]
  
  fitControl <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10, 
    classProbs = T,
    savePredictions = T)
  
  fit <- train(formula(paste0(i, '~',  
                              paste(sci_controls, collapse = '+'), '+',
                       paste(dhs_controls, collapse = '+'), '+',
                       paste(fb_controls, collapse = '+'), '+',
                       paste(wp_controls, collapse = '+'))), 
               data = temp_train, 
               method = "lm", #svmRadial #gbm #rf #gaussprRadial #qrf #lm
               trControl = fitControl,
               verbose = FALSE)
 
  # Training performance 
 getTrainPerf(fit)
  
 # OOS performance
  temp_pred <- predict(fit, temp_test)
  postResample(pred = temp_pred, obs = temp_test[[i]])

  # Variable importance
  plot(varImp(fit))
  
  # Regression table (for linear model)
  summary(fit$finalModel)

}

# Principal component analysis --------------------------------------------





