library(tidyverse)    # Data wrangling
library(sf)           # Spatial analysis
library(caret)        # For machine learning and preprocessing
library(MASS)         # For stepwise feature selection

library(sjPlot)
library(sjmisc)



select <- dplyr::select

# Loading relevant data ---------------------------------------------------

### SCI and other data
sci <- read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/combined_dataset/GADM_1_variables.csv")

### DHS data
dhs <- read.csv('/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/dhs_health.csv')

### Afrobarometer
afr <- read.csv('/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/afrobarometer.csv')

# Data preparation --------------------------------------------------------

### Define targets
dhs_targets <- dhs %>% 
  select(rh_anc_pv:we_num_justifydv) %>% 
  names()

### Define controls
dhs_controls <- c(
                  'poorest',
                  'poorer',
                  'middle',
                  # 'richer',
                  # 'richest',
                  'female',
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

sci_controls <- c('Mean_dist_to_SCI',
                  'Std_dist_to_SCI',
                  'Total_dist_to_SCI',
                  'ratio_selfloop_to_country',
                  'ratio_selfloop_to_africa',
                  'ratio_selfloop_to_all_sci',
                  'Mean_SCI_without_Self',
                  'Std_SCI_without_Self',
                  'Mean_SCI_with_Self',
                  'Std_SCI_with_Self',
                  'Mean_friendship',
                  'Std_friendship',
                  'Total_friendship'
)

wp_controls <- c(
                 'Mean_of_Night_Light',
                 'Std_of_Night_Light',
                 'Mean_distance_to_major_rd_intesection',
                 'Std_distance_to_major_rd_intesection',
                 'Mean_distance_to_major_rd',
                 'Std_distance_to_major_rd')

fb_controls <- c('fb_rwi_mean',
                 'fb_rwi_mean_pop_wght',
                 'FB_pntr_15to49_female',
                 'FB_pntr_15to49_all',
                 'All_devices_age_13_plus_fm_ratio'
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
              select(GID_1,
                     all_of(sci_controls),
                     all_of(fb_controls),
                     all_of(wp_controls)),
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

### Avoid infinite ratios
dat[dat == 0] <- 0.00001

### Define ratio operator
ratio_operator <- '-' # '/'

# Linear regression -------------------------------------------------------

overview <- data.frame()

for(i in dhs_targets){ #dhs_targets[grepl("rh_", dhs_targets)]
  
  temp <- dat %>% 
    select(i, dhs_controls, 
           sci_controls, fb_controls, wp_controls, afr_controls) %>%
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
                      paste(afr_controls, collapse = '+'), '+',
                      paste(sci_dhs_interaction, collapse = '+')), temp)
  
  ### Without interactions
  results_no_interactions <- lm(paste(i, '~', 
                          paste(dhs_controls, collapse = '+'), '+',
                          paste(sci_controls, collapse = '+'), '+',
                          paste(fb_controls, collapse = '+'), '+',
                          paste(wp_controls, collapse = '+'), '+',
                          paste(afr_controls, collapse = '+')), temp)
  
  ### Without SCI
  results_no_sci <- lm(paste(i, '~', 
                             paste(dhs_controls, collapse = '+'), '+',
                             paste(fb_controls, collapse = '+'), '+',
                             paste(wp_controls, collapse = '+'), '+',
                             paste(afr_controls, collapse = '+')), temp)
  
  ### Without WP
  results_no_wp <- lm(paste(i, '~', 
                             paste(dhs_controls, collapse = '+'), '+',
                            paste(sci_controls, collapse = '+'), '+',
                             paste(fb_controls, collapse = '+'), '+',
                            paste(afr_controls, collapse = '+'), '+',
                            paste(sci_dhs_interaction, collapse = '+')), temp)
  
  ### Without FB
  results_no_fb <- lm(paste(i, '~', 
                            paste(dhs_controls, collapse = '+'), '+',
                            paste(sci_controls, collapse = '+'), '+',
                            paste(wp_controls, collapse = '+'), '+',
                            paste(afr_controls, collapse = '+'), '+',
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
                            paste(afr_controls, collapse = '+'), '+',
                            paste(sci_dhs_interaction, collapse = '+')), 
                      temp %>% 
                        mutate(across(c(dhs_controls, sci_controls, fb_controls, wp_controls, afr_controls), 
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
  
}

overview <- overview %>% 
  mutate(diff = adj_r_all - adj_r_no_sci)

# Linear regression hand-picked -------------------------------------------

i <- 'nt_bf_ever'

temp <- dat %>% 
  select(i, dhs_controls, sci_controls, fb_controls, wp_controls) %>% #, afr_controls
  drop_na

results_hand <- lm(paste0(i, '~', 
                         paste(dhs_controls, collapse = '+'), '+',
                         paste(sci_controls, collapse = '+'), '+',
                         paste(fb_controls, collapse = '+'), '+',
                         paste(wp_controls, collapse = '+'), '+',
                         paste(sci_dhs_interaction, collapse = '+')), temp)

results_hand_select <- stepAIC(results_hand, direction = "both", trace=FALSE)

results_hand_no_inter <- lm(paste0(i, '~', 
                          paste(dhs_controls, collapse = '+'), '+',
                          paste(sci_controls, collapse = '+'), '+',
                          paste(fb_controls, collapse = '+'), '+',
                          paste(wp_controls, collapse = '+')), temp)

results_hand_no_inter_select <- stepAIC(results_hand_no_inter, direction = "both", trace=FALSE)

results_hand_no_sci <- lm(paste0(i, '~', 
                                 paste(dhs_controls, collapse = '+'), '+',
                                 paste(fb_controls, collapse = '+'), '+',
                                 paste(wp_controls, collapse = '+'), '+',
                                 paste(afr_controls, collapse = '+')), temp)

results_hand_select_no_sci <- stepAIC(results_hand_no_sci, direction = "both", trace=FALSE)

tab_model(results_hand_select, results_hand_no_inter_select, results_hand_select_no_sci)

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



# Random Forest -----------------------------------------------------------

i <- 'fp_message_noneof3_female_no_poorest'

temp <- dat %>% 
  select(i, dhs_controls, sci_controls, fb_controls) %>% 
  drop_na

inTraining <- createDataPartition(temp[[i]], p = .75, list = FALSE)
training <- temp[ inTraining,]
testing  <- temp[-inTraining,]

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

tuneGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(tuneGrid)

fit1 <- train(fp_message_noneof3_female_no_poorest ~ ., data = training, 
                 method = "qrf", #svmRadial #gbm #rf #gaussprRadial #qrf
                 trControl = fitControl, 
                 verbose = FALSE, 
                 # tuneGrid = tuneGrid
                 )

fit1

ggplot(fit1)

# Principal component analysis --------------------------------------------





