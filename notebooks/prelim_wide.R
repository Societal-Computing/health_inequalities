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

# Data preparation --------------------------------------------------------

dhs_targets <- c('rh_anc_pv_female_yes',
                 'rh_anc_pv_female_no',
                 'rh_anc_pvskill_female_yes',
                 'rh_anc_pvskill_female_no',
                 "rh_anc_numvs_female_no",
                 "rh_anc_numvs_female_yes",
                 "rh_anc_4vs_female_no",
                 "rh_anc_4vs_female_yes", 
                 'nt_bf_ever_yes',
                 'nt_bf_ever_no',
                 'ch_allvac_either_yes',
                 'ch_allvac_either_no',
                 'ch_novac_either_yes',
                 'ch_novac_either_no',
                 # 'fp_know_any_female_yes',
                 # 'fp_know_any_female_no',
                 'fp_know_mod_female_yes',
                 'fp_know_mod_female_no',
                 # 'fp_know_any_male_yes',
                 # 'fp_know_any_male_no',
                 'fp_know_mod_male_yes',
                 'fp_know_mod_male_no',
                 'fp_message_noneof3_female_yes',
                 'fp_message_noneof3_female_no',
                 'fp_message_noneof3_male_yes',
                 'fp_message_noneof3_male_no',
                 # 'dv_sex_12m_female_yes',
                 # 'dv_sex_12m_female_no',
                 # 'dv_phy_12m_female_yes',
                 # 'dv_phy_12m_female_no',
                 'we_num_decide_female_yes',
                 'we_num_decide_female_no',
                 'we_num_justifydv_female_yes',
                 'we_num_justifydv_female_no')


dhs_controls <- c('poorest',
                  'poorer',
                  'middle',
                  'richer',
                  'richest',
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

sci_controls <- c('Local_mean_SCI',
                  'Local_std_SCI',
                  'Mean_dist_to_SCI',
                  'LMIC_interconnection_index',
                  'Intraconnection_index',
                  'Mean_SCI_without_Self',
                  'Std_SCI_without_Self',
                  'Mean_SCI_with_Self',
                  'Std_SCI_with_Self'
)

fb_controls <- c('fb_rwi_mean',
                 'fb_rwi_mean_pop_wght',
                 'FB_pntr_15to49_female',
                 'FB_pntr_15to49_all'
)

dat <- dhs %>% 
  select(GID_1,
         all_of(dhs_targets),
         all_of(dhs_controls)) %>% 
  left_join(sci %>% 
              select(GID_1,
                     all_of(sci_controls),
                     all_of(fb_controls)),
            by = 'GID_1')


# Preprocess --------------------------------------------------------------

### Center and scale SCI variables
dat <- dat %>% 
  mutate(across(sci_controls, ~ scale(.x, scale = T)))

# Define interaction effects ----------------------------------------------

dhs_controls_interaction <- crossing(dhs_controls, dhs_controls) %>% 
  mutate(interaction = paste0(dhs_controls...1,':',dhs_controls...2)) %>% 
  select(interaction) %>% 
  t %>% 
  as.vector

# Linear regression -------------------------------------------------------

overview <- data.frame()

for(i in dhs_targets){ #dhs_targets[grepl("rh_", dhs_targets)]
  
  temp <- dat %>% 
    select(i, dhs_controls, sci_controls, fb_controls) %>% 
    drop_na
  
  ### With SCI
  results <- lm(paste(i, '~', 
                      paste(dhs_controls, collapse = '+'), '+',
                      paste(sci_controls, collapse = '+'), '+',
                      paste(fb_controls, collapse = '+'), '+',
                      paste(dhs_controls_interaction, collapse = '+')), temp)
  
  ### Without SCI
  results_no_sci <- lm(paste(i, '~', 
                             paste(dhs_controls, collapse = '+'), '+',
                             paste(fb_controls, collapse = '+'), '+',
                             paste(dhs_controls_interaction, collapse = '+')), temp)
  
  ### Lucky shot
  results_lucky <- lm(paste(i, '~', 
                            paste(dhs_controls, collapse = '+'), '+',
                            paste(fb_controls, collapse = '+')), 
                      temp %>% 
                        mutate(across(c(dhs_controls, sci_controls, fb_controls), 
                                      ~ sample(.x, length(.x), replace = T)))
  )
  
  
  
  temp <- data.frame(
    target = i,
    adj_r_sci = round(summary(results)$adj.r.squared, digits = 2),
    adj_r_no_sci = round(summary(results_no_sci)$adj.r.squared, digits = 2),
    adj_r_lucky = round(summary(results_lucky)$adj.r.squared, digits = 2),
    n_obs = nobs(results)
  )
  
  overview <- overview %>% 
    bind_rows(temp)
  
}

overview <- overview %>% 
  mutate(diff = adj_r_sci - adj_r_no_sci)



# Linear regression hand-picked -------------------------------------------

i <- 'rh_anc_pvskill_female_no'

temp <- dat %>% 
  select(all_of(i), dhs_controls, sci_controls, fb_controls) %>% 
  drop_na

results_hand <- lm(paste(i, '~', 
                         paste(dhs_controls, collapse = '+'), '+',
                         paste(sci_controls, collapse = '+'), '+',
                         paste(fb_controls, collapse = '+'), '+',
                         paste(dhs_controls_interaction, collapse = '+')),
                   temp)

results_hand_select <- stepAIC(results_hand, direction = "both", trace=FALSE)

results_hand_no_sci <- lm(paste(i, '~', 
                                paste(dhs_controls, collapse = '+'), '+',
                                paste(fb_controls, collapse = '+'), '+',
                                paste(dhs_controls_interaction, collapse = '+')),
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

tab_model(results_hand_select, results_hand_select_no_sci)

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
                                 paste(fb_controls, collapse = '+'), '+',
                                 paste(dhs_controls_interaction, collapse = '+')),
                           temp)
  
  results_select <- stepAIC(results_select_all, direction = "both", trace=FALSE)
  
  ### Without SCI
  results_select_all_no_sci <- lm(paste(i, '~', 
                                        paste(dhs_controls, collapse = '+'), '+',
                                        paste(fb_controls, collapse = '+'), '+',
                                        paste(dhs_controls_interaction, collapse = '+')),
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

i <- 'fp_message_noneof3_female_no'

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

fit1 <- train(fp_message_noneof3_female_no ~ ., data = training, 
                 method = "qrf", #svmRadial #gbm #rf #gaussprRadial #qrf
                 trControl = fitControl, 
                 verbose = FALSE, 
                 # tuneGrid = tuneGrid
                 )

fit1

ggplot(fit1)

# Principal component analysis --------------------------------------------





