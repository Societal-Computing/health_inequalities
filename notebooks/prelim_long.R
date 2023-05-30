library(tidyverse)    # Data wrangling
library(sf)           # Spatial analysis
library(caret)        # For machine learning and preprocessing
library(MASS)         # For stepwise feature selection

library(sjPlot)
library(sjmisc)



select <- dplyr::select

# Loading relevant data ---------------------------------------------------

### SCI and other data
sci_raw <- read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/combined_dataset/GADM_1_variables.csv")

### DHS data
dhs_raw <- read.csv('/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/dhs_health.csv')

### Raw SCI
sci <- read.csv("/Users/tillkoebe/Documents/Data/SCI/sci_gadm1.tsv", sep = "\t") %>% 
  filter(user_loc %in% unique(dhs_raw$GID_1),
         fr_loc %in% unique(dhs_raw$GID_1))

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
                 'fp_know_any_female_yes',
                 'fp_know_any_female_no',
                 'fp_know_mod_female_yes',
                 'fp_know_mod_female_no',
                 'fp_know_any_male_yes',
                 'fp_know_any_male_no',
                 'fp_know_mod_male_yes',
                 'fp_know_mod_male_no',
                 'fp_message_noneof3_female_yes',
                 'fp_message_noneof3_female_no',
                 'fp_message_noneof3_male_yes',
                 'fp_message_noneof3_male_no',
                 'dv_sex_12m_female_yes',
                 'dv_sex_12m_female_no',
                 'dv_phy_12m_female_yes',
                 'dv_phy_12m_female_no',
                 'we_num_decide_female_yes',
                 'we_num_decide_female_no',
                 'we_num_justifydv_female_yes',
                 'we_num_justifydv_female_no')


dhs_controls <- c('poorest',
                  'poorer',
                  'has_mobile_phone_yes',
                  'rural'
                  # 'rel_minor',
                  # 'lang_minor'
                  # 'muslim',
                  # 'protestant',
                  # 'catholic'
)

sci_controls <- c('scaled_sci'
)

fb_controls <- c('fb_rwi_mean',
                 'fb_rwi_mean_pop_wght',
                 'FB_pntr_15to49_female',
                 'FB_pntr_15to49_all'
)


# Get inter-regional differences ------------------------------------------

dat <- sci %>% 
  rename(GID_1 = user_loc) %>% 
  mutate(scaled_sci = scaled_sci / 1000000000)

for(i in dhs_targets){
  dat <- dat %>% 
    left_join(
      sci %>% 
        rename(GID_1 = user_loc) %>% 
        select(GID_1, fr_loc) %>% 
        left_join(dhs_raw %>% 
                    select(GID_1,
                           x = i),
                  by = 'GID_1') %>% 
        left_join(dhs_raw %>% 
                    select(GID_1,
                           y = i),
                  by = join_by(fr_loc == GID_1)) %>% 
        mutate(!!sym(i) := y - x) %>% 
        select(-x, -y),
      by = join_by(GID_1, fr_loc))
}

for(i in dhs_controls){
  dat <- dat %>% 
    left_join(
      sci %>% 
        rename(GID_1 = user_loc) %>% 
        select(GID_1, fr_loc) %>% 
        left_join(dhs_raw %>% 
                    select(GID_1,
                           x = i),
                  by = 'GID_1') %>% 
        left_join(dhs_raw %>% 
                    select(GID_1,
                           y = i),
                  by = join_by(fr_loc == GID_1)) %>% 
        mutate(!!sym(i) := y - x) %>% 
        select(-x, -y),
      by = join_by(GID_1, fr_loc))
}

for(i in fb_controls){
  dat <- dat %>% 
    left_join(
      sci %>% 
        rename(GID_1 = user_loc) %>% 
        select(GID_1, fr_loc) %>% 
        left_join(sci_raw %>% 
                    select(GID_1,
                           x = i),
                  by = 'GID_1') %>% 
        left_join(sci_raw %>% 
                    select(GID_1,
                           y = i),
                  by = join_by(fr_loc == GID_1)) %>% 
        mutate(!!sym(i) := y - x) %>% 
        select(-x, -y),
      by = join_by(GID_1, fr_loc))
}


# Define interaction effects ----------------------------------------------

dhs_controls_interaction <- crossing(dhs_controls, dhs_controls) %>% 
  mutate(interaction = paste0(dhs_controls...1,':',dhs_controls...2)) %>% 
  select(interaction) %>% 
  t %>% 
  as.vector

# Linear regression -------------------------------------------------------

overview <- data.frame()

for(i in dhs_targets[grepl("rh_", dhs_targets)]){
  
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
    adj_r_sci = summary(results)$adj.r.squared,
    adj_r_no_sci = summary(results_no_sci)$adj.r.squared,
    adj_r_lucky = summary(results_lucky)$adj.r.squared,
    n_obs = nobs(results)
  )
  
  overview <- overview %>% 
    bind_rows(temp)
  
}

overview <- overview %>% 
  mutate(diff = adj_r_sci - adj_r_no_sci)



# Linear regression hand-picked -------------------------------------------

i <- 'rh_anc_pv_female_no'

temp <- dat %>% 
  select(all_of(i), dhs_controls, sci_controls, fb_controls) %>% 
  drop_na

results_hand <- lm(paste(i, '~', 
                         paste(dhs_controls, collapse = '+'), '+',
                         paste(sci_controls, collapse = '+'), '+',
                         paste(fb_controls, collapse = '+')),
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
