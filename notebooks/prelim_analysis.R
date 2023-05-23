library(tidyverse)
library(sf)



# Loading relevant data ---------------------------------------------------

### SCI and other data
sci <- read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/combined_dataset/GADM_1_variables.csv")

### DHS data
dhs <- read.csv('/Users/tillkoebe/Documents/GitHub/health_inequalities/combined_dataset/dhs_health.csv')


# Data preparation --------------------------------------------------------

dhs_targets <- c('rh_anc_pv_female_yes',
                 'rh_anc_pv_female_no',
                 'rh_anc_pvskill_female_yes',
                 'rh_anc_pvskill_female_no',
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


dhs_controls <- c('rel_minor',
                  'lang_minor',
                  'poorest',
                  'poorer',
                  'has_mobile_phone_yes',
                  'rural',
                  'muslim',
                  'protestant',
                  'catholic'
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

# Linear regression -------------------------------------------------------

overview <- data.frame()

for(i in dhs_targets){
  results <- lm(paste(i, '~', 
                      paste(dhs_controls, collapse = '+'), '+',
                      paste(sci_controls, collapse = '+'), '+',
                      paste(fb_controls, collapse = '+')), dat)
  
  results_no_sci <- lm(paste(i, '~', 
                             paste(dhs_controls, collapse = '+'), '+',
                             paste(fb_controls, collapse = '+')), dat)
  
  temp <- data.frame(
    target = i,
    adj_r_sci = summary(results)$adj.r.squared,
    adj_r_no_sci = summary(results_no_sci)$adj.r.squared
  )
  
  overview <- overview %>% 
    bind_rows(temp)
  
}

overview <- overview %>% 
  mutate(diff = adj_r_sci - adj_r_no_sci)

# Linear regression with model selection ----------------------------------




# Random Forest -----------------------------------------------------------

init_popfit = tuneRF(
  x=x_data_b, y=y_data, plot=TRUE, mtryStart=length(x_data_b)/3, 
  ntreeTry=length(y_data)/20, improve=0.0001, stepFactor=1.20, trace=F,
  doBest=TRUE, nodesize=length(y_data)/1000, na.action=na.omit, 
  importance=TRUE, proximity=TRUE, sampsize=length(y_data), replace=TRUE
) 


# Principal component analysis --------------------------------------------





