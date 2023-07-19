library(tidyverse)    # Data wrangling
library(sf)           # Spatial analysis
library(caret)        # For machine learning and preprocessing
library(spdep)        # For spatial analysis
library(spatialreg)   # For spatial regressions
library(olsrr)        # For OLS residual diagnostics
library(lme4)         # For random effects models

library(sjPlot)       # for tab_model function

select <- dplyr::select

# Loading relevant data ---------------------------------------------------

# Map data
afr_dat <- st_read(dsn = "/Users/tillkoebe/Documents/GitHub/health_inequalities/combined_dataset/GADM_1_geometries.gpkg") %>% 
  st_make_valid() %>% 
  st_transform(4326)

# Social Connectedness Index
sci <- read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/sci_indices.csv")

# Worldpop covariates
wp <- read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/wp.csv") %>% 
  distinct(GID_1, .keep_all = T)

# Human Development Index
hdi <- read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/hdi.csv")

# Facebook covariates
fb <- read.csv("/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/fb.csv") %>% 
  distinct(GID_1, .keep_all = T)

# DHS data
dhs <- read.csv('/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/dhs_health.csv')

# Afrobarometer
afr <- read.csv('/Users/tillkoebe/Documents/GitHub/health_inequalities/external_dataset/afrobarometer.csv') %>% 
  distinct(GID_1, .keep_all = T)

# # Raw SCI
# sci_raw <- read.csv("https://data.humdata.org/dataset/e9988552-74e4-4ff4-943f-c782ac8bca87/resource/5de92e01-606e-4e4d-ad7c-4a3d493a0cc3/download/gadm1_nuts2-gadm1_nuts2-fb-social-connectedness-index-october-2021.tsv", sep = "\t") %>% 
#   filter(user_loc %in% unique(dhs$GID_1),
#          fr_loc %in% unique(dhs$GID_1))

# Data preparation --------------------------------------------------------

### Define targets
dhs_targets <- dhs %>% 
  select(rh_anc_pv:we_num_justifydv) %>% 
  select(starts_with(c('rh', 'fp'))) %>%
  select(-fp_message_noneof4) %>% 
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
  "X.0.15.",
  "X.15.20.",
  "X.20.25.",
  "X.25.30.",
  "X.30.35.",
  "X.35.40.",
  "X.40.45.",
  "X.45.50.",
  "X.50.inf." 
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

# Prepare dataset
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
  mutate(iso3 = substr(GID_1, 1, 3))


# Preprocess --------------------------------------------------------------

# Center and scale SCI variables
dat <- dat %>%
  mutate(across(all_of(sci_controls), ~ scale(.x, scale = T)),
         across(all_of(wp_controls), ~ scale(.x, scale = T)),
         # across(all_of(afr_controls), ~ scale(.x, scale = T))
         )

# Define interaction effects
sci_dhs_interaction <- crossing(sci_controls, dhs_controls) %>% 
  mutate(interaction = paste0(sci_controls,':',dhs_controls)) %>% 
  select(interaction) %>% 
  t %>% 
  as.vector

# Prepare data
temp <- dat %>% 
  select(GID_1,
         all_of(dhs_targets),
                all_of(dhs_controls), 
                       all_of(sci_controls), 
                              all_of(fb_controls), 
         # all_of(afr_controls),
         all_of(hdi_controls),
         all_of(wp_controls),
         iso3) %>% 
  drop_na

# Neighborhood matrix for spatial dependencies ----------------------------

# Get spatial data in same shape as analysis
geodat <- afr_dat %>% 
  left_join(temp,
            by = 'GID_1') %>% 
  drop_na(all_of(dhs_targets)) %>%
  st_make_valid()

set.ZeroPolicyOption(TRUE)

# Create neighborhood matrix
### by adjacency of polygons
nb <- poly2nb(geodat, queen = T)

### by distance of centroids
nb <- dnearneigh(st_centroid(geodat), d1 = 0, d2 = 10, use_s2 = T, longlat = T)

# Create neighborhood weight matrix
nb_w <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Define model ------------------------------------------------------------

# Define controls
controls <- c('Mean_friendship + 
                poorest +
                poorer +
                female +
                secondary_or_higher + 
                has_mobile_phone_yes +
                rural +
                X.0.15. +
                X.15.20. +
                X.20.25. +
                X.25.30. +
                X.30.35. +
                X.35.40. +
                X.40.45. +
                X.45.50. +
                X.50.inf. +
                Mean_of_Night_Light +
                Mean_distance_to_major_rd +
                FB_pntr_15to49_all')

interactions <- ('female:has_mobile_phone_yes +
                female:Mean_friendship +
                has_mobile_phone_yes:Mean_friendship')

# Run models
### Standard OLS
fit_rh_anc_pv <- lm(paste('rh_anc_pv ~',controls), temp)
fit_rh_anc_pvskill <- lm(paste('rh_anc_pvskill ~',controls), temp)
fit_fp_know_mod <- lm(paste('fp_know_mod ~',controls), temp)
fit_fp_message_noneof3 <- lm(paste('fp_message_noneof3 ~',controls), temp)

### Arcsine
fit_rh_anc_pv_as <- lm(paste('asin(sqrt(rh_anc_pv)) ~',controls), temp)
fit_rh_anc_pvskill_as <- lm(paste('asin(sqrt(rh_anc_pvskill)) ~',controls), temp)
fit_fp_know_mod_as <- lm(paste('asin(sqrt(fp_know_mod)) ~',controls), temp)
fit_fp_message_noneof3_as <- lm(paste('asin(sqrt(fp_message_noneof3)) ~',controls), temp)

### Standard OLS w/ interactions
fit_rh_anc_pv_ia <- lm(paste('rh_anc_pv ~',controls,'+',interactions), temp)
fit_rh_anc_pvskill_ia <- lm(paste('rh_anc_pvskill ~',controls,'+',interactions), temp)
fit_fp_know_mod_ia <- lm(paste('fp_know_mod ~',controls,'+',interactions), temp)
fit_fp_message_noneof3_ia <- lm(paste('fp_message_noneof3 ~',controls,'+',interactions), temp)

### OLS with Country Fixed Effect
fit_rh_anc_pv_fe <- lm(paste('rh_anc_pv ~',controls,'+ iso3'), temp)
fit_rh_anc_pvskill_fe <- lm(paste('rh_anc_pvskill ~',controls,'+ iso3'), temp)
fit_fp_know_mod_fe <- lm(paste('fp_know_mod ~',controls,'+ iso3'), temp)
fit_fp_message_noneof3_fe <- lm(paste('fp_message_noneof3 ~',controls,'+ iso3'), temp)

### OLS with Country Fixed Effect and interactions
fit_rh_anc_pv_fe_ia <- lm(paste('rh_anc_pv ~',controls,'+',interactions,'+ iso3'), temp)
fit_rh_anc_pvskill_fe_ia <- lm(paste('rh_anc_pvskill ~',controls,'+',interactions,'+ iso3'), temp)
fit_fp_know_mod_fe_ia <- lm(paste('fp_know_mod ~',controls,'+',interactions,'+ iso3'), temp)
fit_fp_message_noneof3_fe_ia <- lm(paste('fp_message_noneof3 ~',controls,'+',interactions,'+ iso3'), temp)

### OLS with Random Intercept
fit_rh_anc_pv_ri <- lmer(paste('rh_anc_pv ~',controls,' + (1 | iso3)'), data = temp)
fit_rh_anc_pvskill_ri <- lmer(paste('rh_anc_pvskill ~',controls,' + (1 | iso3)'), data = temp)
fit_fp_know_mod_ri <- lmer(paste('fp_know_mod ~',controls,' + (1 | iso3)'), data = temp)
fit_fp_message_noneof3_ri <- lmer(paste('fp_message_noneof3 ~',controls,' + (1 | iso3)'), data = temp)

### OLS with Random Intercept and interactions
fit_rh_anc_pv_ri_ia <- lmer(paste('rh_anc_pv ~',controls,'+',interactions,' + (1 | iso3)'), data = temp)
fit_rh_anc_pvskill_ri_ia <- lmer(paste('rh_anc_pvskill ~',controls,'+',interactions,' + (1 | iso3)'), data = temp)
fit_fp_know_mod_ri_ia <- lmer(paste('fp_know_mod ~',controls,'+',interactions,' + (1 | iso3)'), data = temp)
fit_fp_message_noneof3_ri_ia <- lmer(paste('fp_message_noneof3 ~',controls,'+',interactions,' + (1 | iso3)'), data = temp)

tab_model(fit_fp_know_mod, fit_fp_know_mod_as, fit_fp_know_mod_ia, 
          fit_fp_know_mod_fe, fit_fp_know_mod_fe_ia, 
          fit_fp_know_mod_ri, fit_fp_know_mod_ri_ia)

tab_model(fit_rh_anc_pv_fe, fit_rh_anc_pv_fe_ia,
          fit_rh_anc_pvskill_fe, fit_rh_anc_pvskill_fe_ia,
          fit_fp_message_noneof3_fe, fit_fp_message_noneof3_fe_ia,
          fit_fp_know_mod_fe, fit_fp_know_mod_fe_ia)

tab_model(fit_rh_anc_pv_fe, fit_rh_anc_pv_fe_ia,
          fit_rh_anc_pvskill_fe, fit_rh_anc_pvskill_fe_ia,
          fit_fp_message_noneof3_fe, fit_fp_message_noneof3_fe_ia,
          fit_fp_know_mod_fe, fit_fp_know_mod_fe_ia)


# Diagnostics -------------------------------------------------------------

'
Recalling OLS assumptions for OLS estimator to be BLUE (best linear unbiased estimator):

1. Linearity: The linear regression model is linear in parameters
2. Independent and identically distributed: There is a random sampling of observations
3. Strict exogeneity: The conditional mean should be zero
4. No linear dependence: There is no multi-collinearity (or perfect collinearity)
5. Spherical errors: There is homoscedasticity and no autocorrelation
6. Normality: Error terms should be normally distributed
'


# Regression output
tab_model(fit_ch_allvac_either, fit_rh_anc_pv, fit_rh_anc_pvskill, fit_fp_know_mod)
tab_model(fit_ch_allvac_either_ll, fit_rh_anc_pv_ll, fit_rh_anc_pvskill_ll, fit_fp_know_mod_ll)
tab_model(fit_ch_allvac_either_as, fit_rh_anc_pv_as, fit_rh_anc_pvskill_as, fit_fp_know_mod_as)
tab_model(fit_fp_know_mod, fit_fp_know_mod_as, fit_fp_know_mod_fe, fit_fp_know_mod_ri)

# Model fit
AIC(fit_ch_allvac_either, fit_rh_anc_pv, fit_rh_anc_pvskill, fit_fp_know_mod)
AIC(fit_ch_allvac_either, fit_ch_allvac_either_ll, fit_ch_allvac_either_as, fit_ch_allvac_either_ri, fit_ch_allvac_either_sl, fit_ch_allvac_either_se)
AIC(fit_rh_anc_pv, fit_rh_anc_pv_ll, fit_rh_anc_pv_as, fit_rh_anc_pv_ri, fit_rh_anc_pv_sl, fit_rh_anc_pv_se)
AIC(fit_rh_anc_pvskill, fit_rh_anc_pvskill_ll, fit_rh_anc_pvskill_as, fit_rh_anc_pvskill_ri, fit_rh_anc_pvskill_sl, fit_rh_anc_pvskill_se)
AIC(fit_fp_know_mod, fit_fp_know_mod_ll, fit_fp_know_mod_as, fit_fp_know_mod_ri, fit_fp_know_mod_sl, fit_fp_know_mod_se)


# Checking for normality of the residuals

### Fitted vs. residuals
plot(fit_fp_know_mod_as)

# QQ-Plot of residuals
qqPlot(fit_fp_know_mod_as)

# Histogram of residuals
hist(residuals(fit_fp_know_mod_as), 
     main = "Residual Histogram", prob=TRUE)
lines(density(residuals(fit_fp_know_mod_as)))

### Testing for normality
ols_test_normality(residuals(fit_fp_know_mod))
ols_test_normality(residuals(fit_fp_know_mod_ll))
ols_test_normality(residuals(fit_fp_know_mod_as))
ols_test_normality(residuals(fit_fp_know_mod_ri))

# Histogram of dependent variable
dat %>%
  ggplot() +
  geom_histogram(aes(x=(asin(sqrt(fp_know_mod))))) +
  xlab("Share of children with full vaccination records")

# Checking for heteroscedasticity

### Visualizing heteroscedasticity
ols_plot_resid_fit(fit_fp_know_mod_as)

### Testing for heteroscedascity
'
Breusch-Pagan H_0 is independency of errors to values of predictors.
Low p-value -> heteroscedastic residuals
'
ols_test_breusch_pagan(fit_fp_know_mod_as)
ols_test_breusch_pagan(fit_fp_know_mod_ri)

# Identifying influential observations
ols_plot_cooksd_bar(fit_fp_know_mod_as)
ols_plot_resid_lev(fit_fp_know_mod_as)
ols_plot_resid_stud_fit(fit_fp_know_mod_as)


# Boxplot of residuals
boxplot(fit_fp_know_mod_as$residuals, main="Residual Box Plot")

# Moran scatterplot
moran.plot(asin(sqrt(geodat$fp_know_mod)), listw=nb_w, 
           xlab="Standardized health outcome", 
           ylab="Neighbors standardized health outcome",
           main=c("Moran Scatterplot for health outcome") )

# Calculate spatial autocorrelation of dependent variable (Global Moran’s I)
moran.mc(asin(sqrt(geodat$fp_know_mod)), nb_w, zero.policy=TRUE, nsim=999)

# Calculate spatial autocorrelation of residuals
lm.morantest(fit_fp_know_mod_as, nb_w)

# create empty vectors for holding the Moran statistics
moran_I_lm <- c()
moran_I_ll <- c()
moran_I_as <- c()
moran_I_ri <- c()
moran_I_sl <- c()
moran_I_se <- c()

# loop through a distance vector d ranging from 50 to 2000 m
dist_start = 1
dist_stop = 50
dist_step = 1

for (d in seq(dist_start, dist_stop, dist_step)) {
  nb <- dnearneigh(st_centroid(geodat), d1 = 0, d2 = d, use_s2 = T, longlat = T)
  nb_w <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  moran_lm <- moran.mc(residuals(fit_fp_know_mod), nb_w, nsim = 999, zero.policy = TRUE)
  moran_I_lm <- c(moran_I_lm, moran_lm$statistic)
  
  moran_ll <- moran.mc(residuals(fit_fp_know_mod_ll), nb_w, nsim = 999, zero.policy = TRUE)
  moran_I_ll <- c(moran_I_ll, moran_ll$statistic)
  
  moran_as <- moran.mc(residuals(fit_fp_know_mod_as), nb_w, nsim = 999, zero.policy = TRUE)
  moran_I_as <- c(moran_I_as, moran_as$statistic)
  
  moran_ri <- moran.mc(residuals(fit_fp_know_mod_ri), nb_w, nsim = 999, zero.policy = TRUE)
  moran_I_ri <- c(moran_I_ri, moran_ri$statistic)
  
  moran_sl <- moran.mc(residuals(fit_fp_know_mod_sl), nb_w, nsim = 999, zero.policy = TRUE)
  moran_I_sl <- c(moran_I_sl, moran_sl$statistic)
  
  moran_se <- moran.mc(residuals(fit_fp_know_mod_se), nb_w, nsim = 999, zero.policy = TRUE)
  moran_I_se <- c(moran_I_se, moran_se$statistic)
}

moran_df_lm  <- data.frame(moran_I = moran_I_lm, 
                          distance = seq(dist_start, dist_stop, dist_step), 
                          model="Simple linear model")

moran_df_ll <- data.frame(moran_I = moran_I_ll, 
                          distance = seq(dist_start, dist_stop, dist_step), 
                          model="Log-linear")

moran_df_as <- data.frame(moran_I = moran_I_as, 
                          distance = seq(dist_start, dist_stop, dist_step), 
                          model="Arcsine")

moran_df_ri <- data.frame(moran_I = moran_I_ri, 
                          distance = seq(dist_start, dist_stop, dist_step), 
                          model="Random intercept model")

moran_df_sl <- data.frame(moran_I = moran_I_sl, 
                          distance = seq(dist_start, dist_stop, dist_step), 
                          model="Spatial lag model")

moran_df_se <- data.frame(moran_I = moran_I_se, 
                          distance = seq(dist_start, dist_stop, dist_step), 
                          model="Spatial error model")

moran.df <- rbind(moran_df_lm, moran_df_ll, moran_df_as, moran_df_ri, moran_df_sl, moran_df_se)

ggplot(moran.df, aes(x = distance, y = moran_I, col=model)) + 
  geom_point() +
  geom_line()
