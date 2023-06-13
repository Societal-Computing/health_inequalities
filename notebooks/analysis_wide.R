library(tidyverse)    # Data wrangling
library(sf)           # Spatial analysis
library(caret)        # For machine learning and preprocessing
library(spdep)        # For spatial analysis
library(spatialreg)   # For spatial regressions

library(sjPlot)       # for tab_model function

select <- dplyr::select

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
                 'FB_pntr_15to49_all')

afr_controls <- c('trpar',
                  'trtax',
                  'trgov',
                  'trlaw',
                  'trgen',
                  'trrel',
                  'trnei',
                  'tracq')

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


# Define model ------------------------------------------------------------

# Prepare data
temp <- dat %>% 
  select(GID_1,
         dhs_targets,
         dhs_controls, 
         sci_controls, 
         fb_controls, 
         # afr_controls,
         # hdi_controls, 
         wp_controls) %>% 
  drop_na

# Define controls
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

# Run models
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

# Map plot dependent variable

# Map plot residuals


# Calculate Moran's I -----------------------------------------------------

# Get spatial data in same shape as analysis
geodat <- afr_dat %>% 
  left_join(temp,
            by = 'GID_1') %>% 
  drop_na(all_of(dhs_targets))

set.ZeroPolicyOption(TRUE)

# Create neighborhood weight matrix
geodat_nb_w <- nb2listw(poly2nb(geodat, queen = T), style = "W", zero.policy = TRUE)

# Moran scatterplot
moran.plot(geodat$rh_anc_pv, listw=geodat_nb_w, 
           xlab="Standardized health outcome", 
           ylab="Neighbors standardized health outcome",
           main=c("Moran Scatterplot for health outcome") )

# Calculate spatial autocorrelation of dependent variable (Global Moran’s I)
moran.mc(geodat$rh_anc_pv, geodat_nb_w, zero.policy=TRUE, nsim=999)

# Calculate spatial autocorrelation of residuals
lm.morantest(fit_rh_anc_pv, geodat_nb_w)


