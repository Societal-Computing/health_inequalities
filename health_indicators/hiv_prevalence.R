# ******************************************************************************
# Program: 		HV_PREV.R
# Purpose: 		Code for HIV prevalence  
# Data inputs: 		IR or MR survey list
# Data outputs:		coded variables
# Author:		Shireen Assaf - translated to R by Mahmoud Elkasabi
# Date last modified: December 08, 2021 by Mahmoud Elkasabi
# Note:	This is using the merged file IRMRARmerge 
# ******************************************************************************
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# hv_hiv_pos				"HIV positive test result"
# hv_hiv1_pos				"HIV-1 positive test result"
# hv_hiv2_pos				"HIV-2 positive test result"
# hv_hiv1or2_pos			"HIV-1 or HIV-2 positive test result"

# hv_pos_ever_test	"Ever tested for HIV and received result of most recent test among HIV positive"
# hv_pos_12m_test	"Tested in the past 12 months and received result among HIV positive"
# hv_pos_more12m_test	"Tested 12 or more months ago and received result among HIV positive"
# hv_pos_ever_noresult	"Ever tested for HIV and did not receive the result of most recent test among HIV positive"
# hv_pos_nottested	"Not previously tested among HIV positive"

# hv_neg_ever_test	"Ever tested for HIV and received result of most recent test among HIV negative"
# hv_neg_12m_test	"Tested in the past 12 months and received result among HIV negative"
# hv_neg_more12m_test	"Tested 12 or more months ago and received result among HIV negative"
# hv_neg_ever_noresult	"Ever tested for HIV and did not receive the result of most recent test among HIV negative"
# hv_neg_nottested	"Not previously tested among HIV negative"

# -----------------------------------------------------------------------------#

IRMRARmerge <- IRdata %>% 
  select(caseid,
         wt,
         v000,
         v001,
         v002,
         v003,
         v013,
         v023,
         v024,
         v025,
         v130,
         v131,
         v169a,
         v190,
         hid = hhid) %>% 
  bind_rows(
    MRdata %>% 
      rename_with(., ~ sub(".", "", .x), .cols = c(mcaseid, mv000:mv190, mv169a)) %>% 
      select(caseid,
             wt,
             v000,
             v001,
             v002,
             v003,
             v013,
             v023,
             v024,
             v025,
             v130,
             v131,
             v169a,
             v190,
             hid = hhid)) %>% 
  left_join(ARdata %>% 
              select(hivclust, hivnumb, hivline, hiv03, hiv05) %>% 
              rename(v001 = hivclust,
                     v002 = hivnumb,
                     v003 = hivline,
                     v005 = hiv05
              ),
            by = c('v001', 'v002', 'v003'))


#HIV positive result
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv_pos = case_when(
    hiv03==1  ~ 1,
    TRUE ~ 0),
    hv_hiv_pos = set_value_labels(hv_hiv_pos, labels = c("No"=0, "Yes"=1)),
    hv_hiv_pos = set_variable_labels(hv_hiv_pos, label = "HIV positive test result"))

#HIV-1 positive result
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv1_pos = case_when(
    hiv03==1 | hiv03==3   ~ 1,
    TRUE ~ 0),
    hv_hiv1_pos = set_value_labels(hv_hiv1_pos, labels = c("No"=0, "Yes"=1)),
    hv_hiv1_pos = set_variable_labels(hv_hiv1_pos, label = "HIV-1 positive test result"))

#HIV-2 positive result
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv2_pos = case_when(
    hiv03==2  ~ 1,
    TRUE ~ 0),
    hv_hiv2_pos = set_value_labels(hv_hiv2_pos, labels = c("No"=0, "Yes"=1)),
    hv_hiv2_pos = set_variable_labels(hv_hiv2_pos, label = "HIV-2 positive test result"))

#HIV-1 or HIV-2 positive result
IRMRARmerge <- IRMRARmerge %>%
  mutate(hv_hiv1or2_pos = case_when(
    hiv03==1 | hiv03==3 | hiv03==2  ~ 1,
    TRUE ~ 0),
    hv_hiv1or2_pos = set_value_labels(hv_hiv1or2_pos, labels = c("No"=0, "Yes"=1)),
    hv_hiv1or2_pos = set_variable_labels(hv_hiv1or2_pos, label = "HIV-1 or HIV-2 positive test result"))


HVdata <- IRMRARmerge %>% 
  select(caseid,
         wt,
         v000,
         v001,
         v013,
         v023,
         v024,
         v025,
         v130,
         v131,
         v169a,
         v190,
         hv_hiv_pos,				# "HIV positive test result"
         hv_hiv1_pos,				# "HIV-1 positive test result"
         hv_hiv2_pos,				# "HIV-2 positive test result"
         hv_hiv1or2_pos    # "HIV-1 or HIV-2 positive test result"
  )
