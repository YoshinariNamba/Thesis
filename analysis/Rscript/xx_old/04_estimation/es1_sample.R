
# library
library(tidyverse)

# source
source("./Rscript/02_clean/cl4_merge.R", encoding = "UTF-8")

# sample based on which estimation is run
df_sample <- 
  df_jpc_smpl_agg_merged %>% 
  mutate(
    AG_dummy = case_when(
      N_ag_2015 > 0 ~ 1, 
      N_ag_2015 == 0 ~ 0
    ), 
    N_entry = N_generic_2020 - N_ag_2020, 
    entry_dummy = ifelse(N_entry > 0, 1, 0), 
    N_entry_f = factor(N_entry), 
    pastAG_dummy = ifelse(N_ag_firmspec > 0, 1, 0), 
    N_br_over2 = ifelse(N_brand_2015 > 2, 1, 0), 
    N_br_over3 = ifelse(N_brand_2015 > 3, 1, 0), 
    revenue = price*total, 
    revenue_hos = price*total_hos
  ) %>% 
  rename(pastAG = N_ag_firmspec, 
         pastAGmean = mean_ag_firmspec)


# save
write_rds(df_sample, "./output/data/sample.rds")

# remove
rm(list = ls())
