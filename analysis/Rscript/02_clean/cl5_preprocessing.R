
# library
library(tidyverse)

# preprocessing -----------------------------------------------------------

# sample based on which estimation is run
df_sample_pre <- 
  df_active %>% 
  rename(active = name_g, 
         pastAG = ag_record) %>% 
  mutate(
    N_entry = Final_generic - Final_ag, 
    N_entry_f = factor(N_entry), 
    entry_dummy = ifelse(N_entry > 0, 1, 0), 
    pastAG_dummy = ifelse(pastAG > 0, 1, 0), 
    pastAG_mean = pastAG / N_incumbent, 
    N_inc_over2 = ifelse(N_incumbent >= 2, 1, 0), 
    N_inc_over3 = ifelse(N_incumbent >= 3, 1, 0), 
    # ln_revenue = log(price*total), 
    # ln_revenue_hos = log(price*total_hos), 
    across(
      .cols = c(starts_with("revenue"), starts_with("subst_rev")), 
      .fns = ~log(.x + 1), 
      .names = "ln_{.col}"
    )
  ) %>% 
  select(active, incumbent, 
         c(N_entry, N_entry_f, entry_dummy), 
         c(pastAG, pastAG_mean, pastAG_dummy), 
         c(N_incumbent, N_inc_over2, N_inc_over3), 
         c(price, starts_with("ln_revenue")), 
         c(subst_act, subst_inc, starts_with("ln_subst")), 
         c(capsule, tablet, granule, siroop, liquid), 
         c(form_variety, inactive_variety))
