
# library
library(tidyverse)

# source
source("./Rscript/01_append/ap0_master.R")
source("./Rscript/02_clean/cl1_shusai.R", encoding = "UTF-8")
source("./Rscript/02_clean/cl2_japic.R", encoding = "UTF-8")
source("./Rscript/02_clean/cl3_ndb.R", encoding = "UTF-8")

## combination of code and active ingredient
df_code_eff <- 
  df_jpc %>% 
  filter(eff != "") %>% 
  select(code_yj, eff) %>% 
  arrange(code_yj, eff) %>% 
  distinct(code_yj, eff)

## combination of code and quantity as of 2014
df_recept <- 
  df_ndb_cl %>% 
  # fix year for controlling market sizes  
  filter(year == 2014) %>% 
  # label eff name based on df_jpc
  inner_join(df_code_eff, by = c("code_shusai" = "code_yj")) %>% 
  # extract brand drug that is included in the sample
  filter(code_shusai %in% ls_code_br_smpl) %>% 
  arrange(eff, -total) %>% 
  select(code_shusai, contains("total"))

## combination of firm and count of AG marketed
df_brand <- 
  df_jpc_cl1 %>% 
  # identify markets with AG marketed
  group_by(eff) %>% 
  mutate(N_ag_firmspec = ifelse(sum(ag) > 0, T, F)) %>% 
  ungroup() %>% 
  # count AG by firm
  group_by(maker) %>% 
  summarise(N_ag_firmspec = length(unique(eff[N_ag_firmspec])), 
            .groups = "drop") 
  

##
df_jpc_br_merged <- 
  df_jpc_cl2 %>% 
  filter(brand) %>% 
  # merge recept data
  left_join(df_recept, by = c("code_yj" = "code_shusai")) %>% 
  filter(year == 2015) %>% 
  # substitute half of minimum value for missing value 
  replace_na(replace = list(total_in = min(df_ndb$total_in, na.rm = TRUE)/2, 
                            total_out = min(df_ndb$total_out, na.rm = TRUE)/2, 
                            total_hos = min(df_ndb$total_hos, na.rm = TRUE)/2)) %>% 
  mutate(total = total_in + total_out + total_hos) %>% 
  # merge firm-specific AG count data
  left_join(df_brand, by = c("maker" = "maker")) %>% 
  group_by(eff) %>% 
  summarise(
    across(.cols = c(price1, contains("total")), 
           .fns = ~weighted.mean(.x, w = num_in_unit1/num_in_unit2), 
           .names = "{.col}"), 
    capsule = ifelse(sum(str_detect(form1, "カプセル")) > 0, 1, 0), 
    tablet = ifelse(sum(str_detect(form1, "錠")) > 0, 1, 0), 
    granule = ifelse(sum(str_detect(form1, "粒")) > 0, 1, 0), 
    n_maker = length(unique(maker[form2 == "内服"])), 
    maker = str_flatten(sort(unique(maker[form2 == "内服"])), collapse = "-"), 
    N_ag_firmspec = sum(unique(N_ag_firmspec)), 
    .groups = "drop"
  ) %>% 
  filter(maker != "") %>% 
  arrange(maker) %>% 
  mutate(mean_ag_firmspec = N_ag_firmspec / n_maker)
  

##
df_jpc_smpl_agg_merged <- 
  df_jpc_smpl_agg %>% 
  inner_join(df_jpc_br_merged, by = c("eff" = "eff")) %>% 
  filter(N_generic_2015 - N_ag_2015 <= 0)  %>% 
  rename(price = price1)
  



