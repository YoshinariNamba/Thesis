
# library
library(tidyverse)

# source
source("./Rscript/01_append/ap0_master.R")
source("./Rscript/02_clean/cl1_shusai.R", encoding = "UTF-8")
source("./Rscript/02_clean/cl2_japic.R", encoding = "UTF-8")
source("./Rscript/02_clean/cl3_ndb.R", encoding = "UTF-8")

##
df_code_eff <- 
  df_jpc %>% 
  filter(eff != "") %>% 
  select(code_yj, eff) %>% 
  distinct(code_yj, eff)

##
df_recept <- 
  df_ndb_cl %>% 
  filter(year == 2014) %>% 
  inner_join(df_code_eff, by = c("code_shusai" = "code_yj")) %>% 
  filter(code_shusai %in% ls_code_br_smpl) %>% 
  arrange(eff, -total) %>% 
  select(code_shusai, contains("total"))

##
df_brand <- 
  df_jpc_cl1 %>% 
  group_by(eff) %>% 
  mutate(N_ag_firmspec = ifelse(sum(ag) > 0, T, F)) %>% 
  ungroup() %>% 
  group_by(maker) %>% 
  summarise(N_ag_firmspec = length(unique(eff[N_ag_firmspec])), 
            .groups = "drop")
  

##
df_jpc_br_merged <- 
  df_jpc_cl2 %>% 
  filter(brand) %>% 
  left_join(df_recept, by = c("code_yj" = "code_shusai")) %>% 
  filter(year == 2015) %>% 
  replace_na(replace = list(total = min(df_ndb$total, na.rm = TRUE)/2, 
                            total_in = min(df_ndb$total_in, na.rm = TRUE)/2, 
                            total_out = min(df_ndb$total_out, na.rm = TRUE)/2, 
                            total_hos = min(df_ndb$total_hos, na.rm = TRUE)/2)) %>% 
  group_by(eff) %>% 
  left_join(df_brand, by = c("maker" = "maker")) %>% 
  arrange(maker) %>% 
  summarise(
    across(.cols = c(price1, contains("total")), 
           .fns = ~weighted.mean(.x, w = num_in_unit1/num_in_unit2), 
           .names = "{.col}"), 
    capsule = ifelse(sum(str_detect(form1, "カプセル")) > 0, 1, 0), 
    tablet = ifelse(sum(str_detect(form1, "錠")) > 0, 1, 0), 
    granule = ifelse(sum(str_detect(form1, "粒")) > 0, 1, 0), 
    maker = str_flatten(sort(unique(maker[form2 == "内服"])), collapse = "-"), 
    N_ag_firmspec = sum(N_ag_firmspec)
  )

##
df_jpc_smpl_agg_merged <- 
  df_jpc_smpl_agg %>% 
  left_join(df_jpc_br_merged, by = c("eff" = "eff")) %>% 
  filter(N_generic_2015 - N_ag_2015 <= 0)  %>% 
  rename(price = price1)
  



