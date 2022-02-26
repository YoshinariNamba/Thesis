
# library
library(tidyverse)

# source
#source("./Rscript/01_append/ap0_master.R")
#source("./Rscript/02_clean/cl1_shusai.R", encoding = "UTF-8")
#source("./Rscript/02_clean/cl2_japic.R", encoding = "UTF-8")
#source("./Rscript/02_clean/cl3_ndb.R", encoding = "UTF-8")


# construction ------------------------------------------------------------

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
  filter(year == FnlYear_exante) %>% 
  # label eff name based on df_jpc
  inner_join(df_code_eff, by = c("code_shusai" = "code_yj")) %>% 
  # extract brand drug that is included in the sample
  filter(code_shusai %in% ls_code_br_smpl) %>% 
  arrange(eff, -total) %>% 
  select(code_shusai, contains("total"))

## combination of firm and count of AG marketed
df_AGrecord <- 
  df_jpc_cl1 %>% 
  filter(year == 2015) %>% 
  # identify markets with AG marketed
  group_by(eff) %>% 
  filter(min(year_listed, na.rm = TRUE) <= FnlYear_exante | is.na(year_listed)) %>% 
  mutate(ag_record = ifelse(sum(ag) > 0, T, F)) %>% 
  ungroup() %>% 
  # count AG by firm
  group_by(maker) %>% 
  summarise(ag_record = length(unique(eff[ag_record])), 
            .groups = "drop") 
  

## brand-specific dataset
df_jpc_br_merged <- 
  df_jpc_cl2 %>% 
  filter(brand) %>% 
  # merge recept data
  left_join(df_recept, by = c("code_yj" = "code_shusai")) %>% 
  filter(year == 2015) %>% 
  # merge firm-specific AG count data
  left_join(df_AGrecord, by = c("maker" = "maker")) %>% 
  # substitute half of minimum value for missing value 
  replace_na(
    replace = list(
      total_in = min(.$total_in, na.rm = T) / 2, 
      total_out = min(.$total_out, na.rm = T) / 2, 
      total_hos = min(.$total_hos, na.rm = T) / 2
    )
  ) %>% 
  mutate(total = total_in + total_out + total_hos) %>% 
  group_by(eff) %>% 
  summarise(
    N_incumbent = length(unique(maker)), 
    incumbent = str_flatten(sort(unique(maker)), collapse = "-"), 
    ag_record = sum(aggregate(x = ag_record, by = list(maker), FUN = mean)$x), 
    across(.cols = c(price1, contains("total")), 
           .fns = ~weighted.mean(.x, w = num_in_unit1/num_in_unit2), 
           .names = "{.col}"), 
    capsule = ifelse(sum(str_detect(form1, "カプセル")) > 0, 1, 0), 
    tablet = ifelse(sum(str_detect(form1, "錠")) > 0, 1, 0), 
    granule = ifelse(sum(str_detect(form1, "粒")) > 0, 1, 0), 
    siroop = ifelse(sum(str_detect(form1, "シロップ")) > 0, 1, 0), 
    liquid = ifelse(sum(str_detect(form1, "液")) > 0, 1, 0), 
    #n_maker = length(unique(maker[form2 == "内服"])), 
    #maker = str_flatten(sort(unique(maker[form2 == "内服"])), collapse = "-"), 
    form_variety = length(unique(form1)), 
    active_variety = length(unique(name_g)), 
    inactive_variety = length(unique(as.vector(str_split(inactive[!is.na(inactive)], "，", simplify = T)))), 
    subst = length(unique(code_eff)) - 1, 
    .groups = "drop"
  ) #%>% 
  #filter(maker != "") %>% 
  #arrange(maker) %>% 
  #mutate(mean_ag_record = ag_record / n_maker)
  

## sample
df_eff <- 
  df_jpc_smpl_agg %>% 
  inner_join(df_jpc_br_merged, by = c("eff" = "eff")) %>% 
  rename(
    Initial_genric = paste0("N_generic_", IntYear_expost), 
    Initial_ag = paste0("N_ag_", IntYear_expost), 
    Final_generic = paste0("N_generic_", 2019), 
    Final_ag = paste0("N_ag_", 2019)
  ) %>% 
  filter(Initial_genric - Initial_ag <= 0)  %>% 
  rename(price = price1) %>% 
  select(-c(contains("N_total"), contains("N_brand"), 
            contains("N_generic"), contains("N_ag")))



