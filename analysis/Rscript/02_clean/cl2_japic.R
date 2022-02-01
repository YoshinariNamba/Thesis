
# library
library(tidyverse)
library(lubridate)
library(magrittr)

# source
source("./Rscript/01_append/ap0_master.R")
source("./Rscript/02_clean/cl1_shusai.R", encoding = "UTF-8")

# cleaning

## ag list
ls_name_ag <- 
  read_csv("./dataset/list/ag_list.csv", locale = locale(encoding = "Shift_JIS")) %>% 
  use_series(ag) %>% 
  unique() %>% 
  sort()


## dataset which covers medicine listed "between 1990-2000"
df_jpc_cl1 <- 
  df_jpc %>% 
  ## form
  mutate(form1 = str_split(.$form, pattern = "：", simplify = TRUE)[, 1], 
         form2_tmp = str_split(.$form, pattern = "：", simplify = TRUE)[, 2]) %>% 
  mutate(form2 = str_split(.$form2_tmp, pattern = "〔", simplify = TRUE)[, 1], 
         form3 = str_replace(str_split(.$form2_tmp, pattern = "〔", simplify = TRUE)[, 2], 
                             pattern = "〕", replacement = "")) %>% 
  select(-form2_tmp) %>% 
  mutate(note1 = str_extract(other, pattern = "薬価収載|薬価削除予定|薬価削除"),
         note2 = str_extract(other, pattern = "(\\d{4}).(\\d{2}).(\\d{2})"), 
         note3 = str_split(.$other, pattern = "，", simplify = TRUE)[, 2]) %>% 
  mutate(note2 = ymd(note2))  %>% 
  mutate(price = ifelse(price == "", NA, price)) %>% 
  mutate(price1 = str_split(price, pattern = "／", simplify = TRUE)[, 1], 
         price2 = str_split(price, pattern = "／", simplify = TRUE)[, 2]) %>% 
  mutate(price1 = str_replace(price1, pattern = ",", replacement = ""), 
         price1 = str_replace(price1, pattern = "円", replacement = ""), 
         price1 = as.numeric(price1)) %>% 
  mutate(brand = ifelse(code_yj %in% ls_code_br_shusai, T, F), # identify brand based on shusai 
         ag = ifelse(name %in% ls_name_ag, T, F)) %>%   # identify ag
  mutate(year_listed = str_split(as.character(.$note2), pattern = "-", simplify = TRUE)[, 1], 
         year_listed = as.numeric(year_listed)) %>% 
  filter(eff != "", 
         !str_detect(name, pattern = "削除品")) %>% 
  mutate(unit_tmp = str_replace_all(unit, "0\\.", ""), 
         unit_tmp = str_replace_all(unit_tmp, "\\.\\d+", ""), 
         unit_tmp = str_replace_all(unit_tmp, ",", ""),
         num_in_name = str_extract(name, "\\d+"), 
         num_in_unit1 = str_split(unit_tmp, "\\D+", simplify = TRUE)[, 1],
         num_in_unit2 = str_split(unit_tmp, "\\D+", simplify = TRUE)[, 2]) %>% 
  mutate(across(.cols = starts_with("num_in_"),
                .fns = as.numeric, 
                .names = "{.col}")) %>% 
  mutate(num_in_unit2 = ifelse(is.na(num_in_unit2), 1, num_in_unit2), 
         num_in_unit1_tmp = ifelse(is.na(num_in_unit1), num_in_unit2, num_in_unit1), 
         num_in_unit2_tmp = ifelse(is.na(num_in_unit1), 1, num_in_unit2), 
         num_in_unit1 = num_in_unit1_tmp, 
         num_in_unit2 = num_in_unit2_tmp) %>% 
  select(-c(num_in_unit1_tmp, num_in_unit2_tmp)) 


df_jpc_cl2 <- 
  df_jpc_cl1 %>% 
  filter(is.na(year_listed) | year_listed <= 2012) %>% 
  filter(!is.na(year_listed) & year_listed >= 1990) %>% 
  arrange(year, eff, form)


# list --------------------------------------------------------------------

ls_code_br_smpl <- 
  df_jpc_cl2 %>% 
  filter(brand) %>% 
  use_series(code_yj) %>% 
  unique() %>% 
  sort()


ls_eff_smpl <- 
  df_jpc_cl2 %>% 
  filter(code_yj %in% ls_code_br_smpl) %>% 
  use_series(eff) %>% 
  unique() %>% 
  sort()


# sample ------------------------------------------------------------------

## sample
df_jpc_smpl <- 
  df_jpc_cl1 %>% 
  filter(eff %in% ls_eff_smpl) %>%  
  group_by(year, eff) %>% 
  mutate(old_identity = if_else(anyNA(note1), T, F)) %>% 
  ungroup() %>% 
  filter(!old_identity, 
         note1 != "薬価削除", 
         form2 == "内服") %>% 
  select(- old_identity) %>% 
  arrange(eff, year, form, brand, ag) %>% 
  mutate(num_in_unit2 = ifelse(is.na(num_in_unit2), 1, num_in_unit2)) %>% 
  group_by(year, eff, maker, brand, ag) %>% 
  summarize(
    price_sum = weighted.mean(price1, w = num_in_unit1/num_in_unit2), 
    .groups = "drop")

## 
df_jpc_smpl_agg <- 
  df_jpc_smpl %>% 
  group_by(year, eff) %>% 
  summarise(
    N_total = n(), 
    N_brand = sum(brand), 
    N_ag = sum(ag), 
    .groups = "drop"
  ) %>% 
  mutate(N_generic = N_total - N_brand) %>% 
  filter(N_brand > 0) %>% 
  pivot_wider(id_cols = c(eff), 
              names_from = year, 
              names_glue = "{.value}_{year}", 
              values_from = c(N_total, N_brand, N_generic, N_ag)) %>% 
  na.omit() 





