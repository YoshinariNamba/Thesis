
# library
library(tidyverse)
library(lubridate)
library(magrittr)

# source
#source("./Rscript/01_append/ap0_master.R")
#source("./Rscript/02_clean/cl1_shusai.R", encoding = "UTF-8")



# preparation -------------------------------------------------------------

## ag list
ls_name_ag <- 
  read_csv("./dataset/list/ag_list.csv", locale = locale(encoding = "Shift_JIS")) %>% 
  use_series(ag) %>% 
  unique() %>% 
  sort()

## define term to analyze
if(! "FnlYear_exante" %in% ls() && ! "IntYear_expost" %in% ls()){
  FnlYear_exante <- 2014 # final year in ex ante term
  IntYear_expost <- 2017 # initial year in ex post term
}

# cleaning ----------------------------------------------------------------

## dataset where 削除品 (and effect missing obs) are omitted
df_jpc_cl1 <- 
  df_jpc %>% 
  ## split form 
  mutate(form1 = str_split(.$form, pattern = "：", simplify = TRUE)[, 1], 
         form2_tmp = str_split(.$form, pattern = "：", simplify = TRUE)[, 2]) %>% 
  mutate(form2 = str_split(.$form2_tmp, pattern = "〔", simplify = TRUE)[, 1], 
         form3 = str_replace(str_split(.$form2_tmp, pattern = "〔", simplify = TRUE)[, 2], 
                             pattern = "〕", replacement = "")) %>% 
  select(-form2_tmp) %>% 
  ## listed date
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
  ## define listed year
  mutate(year_listed = str_split(as.character(.$note2), pattern = "-", simplify = TRUE)[, 1], 
         year_listed = as.numeric(year_listed)) %>% 
  ## identify brand and AG
  mutate(brand = ifelse(code_yj %in% ls_code_br_shusai, T, F),
         ag = ifelse(name %in% ls_name_ag, T, F)) %>% 
  ## omit 削除品
  filter(#eff != "", 
         !str_detect(name, pattern = "削除品")) %>%
  ## extract values that will be used to aggregate price and sales based on strength
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
  select(-c(num_in_unit1_tmp, num_in_unit2_tmp)) %>% 
  mutate(inactive = ifelse(inactive == "", NA, inactive))


# listed lag --------------------------------------------------------------

# lag
df_listlag <- 
  df_jpc_cl1 %>% 
  filter(year == 2021) %>% 
  # filter(!note1 %in% c("薬価削除", "薬価削除予定")) %>% 
  mutate(
    remove_id = case_when(
      note1 == "薬価収載" ~ T, 
      note1 != "薬価収載" | is.na(note1) ~ F
    )
  ) %>% 
  group_by(name_g) %>% 
  filter(
    #!anyNA(year_listed),
    n() == sum(remove_id), 
    sum(brand) > 0, 
    sum(!brand) > 0
  ) %>% 
  summarise(
    listed_brand = min(year_listed[brand]), 
    listed_generic = min(year_listed[!brand]), 
    .groups = "drop"
  ) %>% 
  mutate(listed_lag = listed_generic - listed_brand) %>% 
  arrange(listed_lag) 

# mean value
listlag_mean <- 
  df_listlag$listed_lag %>% 
  mean() %>% 
  round()



# sample selection --------------------------------------------------------

## dataset includes medicine listed 1990 ~ (IntYear_expost - listlag_mean)
df_jpc_cl2 <- 
  df_jpc_cl1 %>% 
  # omit obs listed after 2013
  filter(is.na(year_listed) | year_listed <= IntYear_expost - listlag_mean) %>% 
  # omit obs lister before 1990
  filter(!is.na(year_listed) & year_listed >= 1990) %>% 
  arrange(year, name_g, form)


## brand drugs' list to be analyzed
ls_code_br_smpl <- 
  df_jpc_cl2 %>% 
  # extract brand
  filter(brand) %>% 
  use_series(code_yj) %>% 
  unique() %>% 
  sort()

## active ingredient (generic name) list to be analyzed
ls_active_smpl <- 
  df_jpc_cl2 %>% 
  # extract active ingredient included in brand drug to be analyzed
  filter(code_yj %in% ls_code_br_smpl) %>% 
  use_series(name_g) %>% 
  unique() %>% 
  sort()

## unit of obs is "year * name_g * maker"
df_jpc_smpl <- 
  df_jpc_cl1 %>% 
  # extract active ingredient to be analyzed
  filter(name_g %in% ls_active_smpl) %>%  
  # omit old market, medicine scheduled to be removed, other than 内用薬
  group_by(year, name_g) %>% 
  mutate(old_identity = if_else(anyNA(note1), T, F)) %>% 
  ungroup() %>% 
  filter(!old_identity, 
         note1 != "薬価削除", 
         form2 == "内服") %>% 
  select(- old_identity) %>% 
  # detect first firm that marketed the brand
  group_by(year, name_g) %>% 
  mutate(
    first_brand = case_when(
      sum(brand) == 0 ~ F, 
      brand & note2 == min(note2[brand], na.rm = T) ~ T, 
      !brand | note2 != min(note2[brand], na.rm = T) | is.na(note2) ~ F
    )
  ) %>% 
  ungroup() %>% 
  # aggregate price
  arrange(name_g, year, form, brand, ag) %>% 
  ## construct weight
  mutate(num_in_unit2 = ifelse(is.na(num_in_unit2), 1, num_in_unit2)) %>% 
  group_by(year, name_g, maker, brand, ag) %>% 
  summarize(
    price_sum = weighted.mean(price1, w = num_in_unit1/num_in_unit2), 
    brand_first = ifelse(sum(first_brand) > 0, T, F), 
    #form_variety = length(unique(form1)), 
    #inactive_variety = length(unique(as.vector(str_split(inactive[!is.na(inactive)], "，", simplify = T)))), 
    #subst = length(unique(eff)) - 1, 
    .groups = "drop"
  )

## unit of obs is "name_g" (wide form data frame with respect to "year")
df_jpc_smpl_agg <- 
  df_jpc_smpl %>% 
  arrange(year, name_g, maker) %>% 
  # count brand, AG, and total entrants
  group_by(year, name_g) %>% 
  summarise(
    N_total = n(), 
    N_brand = sum(brand), 
    N_ag = sum(ag), 
    #N_incumbent = sum(brand), 
    #N_incumbent_first = sum(brand_first), 
    #incumbent = str_flatten(sort(maker[brand]), collapse = "-"), 
    #incumbent_first = str_flatten(sort(maker[brand_first]), collapse = "-"), 
    .groups = "drop"
  ) %>% 
  # remove markets any generics has already entered
  mutate(N_generic = N_total - N_brand) %>% 
  filter(N_brand > 0, 
         year <= 2019, 
         year >= IntYear_expost) %>% 
  # transform long-formed data frame into wide-formed one
  pivot_wider(id_cols = c(name_g#, 
                          #contains("incumbent")
                          ), 
              names_from = year, 
              names_glue = "{.value}_{year}", 
              values_from = c(N_total, N_brand, N_generic, N_ag)) %>% 
  na.omit() 





