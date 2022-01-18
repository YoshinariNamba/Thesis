
# library
library(tidyverse)
library(lubridate)
library(magrittr)

# source
source("./Rscript/01_append/ap0_master.R")
source("./Rscript/02_clean/cl1_shusai.R", encoding = "UTF-8")

# cleaning
## dataset which covers "oral" medicine listed "between 1990-2000"
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
         price1 = as.numeric(price1))


df_jpc_cl2 <- 
  df_jpc_cl1 %>% 
  mutate(brand = ifelse(code_yj %in% ls_code_br, T, F)) %>%  
  group_by(year, eff) %>% 
  mutate(N_brand = length(unique(maker[brand])), 
         N = length(unique(maker))) %>% 
  ungroup() %>% 
  mutate(year_listed = str_split(as.character(.$note2), pattern = "-", simplify = TRUE)[, 1], 
         year_listed = as.numeric(year_listed)) %>% 
  filter(is.na(year_listed) | year_listed < 2005) %>% 
  filter(!is.na(year_listed) & year_listed > 1990) %>% 
  arrange(year, eff, form)


# list --------------------------------------------------------------------

ls_code_br_smpl <- 
  df_jpc_cl2 %>% 
  filter(year == 2015, brand) %>% 
  use_series(code_yj) %>% 
  unique() %>% 
  sort()


ls_eff_smpl <- 
  df_jpc_cl2 %>% 
  filter(code_yj %in% ls_code_br_smpl) %>% 
  use_series(eff) %>% 
  unique() %>% 
  sort()

ls_name_ag <- 
  read_csv("./dataset/list/ag_list.csv", locale = locale(encoding = "Shift_JIS")) %>% 
  use_series(ag) %>% 
  unique() %>% 
  sort()



# sample ------------------------------------------------------------------

## sample
df_jpc_smpl <- 
  df_jpc_cl1 %>% 
  filter(eff %in% ls_eff_smpl) %>% 
  mutate(brand = ifelse(code_yj %in% ls_code_br, T, F), 
         ag = ifelse(name %in% ls_name_ag, T, F)) %>%  
  group_by(year, eff) %>% 
  mutate(N_brand = length(unique(maker[brand])), 
         N_ag = length(unique(maker[ag])),
         N_firm = length(unique(maker)), 
         N_gen = N_firm - N_brand, 
         old_identity = if_else(anyNA(note1), T, F)) %>% 
  ungroup() %>% 
  filter(!old_identity) %>% 
  select(- old_identity) %>% 
  arrange(year, eff, form)

## plot
df_jpc_plt <- 
  df_jpc_cl1 %>% 
  filter(eff %in% ls_eff_smpl) %>% 
  mutate(brand = ifelse(code_yj %in% ls_code_br, T, F), 
         ag = ifelse(name %in% ls_name_ag, T, F)) %>%  
  group_by(year, eff) %>% 
  summarize(N_brand = length(unique(maker[brand])), 
            N_ag = length(unique(maker[ag])),
            N_firm = length(unique(maker)), 
            N_gen = N_firm - N_brand, 
            old_identity = if_else(anyNA(note1), T, F), 
            .groups = "drop") %>% 
  filter(!old_identity) %>% 
  select(- old_identity) %>% 
  arrange(year, eff)

