
# library
library(tidyverse)
library(lubridate)

# source
source("./dataset/00_source.R")
source("./dataset/01_1_cleaning_shusai.R", encoding = "UTF-8")

# cleaning
## dataset which covers "oral" medicine listed "between 1990-2000"
df_jpc_cl <- 
  df_jpc %>% 
  ## form
  mutate(form1 = str_split(.$form, pattern = "：", simplify = TRUE)[, 1], 
         form2_tmp = str_split(.$form, pattern = "：", simplify = TRUE)[, 2]) %>% 
  mutate(form2 = str_split(.$form2_tmp, pattern = "〔", simplify = TRUE)[, 1], 
         form3 = str_replace(str_split(.$form2_tmp, pattern = "〔", simplify = TRUE)[, 2], 
                             pattern = "〕", replacement = "")) %>% 
  select(-form2_tmp) %>% 
  filter(!str_detect(other, pattern = "薬価削除")) %>% 
  mutate(note_tmp = str_split(.$other, pattern = "，", simplify = TRUE)[, 1]) %>% 
  mutate(note1 = str_extract(note_tmp, pattern = "\\p{Han}+"),
         note2 = str_extract(note_tmp, pattern = "(\\d+).(\\d+).(\\d+)"), 
         note3 = str_split(.$other, pattern = "，", simplify = TRUE)[, 2]) %>% 
  mutate(note2 = ymd(note2)) %>% 
  select(-note_tmp)  %>% 
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
  df_jpc_cl %>% 
  filter(year == 2015, brand) %>% 
  use_series(code_yj) %>% 
  unique() %>% 
  sort()


ls_eff_smpl <- 
  df_jpc_cl %>% 
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

df_jpc_smple <- 
  df_jpc %>% 
  filter(eff %in% ls_eff_smpl) %>% 
  ## form
  mutate(form1 = str_split(.$form, pattern = "：", simplify = TRUE)[, 1], 
         form2_tmp = str_split(.$form, pattern = "：", simplify = TRUE)[, 2]) %>% 
  mutate(form2 = str_split(.$form2_tmp, pattern = "〔", simplify = TRUE)[, 1], 
         form3 = str_replace(str_split(.$form2_tmp, pattern = "〔", simplify = TRUE)[, 2], 
                             pattern = "〕", replacement = "")) %>% 
  select(-form2_tmp) %>% 
  filter(!str_detect(other, pattern = "薬価削除")) %>% 
  mutate(note_tmp = str_split(.$other, pattern = "，", simplify = TRUE)[, 1]) %>% 
  mutate(note1 = str_extract(note_tmp, pattern = "\\p{Han}+"),
         note2 = str_extract(note_tmp, pattern = "(\\d+).(\\d+).(\\d+)"), 
         note3 = str_split(.$other, pattern = "，", simplify = TRUE)[, 2]) %>% 
  mutate(note2 = ymd(note2)) %>% 
  select(-note_tmp)  %>% 
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
