
# library
library(tidyverse)
library(stringi)

# source
source("./dataset/00_source.R")

df_jpc_tmp <- 
  df_jpc %>% 
  mutate(year = year -1)

df_ndb_cl <-
  df_ndb %>% 
  replace_na(replace = list(total = 0, 
                            total_in = 0, 
                            total_out = 0, 
                            total_hos = 0)) %>% 
  mutate(name = stri_trans_nfkc(name)) %>% 
  mutate(brand = ifelse(code_shusai %in% ls_code_br, T, F), 
         ag = ifelse(name %in% ls_name_ag, T, F)) %>% 
  left_join(df_jpc_tmp[, c(1, 4, 10, 14)], by = c("year" = "year", "code_shusai" = "code_yj")) %>% 
  mutate(name2 = str_replace_all(name, pattern = "\\d+", ""), 
         name2 = str_replace_all(name2, pattern = "「\\w+」", ""), 
         name2 = str_extract(name2, pattern = "(\\p{Han}|\\p{Katakana}|\\p{Hiragana}|ー)+"))


df_ndb_agg <- 
  df_ndb_cl %>% 
  group_by(year, eff, maker, form, brand, generic, ag, name2) %>% 
  summarise(
    across(.cols = contains("total"), 
           .fns = sum, 
           .names = "{.col}"
           ), 
    .groups = "drop"
  )
