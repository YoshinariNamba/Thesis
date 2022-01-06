
# library
library(tidyverse)
library(magrittr)
library(stringi)

# source
source("./dataset/00_source.R")

# uniform unit -------------------------------------------------------------

## japic
ls_unit_jpc <- df_jpc %>% 
  select(code_yj, year, unit, price) %>% 
  mutate(numerator = str_split(.$price, pattern = "／", simplify = T)[, 1], 
         denominator = str_split(.$price, pattern = "／", simplify = T)[, 2]) %>% 
  filter(denominator != "") %>% 
  use_series(denominator) %>% 
  unique() %>% 
  sort() %>% 
  str_replace(pattern = "（", replacement = "(") %>% 
  str_replace(pattern = "）", replacement = ")")

## ndb
ls_unit_ndb <- df_ndb %>% 
  filter(!is.na(unit)) %>% 
  use_series(unit) %>% 
  unique() %>% 
  sort() %>% 
  stri_trans_nfkc() %>% 
  str_replace(pattern = "(", replacement = "(") %>% 
  str_replace(pattern = ")", replacement = ")")


## 
ls_unit_jpc[!ls_unit_jpc %in% ls_unit_ndb]
ls_unit_ndb[!ls_unit_ndb %in% ls_unit_jpc]


## 

df_jpc %>% 
  mutate(numerator = str_split(.$price, pattern = "／", simplify = T)[, 1], 
         denominator = str_split(.$price, pattern = "／", simplify = T)[, 2]) %>% 
  filter(denominator %in% ls_unit_jpc[!ls_unit_jpc %in% ls_unit_ndb]) %>% 
  nrow()

df_jpc %>% 
  mutate(numerator = str_split(.$price, pattern = "／", simplify = T)[, 1], 
         denominator = str_split(.$price, pattern = "／", simplify = T)[, 2]) %>% 
  filter(denominator == "") %>%  
  select(unit, price) %>% 
  arrange(desc(unit), desc(price)) %>%   
  head(20) 

df_ndb %>% 
  filter(unit %in% ls_unit_ndb[!ls_unit_ndb %in% ls_unit_jpc]) %>% 
  nrow()


# japic -------------------------------------------------------------------


df_jpc_unit <- df_jpc %>% 
  select(code_yj, year, unit, price) %>% 
  mutate(numerator = str_split(.$price, pattern = "／", simplify = T)[, 1], 
         denominator = str_split(.$price, pattern = "／", simplify = T)[, 2])
ls_unit_jpc <- df_jpc_unit %>% 
  use_series(denominator) %>% 
  unique() %>% 
  sort()


df_jpc_unit %>% 
  filter(denominator == "mL（g）") %>% 
  head()



# ndb ---------------------------------------------------------------------

df_ndb %>% 
  use_series(unit) %>% 
  unique()

df_ndb %>% 
  filter(unit == "カプセル") %>% 
  select(3, year, 2, 4) %>% 
  filter(!str_detect(.$name, pattern = "カプセル")) %>% 
  use_series(name) %>% 
  unique()


df_ndb %>% 
  filter(unit == "シート") %>% 
  select(3, year, 2, 4) %>% 
  filter(!str_detect(.$name, pattern = "シート")) %>% 
  use_series(name) %>% 
  unique()


df_ndb %>% 
  filter(unit == "錠") %>% 
  select(3, year, 2, 4) %>% 
  filter(!str_detect(.$name, pattern = "錠")) %>% 
  use_series(name) %>% 
  unique()


ls_unit <- df_ndb %>% 
  filter(!is.na(unit)) %>% 
  use_series(unit) %>% 
  unique()


for(i in 1:length(ls_unit)){
  ls_tmp <- df_ndb %>% 
    filter(unit == ls_unit[i]) %>% 
    select(3, year, 2, 4) %>% 
    filter(!str_detect(.$name, pattern = ls_unit[i])) %>% 
    use_series(name) %>% 
    unique()
  df_ndb$unit[df_ndb$year == 2014 && str_detect(df_ndb$name, pattern = ls_unit[i])] <- 
    ls_unit[i]
  df_ndb$unit[df_ndb$year == 2014] <- ifelse(df_ndb$name[df_ndb$year == 2014] %in% ls_tmp, 
                        ls_unit[i], df_ndb$unit[df_ndb$year == 2014])
  
}

for(i in 1:length(ls_unit)){
  ls_tmp <- df_ndb %>% 
    filter(unit == ls_unit[i]) %>% 
    select(3, year, 2, 4) %>% 
    filter(!str_detect(.$name, pattern = ls_unit[i])) %>% 
    use_series(name) %>% 
    unique()
  print(ls_tmp)
  print("\n")
}




