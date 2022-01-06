
# library
library(tidyverse)
library(stringi)
library(magrittr)

# source
source("./dataset/00_source.R")

# cleaning
df_shusai_cl <- 
  df_shusai %>% 
  filter(!is.na(dl) | !is.na(other)) %>% 
  filter(year > 2012, form == "内用薬") %>% 
  filter(!is.na(other)) %>% 
  mutate(date_list = str_extract(other, pattern = "H\\d+.\\d+.\\d+収載")) %>%
  filter(!is.na(date_list)) %>% 
  mutate(year_list = str_extract(date_list, pattern = "H\\d+"), 
         year_list = as.numeric(str_replace(year_list, pattern = "H", replacement = "")) + 1989)

# create lists
## code
ls_code_br <- 
  df_shusai %>% 
  filter(year == 2014, 
         is.na(other) | !str_detect(other, pattern = "H\\d+.\\d+.\\d+"), 
         brand == "先発品", 
         form == "内用薬") %>% 
  use_series(code_shusai) %>% 
  unique() %>% 
  sort()

