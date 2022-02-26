
# library
library(tidyverse)
library(stringi)
library(magrittr)

# source
source("./Rscript/01_append/ap0_master.R")

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
ls_code_br_shusai <- 
  df_shusai %>% 
  # use 2014 ~ because data of 2012 does not cover brand ID
  filter(year > 2012, 
         brand == "先発品") %>% 
  use_series(code_shusai) %>% 
  unique() %>% 
  sort()

