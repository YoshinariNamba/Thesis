
# library
library(tidyverse)
library(dlookr)

# directory
setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis/dataset/shusai/append")

# source
source("merge_2012.R")
source("merge_2014.R")
source("merge_2016.R")
source("merge_2018.R")
source("merge_2020.R")

# append
df_shusai <- df_shusai_2012  %>% 
  mutate(ag = "missing", 
         brand = "missing") %>% 
  bind_rows(df_shusai_2014) %>% 
  bind_rows(df_shusai_2016) %>% 
  bind_rows(df_shusai_2018) %>% 
  bind_rows(df_shusai_2020) %>% 
  filter(!is.na(code_shusai))

# delete 
rm(list = paste0("df_shusai_", seq(2012, 2020, by = 2)))

# diagnose
df_shusai %>% 
  diagnose()
