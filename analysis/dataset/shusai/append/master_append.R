
# library
library(tidyverse)

# source
source("./dataset/shusai/append/merge_2012.R")
source("./dataset/shusai/append/merge_2014.R")
source("./dataset/shusai/append/merge_2016.R")
source("./dataset/shusai/append/merge_2018.R")
source("./dataset/shusai/append/merge_2020.R")

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

# output
write_rds(df_shusai, "./output/data/shusai.rds")
