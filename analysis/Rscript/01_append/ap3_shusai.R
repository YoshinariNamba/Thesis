
# library
library(tidyverse)

# source
for(i in seq(2012, 2020, by = 2)){
  source(paste0("./Rscript/01_append/shusai/merge_", i, ".R"))
}

# append
df_shusai <- df_shusai_2012  %>% 
  mutate(ag = "missing", 
         brand = "missing") %>% 
  bind_rows(df_shusai_2014) %>% 
  bind_rows(df_shusai_2016) %>% 
  bind_rows(df_shusai_2018) %>% 
  bind_rows(df_shusai_2020) %>% 
  filter(!is.na(code_shusai))

# output
write_rds(df_shusai, "./output/data/shusai.rds")

# remove
rm(list = ls())

