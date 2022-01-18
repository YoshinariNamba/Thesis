
# library
library(tidyverse)

# source
for(i in 2014:2019){
  source(paste0("./Rscript/01_append/ndb/merge_", i, ".R"))
}


# 2014 and 2015
df_ndb_2014_15 <- df_ndb_2014 %>% 
  bind_rows(df_ndb_2015) %>% 
  mutate(unit = NA) %>% 
  select(1:6, 12, 11, 7:10)

# append
df_ndb <- df_ndb_2014_15 %>% 
  bind_rows(df_ndb_2016) %>% 
  bind_rows(df_ndb_2017) %>% 
  bind_rows(df_ndb_2018) %>% 
  bind_rows(df_ndb_2019)

# output
write_rds(df_ndb, "./output/data/ndb.rds")

# remove
rm(list = ls())

