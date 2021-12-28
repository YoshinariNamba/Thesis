
# library
library(tidyverse)

# directory
setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis/dataset/ndb/append")


# source
for(i in 2014:2019){
  source(paste0("merge_", i, ".R"))
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
  
# delete
rm(list = c("df_ndb_2014", "df_ndb_2015", "df_ndb_2016", "df_ndb_2017", 
            "df_ndb_2018", "df_ndb_2019", "df_ndb_2014_15"))

setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis")
