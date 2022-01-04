
# library
library(tidyverse)

# source
source("./dataset/00_source.R")

#
df_merged <- df_ndb %>% 
  mutate(year = year + 1) %>% 
  left_join(df_shusai, by = c("code_shusai", "year")) %>% 
  left_join(df_jpc, by = c("code_shusai" = "code_yj", "year"))

df_merged <- df_ndb %>% 
  mutate(year = year + 1) %>% 
  select(- c(unit, name, code_receipt)) %>% 
  left_join(df_jpc, by = c("code_shusai" = "code_yj", "year")) 



