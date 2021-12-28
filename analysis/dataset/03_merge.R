
# library
library(tidyverse)

# source
setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis/dataset")
source("./japic/append/append.R"); setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis/dataset")
source("./ndb/append/master_append.R"); setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis/dataset")
source("./shusai/append/master_append.R"); setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis/dataset")

#
df_merged <- df_ndb %>% 
  mutate(year = year + 1) %>% 
  left_join(df_shusai, by = c("code_shusai", "year")) %>% 
  left_join(df_jpc, by = c("code_shusai" = "code_yj", "year"))

df_merged <- df_ndb %>% 
  mutate(year = year + 1) %>% 
  select(- c(unit, name, code_receipt)) %>% 
  left_join(df_jpc, by = c("code_shusai" = "code_yj", "year")) 



