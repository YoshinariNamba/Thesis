
# library
library(tidyverse)
library(stringi)

# source
source("./Rscript/01_append/ap0_master.R")
source("./Rscript/02_clean/cl1_shusai.R", encoding = "UTF-8")
source("./Rscript/02_clean/cl2_japic.R", encoding = "UTF-8")

df_jpc_tmp <- 
  df_jpc %>% 
  mutate(year = year -1)

df_ndb_cl <-
  df_ndb %>%
  filter(year == 2014) %>% 
  replace_na(replace = list(total = min(.$total, na.rm = TRUE)/2, 
                            total_in = min(.$total_in, na.rm = TRUE)/2, 
                            total_out = min(.$total_out, na.rm = TRUE)/2, 
                            total_hos = min(.$total_hos, na.rm = TRUE)/2)) %>% 
  mutate(name = stri_trans_nfkc(name)) 










