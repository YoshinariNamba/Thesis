
# library
library(tidyverse)
library(stringi)

# source
#source("./Rscript/01_append/ap0_master.R")
#source("./Rscript/02_clean/cl1_shusai.R", encoding = "UTF-8")
#source("./Rscript/02_clean/cl2_japic.R", encoding = "UTF-8")

# cleaning
df_ndb_cl <-
  df_ndb %>%
  mutate(name = stri_trans_nfkc(name)) #%>% 
  #group_by(year) %>% 
  # substitute half of minimum value for missing value 
  #replace_na(
  #  replace = list(
  #    total_in = min(.$total_in, na.rm = T) / 2, 
  #    total_out = min(.$total_out, na.rm = T) / 2, 
  #    total_hos = min(.$total_hos, na.rm = T) / 2
  #  )
  #) %>% 
  #mutate(total = total_in + total_out + total_hos) %>% 
  #ungroup()

