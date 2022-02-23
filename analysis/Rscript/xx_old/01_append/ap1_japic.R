
# library
library(tidyverse)

# empty data.frame
df_jpc_raw <- 
  data.frame(NULL)

# outer loop
for(i in 15:21){
  ## temporary data.frame
  df_jpc_tmp <- 
    data.frame(NULL)
  
  ## inner loop
  for(j in 1:9){
    df_jpc_tmp <- 
      df_jpc_tmp %>% 
      rbind(read.delim(paste0("./dataset/japic/20", i, "07/h", j, ".txt")))
  }
  
  ## arrange tmp
  df_jpc_tmp <- 
    df_jpc_tmp %>% 
    mutate(year = 2000 + i) %>%  # add year
    select(ncol(.), 1:(ncol(.)-1))
  
  ## append
  df_jpc_raw <- 
    df_jpc_raw %>% 
    rbind(df_jpc_tmp)
}

# naming
df_jpc <- 
  df_jpc_raw %>% 
  rename(name = 3, 
         name_en = 4, 
         maker = 5, 
         maker_sale = 6, 
         form = 7, 
         unit = 8, 
         price = 9, 
         reg = 10, 
         eff = 11, 
         code_eff = 12, 
         name_g = 13, 
         name_g_en = 14, 
         code_yj = 15, 
         code_hot = 16, 
         other = 17, 
         code_id = 18, 
         color = 19, 
         line = 20, 
         inactive = 21) %>% 
  select(-2)

# variable label
lbl_jpc <- 
  list(raw = colnames(df_jpc_raw), new = colnames(df_jpc))
write_rds(lbl_jpc, "./output/data/japic_lbl.rds")

# output
write_rds(df_jpc, "./output/data/japic.rds")

# remove
rm(list = ls())

