
# library
library(tidyverse)
library(dlookr)

# directory
setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis/dataset/shusai/append")

# read
df_shusai_o_2014 <- read_csv(file = "../arranged/2014/shusai_o_2014.csv", 
                             locale = locale(encoding = "Shift-JIS"))

df_shusai_p_2014 <- read_csv(file = "../arranged/2014/shusai_p_2014.csv", 
                             locale = locale(encoding = "Shift-JIS"))

df_shusai_t_2014 <- read_csv(file = "../arranged/2014/shusai_t_2014.csv", 
                             locale = locale(encoding = "Shift-JIS"))

# merge
df_shusai_2014 <- df_shusai_o_2014 %>% 
  bind_rows(df_shusai_p_2014) %>% 
  bind_rows(df_shusai_t_2014) %>% 
  rename(form = 1, 
         code_shusai = 2,  
         ingredient = 3, 
         unit = 4,
         name = 7, 
         maker = 8, 
         generic = 9, 
         brand = 10, 
         ag = 11, 
         price = 12, 
         dl = 13, 
         other = 14) %>% 
  select(-c(5, 6)) %>% 
  mutate(year = 2014)

# delete
rm(list = c(paste0("df_shusai_", c("o", "p", "t"), "_2014")))




