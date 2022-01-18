
# library
library(tidyverse)

# read
df_shusai_o_2018 <- read_csv(file = "./dataset/shusai/arranged/2018/shusai_o_2018.csv", 
                             locale = locale(encoding = "Shift-JIS"))

df_shusai_p_2018 <- read_csv(file = "./dataset/shusai/arranged/2018/shusai_p_2018.csv", 
                             locale = locale(encoding = "Shift-JIS"))

df_shusai_t_2018 <- read_csv(file = "./dataset/shusai/arranged/2018/shusai_t_2018.csv", 
                             locale = locale(encoding = "Shift-JIS"))

# merge
df_shusai_2018 <- df_shusai_o_2018 %>% 
  bind_rows(df_shusai_p_2018) %>% 
  bind_rows(df_shusai_t_2018) %>% 
  rename(form = 1, 
         code_shusai = 2,  
         ingredient = 3, 
         unit = 4,
         name = 8, 
         maker = 9, 
         generic = 10, 
         brand = 11, 
         ag = 12, 
         price = 13, 
         dl = 14, 
         other = 15) %>% 
  select(-c(5:7)) %>% 
  mutate(year = 2018)

# delete
rm(list = c(paste0("df_shusai_", c("o", "p", "t"), "_2018")))




