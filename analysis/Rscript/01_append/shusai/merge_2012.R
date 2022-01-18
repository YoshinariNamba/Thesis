
# library
library(tidyverse)

# read
df_shusai_o_2012 <- read_csv(file = "./dataset/shusai/arranged/2012/shusai_o_2012.csv", 
                             locale = locale(encoding = "Shift-JIS"))

df_shusai_p_2012 <- read_csv(file = "./dataset/shusai/arranged/2012/shusai_p_2012.csv", 
                             locale = locale(encoding = "Shift-JIS"))

df_shusai_t_2012 <- read_csv(file = "./dataset/shusai/arranged/2012/shusai_t_2012.csv", 
                             locale = locale(encoding = "Shift-JIS"))

# merge
df_shusai_2012 <- df_shusai_o_2012 %>% 
  bind_rows(df_shusai_p_2012) %>% 
  bind_rows(df_shusai_t_2012) %>% 
  rename(form = 1, 
         code_shusai = 2,  
         ingredient = 3, 
         unit = 4,
         name = 7, 
         maker = 8, 
         generic = 9, 
         price = 10, 
         dl = 11, 
         other = 12) %>% 
  select(-c(5, 6)) %>% 
  mutate(year = 2012)

# delete
rm(list = c(paste0("df_shusai_", c("o", "p", "t"), "_2012")))




