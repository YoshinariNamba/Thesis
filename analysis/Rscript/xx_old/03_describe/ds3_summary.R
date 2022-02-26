
# library
library(tidyverse)
library(stargazer)

# data
df_sample <- 
  read_rds("./output/data/sample.rds")

# summary stats
df_sample %>% 
  select(-c(where(is.character), where(is.factor))) %>% 
  as.data.frame() %>% 
  stargazer(type = "latex", 
            digits = 3)

