
# library
library(tidyverse)
library(stargazer)

# data
df_sample <- 
  read_rds("./output/data/sample.rds") %>% 
  select(-c(entry_dummy))

# summary stats
df_sample %>% 
  select(-c(where(is.character), where(is.factor), 
            contains("_in"), contains("_out"), price, 
            contains("N_inc_over"))) %>% 
  as.data.frame() %>% 
  stargazer(type = "latex", 
            digits = 3)


# first stage
mdl_belief <- 
  read_rds("./output/result/mdl_belief.rds") %>% 
  stargazer()

