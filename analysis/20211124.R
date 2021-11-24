# library
library(tidyverse)
library(magrittr)


# source
source("shusai.R")
source("arrange.R")


# 
firms <- df_shusai %>% 
  arrange(maker) %>% 
  filter(!is.na(maker)) %>% 
  use_series(maker) %>% 
  unique()

# glimse
firms


# output
df_firm_names <- data.frame(name = firms)
write.csv(df_firm_names, "firm_name_checklist.csv")

