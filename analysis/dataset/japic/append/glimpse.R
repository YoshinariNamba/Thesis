# library
library(tidyverse)
library(magrittr)

# directory
setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis/dataset/japic/append")

# source
source("const.R")

# summary
df_jpc %>% 
  summary()


# 
df_jpc %>% 
  select(2) %>% 
  na.omit()


library(dlookr)
library(naniar)


df_jpc %>% diagnose()
df_jpc %>% colnames()  
