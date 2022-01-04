
# library
library(tidyverse)

## source 
# source("./dataset/shusai/append/master_append.R")
# source("./dataset/ndb/append/master_append.R")
# source("./dataset/japic/append/append.R")


# read
df_shusai <- read_rds("./output/data/shusai.rds")
df_ndb <- read_rds("./output/data/ndb.rds")
df_jpc <- read_rds("./output/data/japic.rds")
