
# library
library(tidyverse)
library(naniar)
library(magrittr)


# source
source("./dataset/shusai/append/master_append.R")
source("./dataset/ndb/append/master_append.R")
source("./dataset/japic/append/append.R")


# output
write_rds(df_shusai, "./output/data/shusai.rds")
write_rds(df_ndb, "./output/data/ndb.rds")
write_rds(df_jpc, "./output/data/japic.rds")
