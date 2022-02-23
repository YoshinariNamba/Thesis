
# library
library(tidyverse)

# source 
#source("./Rscript/01_append/ap1_japic.R")
#source("./Rscript/01_append/ap2_ndb.R")
#source("./Rscript/01_append/ap3_shusai.R")

# read rds
df_shusai <- read_rds("./output/data/shusai.rds")
df_ndb <- read_rds("./output/data/ndb.rds")
df_jpc <- read_rds("./output/data/japic.rds")
