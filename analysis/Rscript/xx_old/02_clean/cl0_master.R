
# library
library(tidyverse)

## define term 
FnlYear_exante <- 2014 # final year in ex ante term
IntYear_expost <- 2016 # initial year in ex post term

# source
source("./Rscript/01_append/ap0_master.R", encoding = "UTF-8")
source("./Rscript/02_clean/cl1_shusai.R", encoding = "UTF-8")
source("./Rscript/02_clean/cl2_japic.R", encoding = "UTF-8")
source("./Rscript/02_clean/cl3_ndb.R", encoding = "UTF-8")
source("./Rscript/02_clean/cl4_construction.R", encoding = "UTF-8")
source("./Rscript/02_clean/cl5_preprocessing.R", encoding = "UTF-8")
source("./Rscript/02_clean/cl6_belief.R", encoding = "UTF-8")

# save
write_rds(df_sample, "./output/data/sample.rds")

# remove
rm(list = ls())
