library(tidyverse)
library(magrittr)


# example
df_example <- read.delim("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis/data/japic/example.txt")

df_example %>% 
  use_series(YJコード) %>% 
  unique()
  

# by YJcode
df_jpc_h1 <- read.delim("D:/prescription drugs/by YJcode/h1.txt")

df_jpc <- df_jpc_h1

for(i in 2:9){
  assign(paste0("df_jpc_h", i), read.delim(paste0("D:/prescription drugs/by YJcode/h", i, ".txt")))
  df_jpc <- df_jpc %>% 
    bind_rows(df_jpc)
}

