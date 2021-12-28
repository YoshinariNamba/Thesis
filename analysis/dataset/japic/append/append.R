library(tidyverse)
library(magrittr)

# directory
setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis/dataset/japic/append")

# construct ---------------------------------------------------------------

## 2015
### read first file
df_jpc_15 <- read.delim("../201507/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("../201507/h", i, ".txt"))) 
  df_jpc_15 <- df_jpc_15 %>% 
    rbind(df_tmp)
}
rm(df_tmp)

### add "year"
df_jpc_15 <- df_jpc_15 %>% 
  mutate(year = 2015) %>% 
  select(ncol(.), 1:(ncol(.)-1))



## 2016
### read first file
df_jpc_16 <- read.delim("../201607/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("../201607/h", i, ".txt"))) 
  df_jpc_16 <- df_jpc_16 %>% 
    rbind(df_tmp)
}
rm(df_tmp)

### add "year"
df_jpc_16 <- df_jpc_16 %>% 
  mutate(year = 2016) %>% 
  select(ncol(.), 1:(ncol(.)-1))




## 2017
### read first file
df_jpc_17 <- read.delim("../201707/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("../201707/h", i, ".txt"))) 
  df_jpc_17 <- df_jpc_17 %>% 
    rbind(df_tmp)
}
rm(df_tmp)

### add "year"
df_jpc_17 <- df_jpc_17 %>% 
  mutate(year = 2017) %>% 
  select(ncol(.), 1:(ncol(.)-1))




## 2018
### read first file
df_jpc_18 <- read.delim("../201807/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("../201807/h", i, ".txt"))) 
  df_jpc_18 <- df_jpc_18 %>% 
    rbind(df_tmp)
}
rm(df_tmp)

### add "year"
df_jpc_18 <- df_jpc_18 %>% 
  mutate(year = 2018) %>% 
  select(ncol(.), 1:(ncol(.)-1))


## 2019
### read first file
df_jpc_19 <- read.delim("../201907/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("../201907/h", i, ".txt"))) 
  df_jpc_19 <- df_jpc_19 %>% 
    rbind(df_tmp)
}
rm(df_tmp)

### add "year"
df_jpc_19 <- df_jpc_19 %>% 
  mutate(year = 2019) %>% 
  select(ncol(.), 1:(ncol(.)-1))




## 2020
### read first file
df_jpc_20 <- read.delim("../202007/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("../202007/h", i, ".txt"))) 
  df_jpc_20 <- df_jpc_20 %>% 
    rbind(df_tmp)
}
rm(df_tmp)

### add "year"
df_jpc_20 <- df_jpc_20 %>% 
  mutate(year = 2020) %>% 
  select(ncol(.), 1:(ncol(.)-1))




## 2021
### read first file
df_jpc_21 <- read.delim("../202107/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("../202107/h", i, ".txt"))) 
  df_jpc_21 <- df_jpc_21 %>% 
    rbind(df_tmp)
}
rm(df_tmp)

### add "year"
df_jpc_21 <- df_jpc_21 %>% 
  mutate(year = 2021) %>% 
  select(ncol(.), 1:(ncol(.)-1))



# append ------------------------------------------------------------------

## glimpse
list_tmp <- list(df_jpc_15, df_jpc_16, df_jpc_17, df_jpc_18, 
                 df_jpc_19, df_jpc_20, df_jpc_21)
for(i in 1:length(2015:2019)){
  print(colnames(list_tmp[[i]]))
}
rm(list_tmp)


## merge
df_jpc <- rbind(df_jpc_15, df_jpc_16, df_jpc_17, df_jpc_18, 
                df_jpc_19, df_jpc_20, df_jpc_21) %>% 
  rename(name = 3, 
         name_en = 4, 
         maker = 5, 
         maker_sale = 6, 
         form = 7, 
         unit = 8, 
         price = 9, 
         reg = 10, 
         eff = 11, 
         code_eff = 12, 
         name_g = 13, 
         name_g_en = 14, 
         code_yj = 15, 
         code_hot = 16, 
         other = 17, 
         code_id = 18, 
         color = 19, 
         line = 20, 
         inactive = 21) %>% 
  select(-2)
rm(list = c("df_jpc_15", "df_jpc_16", "df_jpc_17", "df_jpc_18", 
            "df_jpc_19", "df_jpc_20", "df_jpc_21"))


# 
setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis")

