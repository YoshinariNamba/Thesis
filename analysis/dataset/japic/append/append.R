
# library
library(tidyverse)
library(magrittr)


# construct ---------------------------------------------------------------

## 2015
### read first file
df_jpc_15 <- read.delim("./dataset/japic/201507/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("./dataset/japic/201507/h", i, ".txt"))) 
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
df_jpc_16 <- read.delim("./dataset/japic/201607/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("./dataset/japic/201607/h", i, ".txt"))) 
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
df_jpc_17 <- read.delim("./dataset/japic/201707/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("./dataset/japic/201707/h", i, ".txt"))) 
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
df_jpc_18 <- read.delim("./dataset/japic/201807/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("./dataset/japic/201807/h", i, ".txt"))) 
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
df_jpc_19 <- read.delim("./dataset/japic/201907/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("./dataset/japic/201907/h", i, ".txt"))) 
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
df_jpc_20 <- read.delim("./dataset/japic/202007/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("./dataset/japic/202007/h", i, ".txt"))) 
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
df_jpc_21 <- read.delim("./dataset/japic/202107/h1.txt")

### loop reading and merging 
for(i in 2:9){
  assign("df_tmp", 
         read.delim(paste0("./dataset/japic/202107/h", i, ".txt"))) 
  df_jpc_21 <- df_jpc_21 %>% 
    rbind(df_tmp)
}
rm(df_tmp)

### add "year"
df_jpc_21 <- df_jpc_21 %>% 
  mutate(year = 2021) %>% 
  select(ncol(.), 1:(ncol(.)-1))



# append ------------------------------------------------------------------

## merge
df_jpc_raw <- rbind(df_jpc_15, df_jpc_16, df_jpc_17, df_jpc_18, 
                    df_jpc_19, df_jpc_20, df_jpc_21)

df_jpc <- df_jpc_raw %>% 
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

### variable label
lbl_jpc <- list(raw = colnames(df_jpc_raw), new = colnames(df_jpc))
write_rds(lbl_jpc, "./output/data/japic_lbl.rds")

### remove
rm(list = c(paste0("df_jpc_", 15:21), "df_jpc_raw", "lbl_jpc"))

## output
write_rds(df_jpc, "./output/data/japic.rds")

