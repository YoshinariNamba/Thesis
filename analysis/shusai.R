
##
library(tidyverse)
library(magrittr)

## 2012
df_shusai_2012 <- readxl::read_xls("./data/raw/oral/shusai/shusai_2012.xls", col_names = FALSE)
colnames(df_shusai_2012) <- c("type", "shusai_code", "ingredient", "measure",	"v1",	"v2", 
                              "name", "maker", "generic", "price", "deadline", "note")

df_shusai_2012 <- df_shusai_2012[-1, ] %>%
  select(-c(v1, v2)) %>% 
  mutate(year = 2012) %>% 
  mutate(brand = "missing", 
         has_ag = "missing")

## 2014
df_shusai_2014 <- readxl::read_xls("./data/raw/oral/shusai/shusai_2014.xls", col_names = FALSE)
colnames(df_shusai_2014) <- c("type", "shusai_code", "ingredient", "measure",	"v1",	"v2", 
                              "name", "maker", "generic", "brand", "has_ag", "price", "deadline", "note")

df_shusai_2014 <- df_shusai_2014[-1, ] %>%
  select(-c(v1, v2)) %>% 
  mutate(year = 2014)

## 2016
df_shusai_2016 <- readxl::read_xls("./data/raw/oral/shusai/shusai_2016.xls", col_names = FALSE) %>% 
  select(1:15)
colnames(df_shusai_2016) <- c("type", "shusai_code", "ingredient", "measure",	"v1",	"v2", "v3",  
                              "name", "maker", "generic", "brand", "has_ag", "price", "deadline", "note")

df_shusai_2016 <- df_shusai_2016[-1, ] %>%
  select(-c(v1, v2, v3)) %>% 
  mutate(year = 2016)


## 2018
df_shusai_2018 <- readxl::read_xls("./data/raw/oral/shusai/shusai_2018.xls", col_names = FALSE) %>% 
  select(1:15)
colnames(df_shusai_2018) <- c("type", "shusai_code", "ingredient", "measure",	"v1",	"v2", "v3",  
                              "name", "maker", "generic", "brand", "has_ag", "price", "deadline", "note")

df_shusai_2018 <- df_shusai_2018[-1, ] %>%
  select(-c(v1, v2, v3)) %>% 
  mutate(year = 2018)

## 2020
df_shusai_2020 <- readxl::read_xls("./data/raw/oral/shusai/shusai_2020.xls", col_names = FALSE) %>% 
  select(1:15)
colnames(df_shusai_2020) <- c("type", "shusai_code", "ingredient", "measure",	"v1",	"v2", "v3",  
                              "name", "maker", "generic", "brand", "has_ag", "price", "deadline", "note")

df_shusai_2020 <- df_shusai_2020[-1, ] %>%
  select(-c(v1, v2, v3)) %>% 
  mutate(year = 2020)


## join
df_shusai <- bind_rows(df_shusai_2012, df_shusai_2014, df_shusai_2016, df_shusai_2018, df_shusai_2020)



