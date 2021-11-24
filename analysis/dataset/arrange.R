
## library
library(tidyverse)
library(magrittr)

## source
source("shusai.R")

## cleaning
df_shusai_01 <- df_shusai %>%
  filter(! is.na(maker)) %>% 
  arrange(ingredient, year, shusai_code) %>% 
  group_by(ingredient, year) %>% 
  mutate(num_firm = length(unique(maker))) %>% 
  ungroup() %>% 
  select(c(2, 11, 3, 5, 6, 14, 1, 4, 7:10, 12:13)) %>% 
  mutate(generic = if_else(is.na(generic), 0, 1)) %>% 
  group_by(ingredient, year) %>% 
  mutate(num_generic = sum(generic)) %>% 
  ungroup()

## glimpse

### # of kinds of ingredients
df_shusai_01 %>% 
  use_series(ingredient) %>% 
  unique() %>% 
  length()


### # of markets
df_shusai_01 %>% 
  group_by(ingredient, year) %>%
  distinct(ingredient, .keep_all = TRUE) %>% 
  ungroup() %>% 
  nrow()


### mean # of firms per market
df_shusai_01$num_firm %>% 
  mean()

### mean # of generic per market excluding one that has no generic
df_shusai_01 %>% 
  filter(generic == 1) %>% 
  group_by(ingredient, year) %>% 
  distinct(maker, .keep_all = TRUE) %>% 
  mutate(num_generic = length(unique(maker))) %>% 
  ungroup() %>% 
  use_series(num_generic) %>% 
  mean()

### # of market occupied only by brand firm
df_shusai_01 %>% 
  filter(num_generic == 0) %>% 
  group_by(ingredient, year) %>% 
  distinct(ingredient, .keep_all = TRUE) %>% 
  ungroup() %>% 
  nrow()


### 
df_shusai %>% 
  use_series(generic) %>% 
  unique()


## firm list 
brand_list <- df_shusai %>% 
  arrange(maker) %>% 
  filter(! is.na(maker)) %>% 
  filter(brand == "先発品") %>% 
  use_series(maker) %>% 
  unique()

maker_list <- df_shusai %>% 
  arrange(maker) %>% 
  filter(! is.na(maker)) %>% 
  use_series(maker) %>% 
  unique()

length(brand_list) ; length(maker_list)

df_makerlist <- data.frame(name = maker_list) %>% 
  mutate(brand = if_else(name %in% brand_list, 1, 0))




