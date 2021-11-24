## library
library(tidyverse)
library(magrittr)

##
df_01 <- read_csv("./data/tp20210218-01_01.csv", locale = locale(encoding = "Shift-Jis"))
df_05 <- read_csv("./data/tp20210218-01_05.csv", locale = locale(encoding = "Shift-Jis"))


df_01$maker %>% table()

## pharma firms
firms <- df_01$maker %>% 
  unique() 
firms <- firms[!is.na(firms)]


## 
df_01 %>% 
  filter(! is.na(generic)) %>% 
  filter(! generic %in% c("後発品", "★")) %>% 
  select(generic) 

unique(df_01$generic)
unique(df_01$brand)
df_01 %>% 
  filter(! generic %in% c("後発品", "★"), 
         ! brand %in% c("先発品", "準先発品")) %>% 
  select(name, maker, price)


## nbd
df_nbd <- read_csv("./data/nbd_2.csv", locale = locale(encoding = "Shift-Jis"), skip = 3)
df_shusai <- read_csv("./data/shusailist.csv", locale = locale(encoding = "Shift-Jis"))

df_tmp <- df_nbd %>% 
  select(1:9) %>% 
  inner_join(df_shusai, by = "shusai_code") %>% 
  select(-c(deadline, other))


## glimpse
### summary
df_tmp %>% 
  summary()

### measure
df_tmp %>% 
  select(measure.x) %>%
  unique()

### type
df_tmp %>% 
  select(type) %>% 
  unique()

### generic
df_tmp %>% 
  select(generic.x) %>% 
  unique()
df_tmp <- df_tmp %>% 
  rename(generic_dummy = generic.x)

### maker
df_tmp %>% 
  select(maker) %>% 
  unique()

### generic.y
df_tmp %>% 
  select(generic.y) %>% 
  unique()

### brand
df_tmp %>% 
  select(brand) %>% 
  unique()

### ag
df_tmp %>% 
  select(ag) %>% 
  unique()

### diff
df_tmp <- df_tmp %>% 
  mutate(price_dif = price.x - price.y)

mean(df_tmp$price_dif)

df_tmp %>% 
  filter(ag == "○", 
         is.na(brand))

df_tmp %>% 
  filter(effect_code == 114) %>% 
  select(name.x, price.x, price.y, total, generic.y, brand, ag)




# 2018 --------------------------------------------------------------------

# 2018年内服薬
df_shusai <- read_csv("./data/shusai_2018.csv", locale = locale(encoding = "Shift-JIS")) %>% 
  select(-c(v1, v2, v3, note))
df_nbd <- read_csv("./data/nbd_2018.csv", locale = locale(encoding = "Shift-JIS"), skip = 3) %>% 
  select(1:9)


df_2018 <- df_nbd %>% 
  inner_join(df_shusai, by = "shusai_code") %>% 
  arrange(type_code, ingredient)

df_2018 %>% 
  filter(generic.x == 1) %>% 
  filter(is.na(generic.y)) %>% 
  select(shusai_code, name.x, maker)

generic_error <- df_2018 %>% 
  filter(generic.x == 1) %>% 
  filter(is.na(generic.y)) %>% 
  use_series(shusai_code)

df_2018_01 <- df_2018 %>% 
  filter(!shusai_code %in% generic_error) %>% 
  filter(is.na(deadline)) %>% 
  select(-deadline) %>% 
  group_by(type_code, ingredient) %>% 
  mutate(num_firms = n()) %>% 
  ungroup()

brand_list <- df_2018 %>% 
  filter(brand == "先発品") %>% 
  arrange(maker) %>% 
  use_series(maker) %>% 
  unique()





