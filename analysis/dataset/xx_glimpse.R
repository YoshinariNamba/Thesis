## library
library(tidyverse)
library(magrittr)
library(dlookr)

## option
options("tibble.print_min" = 30, 
        "tibble.print_max" = 40)

# old ---------------------------------------------------------------------


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




# 2018 
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

# merged 
df_merged %>% 
  select(price.x, price.y) %>% 
  head(30)

df_merged %>% 
  diagnose()

df_merged %>% 
  filter(!is.na(price.x), 
         !is.na(price.y)) %>% 
  mutate(price_diff = abs(price.x - price.y)) %>% 
  filter(price_diff > 1) %>% 
  select(1, 2, price_diff, price.x, price.y)

df_merged %>% 
  mutate(price = abs(price.x - price.y)) %>% 
  select(price.x, price.y, price) %>% 
  summary()


df_tmp <- read.csv("C:/Users/Namba Yoshinari/Downloads/product.csv")



# 20220105 ----------------------------------------------------------------

## 01_01_cleaning_jpc
df_jpc_cl %>% 
  select(contains("form")) %>% 
  map(unique)


"form2" %>% str_extract("\\d")

"2016.03.31" %>% class()
c("2016.03.31", "2016.03.31") %>% ymd() %>% class()

df_jpc_cl %>% 
  select(note1) %>% 
  map(unique)

df_jpc_cl %>% 
  filter(note1 == "代替品") %>% 
  select(other, contains("note")) %>% 
  head(15)


df_jpc_cl %>% 
  filter(note1 == "薬価削除") %>% 
  select(other, contains("note")) %>% 
  head(15)


library(dlookr)
df_jpc_cl %>% 
  diagnose()
df_jpc_cl %>% 
  ggplot(aes(x = note2)) + 
  geom_histogram()


df_jpc_cl %>% 
  select(eff, maker, maker_sale, eff, code_eff) %>% 
  map(~summary(str_count(.x, pattern = "，")))


df_jpc_cl %>% 
  filter(str_detect(other, pattern = "薬価収載\\d+.\\d+.\\d+")) %>% 
  mutate(year_list = str_extract(other, pattern = "薬価収載\\d+"), 
         year_list = as.numeric(str_replace(year_list, pattern = "薬価収載", replacement = ""))) %>% 
  select(year_list) %>% 
  summary()

df_jpc_cl3 %>% 
  arrange(year, eff, maker) %>% 
  distinct(year, eff, maker, .keep_all = TRUE) %>% 
  ggplot(aes(x = N_brand)) + 
  geom_histogram()
  
df_jpc_tmp <- 
  df_jpc_sample %>% 
  arrange(year, eff, maker) %>% 
  distinct(year, eff, maker, .keep_all = TRUE)

df_jpc_tmp %>%
  filter(year == 2015) %>% 
  ggplot(aes(x = as.factor(N_brand)) )+ 
  geom_bar()

df_jpc_tmp %>%
  ggplot(aes(x = as.factor(N_ag)) )+ 
  geom_bar()


df_jpc_tmp %>% 
  select(starts_with("N")) %>% 
  summary()

df_jpc_cl2 %>% 
  select(year_listed) %>% 
  with(hist(year_listed))


## shusai
df_shusai_cl %>% 
  use_series(status) %>% 
  unique()


df_shusai_cl %>% 
  mutate(other = str_replace(.$other, pattern = "H.\\d+.\\d+.\\d+収載", replacement = "XXX")) %>% 
  filter(str_detect(other, pattern = "XXX")) %>% 
  use_series(other) %>% 
  unique()



df_shusai_cl %>% 
  select(year_list) %>% 
  summary()

df_jpc_cl2 %>% 
  diagnose()
df_jpc_cl2$brand %>% sum()
df_jpc_cl2 %>% use_series(year) %>% unique()


df_jpc_sample %>% 
  use_series(maker) %>% 
  unique() %>% 
  sort()


df_shusai_wide <- readRDS("./output/data/shusai_wide.rds")
df_shusai_long <- readRDS("./output/data/shusai_long.rds")


## firm list
df_flist <- 
  read.csv("./dataset/firm_list/firm_list.csv")

ls_firm_br_flist <- 
  df_flist %>% 
  filter(brand > generic) %>%
  mutate(pharma = stri_trans_nfkc(pharma), 
         pharma = str_replace_all(pharma, pattern = "株式会社", replacement = ""), 
         pharma = str_replace(pharma, pattern = " ", replacement = "")) %>% 
  use_series(pharma) %>% 
  unique() %>% 
  sort()

ls_firm_br_flist


df_jpc_cl2 %>% 
  filter(year == 2015) %>% 
  select(eff, starts_with("N")) %>% 
  with(hist(N_brand))


ls_ag <- 
  read_csv("./dataset/firm_list/ag_list.csv", locale = locale(encoding = "Shift_JIS")) %>% 
  use_series(ag) %>% 
  unique() %>% 
  sort()

ls_br_withAG <- 
  read_csv("./dataset/firm_list/ag_list.csv", locale = locale(encoding = "Shift_JIS")) %>% 
  use_series(brand) %>% 
  unique() %>% 
  sort()

sum(df_jpc_sample$name %in% ls_ag)

