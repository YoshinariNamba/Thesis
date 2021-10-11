## library
library(tidyverse)

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


