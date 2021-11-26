# library
library(tidyverse)

# directory
setwd("C:/Users/Namba Yoshinari/Documents/Github_Repositories/Thesis/analysis/dataset/ndb/append")




# oral --------------------------------------------------------------------

### read csv
df_ndb_o_2014_in <- read_csv("../arranged/2014/ndb_o_2014_in.csv", 
                             locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         code_shusai = 3, 
         price_in = 4, 
         generic = 5, 
         total_in = 6) %>% 
  mutate(total_in = as.numeric(str_replace_all(.$total_in, pattern = ",", replacement = "")), 
         form = "oral")
df_ndb_o_2014_out <- read_csv("../arranged/2014/ndb_o_2014_out.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         code_shusai = 3, 
         price_out = 4, 
         generic = 5, 
         total_out = 6) %>% 
  mutate(total_out = as.numeric(str_replace_all(.$total_out, pattern = ",", replacement = "")), 
         form = "oral")
df_ndb_o_2014_hos <- read_csv("../arranged/2014/ndb_o_2014_hos.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         code_shusai = 3, 
         price_hos = 4, 
         generic = 5, 
         total_hos = 6) %>% 
  mutate(total_hos = as.numeric(str_replace_all(.$total_hos, pattern = ",", replacement = "")), 
         form = "oral")

### join
df_ndb_o_2014 <- df_ndb_o_2014_in %>% 
  full_join(df_ndb_o_2014_out, by = c("code_receipt", "name", "code_shusai", "generic", "form")) %>% 
  full_join(df_ndb_o_2014_hos, by = c("code_receipt", "name", "code_shusai", "generic", "form")) %>% 
  mutate(total = if_else(is.na(total_in), 0, total_in) + 
           if_else(is.na(total_out), 0, total_out) + 
           if_else(is.na(total_hos), 0, total_hos)) %>% 
  mutate(price = ifelse(!is.na(price_in), price_in, 
                        ifelse(!is.na(price_out), price_out, 
                               ifelse(is.na(price_hos), price_hos, 
                                      NA)))) %>% 
  select(code_receipt, name, code_shusai, price, generic, form, 
         total, total_in, total_out, total_hos) 

### delete
rm(list = c("df_ndb_o_2014_in", "df_ndb_o_2014_out", "df_ndb_o_2014_hos"))


# parenteral --------------------------------------------------------------


### read csv
df_ndb_p_2014_in <- read_csv("../arranged/2014/ndb_p_2014_in.csv", 
                             locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         code_shusai = 3, 
         price_in = 4, 
         generic = 5, 
         total_in = 6) %>% 
  mutate(total_in = as.numeric(str_replace_all(.$total_in, pattern = ",", replacement = "")), 
         form = "parenteral")
df_ndb_p_2014_out <- read_csv("../arranged/2014/ndb_p_2014_out.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         code_shusai = 3, 
         price_out = 4, 
         generic = 5, 
         total_out = 6) %>% 
  mutate(total_out = as.numeric(str_replace_all(.$total_out, pattern = ",", replacement = "")), 
         form = "parenteral")
df_ndb_p_2014_hos <- read_csv("../arranged/2014/ndb_p_2014_hos.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         code_shusai = 3, 
         price_hos = 4, 
         generic = 5, 
         total_hos = 6) %>% 
  mutate(total_hos = as.numeric(str_replace_all(.$total_hos, pattern = ",", replacement = "")), 
         form = "parenteral")

### join
df_ndb_p_2014 <- df_ndb_p_2014_in %>% 
  full_join(df_ndb_p_2014_out, by = c("code_receipt", "name", "code_shusai", "generic", "form")) %>% 
  full_join(df_ndb_p_2014_hos, by = c("code_receipt", "name", "code_shusai", "generic", "form")) %>% 
  mutate(total = if_else(is.na(total_in), 0, total_in) + 
           if_else(is.na(total_out), 0, total_out) + 
           if_else(is.na(total_hos), 0, total_hos)) %>% 
  mutate(price = ifelse(!is.na(price_in), price_in, 
                        ifelse(!is.na(price_out), price_out, 
                               ifelse(is.na(price_hos), price_hos, 
                                      NA)))) %>% 
  select(code_receipt, name, code_shusai, price, generic, form, 
         total, total_in, total_out, total_hos) 

### delete
rm(list = c("df_ndb_p_2014_in", "df_ndb_p_2014_out", "df_ndb_p_2014_hos"))




# topical -----------------------------------------------------------------

### read csv
df_ndb_t_2014_in <- read_csv("../arranged/2014/ndb_t_2014_in.csv", 
                             locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         code_shusai = 3, 
         price_in = 4, 
         generic = 5, 
         total_in = 6) %>% 
  mutate(total_in = as.numeric(str_replace_all(.$total_in, pattern = ",", replacement = "")), 
         form = "topical")
df_ndb_t_2014_out <- read_csv("../arranged/2014/ndb_t_2014_out.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         code_shusai = 3, 
         price_out = 4, 
         generic = 5, 
         total_out = 6) %>% 
  mutate(total_out = as.numeric(str_replace_all(.$total_out, pattern = ",", replacement = "")), 
         form = "topical")
df_ndb_t_2014_hos <- read_csv("../arranged/2014/ndb_t_2014_hos.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         code_shusai = 3, 
         price_hos = 4, 
         generic = 5, 
         total_hos = 6) %>% 
  mutate(total_hos = as.numeric(str_replace_all(.$total_hos, pattern = ",", replacement = "")), 
         form = "topical")

### join
df_ndb_t_2014 <- df_ndb_t_2014_in %>% 
  full_join(df_ndb_t_2014_out, by = c("code_receipt", "name", "code_shusai", "generic", "form")) %>% 
  full_join(df_ndb_t_2014_hos, by = c("code_receipt", "name", "code_shusai", "generic", "form")) %>% 
  mutate(total = if_else(is.na(total_in), 0, total_in) + 
           if_else(is.na(total_out), 0, total_out) + 
           if_else(is.na(total_hos), 0, total_hos)) %>% 
  mutate(price = ifelse(!is.na(price_in), price_in, 
                        ifelse(!is.na(price_out), price_out, 
                               ifelse(is.na(price_hos), price_hos, 
                                      NA)))) %>% 
  select(code_receipt, name, code_shusai, price, generic, form, 
         total, total_in, total_out, total_hos) 

### delete
rm(list = c("df_ndb_t_2014_in", "df_ndb_t_2014_out", "df_ndb_t_2014_hos"))



# merge -------------------------------------------------------------------

## merge 
df_ndb_2014 <- df_ndb_o_2014 %>% 
  bind_rows(df_ndb_t_2014) %>% 
  bind_rows(df_ndb_p_2014)

## delete
rm(list = c("df_ndb_o_2014", "df_ndb_t_2014", "df_ndb_p_2014"))


