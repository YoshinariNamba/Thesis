
# library
library(tidyverse)

# oral --------------------------------------------------------------------

### read csv
df_ndb_o_2016_in <- read_csv("./dataset/ndb/arranged/2016/ndb_o_2016_in.csv", 
                             locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         unit = 3, 
         code_shusai = 4, 
         price_in = 5, 
         generic = 6, 
         total_in = 7) %>% 
  mutate(total_in = as.numeric(str_replace_all(.$total_in, pattern = ",", replacement = "")), 
         form = "oral")
df_ndb_o_2016_out <- read_csv("./dataset/ndb/arranged/2016/ndb_o_2016_out.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         unit = 3, 
         code_shusai = 4, 
         price_out = 5, 
         generic = 6, 
         total_out = 7) %>% 
  mutate(total_out = as.numeric(str_replace_all(.$total_out, pattern = ",", replacement = "")), 
         form = "oral")
df_ndb_o_2016_hos <- read_csv("./dataset/ndb/arranged/2016/ndb_o_2016_hos.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         unit = 3, 
         code_shusai = 4, 
         price_hos = 5, 
         generic = 6, 
         total_hos = 7) %>% 
  mutate(total_hos = as.numeric(str_replace_all(.$total_hos, pattern = ",", replacement = "")), 
         form = "oral")

### join
df_ndb_o_2016 <- df_ndb_o_2016_in %>% 
  full_join(df_ndb_o_2016_out, by = c("code_receipt", "name", "code_shusai", "generic", "form", "unit")) %>% 
  full_join(df_ndb_o_2016_hos, by = c("code_receipt", "name", "code_shusai", "generic", "form", "unit")) %>% 
  mutate(total = if_else(is.na(total_in), 0, total_in) + 
           if_else(is.na(total_out), 0, total_out) + 
           if_else(is.na(total_hos), 0, total_hos)) %>% 
  mutate(price = ifelse(!is.na(price_in), price_in, 
                        ifelse(!is.na(price_out), price_out, 
                               ifelse(is.na(price_hos), price_hos, 
                                      NA)))) %>% 
  select(code_receipt, name, code_shusai, price, generic, form, unit, 
         total, total_in, total_out, total_hos) 

### delete
rm(list = c("df_ndb_o_2016_in", "df_ndb_o_2016_out", "df_ndb_o_2016_hos"))




# parenteral --------------------------------------------------------------

### read csv
df_ndb_p_2016_in <- read_csv("./dataset/ndb/arranged/2016/ndb_p_2016_in.csv", 
                             locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         unit = 3, 
         code_shusai = 4, 
         price_in = 5, 
         generic = 6, 
         total_in = 7) %>% 
  mutate(total_in = as.numeric(str_replace_all(.$total_in, pattern = ",", replacement = "")), 
         form = "parenteral")
df_ndb_p_2016_out <- read_csv("./dataset/ndb/arranged/2016/ndb_p_2016_out.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         unit = 3, 
         code_shusai = 4, 
         price_out = 5, 
         generic = 6, 
         total_out = 7) %>% 
  mutate(total_out = as.numeric(str_replace_all(.$total_out, pattern = ",", replacement = "")), 
         form = "parenteral")
df_ndb_p_2016_hos <- read_csv("./dataset/ndb/arranged/2016/ndb_p_2016_hos.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         unit = 3, 
         code_shusai = 4, 
         price_hos = 5, 
         generic = 6, 
         total_hos = 7) %>% 
  mutate(total_hos = as.numeric(str_replace_all(.$total_hos, pattern = ",", replacement = "")), 
         form = "parenteral")

### join
df_ndb_p_2016 <- df_ndb_p_2016_in %>% 
  full_join(df_ndb_p_2016_out, by = c("code_receipt", "name", "code_shusai", "generic", "form", "unit")) %>% 
  full_join(df_ndb_p_2016_hos, by = c("code_receipt", "name", "code_shusai", "generic", "form", "unit")) %>% 
  mutate(total = if_else(is.na(total_in), 0, total_in) + 
           if_else(is.na(total_out), 0, total_out) + 
           if_else(is.na(total_hos), 0, total_hos)) %>% 
  mutate(price = ifelse(!is.na(price_in), price_in, 
                        ifelse(!is.na(price_out), price_out, 
                               ifelse(is.na(price_hos), price_hos, 
                                      NA)))) %>% 
  select(code_receipt, name, code_shusai, price, generic, form, unit, 
         total, total_in, total_out, total_hos) 

### delete
rm(list = c("df_ndb_p_2016_in", "df_ndb_p_2016_out", "df_ndb_p_2016_hos"))




# topical -----------------------------------------------------------------

### read csv
df_ndb_t_2016_in <- read_csv("./dataset/ndb/arranged/2016/ndb_t_2016_in.csv", 
                             locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         unit = 3, 
         code_shusai = 4, 
         price_in = 5, 
         generic = 6, 
         total_in = 7) %>% 
  mutate(total_in = as.numeric(str_replace_all(.$total_in, pattern = ",", replacement = "")), 
         form = "topical")
df_ndb_t_2016_out <- read_csv("./dataset/ndb/arranged/2016/ndb_t_2016_out.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         unit = 3, 
         code_shusai = 4, 
         price_out = 5, 
         generic = 6, 
         total_out = 7) %>% 
  mutate(total_out = as.numeric(str_replace_all(.$total_out, pattern = ",", replacement = "")), 
         form = "topical")
df_ndb_t_2016_hos <- read_csv("./dataset/ndb/arranged/2016/ndb_t_2016_hos.csv", 
                              locale = locale(encoding = "Shift-JIS")) %>% 
  rename(code_receipt = 1, 
         name = 2, 
         unit = 3, 
         code_shusai = 4, 
         price_hos = 5, 
         generic = 6, 
         total_hos = 7) %>% 
  mutate(total_hos = as.numeric(str_replace_all(.$total_hos, pattern = ",", replacement = "")), 
         form = "topical")

### join
df_ndb_t_2016 <- df_ndb_t_2016_in %>% 
  full_join(df_ndb_t_2016_out, by = c("code_receipt", "name", "code_shusai", "generic", "form", "unit")) %>% 
  full_join(df_ndb_t_2016_hos, by = c("code_receipt", "name", "code_shusai", "generic", "form", "unit")) %>% 
  mutate(total = if_else(is.na(total_in), 0, total_in) + 
           if_else(is.na(total_out), 0, total_out) + 
           if_else(is.na(total_hos), 0, total_hos)) %>% 
  mutate(price = ifelse(!is.na(price_in), price_in, 
                        ifelse(!is.na(price_out), price_out, 
                               ifelse(is.na(price_hos), price_hos, 
                                      NA)))) %>% 
  select(code_receipt, name, code_shusai, price, generic, form, unit, 
         total, total_in, total_out, total_hos) 

### delete
rm(list = c("df_ndb_t_2016_in", "df_ndb_t_2016_out", "df_ndb_t_2016_hos"))



# merge -------------------------------------------------------------------

## merge 
df_ndb_2016 <- df_ndb_o_2016 %>% 
  bind_rows(df_ndb_t_2016) %>% 
  bind_rows(df_ndb_p_2016) %>% 
  mutate(year = 2016)

## delete
rm(list = c("df_ndb_o_2016", "df_ndb_t_2016", "df_ndb_p_2016"))


