
# library
library(tidyverse)

# source
source("./dataset/00_source.R")
source("./dataset/01_1_cleaning_shusai.R", encoding = "UTF-8")

#
## wider
df_jpc_wide <- 
  df_jpc %>% 
  filter(eff %in% ls_eff_smpl) %>% 
  ## form
  mutate(form1 = str_split(.$form, pattern = "：", simplify = TRUE)[, 1], 
         form2_tmp = str_split(.$form, pattern = "：", simplify = TRUE)[, 2]) %>% 
  mutate(form2 = str_split(.$form2_tmp, pattern = "〔", simplify = TRUE)[, 1], 
         form3 = str_replace(str_split(.$form2_tmp, pattern = "〔", simplify = TRUE)[, 2], 
                             pattern = "〕", replacement = "")) %>% 
  select(-form2_tmp) %>% 
  filter(!str_detect(other, pattern = "薬価削除")) %>% 
  mutate(note_tmp = str_split(.$other, pattern = "，", simplify = TRUE)[, 1]) %>% 
  mutate(note1 = str_extract(note_tmp, pattern = "\\p{Han}+"),
         note2 = str_extract(note_tmp, pattern = "(\\d+).(\\d+).(\\d+)"), 
         note3 = str_split(.$other, pattern = "，", simplify = TRUE)[, 2]) %>% 
  mutate(note2 = ymd(note2)) %>% 
  select(-note_tmp)  %>% 
  mutate(brand = ifelse(code_yj %in% ls_code_br, T, F), 
         ag = ifelse(name %in% ls_name_ag, T, F)) %>%  
  group_by(year, eff) %>% 
  summarize(N_brand = length(unique(maker[brand])), 
         N_ag = length(unique(maker[ag])),
         N_firm = length(unique(maker)), 
         N_gen = N_firm - N_brand, 
         old_identity = if_else(anyNA(note1), T, F), 
         .groups = "drop") %>% 
  filter(!old_identity) %>% 
  select(- old_identity) %>% 
  arrange(year, eff) %>% 
  pivot_wider(id_cols = c(eff), 
              names_from = year, 
              names_glue = "{.value}_{year}", 
              values_from = c(N_firm, N_brand, N_gen, N_ag)) %>% 
  na.omit() 

## longer
df_jpc_long <- df_jpc_wide %>% 
  pivot_longer(cols = - c(eff), 
               names_to = "type", 
               values_to = "N") %>% 
  mutate(year = str_split(.$type, pattern = "_", simplify = TRUE)[, 3], 
         metrics = str_split(.$type, pattern = "_", simplify = TRUE)[, 2]) %>% 
  select(eff, year, metrics, N) %>% 
  mutate(year = as.numeric(year))

## plot
df_jpc_long %>% 
  mutate(metrics = ifelse(metrics == "firm", "whole", metrics)) %>% 
  group_by(year, metrics) %>% 
  summarise(N = mean(N), .groups = "drop") %>% 
  mutate(N = round(N, digits = 2)) %>% 
  ggplot(aes(x = year, y = N, colour = metrics)) +
  geom_point() + 
  geom_line(position = "identity", size = 1) + 
  geom_text(aes(label = N)) + 
  theme_minimal()




