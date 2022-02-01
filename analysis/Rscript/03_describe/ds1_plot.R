
# library
library(tidyverse)

# source
source("./Rscript/01_append/ap0_master.R")
source("./Rscript/02_clean/cl2_japic.R", encoding = "UTF-8")


# time series -------------------------------------------------------------

#
## plot
df_jpc_plt <- 
  df_jpc_smpl %>% 
  group_by(year, eff) %>% 
  summarize(N_brand = length(unique(maker[brand])), 
            N_ag = length(unique(maker[ag])),
            N_firm = length(unique(maker)), 
            N_gen = N_firm - N_brand, 
            .groups = "drop") %>% 
  arrange(year, eff)

## wider
df_jpc_wide <- 
  df_jpc_plt %>% 
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
  mutate(metrics = ifelse(metrics == "firm", "total", metrics)) %>% 
  group_by(year, metrics) %>% 
  summarise(N = mean(N), .groups = "drop") %>% 
  mutate(N = round(N, digits = 2)) %>% 
  ggplot(aes(x = year, y = N, colour = metrics)) +
  geom_point() + 
  geom_line(position = "identity", size = 1) + 
  geom_text(aes(label = N)) + 
  theme_minimal()



# listed ------------------------------------------------------------------

# listed year
df_jpc_cl1 %>% 
  filter(note1 == "薬価収載") %>% 
  arrange(year) %>% 
  distinct(code_yj, .keep_all = T) %>% 
  ggplot(aes(x = note2)) +
  geom_histogram() + 
  labs(x = "Date") +
  theme_minimal() 

#
df_jpc_cl1 %>% 
  filter(note1 == "薬価収載", 
         brand) %>%  
  arrange(year) %>% 
  distinct(code_yj, .keep_all = T) %>% 
  ggplot(aes(x = note2)) +
  geom_histogram() + 
  labs(x = "Date") +
  theme_minimal() 

# lag
df_jpc_cl1 %>% 
  filter(note1 == "薬価収載", 
         !is.na(year_listed)) %>% 
  group_by(year, eff) %>% 
  summarize(
    lag = min(year_listed[!brand], na.rm = T) - min(year_listed[brand], na.rm = T), 
    .groups = "drop"
    ) %>% 
  filter(!is.na(lag), 
         !lag %in% c(Inf, -Inf), 
         lag >= 0) %>% 
  arrange(year) %>% 
  distinct(eff, .keep_all = T) %>% 
  ggplot(aes(lag)) + 
  geom_histogram() + 
  theme_minimal() + 
  geom_vline(xintercept = mean(lag, na.rm = T), color = "red")


df_jpc_cl1 %>% 
  filter(note1 == "薬価収載", 
         !is.na(year_listed)) %>% 
  group_by(year, eff) %>% 
  summarize(
    lag = min(year_listed[!brand], na.rm = T) - min(year_listed[brand], na.rm = T), 
    .groups = "drop"
  ) %>% 
  filter(!is.na(lag), 
         !lag %in% c(Inf, -Inf), 
         lag >= 0) %>% 
  arrange(year) %>% 
  distinct(eff, .keep_all = T) %>% 
  select(lag) %>% 
  mean(na.rm = T)



df_jpc_cl1 %>% 
  filter(note1 == "薬価収載", 
         !is.na(year_listed)) %>% 
  group_by(year, eff) %>% 
  summarize(
    listed_lag = case_when(
      sum(brand) > 0 & sum(!brand) > 0 ~ min(year_listed[!brand], na.rm = T) - min(year_listed[brand], na.rm = T), 
      sum(brand) <= 0 & sum(!brand) <= 0 ~ -1
    ), 
    .groups = "drop"
  ) %>% 
  filter(listed_lag != "missing", 
         !is.na(listed_lag), 
         !listed_lag %in% c(Inf, -Inf), 
         listed_lag >= 0) %>% 
  arrange(year) %>% 
  distinct(eff, .keep_all = T) %>% 
  ggplot(aes(listed_lag)) + 
  geom_bar() + 
  theme_minimal() + 
  geom_vline(xintercept = mean(listed_lag, na.rm = TRUE))

df_jpc_cl1 %>% 
  filter(note1 == "薬価収載", 
         !is.na(year_listed)) %>% 
  group_by(year, eff) %>% 
  summarize(
    listed_lag = case_when(
      sum(brand) > 0 & sum(!brand) > 0 ~ min(year_listed[!brand], na.rm = T) - min(year_listed[brand], na.rm = T), 
      sum(brand) <= 0 & sum(!brand) <= 0 ~ -100
    ), 
    .groups = "drop"
  ) %>% 
  arrange(year) %>% 
  distinct(eff, .keep_all = T) %>% 
  count(listed_lag) %>% 
  View()

df_jpc_tmp <- 
  df_jpc_cl1 %>% 
  filter(!note1 %in% c("薬価削除", "薬価削除予定")) %>% 
  group_by(eff) %>% 
  filter(!anyNA(year_listed), 
         sum(brand) > 0, 
         sum(!brand) > 0) %>% 
  summarise(
    listed_brand = min(year_listed[brand]), 
    listed_generic = min(year_listed[!brand]), 
    .groups = "drop"
  ) %>% 
  mutate(listed_lag = listed_generic - listed_brand) %>% 
  arrange(listed_lag) 

df_jpc_tmp_view <- 
  df_jpc_cl1 %>% 
  filter(!note1 %in% c("薬価削除", "薬価削除予定")) %>% 
  group_by(eff) %>% 
  filter(!anyNA(year_listed), 
         sum(brand) > 0, 
         sum(!brand) > 0) %>% 
  ungroup() %>% 
  select(year, code_yj, name, maker, eff, name_g, year_listed, brand, form1, form2) %>% 
  arrange(eff, year, !brand, year_listed) %>% 
  filter(eff == "選択的β1-アンタゴニスト") 

df_shusai %>% 
  filter(code_shusai %in% c("3319563A2031", "3319557A1030")) %>% 
  View()

df_jpc_tmp2 <- 
  df_jpc_cl1 %>% 
  filter(!note1 %in% c("薬価削除", "薬価削除予定")) %>% 
  group_by(name_g) %>% 
  filter(!anyNA(year_listed), 
         sum(brand) > 0, 
         sum(!brand) > 0) %>% 
  summarise(
    listed_brand = min(year_listed[brand]), 
    listed_generic = min(year_listed[!brand]), 
    .groups = "drop"
  ) %>% 
  mutate(listed_lag = listed_generic - listed_brand) %>% 
  arrange(listed_lag) 

df_jpc_tmp_view2 <- 
  df_jpc_cl1 %>% 
  filter(!note1 %in% c("薬価削除", "薬価削除予定")) %>% 
  group_by(name_g) %>% 
  filter(!anyNA(year_listed), 
         sum(brand) > 0, 
         sum(!brand) > 0) %>% 
  ungroup() %>% 
  select(year, code_yj, name, maker, eff, name_g, year_listed, brand) %>% 
  arrange(name_g, year, !brand, year_listed) %>% 
  filter(eff == "細胞外液補充液") 



df_jpc_tmp %>% 
  ggplot(aes(listed_lag)) + 
  geom_bar() + 
  geom_vline(xintercept = mean(df_jpc_tmp$listed_lag), color = "blue") + 
  theme_minimal()

df_jpc_tmp2 %>% 
  ggplot(aes(listed_lag)) + 
  geom_bar() + 
  geom_vline(xintercept = mean(df_jpc_tmp2$listed_lag), color = "blue") + 
  theme_minimal()
