
## library
library(tidyverse)
library(magrittr)
library(dlookr)

## read
df_shusai_wide <- readRDS("./output/data/shusai_wide.rds")

## summary
df_shusai %>% diagnose()

df_shusai_wide %>% summary()

df_shusai_wide %>% 
  filter(N_brand > 1) %>% 
  View()


df_shusai %>% 
  use_series(generic) %>% 
  unique()


df_shusai %>% 
  use_series(ag) %>% 
  unique()

df_shusai %>% 
  use_series(brand) %>% 
  unique()



df_shusai_wide %>% 
  ggplot() + 
  geom_histogram(aes(x = N_firm, fill = as.factor(year)), 
                 position = "identity", 
                 binwidth = 0.5)



# draft -------------------------------------------------------------------

df_shusai_wide %>% 
  filter(ingredient %in% ls_ingr_2012) %>% 
  group_by(year, ingredient) %>% 
  summarise(count = length(unique(maker)), 
            .groups = "drop") %>% 
  group_by(year) %>% 
  summarise(count = mean(count)) %>% 
  ggplot(aes(x = year, y = count)) +
  geom_bar(stat = "identity")



ls_ingr_2012 <- df_shusai %>% 
  filter(year == 2012) %>% 
  use_series(ingredient) %>% 
  unique() %>% 
  sort()

df_shusai_wide %>% 
  filter(ingredient %in% ls_ingr_2012) %>% 
  group_by(year, ingredient) %>% 
  summarise(count = length(unique(maker)), 
            .groups = "drop") %>% 
  group_by(year) %>% 
  summarise(count = mean(count)) %>% 
  View()


df_shusai_wide <- df_shusai %>% 
  filter(year != 2012) %>% 
  filter(ingredient %in% ls_ingr_2012) %>%
  filter(!is.na(maker)) %>% 
  filter(!(is.na(brand) & is.na(generic) & is.na(ag))) %>% 
  group_by(year, ingredient) %>% 
  mutate(N_firm = length(unique(maker)), 
         N_brand = length(unique(maker[!is.na(.$brand)])),
         N_gen = length(unique(maker[!is.na(.$generic)])), 
         N_ag = length(unique(maker[!is.na(.$ag)]))) %>% 
  ungroup() %>% 
  select(ingredient, year, N_firm) %>% 
  pivot_wider(id_cols = c(ingredient), 
              names_from = year, 
              names_glue = "{.value}{year}", 
              values_from = N_firm)



df_tmp <- data.frame(name = rep(c("A", "B", "C", "D"), each = 3), 
                     year = rep(c(2012, 2014, 2016), times = 4), 
                     N = c(1:12)) %>% 
  filter(!N %in% c(4, 7:9))


df_tmp %>% pivot_wider(id_cols = name, 
                       names_from = year, 
                       values_from = N, 
                       names_glue = "{.value}{year}")



df_shusai_wide <- df_shusai %>% 
  filter(year != 2012) %>% 
  filter(ingredient %in% ls_ingr_2012) %>%
  filter(!is.na(maker)) %>% 
  filter(!(is.na(brand) & is.na(generic) & is.na(ag))) %>% 
  group_by(ingredient, year) %>% 
  summarise(N_firm = length(unique(maker)), 
         N_brand = length(unique(maker[!is.na(.$brand)])[!is.na(unique(maker[!is.na(.$ag)]))]),
         N_gen = length(unique(maker[!is.na(.$generic)])[!is.na(unique(maker[!is.na(.$ag)]))]), 
         N_ag = length(unique(maker[!is.na(.$ag)])[!is.na(unique(maker[!is.na(.$ag)]))]), 
         .groups = "drop") %>% 
  pivot_wider(id_cols = c(ingredient), 
              names_from = year, 
              names_glue = "{.value}{year}", 
              values_from = c(N_firm, N_brand, N_gen, N_ag)) %>% 
  na.omit()



df_shusai_wide <- df_shusai %>% 
  filter(year != 2012) %>% 
  filter(ingredient %in% ls_ingr_2012) %>%
  filter(!is.na(maker)) %>% 
  filter(!(is.na(brand) & is.na(generic) & is.na(ag))) %>% 
  group_by(ingredient, year) %>% 
  summarise(N_firm = length(unique(maker)), 
            N_brand = length(unique(maker[!is.na(.$brand)])[!is.na(unique(maker[!is.na(.$ag)]))]),
            N_gen = length(unique(maker[!is.na(.$generic)])[!is.na(unique(maker[!is.na(.$ag)]))]), 
            N_ag = length(unique(maker[!is.na(.$ag)])[!is.na(unique(maker[!is.na(.$ag)]))]), 
            .groups = "drop") %>% 
  group_by(year) %>% 
  summarise(across(.cols = starts_with("N_"), 
                   .fns = ~sum(.x), 
                   .names = "{.col}"), 
            .groups = "drop")


df_shusai_wide <- df_shusai %>% 
  filter(year != 2012) %>% 
  filter(ingredient %in% ls_ingr_2012) %>%
  filter(!is.na(maker)) %>% 
  filter(!(is.na(brand) & is.na(generic) & is.na(ag))) %>% 
  group_by(ingredient, year) %>% 
  summarise(N_firm = length(unique(maker)), 
            N_brand = length(unique(maker[!is.na(.$brand)])[!is.na(unique(maker[!is.na(.$ag)]))]),
            N_gen = length(unique(maker[!is.na(.$generic)])[!is.na(unique(maker[!is.na(.$ag)]))]), 
            N_ag = length(unique(maker[!is.na(.$ag)])[!is.na(unique(maker[!is.na(.$ag)]))]), 
            .groups = "drop")

df_shusai_wide <- df_shusai %>% 
  filter(year != 2012) %>% 
  filter(ingredient %in% ls_ingr_2012) %>%
  filter(!is.na(maker)) %>% 
  filter(!(is.na(brand) & is.na(generic) & is.na(ag))) %>% 
  group_by(ingredient, year) %>% 
  summarise(N_firm = length(unique(maker)), 
            N_brand = length(unique(maker[!is.na(.$brand)])[!is.na(unique(maker[!is.na(.$ag)]))]),
            N_gen = length(unique(maker[!is.na(.$generic)])[!is.na(unique(maker[!is.na(.$ag)]))]), 
            N_ag = length(unique(maker[!is.na(.$ag)])[!is.na(unique(maker[!is.na(.$ag)]))]), 
            .groups = "drop") %>% 
  pivot_wider(id_cols = c(ingredient), 
              names_from = year, 
              names_glue = "{.value}_{year}", 
              values_from = c(N_firm, N_brand, N_gen, N_ag)) %>% 
  na.omit() %>% 
  pivot_longer(cols = - c(ingredient), 
               names_to = "type", 
               values_to = "N") %>% 
  mutate(year = str_split(.$type, pattern = "_", simplify = TRUE)[, 3], 
         metrics = str_split(.$type, pattern = "_", simplify = TRUE)[, 2]) %>% 
  select(ingredient, year, metrics, N) %>% 
  mutate(year = as.numeric(year))


df_shusai_wide %>% 
  group_by(year, metrics) %>% 
  summarise(N = sum(N), .groups = "drop") %>% 
  ggplot(aes(x = year, y = N, colour = metrics)) +
  geom_point() + 
  geom_line(position = "identity", size = 2) + 
  geom_text(aes(label = N))



df_shusai_wide <- df_shusai %>% 
  filter(year != 2012) %>% 
  filter(ingredient %in% ls_ingr_2012) %>%
  filter(!is.na(maker)) %>% 
  filter(!(is.na(brand) & is.na(generic) & is.na(ag))) %>% 
  group_by(ingredient, year) %>% 
  summarise(N_firm = length(unique(maker)), 
            N_brand = length(unique(maker[.$brand == "先発品"])[!is.na(unique(maker[.$brand == "先発品"]))]),
            N_gen = length(unique(maker[.$generic == "後発品"])[!is.na(unique(maker[.$generic == "後発品"]))]), 
            N_ag = length(unique(maker[.$ag == "○"])[!is.na(unique(maker[.$ag == "○"]))]), 
            .groups = "drop") %>% 
  pivot_wider(id_cols = c(ingredient), 
              names_from = year, 
              names_glue = "{.value}_{year}", 
              values_from = c(N_firm, N_brand, N_gen, N_ag)) %>% 
  na.omit() %>% 
  pivot_longer(cols = - c(ingredient), 
               names_to = "type", 
               values_to = "N") %>% 
  mutate(year = str_split(.$type, pattern = "_", simplify = TRUE)[, 3], 
         metrics = str_split(.$type, pattern = "_", simplify = TRUE)[, 2]) %>% 
  select(ingredient, year, metrics, N) %>% 
  mutate(year = as.numeric(year))


df_shusai_wide %>% 
  group_by(year, metrics) %>% 
  summarise(N = mean(N), .groups = "drop") %>% 
  mutate(N = round(N, digits = 2)) %>% 
  ggplot(aes(x = year, y = N, colour = metrics)) +
  geom_point() + 
  geom_line(position = "identity", size = 1) + 
  geom_text(aes(label = N))



df_shusai_wide_01 <- df_shusai_wide %>% 
  left_join(y = na.omit(df_shusai[df_shusai$year == "2012" & is.na(df_shusai$generic), 
                          c("ingredient", "code_shusai", "maker")]), 
            by = c("ingredient" = "ingredient"))


df_shusai_wide_02 <- df_shusai_wide %>% 
  select(ingredient, ends_with("_2014"), ends_with("_2016"), 
         ends_with("_2016"), ends_with("_2018"), ends_with("_2020")) %>% 
  mutate(N_brand_2014 = N_brand_2014 - N_ag_2014, 
         N_brand_2016 = N_brand_2016 - N_ag_2016, 
         N_brand_2018 = N_brand_2018 - N_ag_2018, 
         N_brand_2020 = N_brand_2020 - N_ag_2014)

df_shusai[df_shusai$year == "2012", ] %>% diagnose()
df_shusai[df_shusai$year == "2012", ] %>% nrow()
