
library(tidyverse)
library(magrittr)

# source
df_shusai <- readRDS("./output/data/shusai.rds")


# create lists ------------------------------------------------------------

## code_shusai #############
### whole
ls_code <- df_shusai %>% 
  filter(year == "2014") %>% 
  use_series(code_shusai) %>% 
  unique() %>% 
  sort()

### brand
ls_code_brnd <- df_shusai %>% 
  filter(year == "2014") %>% 
  filter(brand == "先発品") %>% 
  use_series(code_shusai) %>% 
  unique() %>% 
  sort()

### sub-brand
ls_code_sbrnd <- df_shusai %>% 
  filter(year == "2014") %>% 
  filter(brand == "準先発品") %>% 
  use_series(code_shusai) %>% 
  unique() %>% 
  sort()

### AG
ls_code_ag <- df_shusai %>% 
  filter(year == "2014") %>% 
  filter(ag == "○") %>% 
  use_series(code_shusai) %>% 
  unique() %>% 
  sort()

### generic
ls_code_gnrc <- df_shusai %>% 
  filter(year == "2014") %>% 
  filter(generic == "後発品") %>% 
  use_series(code_shusai) %>% 
  unique() %>% 
  sort()

ls_code_star <- df_shusai %>% 
  filter(year == "2014") %>% 
  filter(generic == "★" ) %>% 
  use_series(code_shusai) %>% 
  unique() %>% 
  sort()


## firm ###############
### whole
ls_firm <- df_shusai %>%
  filter(year == "2014") %>% 
  filter(!is.na(maker)) %>% 
  use_series(maker) %>% 
  unique() %>% 
  sort()

### brand
ls_firm_brnd <- df_shusai %>% 
  filter(year == "2014") %>% 
  filter(!is.na(maker), 
         brand == "先発品") %>% 
  use_series(maker) %>% 
  unique() %>% 
  sort()

### generic
ls_firm_gnrc <- df_shusai %>% 
  filter(year == "2014") %>% 
  filter(!is.na(maker), 
         generic == "後発品") %>% 
  use_series(maker) %>% 
  unique() %>% 
  sort()

### ag
ls_firm_ag <- df_shusai %>% 
  filter(year == "2014") %>% 
  filter(!is.na(maker), 
         ag == "○") %>% 
  use_series(maker) %>% 
  unique() %>% 
  sort()


## ingredients #########
ls_ingr_2012 <- df_shusai %>% 
  filter(year == 2012) %>% 
  use_series(ingredient) %>% 
  unique() %>% 
  sort()



# detect overlapping label ------------------------------------------------

## code_shusai #######
### full code
ls_code %>% length() ## 25768

### brnd
intersect(ls_code_brnd, ls_code_sbrnd) %>% length() ## 0
intersect(ls_code_brnd, ls_code_ag) %>% length()  ## 1715
intersect(ls_code_brnd, ls_code_gnrc) %>% length() ## 2
intersect(ls_code_brnd, ls_code_star) %>% length() ## 0

### sbrnd
intersect(ls_code_sbrnd, ls_code_ag) %>% length() ## 207
intersect(ls_code_sbrnd, ls_code_gnrc) %>% length() ## 0
intersect(ls_code_sbrnd, ls_code_star) %>% length() ## 0

### ag
intersect(ls_code_ag, ls_code_gnrc) %>% length() ## 2
intersect(ls_code_ag, ls_code_star) %>% length() ## 0

### gnrc
intersect(ls_code_gnrc, ls_code_star) %>% length() ## 120



## firms #######
### full code
ls_firm %>% length() ## 416

### dupulication
intersect(ls_firm_brnd, ls_firm_ag) %>% length()  ## 137
intersect(ls_firm_brnd, ls_firm_gnrc) %>% length() ## 131
intersect(ls_firm_ag, ls_firm_gnrc) %>% length() ## 110





# wide --------------------------------------------------------------------

## wider
df_shusai_wide <- df_shusai %>% 
  filter(year != 2012) %>% 
  filter(ingredient %in% ls_ingr_2012) %>%
  filter(!is.na(maker)) %>% 
  filter(!(is.na(brand) & is.na(generic) & is.na(ag))) %>% 
  group_by(ingredient, year) %>% 
  summarise(N_firm = length(unique(maker)[!is.na(unique(maker))]), 
            N_brand = length(unique(maker[.$brand == "先発品"])[!is.na(unique(maker[.$brand == "先発品"]))]),
            N_gen = length(unique(maker[.$generic == "後発品"])[!is.na(unique(maker[.$generic == "後発品"]))]), 
            N_ag = length(unique(maker[.$ag == "○"])[!is.na(unique(maker[.$ag == "○"]))]), 
            .groups = "drop") %>% 
  pivot_wider(id_cols = c(ingredient), 
              names_from = year, 
              names_glue = "{.value}_{year}", 
              values_from = c(N_firm, N_brand, N_gen, N_ag)) %>% 
  na.omit() 

## longer
df_shusai_long <- df_shusai_wide %>% 
  pivot_longer(cols = - c(ingredient), 
               names_to = "type", 
               values_to = "N") %>% 
  mutate(year = str_split(.$type, pattern = "_", simplify = TRUE)[, 3], 
         metrics = str_split(.$type, pattern = "_", simplify = TRUE)[, 2]) %>% 
  select(ingredient, year, metrics, N) %>% 
  mutate(year = as.numeric(year))

## plot
df_shusai_long %>% 
  group_by(year, metrics) %>% 
  summarise(N = mean(N), .groups = "drop") %>% 
  mutate(N = round(N, digits = 2)) %>% 
  ggplot(aes(x = year, y = N, colour = metrics)) +
  geom_point() + 
  geom_line(position = "identity", size = 1) + 
  geom_text(aes(label = N))



## complete mising cells ########
df_shusai$brand[df_shusai$year == 2012] <- ifelse(df_shusai$code_shusai[df_shusai$year == 2012] %in% ls_brnd, 
                                                  "先発品", 
                                                  NA)

df_shusai$brand[df_shusai$year == 2012] <- ifelse(df_shusai$code_shusai[df_shusai$year == 2012] %in% ls_sbrnd, 
                                                  "準先発品", 
                                                  df_shusai$brand[df_shusai$year == 2012])


df_shusai$ag[df_shusai$year == 2012] <- ifelse(df_shusai$code_shusai[df_shusai$year == 2012] %in% ls_ag, 
                                                  "○", NA)


## create lists
### brand firms
ls_brnd_firm <- df_shusai %>% 
  filter(brand == "先発品", !is.na(maker)) %>% 
  group_by(maker, year) %>%
  summarise(count = n(), .groups = "drop") %>% 
  arrange(-count, maker) %>% 
  use_series(maker) %>% 
  unique()

ls_brnd_firm_top <- ls_brnd_firm[1:15]


## aggregation
df_shusai_agg <- df_shusai %>% 
  filter(!is.na(maker)) %>% 
  filter(!is.na(brand) | !is.na(generic) | !is.na(ag)) %>% 
  group_by(year, ingredient) %>% 
  arrange(maker, unit) %>% 
  distinct(maker, .keep_all = TRUE) %>% 
  mutate(n_firm = n()) %>% 
  ungroup() %>% 
  arrange(year, ingredient) %>% 
  mutate(patent = NA) %>% 
  filter(generic != "★" | is.na(generic)) %>% 
  select(ingredient, year, n_firm, patent, maker, brand, generic, ag, name, code_shusai)

for(i in 1:nrow(df_shusai_agg)){
  if(!is.na(df_shusai_agg$brand[i]) && df_shusai_agg$brand[i] == "先発品"){
    df_shusai_agg$patent[i] <- "brand"
  } else if(!is.na(df_shusai_agg$brand[i]) && !is.na(!is.na(df_shusai_agg$ag[i])) && 
            df_shusai_agg$brand[i] == "準先発品" && 
            df_shusai_agg$ag[i] == "○"){
    df_shusai_agg$patent[i] <- "ag"
  } else if(!is.na(df_shusai_agg$generic[i]) && 
            df_shusai_agg$generic[i] == "後発品"){
    df_shusai_agg$patent[i] <- "generic"
  } else{
    df_shusai_agg$patent[i] <- NA
  }
}   


df_shusai_agg <- df_shusai_agg %>% 
  group_by(year, ingredient, patent) %>% 
  mutate(n_patent = n()) %>% 
  ungroup() %>% 
  select(ingredient, year, n_firm, n_patent, patent, maker, brand, generic, ag, name, code_shusai)


df_shusai_agg_MultiBrand <- df_shusai_agg %>% 
  filter(patent == "brand") %>% 
  filter(n_patent > 1) %>% 
  group_by(year, ingredient, patent) %>% 
  arrange(name) %>% 
  distinct(maker, .keep_all = T) %>% 
  ungroup() %>% 
  arrange(ingredient, year)



# output ------------------------------------------------------------------

write_rds(df_shusai_wide, "./output/data/shusai_wide.rds")
write_rds(df_shusai_long, "./output/data/shusai_long.rds")

