
# library
library(tidyverse)


# construction ------------------------------------------------------------

df_exante <- 
  df_jpc_cl1 %>% 
  filter(year == 2015) %>% 
  filter(note1 == "薬価収載", 
         year_listed <= FnlYear_exante) %>% 
  group_by(eff) %>% 
  mutate(
    ag_dummy = ifelse(sum(ag) > 0, 1, 0)
  ) %>% 
  ungroup()

df_exante_brand <-
  df_exante %>% 
  filter(brand)


## brand drugs' list to be analyzed
ls_code_br_exante <- 
  df_exante_brand %>% 
  # extract brand
  filter(brand) %>% 
  use_series(code_yj) %>% 
  unique() %>% 
  sort()

## combination of code and quantity as of 2014
df_recept_exante <- 
  df_ndb_cl %>% 
  # fix year for controlling market sizes  
  filter(year == FnlYear_exante) %>% 
  # label eff name based on df_jpc
  inner_join(df_code_eff, by = c("code_shusai" = "code_yj")) %>% 
  # extract brand drug that is included in the sample
  filter(code_shusai %in% ls_code_br_exante) %>% 
  arrange(eff, -total) %>% 
  select(code_shusai, contains("total"))


## brand-specific dataset
df_exante_brand_merged <- 
  df_exante_brand %>% 
  #filter(brand) %>% 
  # merge recept data
  left_join(df_recept_exante, by = c("code_yj" = "code_shusai")) %>% 
  #filter(year == 2015) %>% 
  # merge firm-specific AG count data
  #left_join(df_AGrecord, by = c("maker" = "maker")) %>% 
  # substitute half of minimum value for missing value 
  replace_na(
    replace = list(
      total_in = min(.$total_in, na.rm = T) / 2, 
      total_out = min(.$total_out, na.rm = T) / 2, 
      total_hos = min(.$total_hos, na.rm = T) / 2
    )
  ) %>% 
  mutate(total = total_in + total_out + total_hos) %>% 
  group_by(eff) %>% 
  summarise(
    AG_entry = ifelse(sum(ag_dummy) > 0, 1, 0), 
    N_incumbent = length(unique(maker)), 
    incumbent = str_flatten(sort(unique(maker)), collapse = "-"), 
    #ag_record = sum(aggregate(x = ag_record, by = list(maker), FUN = mean)$x), 
    across(.cols = c(price1, contains("total")), 
           .fns = ~weighted.mean(.x, w = num_in_unit1/num_in_unit2), 
           .names = "{.col}"), 
    capsule = ifelse(sum(str_detect(form1, "カプセル")) > 0, 1, 0), 
    tablet = ifelse(sum(str_detect(form1, "錠")) > 0, 1, 0), 
    granule = ifelse(sum(str_detect(form1, "粒")) > 0, 1, 0), 
    siroop = ifelse(sum(str_detect(form1, "シロップ")) > 0, 1, 0), 
    liquid = ifelse(sum(str_detect(form1, "液")) > 0, 1, 0), 
    #n_maker = length(unique(maker[form2 == "内服"])), 
    #maker = str_flatten(sort(unique(maker[form2 == "内服"])), collapse = "-"), 
    form_variety = length(unique(form1)), 
    active_variety = length(unique(name_g)), 
    inactive_variety = length(unique(as.vector(str_split(inactive[!is.na(inactive)], "，", simplify = T)))), 
    subst = length(unique(code_eff)) - 1, 
    .groups = "drop"
  ) 




# preprocessing -----------------------------------------------------------

df_sample_exante <- 
  df_exante_brand_merged %>% 
  rename(effectiveness = eff, 
         price = price1, 
         substitute = subst) %>% 
  mutate(
    N_inc_over2 = ifelse(N_incumbent > 2, 1, 0), 
    N_inc_over3 = ifelse(N_incumbent > 3, 1, 0), 
    ln_revenue = log(price*total), 
    ln_revenue_hos = log(price*total_hos)
  ) %>% 
  select(effectiveness, incumbent, AG_entry, c(N_incumbent, N_inc_over2, N_inc_over3), 
         c(price, ln_revenue, ln_revenue_hos), c(capsule, tablet, granule, siroop, liquid), 
         c(form_variety, active_variety, inactive_variety, substitute))



# estimation --------------------------------------------------------------

# estimation
mdl_belief <- 
  glm(formula = AG_entry ~ ln_revenue + ln_revenue:N_incumbent + 
        ln_revenue_hos + ln_revenue_hos:N_incumbent + price + 
        capsule + tablet + granule + siroop + liquid + 
        form_variety + active_variety + inactive_variety + substitute, 
      data = df_sample_exante, 
      family = binomial(link = "probit"))

mdl_belief2 <- 
  glm(formula = AG_entry ~ ln_revenue + ln_revenue:N_incumbent + price + 
        incumbent + incumbent:ln_revenue + incumbent:price, 
      data = df_sample_exante, 
      family = binomial(link = "probit"))


# predicted belief
vec_belief <- predict(mdl_belief, df_sample_pre, type = "response") 

# merge
df_sample <-
  df_sample_pre %>% 
  mutate(belief = vec_belief) %>% 
  relocate(belief, .before = pastAG)




