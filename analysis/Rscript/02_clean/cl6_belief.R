
# library
library(tidyverse)


# construction ------------------------------------------------------------

df_exante <- 
  df_jpc_cl1 %>% 
  filter(year == 2015) %>% 
  filter(note1 == "薬価収載", 
         year_listed <= FnlYear_exante) %>% 
  group_by(name_g) %>% 
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
  # label generic name based on df_jpc
  inner_join(df_code_active, by = c("code_shusai" = "code_yj")) %>% 
  # extract brand drug that is included in the sample
  filter(code_shusai %in% ls_code_br_exante) %>% 
  arrange(name_g, -total) %>% 
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
  mutate(
    across(
      .cols = contains("total"), 
      .fns = ~ (.x * price1), 
      .names = "revenue_{.col}"
    )
  ) %>% 
  group_by(eff) %>% 
  mutate(
    subst_act = length(unique(name_g)) - 1, 
    subst_inc = length(unique(maker)), 
    subst_rev = sum(revenue_total) - revenue_total, 
    subst_rev_in = sum(revenue_total_in) - revenue_total_in, 
    subst_rev_out = sum(revenue_total_out) - revenue_total_out, 
    subst_rev_hos = sum(revenue_total_hos) - revenue_total_hos
  ) %>% 
  ungroup() %>% 
  group_by(name_g) %>% 
  summarise(
    # entrant and incumbent
    AG_entry = ifelse(sum(ag_dummy) > 0, 1, 0), 
    N_incumbent = length(unique(maker)), 
    incumbent = str_flatten(sort(unique(maker)), collapse = "-"), 
    #ag_record = sum(aggregate(x = ag_record, by = list(maker), FUN = mean)$x), 
    # across(.cols = c(price1, contains("total")), 
    #        .fns = ~weighted.mean(.x, w = num_in_unit1/num_in_unit2), 
    #        .names = "{.col}"), 
    
    # price
    price = weighted.mean(price1, w = num_in_unit1/num_in_unit2), 
    
    # revenue
    across(
      .cols = contains("revenue"), 
      .fns = ~sum(.x), 
      .names = "{.col}"
    ), 
    
    # related to fixed cost
    capsule = ifelse(sum(str_detect(form1, "カプセル")) > 0, 1, 0), 
    tablet = ifelse(sum(str_detect(form1, "錠")) > 0, 1, 0), 
    granule = ifelse(sum(str_detect(form1, "粒")) > 0, 1, 0), 
    siroop = ifelse(sum(str_detect(form1, "シロップ")) > 0, 1, 0), 
    liquid = ifelse(sum(str_detect(form1, "液")) > 0, 1, 0), 
    #n_maker = length(unique(maker[form2 == "内服"])), 
    #maker = str_flatten(sort(unique(maker[form2 == "内服"])), collapse = "-"), 
    form_variety = length(unique(form1)), 
    inactive_variety = length(unique(as.vector(str_split(inactive[!is.na(inactive)], "，", simplify = T)))), 
    
    # related to substituttion
    # substitute
    subst_act = unique(subst_act), 
    subst_inc = unique(subst_inc) - N_incumbent, 
    across(
      .cols = starts_with("subst_rev"), 
      .fns = ~sum(.x), 
      .names = "{.col}"
    ), 
    .groups = "drop"
  ) %>% 
  na.omit()




# preprocessing -----------------------------------------------------------

df_sample_exante <- 
  df_exante_brand_merged %>% 
  rename(active = name_g) %>% 
  mutate(
    N_inc_over2 = ifelse(N_incumbent > 2, 1, 0), 
    N_inc_over3 = ifelse(N_incumbent > 3, 1, 0), 
    across(
      .cols = c(starts_with("revenue"), starts_with("subst_rev")), 
      .fns = ~log(.x + 1), 
      .names = "ln_{.col}"
    )
  ) %>% 
  select(active, incumbent, AG_entry, 
         c(N_incumbent, N_inc_over2, N_inc_over3), 
         c(price, starts_with("ln_revenue")), 
         c(subst_act, subst_inc, starts_with("ln_subst_rev")), 
         c(capsule, tablet, granule, siroop, liquid), 
         c(form_variety, inactive_variety))



# estimation --------------------------------------------------------------

# estimation
#mdl_belief <- 
#  glm(formula = AG_entry ~ ln_revenue + ln_revenue:N_incumbent + 
#        ln_revenue_hos + ln_revenue_hos:N_incumbent + price + 
#        capsule + tablet + granule + siroop + liquid + 
#        form_variety + inactive_variety + substitute, 
#      data = df_sample_exante, 
#      family = binomial(link = "probit"))


# predicted belief
#vec_belief <- predict(mdl_belief, df_sample_pre, type = "response") 


### incumbents vector ####
vec_inc_pre <- 
  df_sample_pre %>% 
  pull(incumbent) %>% 
  str_split("-", simplify = T) %>% 
  as.vector() %>% 
  unique() %>% 
  sort %>% 
  .[. != ""]

vec_inc_exante <- 
  df_sample_exante %>% 
  pull(incumbent) %>% 
  str_split("-", simplify = T) %>% 
  as.vector() %>% 
  unique() %>% 
  sort() %>% 
  .[. != ""]

## difference is NULL
setdiff(vec_inc_pre, vec_inc_exante)


## create dummy ####
### ex ante dataset
df_sample_exante_dum <- 
  df_sample_exante

for(i in 1:length(vec_inc_pre)){
  df_sample_exante_dum <- 
    df_sample_exante_dum %>% 
    mutate(
      "incumbent_dum{i}" := 
        case_when(
          str_detect(incumbent, pattern = vec_inc_pre[i]) ~ 1,
          !str_detect(incumbent, pattern = vec_inc_pre[i]) ~ 0
        )
    )
}

### ex post dataset
df_sample_dum <- 
  df_sample_pre

for(i in 1:length(vec_inc_pre)){
  df_sample_dum <- 
    df_sample_dum %>% 
    mutate(
      "incumbent_dum{i}" := 
        case_when(
          str_detect(incumbent, pattern = vec_inc_pre[i]) ~ 1,
          !str_detect(incumbent, pattern = vec_inc_pre[i]) ~ 0
        )
    )
}

## define formula
fml <- as.formula(paste("AG_entry ~ ", 
                        paste(c("ln_revenue_total", "ln_revenue_total:N_incumbent", 
                                "price", "capsule", "tablet", "granule", "siroop", "liquid", 
                                "form_variety", 
                                "inactive_variety", "subst_act", "subst_inc", 
                                "ln_subst_rev", "subst_inc:ln_subst_rev", 
                                paste0("incumbent_dum", 1:length(vec_inc_pre))), 
                              collapse = "+")))

## implement probit estimation
mdl_belief <- 
  glm(formula = fml, 
      data = df_sample_exante_dum, 
      family = binomial(link = "probit"))

mdl_belief %>% summary()

## predict
vec_belief <- predict(mdl_belief, df_sample_dum, type = "response") 


# complete data -----------------------------------------------------------

# merge
df_sample <-
  df_sample_pre %>% 
  mutate(belief = vec_belief) %>% 
  relocate(belief, .before = pastAG)




