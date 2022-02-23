
#library
library(tidyverse)
library(MASS)
library(oglmx)
library(stargazer)

# confliction
library(conflicted)
conflict_prefer_all(winner = "dplyr")
conflict_prefer("select", "dplyr")

# data --------------------------------------------------------------------

df_sample <- 
  df_jpc_smpl_agg_merged %>% 
  mutate(
    AG_dummy = case_when(
      N_ag_2015 > 0 ~ 1, 
      N_ag_2015 == 0 ~ 0
    ), 
    N_entry = N_generic_2020 - N_ag_2020, 
    entry_dummy = ifelse(N_entry > 0, 1, 0), 
    N_entry_f = factor(N_entry), 
    pastAG_dummy = ifelse(N_ag_firmspec > 0, 1, 0), 
    N_br_over2 = ifelse(N_brand_2015 > 2, 1, 0), 
    N_br_over3 = ifelse(N_brand_2015 > 3, 1, 0), 
    revenue = price*total, 
    revenue_hos = price*total_hos
  ) %>% 
  rename(pastAG = N_ag_firmspec, 
         pastAGmean = mean_ag_firmspec)


df_sample %>% 
  select(N_entry, pastAG, pastAG_dummy, price, starts_with("total")) %>% 
  cor()



# ols ---------------------------------------------------------------------

mdl_ols1 <- lm(data = df_sample, 
              formula = N_entry ~ pastAG_dummy + price + 
                revenue + I(revenue^2) + revenue_hos + I(revenue_hos^2) +
                N_br_over2:revenue + N_br_over3:revenue + 
                capsule + tablet + granule)

summary(mdl_ols1)


mdl_ols2 <- lm(data = df_sample, 
              formula = N_entry ~ pastAG + I(pastAG^2) + price + 
                revenue + I(revenue^2) + revenue_hos + I(revenue_hos^2) +
                N_br_over2:revenue + N_br_over3:revenue + 
                capsule + tablet + granule)

summary(mdl_ols2)




# binary probit -----------------------------------------------------------

## binary
mdl_probit_binary1 <- 
  glm(formula = entry_dummy ~ pastAG_dummy + price + 
        revenue + I(revenue^2) + revenue_hos + I(revenue_hos^2) +
        N_br_over2:revenue + N_br_over3:revenue + 
        capsule + tablet + granule, 
      data = df_sample, 
      family = binomial(link = "probit"))
summary(mdl_probit_binary1)


mdl_probit_binary2 <- 
  glm(formula = entry_dummy ~ pastAG + I(pastAG^2) + price + 
        revenue + I(revenue^2) + revenue_hos + I(revenue_hos^2) +
        N_br_over2:revenue + N_br_over3:revenue + 
        capsule + tablet + granule, 
      data = df_sample, 
      family = binomial(link = "probit"))
summary(mdl_probit_binary2)




# ordered probit ----------------------------------------------------------

## ordered
mdl_probit1 <- oglmx(N_entry_f ~ pastAG_dummy + price + revenue +
                       N_br_over2:revenue + N_br_over3:revenue + 
                       capsule + tablet + granule, 
                   data = df_sample, 
                   link = "probit", 
                   constantMEAN = FALSE,
                   constantSD = FALSE, 
                   delta = 0, threshparam = NULL, 
                   savemodelframe=TRUE)
summary(mdl_probit1)

mdl_probit2 <- oglmx(N_entry_f ~ pastAG +price + revenue +
                       N_br_over2:revenue + N_br_over3:revenue + 
                       capsule + tablet + granule,
                     data = df_sample, 
                     link = "probit", 
                     constantMEAN = FALSE,
                     constantSD = FALSE, 
                     delta = 0, threshparam = NULL, 
                     savemodelframe=TRUE)
summary(mdl_probit2)


# reporting ---------------------------------------------------------------

## table
stargazer(mdl_ols2, mdl_ols1, mdl_probit_binary2, mdl_probit_binary1, 
          omit = "revenue", omit.labels = "revenue")


## marginal effect
margin1 <- margins.oglmx(mdl_probit1)
vec_margin1 <- as.numeric(NULL)
vec_outcome <- c(0, 1:11, 16, 18, 19, 22, 23)

for(i in 1:length(margin1)){
  vec_tmp <- margin1[[i]][1, 1]
  vec_margin1 <- c(vec_margin1, vec_tmp)
}

margin2 <- margins.oglmx(mdl_probit2)
vec_margin2 <- as.numeric(NULL)
vec_outcome <- c(0, 1:11, 16, 18, 19, 22, 23)

for(i in 1:length(margin2)){
  vec_tmp <- margin1[[i]][1, 1]
  vec_margin2 <- c(vec_margin2, vec_tmp)
}

## summary
df_margin <- data.frame(outcome = vec_outcome, 
                        margin1 = vec_margin1, 
                        margin2 = vec_margin2)

df_margin %>% 
  ggplot(aes(x = outcome, y = margin1)) + 
  geom_line(color = "blue") + 
  theme_minimal() + 
  labs(x = "N_entry", y = "Marginal Effect on Pr(N = n)") + 
  geom_text(aes(label = round(margin1, digits = 2)))
