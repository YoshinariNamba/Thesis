
# library
library(tidyverse)
library(stargazer)

# source
df_sample <- 
  read_rds("./output/data/sample.rds")


# pastAG as dummy ---------------------------------------------------------

# real revenue
mdl_ols_dum1 <- lm(data = df_sample, 
               formula = N_entry ~ pastAG_dummy + price + 
                 revenue + I(revenue^2) + revenue_hos + I(revenue_hos^2) +
                 N_br_over2:revenue + N_br_over3:revenue + 
                 capsule + tablet + granule)

summary(mdl_ols_dum1)


# log revenue
mdl_ols_dum2 <- lm(data = df_sample, 
               formula = N_entry ~ pastAG_dummy + price + 
                 log(revenue) + log(revenue_hos) +
                 N_br_over2:log(revenue) + N_br_over3:log(revenue) + 
                 capsule + tablet + granule)

summary(mdl_ols_dum2)


# summary #####
stargazer(mdl_ols_dum1, mdl_ols_dum2, type = "text")


# pastAG as count ---------------------------------------------------------

# total count #####
## without brand count
mdl_ols_tcount1 <- lm(data = df_sample, 
               formula = N_entry ~ pastAG + I(pastAG^2) + price + 
                 revenue + I(revenue^2) + revenue_hos + I(revenue_hos^2) +
                 N_br_over2:revenue + N_br_over3:revenue + 
                 capsule + tablet + granule)

## with  brand count
mdl_ols_tcount2 <- lm(data = df_sample, 
                      formula = N_entry ~ pastAG + I(pastAG^2) + price + n_maker + 
                        revenue + I(revenue^2) + revenue_hos + I(revenue_hos^2) + n_maker:revenue +
                        N_br_over2:revenue + N_br_over3:revenue + 
                        capsule + tablet + granule)


# mean count ####
mdl_ols_mcount1 <- lm(data = df_sample, 
               formula = N_entry ~ pastAGmean + I(pastAG^2) + price + 
                 revenue + I(revenue^2) + revenue_hos + I(revenue_hos^2) +
                 N_br_over2:revenue + N_br_over3:revenue + 
                 capsule + tablet + granule)



# summary #####
stargazer(mdl_ols_tcount1, mdl_ols_tcount2, mdl_ols_mcount1, type = "text")
