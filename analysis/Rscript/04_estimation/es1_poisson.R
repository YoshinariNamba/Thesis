
# library
library(tidyverse)
library(stargazer)
library(margins)

# data
df_sample <- 
  read_rds("./output/data/sample.rds")


# regression --------------------------------------------------------------

## dummy
mdl_poisson_dummy1 <- glm(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue,  
                           data = df_sample, 
                           family = "poisson")
mdl_poisson_dummy2 <- glm(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             capsule + tablet + granule + siroop + liquid, 
                           #inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")
mdl_poisson_dummy3 <- glm(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             #capsule + tablet + granule + siroop + liquid + 
                             inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")
mdl_poisson_dummy4 <- glm(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             N_incumbent:ln_revenue_hos + ln_revenue_hos + 
                             capsule + tablet + granule + siroop + liquid + 
                             inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")


## total count
mdl_poisson_tcount1 <- glm(formula = N_entry ~ pastAG + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue,  
                             data = df_sample, 
                           family = "poisson")
mdl_poisson_tcount2 <- glm(formula = N_entry ~ pastAG + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             capsule + tablet + granule + siroop + liquid, 
                             #inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")
mdl_poisson_tcount3 <- glm(formula = N_entry ~ pastAG + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             #capsule + tablet + granule + siroop + liquid + 
                             inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")
mdl_poisson_tcount4 <- glm(formula = N_entry ~ pastAG + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             N_incumbent:ln_revenue_hos + ln_revenue_hos + 
                             capsule + tablet + granule + siroop + liquid + 
                             inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")



## mean count
mdl_poisson_mcount1 <- glm(formula = N_entry ~ pastAG_mean + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue,  
                           data = df_sample, 
                           family = "poisson")
mdl_poisson_mcount2 <- glm(formula = N_entry ~ pastAG_mean + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             capsule + tablet + granule + siroop + liquid, 
                           #inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")
mdl_poisson_mcount3 <- glm(formula = N_entry ~ pastAG_mean + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             #capsule + tablet + granule + siroop + liquid + 
                             inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")
mdl_poisson_mcount4 <- glm(formula = N_entry ~ pastAG_mean + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             N_incumbent:ln_revenue_hos + ln_revenue_hos + 
                             capsule + tablet + granule + siroop + liquid + 
                             inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")

## belief
mdl_poisson_belief1 <- glm(formula = N_entry ~ belief + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue,  
                           data = df_sample, 
                           family = "poisson")
mdl_poisson_belief2 <- glm(formula = N_entry ~ belief + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             capsule + tablet + granule + siroop + liquid, 
                           #inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")
mdl_poisson_belief3 <- glm(formula = N_entry ~ belief + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             #capsule + tablet + granule + siroop + liquid + 
                             inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")
mdl_poisson_belief4 <- glm(formula = N_entry ~ belief + ln_revenue + N_incumbent +
                             N_incumbent:ln_revenue + 
                             N_incumbent:ln_revenue_hos + ln_revenue_hos + 
                             capsule + tablet + granule + siroop + liquid + 
                             inactive_variety + substitute, 
                           data = df_sample, 
                           family = "poisson")


# negative binomial -------------------------------------------------------

## dummy
mdl_nb_dummy1 <- MASS::glm.nb(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                            N_incumbent:ln_revenue,  
                          data = df_sample)
mdl_nb_dummy2 <- MASS::glm.nb(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                            N_incumbent:ln_revenue + 
                            capsule + tablet + granule + siroop + liquid, 
                          #inactive_variety + substitute, 
                          data = df_sample)
mdl_nb_dummy3 <- MASS::glm.nb(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                            N_incumbent:ln_revenue + 
                            #capsule + tablet + granule + siroop + liquid + 
                            inactive_variety + substitute, 
                          data = df_sample)
mdl_nb_dummy4 <- MASS::glm.nb(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                            N_incumbent:ln_revenue + 
                            N_incumbent:ln_revenue_hos + ln_revenue_hos + 
                            capsule + tablet + granule + siroop + liquid + 
                            inactive_variety + substitute, 
                          data = df_sample)

## total count
mdl_nb_tcount1 <- MASS::glm.nb(formula = N_entry ~ pastAG + ln_revenue + N_incumbent +
                                N_incumbent:ln_revenue,  
                              data = df_sample)
mdl_nb_tcount2 <- MASS::glm.nb(formula = N_entry ~ pastAG + ln_revenue + N_incumbent +
                                N_incumbent:ln_revenue + 
                                capsule + tablet + granule + siroop + liquid, 
                              #inactive_variety + substitute, 
                              data = df_sample)
mdl_nb_tcount3 <- MASS::glm.nb(formula = N_entry ~ pastAG + ln_revenue + N_incumbent +
                                N_incumbent:ln_revenue + 
                                #capsule + tablet + granule + siroop + liquid + 
                                inactive_variety + substitute, 
                              data = df_sample)
mdl_nb_tcount4 <- MASS::glm.nb(formula = N_entry ~ pastAG + ln_revenue + N_incumbent +
                                N_incumbent:ln_revenue + 
                                N_incumbent:ln_revenue_hos + ln_revenue_hos + 
                                capsule + tablet + granule + siroop + liquid + 
                                inactive_variety + substitute, 
                              data = df_sample)

## mean count
mdl_nb_mcount1 <- MASS::glm.nb(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                                N_incumbent:ln_revenue,  
                              data = df_sample)
mdl_nb_mcount2 <- MASS::glm.nb(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                                N_incumbent:ln_revenue + 
                                capsule + tablet + granule + siroop + liquid, 
                              #inactive_variety + substitute, 
                              data = df_sample)
mdl_nb_mcount3 <- MASS::glm.nb(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                                N_incumbent:ln_revenue + 
                                #capsule + tablet + granule + siroop + liquid + 
                                inactive_variety + substitute, 
                              data = df_sample)
mdl_nb_mcount4 <- MASS::glm.nb(formula = N_entry ~ pastAG_dummy + ln_revenue + N_incumbent +
                                N_incumbent:ln_revenue + 
                                N_incumbent:ln_revenue_hos + ln_revenue_hos + 
                                capsule + tablet + granule + siroop + liquid + 
                                inactive_variety + substitute, 
                              data = df_sample)

## belief
mdl_nb_belief1 <- MASS::glm.nb(formula = N_entry ~ belief + ln_revenue + N_incumbent +
                                 N_incumbent:ln_revenue,  
                               data = df_sample)
mdl_nb_belief2 <- MASS::glm.nb(formula = N_entry ~ belief + ln_revenue + N_incumbent +
                                 N_incumbent:ln_revenue + 
                                 capsule + tablet + granule + siroop + liquid, 
                               #inactive_variety + substitute, 
                               data = df_sample)
mdl_nb_belief3 <- MASS::glm.nb(formula = N_entry ~ belief + ln_revenue + N_incumbent +
                                 N_incumbent:ln_revenue + 
                                 #capsule + tablet + granule + siroop + liquid + 
                                 inactive_variety + substitute, 
                               data = df_sample)
mdl_nb_belief4 <- MASS::glm.nb(formula = N_entry ~ belief + ln_revenue + N_incumbent +
                                 N_incumbent:ln_revenue + 
                                 N_incumbent:ln_revenue_hos + ln_revenue_hos + 
                                 capsule + tablet + granule + siroop + liquid + 
                                 inactive_variety + substitute, 
                               data = df_sample)




# summary -----------------------------------------------------------------

# intra-method
## poisson
stargazer(mdl_poisson_dummy1, mdl_poisson_dummy2, mdl_poisson_dummy3, mdl_poisson_dummy4, 
          type = "text")
stargazer(mdl_poisson_mcount1, mdl_poisson_mcount2, mdl_poisson_mcount3, mdl_poisson_mcount4, 
          type = "text")
stargazer(mdl_poisson_tcount1, mdl_poisson_tcount2, mdl_poisson_tcount3, mdl_poisson_tcount4, 
          type = "text")
stargazer(mdl_poisson_belief1, mdl_poisson_belief2, mdl_poisson_belief3, mdl_poisson_belief4, 
          type = "text")

## negative binomial
#stargazer(mdl_nb_dummy1, mdl_nb_dummy2, mdl_nb_dummy3, mdl_nb_dummy4, 
#          type = "text")
#stargazer(mdl_nb_mcount1, mdl_nb_mcount2, mdl_nb_mcount3, mdl_nb_mcount4, 
#          type = "text")
#stargazer(mdl_nb_tcount1, mdl_nb_tcount2, mdl_nb_tcount3, mdl_nb_tcount4, 
#          type = "text")
#stargazer(mdl_nb_belief1, mdl_nb_belief2, mdl_nb_belief3, mdl_nb_belief4, 
#          type = "text")

## summary
### dummy 
summary(mdl_nb_dummy1)
summary(mdl_nb_dummy2)
summary(mdl_nb_dummy3)
summary(mdl_nb_dummy4)

### total count
summary(mdl_nb_tcount1)
summary(mdl_nb_tcount2)
summary(mdl_nb_tcount3)
summary(mdl_nb_tcount4)

### mean count
summary(mdl_nb_mcount1)
summary(mdl_nb_mcount2)
summary(mdl_nb_mcount3)
summary(mdl_nb_mcount4)

### belief
summary(mdl_nb_belief1)
summary(mdl_nb_belief2)
summary(mdl_nb_belief3)
summary(mdl_nb_belief4)



# marginal effects --------------------------------------------------------


for(i in c("poisson", "nb")){
  for(j in c("dummy", "tcount", "mcount", "belief")){
    for(k in 1:4){
      assign(
        paste0("margin_", i, "_", j, k), 
        margins(eval(parse(text = paste0("mdl_", i, "_", j, k))))
      )
      print(eval(parse(text = paste0("margin_", i, "_", j, k))))
    }
  }
}


