
# library
library(tidyverse)
library(oglmx)

# data
df_sample <- 
  read_rds("./output/data/sample.rds")


# pastAG as dummy ---------------------------------------------------------

mdl_probit_dum1 <- oglmx(N_entry_f ~ pastAG_dummy + ln_revenue +
                              N_incumbent:ln_revenue, 
                              #capsule + tablet + granule + siroop + liquid + 
                              #inactive_variety + substitute
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_dum2 <- oglmx(N_entry_f ~ pastAG_dummy + ln_revenue + N_incumbent +
                           N_incumbent:ln_revenue + 
                           capsule + tablet + granule + siroop + liquid,  
                           #inactive_variety + substitute,
                         data = df_sample, 
                         link = "probit", 
                         constantMEAN = FALSE,
                         constantSD = FALSE, 
                         delta = 0, threshparam = NULL, 
                         savemodelframe=TRUE)

mdl_probit_dum3 <- oglmx(N_entry_f ~ pastAG_dummy + ln_revenue + N_incumbent +
                           N_incumbent:ln_revenue + 
                           capsule + tablet + granule + siroop + liquid + 
                           inactive_variety + substitute,
                         data = df_sample, 
                         link = "probit", 
                         constantMEAN = FALSE,
                         constantSD = FALSE, 
                         delta = 0, threshparam = NULL, 
                         savemodelframe=TRUE)

mdl_probit_dum4 <- oglmx(N_entry_f ~ pastAG_dummy + ln_revenue + N_incumbent +
                           N_incumbent:ln_revenue + 
                           N_incumbent:ln_revenue_hos + ln_revenue_hos + 
                           capsule + tablet + granule + siroop + liquid + 
                           inactive_variety + substitute,
                         data = df_sample, 
                         link = "probit", 
                         constantMEAN = FALSE,
                         constantSD = FALSE, 
                         delta = 0, threshparam = NULL, 
                         savemodelframe=TRUE)



# pastAG as count ---------------------------------------------------------

## total count
###
mdl_probit_tcount1 <- oglmx(N_entry_f ~ pastAG + ln_revenue +
                           N_incumbent:ln_revenue, 
                         #capsule + tablet + granule + siroop + liquid + 
                         #inactive_variety + substitute
                         data = df_sample, 
                         link = "probit", 
                         constantMEAN = FALSE,
                         constantSD = FALSE, 
                         delta = 0, threshparam = NULL, 
                         savemodelframe=TRUE)

mdl_probit_tcount2 <- oglmx(N_entry_f ~ pastAG + ln_revenue + N_incumbent +
                           N_incumbent:ln_revenue + 
                           capsule + tablet + granule + siroop + liquid,  
                         #inactive_variety + substitute,
                         data = df_sample, 
                         link = "probit", 
                         constantMEAN = FALSE,
                         constantSD = FALSE, 
                         delta = 0, threshparam = NULL, 
                         savemodelframe=TRUE)

mdl_probit_tcount3 <- oglmx(N_entry_f ~ pastAG + ln_revenue + N_incumbent +
                           N_incumbent:ln_revenue + 
                           capsule + tablet + granule + siroop + liquid + 
                           inactive_variety + substitute,
                         data = df_sample, 
                         link = "probit", 
                         constantMEAN = FALSE,
                         constantSD = FALSE, 
                         delta = 0, threshparam = NULL, 
                         savemodelframe=TRUE)

mdl_probit_tcount4 <- oglmx(N_entry_f ~ pastAG + ln_revenue + N_incumbent +
                           N_incumbent:ln_revenue + 
                           N_incumbent:ln_revenue_hos + ln_revenue_hos + 
                           capsule + tablet + granule + siroop + liquid + 
                           inactive_variety + substitute,
                         data = df_sample, 
                         link = "probit", 
                         constantMEAN = FALSE,
                         constantSD = FALSE, 
                         delta = 0, threshparam = NULL, 
                         savemodelframe=TRUE)


## mean count
mdl_probit_mcount1 <- oglmx(N_entry_f ~ pastAG_mean + ln_revenue +
                              N_incumbent:ln_revenue, 
                            #capsule + tablet + granule + siroop + liquid + 
                            #inactive_variety + substitute
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_mcount2 <- oglmx(N_entry_f ~ pastAG_mean + ln_revenue + N_incumbent +
                              N_incumbent:ln_revenue + 
                              capsule + tablet + granule + siroop + liquid,  
                            #inactive_variety + substitute,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_mcount3 <- oglmx(N_entry_f ~ pastAG_mean + ln_revenue + N_incumbent +
                              N_incumbent:ln_revenue + 
                              capsule + tablet + granule + siroop + liquid + 
                              inactive_variety + substitute,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_mcount4 <- oglmx(N_entry_f ~ pastAG_mean + ln_revenue + N_incumbent +
                              N_incumbent:ln_revenue + 
                              N_incumbent:ln_revenue_hos + ln_revenue_hos + 
                              capsule + tablet + granule + siroop + liquid + 
                              inactive_variety + substitute,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

# summary -----------------------------------------------------------------

## past AG as dummy ####
summary(mdl_probit_dum1)
summary(mdl_probit_dum2)
summary(mdl_probit_dum3)

## past AG as total count ####
summary(mdl_probit_tcount1)
summary(mdl_probit_tcount2)
summary(mdl_probit_tcount3)
summary(mdl_probit_tcount4)
#summary(mdl_probit_tcount5)

## past AG as mean value ####
summary(mdl_probit_mcount1)
summary(mdl_probit_mcount2)
summary(mdl_probit_mcount3)
summary(mdl_probit_mcount4)
#summary(mdl_probit_mcount5)


# marginal effects
mdl_probit_mcount4 %>% margins.oglmx(Vars = "pastAG_mean")





