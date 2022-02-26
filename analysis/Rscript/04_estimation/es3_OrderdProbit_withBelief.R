
# library
library(tidyverse)
library(oglmx)

# data
df_sample <- 
  read_rds("./output/data/sample.rds")


# estimation
mdl_probit_belief1 <- oglmx(N_entry_f ~ belief + ln_revenue +
                              N_incumbent:ln_revenue, 
                            #capsule + tablet + granule + siroop + liquid + 
                            #inactive_variety + substitute
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_belief2 <- oglmx(N_entry_f ~ belief + ln_revenue + N_incumbent +
                              N_incumbent:ln_revenue + 
                              capsule + tablet + granule + siroop + liquid,  
                            #inactive_variety + substitute,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_belief3 <- oglmx(N_entry_f ~ belief + ln_revenue + N_incumbent +
                              N_incumbent:ln_revenue + 
                              capsule + tablet + granule + siroop + liquid + 
                              inactive_variety + substitute,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_belief4 <- oglmx(N_entry_f ~ belief + ln_revenue + N_incumbent +
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

mdl_probit_belief5 <- oglmx(N_entry_f ~ belief + ln_revenue + N_incumbent +
                              I(ln_revenue/N_incumbent) + 
                              I(ln_revenue_hos/N_incumbent) + ln_revenue_hos + 
                              capsule + tablet + granule + siroop + liquid + 
                              inactive_variety + substitute,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)


## summary
summary(mdl_probit_belief1)
summary(mdl_probit_belief2)
summary(mdl_probit_belief3)
summary(mdl_probit_belief4)
summary(mdl_probit_belief5)


## marginal effect
margins.oglmx(mdl_probit_belief1, Vars = "belief")
margins.oglmx(mdl_probit_belief2, Vars = "belief")
margins.oglmx(mdl_probit_belief3, Vars = "belief")
margins.oglmx(mdl_probit_belief4, Vars = "belief")



