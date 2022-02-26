
# library
library(tidyverse)
library(oglmx)

# data
df_sample <- 
  read_rds("./output/data/sample.rds")


# estimation
mdl_probit_belief1 <- oglmx(N_entry_f ~ belief + ln_revenue + N_incumbent +
                              N_incumbent:ln_revenue +
                              capsule + tablet + granule + siroop + liquid + 
                              active_variety + inactive_variety + substitute,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_belief2 <- oglmx(N_entry_f ~ belief + ln_revenue + N_incumbent + price +
                              N_incumbent:ln_revenue +
                              capsule + tablet + granule + siroop + liquid + 
                              active_variety + inactive_variety + substitute,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)


## summary
summary(mdl_probit_belief1)
summary(mdl_probit_belief2)
