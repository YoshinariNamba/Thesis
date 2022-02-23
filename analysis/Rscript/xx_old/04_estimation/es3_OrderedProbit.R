
# library
library(tidyverse)
library(stargazer)
library(oglmx)

# source
df_sample <- 
  read_rds("./output/data/sample.rds")


# pastAG as dummy ---------------------------------------------------------

mdl_probit_dum1 <- oglmx(N_entry_f ~ pastAG_dummy + price + revenue +
                       N_br_over2:revenue + N_br_over3:revenue + 
                       capsule + tablet + granule, 
                     data = df_sample, 
                     link = "probit", 
                     constantMEAN = FALSE,
                     constantSD = FALSE, 
                     delta = 0, threshparam = NULL, 
                     savemodelframe=TRUE)


##  + n_maker:revenue
mdl_probit_dum2 <- oglmx(N_entry_f ~ pastAG_dummy + price + revenue + n_maker +
                           N_br_over2:revenue + N_br_over3:revenue + n_maker:revenue +
                           capsule + tablet + granule, 
                         data = df_sample, 
                         link = "probit", 
                         constantMEAN = FALSE,
                         constantSD = FALSE, 
                         delta = 0, threshparam = NULL, 
                         savemodelframe=TRUE)

## summary ####
summary(mdl_probit_dum1)
summary(mdl_probit_dum2)

# pastAG as count ---------------------------------------------------------

## total count
###
mdl_probit_tcount1 <- oglmx(N_entry_f ~ pastAG +price + revenue +
                       N_br_over2:revenue + N_br_over3:revenue + 
                       capsule + tablet + granule,
                     data = df_sample, 
                     link = "probit", 
                     constantMEAN = FALSE,
                     constantSD = FALSE, 
                     delta = 0, threshparam = NULL, 
                     savemodelframe=TRUE)


###
mdl_probit_tcount2 <- oglmx(N_entry_f ~ pastAG +price + revenue + n_maker +
                              N_br_over2:revenue + N_br_over3:revenue + n_maker:revenue +
                              capsule + tablet + granule,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)


## mean count
mdl_probit_mcount <- oglmx(N_entry_f ~ pastAGmean +price + revenue + 
                             N_br_over2:revenue + N_br_over3:revenue + 
                             capsule + tablet + granule,
                           data = df_sample, 
                           link = "probit", 
                           constantMEAN = FALSE,
                           constantSD = FALSE, 
                           delta = 0, threshparam = NULL, 
                           savemodelframe=TRUE)

mdl_probit_mcount2 <- oglmx(N_entry_f ~ pastAGmean +price + revenue + n_maker +
                             N_br_over2:revenue + N_br_over3:revenue + n_maker:revenue +
                             capsule + tablet + granule,
                           data = df_sample, 
                           link = "probit", 
                           constantMEAN = FALSE,
                           constantSD = FALSE, 
                           delta = 0, threshparam = NULL, 
                           savemodelframe=TRUE)


## summary ####
summary(mdl_probit_tcount)
summary(mdl_probit_tcount2)
summary(mdl_probit_mcount)
summary(mdl_probit_mcount2)

