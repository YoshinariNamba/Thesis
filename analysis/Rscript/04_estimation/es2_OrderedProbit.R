
# library
library(tidyverse)
library(oglmx)

# data
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
mdl_probit_dum2 <- oglmx(N_entry_f ~ pastAG_dummy +price + revenue + N_incumbent +
                              N_incumbent:revenue +
                              capsule + tablet + granule + siroop + liquid + 
                              active_variety + inactive_variety + subst,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_dum3 <- oglmx(N_entry_f ~ pastAG_dummy + N_incumbent + price + total + 
                              log(revenue) + log(revenue):N_incumbent + 
                              capsule + tablet + granule + siroop + liquid + 
                              inactive_variety + subst,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)


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
mdl_probit_tcount2 <- oglmx(N_entry_f ~ pastAG +price + revenue + N_incumbent +
                              N_br_over2:revenue + N_br_over3:revenue + N_incumbent:revenue +
                              capsule + tablet + granule + siroop + liquid + 
                              active_variety + inactive_variety + subst,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)


mdl_probit_tcount3 <- oglmx(N_entry_f ~ pastAG + N_incumbent + 
                              log(revenue) + log(revenue):N_incumbent + 
                              capsule + tablet + granule + siroop + liquid + 
                              inactive_variety + subst,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_tcount4 <- oglmx(N_entry_f ~ pastAG + N_incumbent + price + total + 
                              log(revenue) + log(revenue):N_incumbent + 
                              capsule + tablet + granule + siroop + liquid + 
                              inactive_variety + subst,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_tcount5 <- oglmx(N_entry_f ~ pastAG + log(revenue) + N_incumbent +
                              N_incumbent:log(revenue) +
                              capsule + tablet + granule + siroop + liquid + 
                              active_variety + inactive_variety + subst,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

## mean count
mdl_probit_mcount1 <- oglmx(N_entry_f ~ pastAG_mean + price + revenue + 
                             N_br_over2:revenue + N_br_over3:revenue + 
                             capsule + tablet + granule,
                           data = df_sample, 
                           link = "probit", 
                           constantMEAN = FALSE,
                           constantSD = FALSE, 
                           delta = 0, threshparam = NULL, 
                           savemodelframe=TRUE)

mdl_probit_mcount2 <- oglmx(N_entry_f ~ pastAG_mean +price + revenue + N_incumbent +
                              N_incumbent:revenue +
                              capsule + tablet + granule + siroop + liquid + 
                              active_variety + inactive_variety + subst,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_mcount3 <- oglmx(N_entry_f ~ pastAG_mean + 
                              price + revenue + N_incumbent +
                              capsule + tablet + granule + siroop + liquid + 
                              active_variety + inactive_variety + subst,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_mcount4 <- oglmx(N_entry_f ~ pastAG_mean + N_incumbent + price + log(total) + 
                              log(revenue) + log(revenue):N_incumbent + 
                              capsule + tablet + granule + siroop + liquid + 
                              inactive_variety + subst,
                            data = df_sample, 
                            link = "probit", 
                            constantMEAN = FALSE,
                            constantSD = FALSE, 
                            delta = 0, threshparam = NULL, 
                            savemodelframe=TRUE)

mdl_probit_mcount5 <- oglmx(N_entry_f ~ pastAG_mean + log(revenue) + N_incumbent +
                              N_incumbent:log(revenue) +
                              capsule + tablet + granule + siroop + liquid + 
                              active_variety + inactive_variety + subst,
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

## past AG as mean value ####
summary(mdl_probit_mcount1)
summary(mdl_probit_mcount2)
summary(mdl_probit_mcount3)
summary(mdl_probit_mcount4)


