
# library
library(tidyverse)
library(oglmx)


# setup -------------------------------------------------------------------

# list
## dependent variable
ls_dep_var <- 
  c("N_entry", "N_entry_f")

## AG variable
ls_ag_var <- 
  c("pastAG", "pastAG_dummy", "pastAG_mean", "belief")

## covariates (control variable)
ls_ctrl_var <- 
  c(
    paste(c("ln_revenue_total", "N_incumbent", "N_incumbent:ln_revenue_total"), 
          collapse = " + "),
    paste(c("ln_revenue_total", "N_incumbent", "N_incumbent:ln_revenue_total", 
          "capsule", "tablet", "granule", "siroop", "liquid"), 
          collapse = " + "),
    paste(c("ln_revenue_total", "N_incumbent", "N_incumbent:ln_revenue_total", 
            "form_variety", 
            "inactive_variety", 
            "subst_inc", "ln_subst_rev", "subst_inc:ln_subst_rev"),
          collapse = " + "),
    paste(c("ln_revenue_total", "N_incumbent", "N_incumbent:ln_revenue_total", 
          "capsule", "tablet", "granule", "siroop", "liquid", 
          "inactive_variety", 
          "subst_inc", "ln_subst_rev", "subst_inc:ln_subst_rev"),
          collapse = " + "),
    paste(c("ln_revenue_total", "N_incumbent",  
            "capsule", "tablet", "granule", "siroop", "liquid", 
            "inactive_variety", 
            "subst_inc", "ln_subst_rev"),
          collapse = " + "),
    paste(c("ln_revenue_total", "N_incumbent", "N_incumbent:ln_revenue_total", 
          "ln_revenue_total_hos", "N_incumbent:ln_revenue_total_hos",
          "capsule", "tablet", "granule", "siroop", "liquid", 
          "inactive_variety", 
          "subst_inc", "ln_subst_rev", "subst_inc:ln_subst_rev"),
          collapse = " + ")
  )

# model
df_model <- 
  expand_grid(
    dep = ls_dep_var, 
    ag = ls_ag_var, 
    ctrl = ls_ctrl_var, 
  ) %>% 
  mutate(
    type = as.numeric(as.factor(ctrl)), 
    model = paste(dep, "~", ag, "+", ctrl) 
  ) 


# load data
df_sample <- 
  read_rds("./output/data/sample.rds")


# negative binomial -------------------------------------------------------

# define empty list for outer loop
ls_negative_binomial <- list(NULL)
length(ls_negative_binomial) <- length(ls_ag_var)

# define empty data.frame for estimates and p.value 
df_nbinomial_summary <- 
  data.frame(ag_var = NULL, type = NULL, estimate = NULL, p.value = NULL)


for(i in 1:length(ls_ag_var)){
  
  # define empty list for inner loop
  ls_negative_binomial[[i]] <- list(NULL)
  length(ls_negative_binomial[[i]]) <- length(ls_ctrl_var)
  
  for(j in 1:length(ls_ctrl_var)){
    # information
    cat("Outer index:", i, "/", length(ls_ag_var), "Inner index", j, "/", length(ls_ctrl_var), "\n")
    
    # implement negative binomial estimation
    ls_negative_binomial[[i]][[j]] <- 
      MASS::glm.nb(
        as.formula(df_model %>% 
                     filter(dep == "N_entry", ag == ls_ag_var[i], type == j) 
                   %>% pull(model)), 
        data = df_sample
      )
    
    # create data.frame for estimates
    df_tmp <- 
      data.frame(
        ag_var = ls_ag_var[i], 
        type = j, 
        estimate = summary(ls_negative_binomial[[i]][[j]])$coefficients[ls_ag_var[i], 1], 
        p.value = summary(ls_negative_binomial[[i]][[j]])$coefficients[ls_ag_var[i], 4]
      )
    
    # bind it with existed data.frame
    df_nbinomial_summary <- 
      rbind(df_nbinomial_summary, df_tmp)
  }
}



# ordered probit ----------------------------------------------------------

# define empty list for outer loop
ls_ordered_probit <- list(NULL)
length(ls_ordered_probit) <- length(ls_ag_var)

# define empty data.frame for estimates and p.value 
df_oprobit_summary <- 
  data.frame(ag_var = NULL, type = NULL, estimate = NULL, p.value = NULL)


for(i in 1:length(ls_ag_var)){
  
  # define empty list for inner loop
  ls_ordered_probit[[i]] <- list(NULL)
  length(ls_ordered_probit[[i]]) <- length(ls_ctrl_var)
  
  for(j in 1:length(ls_ctrl_var)){
    # information
    cat("Outer index:", i, "/", length(ls_ag_var), "Inner index", j, "/", length(ls_ctrl_var), "\n")
    
    # implement ordered probit estimation
    ls_ordered_probit[[i]][[j]] <- 
      oglmx(
        as.formula(df_model %>% 
                     filter(dep == "N_entry_f", ag == ls_ag_var[i], type == j) 
                   %>% pull(model)), 
        data = df_sample, 
        link = "probit", 
        constantMEAN = FALSE,
        constantSD = FALSE, 
        delta = 0, threshparam = NULL, 
        savemodelframe=TRUE
      )
    
    # create data.frame for estimates
    df_tmp <- 
      data.frame(
        ag_var = ls_ag_var[i], 
        type = j, 
        estimate = summary(ls_ordered_probit[[i]][[j]])$estimate[ls_ag_var[i], 1], 
        p.value = summary(ls_ordered_probit[[i]][[j]])$estimate[ls_ag_var[i], 4]
      )
    
    # bind it with existed data.frame
    df_oprobit_summary <- 
      rbind(df_oprobit_summary, df_tmp)
  }
}


# save --------------------------------------------------------------------

write_rds(df_nbinomial_summary, "./output/result/df_nbinomial_summary.rds")
write_rds(ls_negative_binomial, "./output/result/ls_negative_binomial")
write_rds(df_oprobit_summary, "./output/result/df_oprobit_summary.rds")
write_rds(ls_ordered_probit, "./output/result/ls_ordered_probit")
