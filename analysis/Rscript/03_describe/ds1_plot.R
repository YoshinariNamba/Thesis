
# library
library(tidyverse)

# source
source("./Rscript/01_append/ap0_master.R")
source("./Rscript/02_clean/cl2_japic.R", encoding = "UTF-8")

#
## wider
df_jpc_wide <- 
  df_jpc_plt %>% 
  pivot_wider(id_cols = c(eff), 
              names_from = year, 
              names_glue = "{.value}_{year}", 
              values_from = c(N_firm, N_brand, N_gen, N_ag)) %>% 
  na.omit() 

## longer
df_jpc_long <- df_jpc_wide %>% 
  pivot_longer(cols = - c(eff), 
               names_to = "type", 
               values_to = "N") %>% 
  mutate(year = str_split(.$type, pattern = "_", simplify = TRUE)[, 3], 
         metrics = str_split(.$type, pattern = "_", simplify = TRUE)[, 2]) %>% 
  select(eff, year, metrics, N) %>% 
  mutate(year = as.numeric(year))

## plot
df_jpc_long %>% 
  mutate(metrics = ifelse(metrics == "firm", "total", metrics)) %>% 
  group_by(year, metrics) %>% 
  summarise(N = mean(N), .groups = "drop") %>% 
  mutate(N = round(N, digits = 2)) %>% 
  ggplot(aes(x = year, y = N, colour = metrics)) +
  geom_point() + 
  geom_line(position = "identity", size = 1) + 
  geom_text(aes(label = N)) + 
  theme_minimal()
