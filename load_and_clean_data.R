library(tidyverse)

od_data <- read_csv(here::here("dataset-ignore", "od.csv"))

## CLEAN the data
o_MA <- od_data %>% 
  filter(o_state_name == "Massachusetts") %>%
  separate(pool,
           into = c("race","income"),
           sep = -2
  ) %>%
  filter(n != 0 & n != -1) %>%
  filter(n_tot_o != -1) %>%
  filter(n_tot_d != -1)

o_MA[is.na(o_MA)] = 0

colnames(o_MA) <- c('Num_original_CZ','Original_CZ','Original_State','Num_dest_CZ','Dest_CZ','Dest_state','n','From_origin','Live_in_dest','race','income','pr_d_o','pr_o_d')


write_csv(o_MA, file = here::here("dataset", "o_MA.csv"))

save(o_MA, file = here::here("dataset/o_MA.RData"))
