library(tidyverse)

od_data <- read_csv(here::here("dataset-ignore", "od.csv"))

## CLEAN the data
o_MA <- data %>% 
  filter(o_state_name == "Massachusetts") %>%
  separate(pool,
           into = c("race","income"),
           sep = "Q"
  ) 

o_MA[is.na(o_MA)] = 0

write_csv(o_MA, file = here::here("dataset-ignore", "o_MA.csv"))

save(o_MA, file = here::here("dataset-ignore/o_MA.RData"))