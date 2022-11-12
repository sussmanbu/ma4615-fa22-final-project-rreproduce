library(tidyverse)
library(readxl)
## Primary Data: Commute Zone Population
od_data <- read_csv(here::here("dataset-ignore", "od.csv"))

#Sub-data: Origin from MA
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

##Secondary Data: Crime at 16(average of 2000-200) and at 26(average from 2010-2017)
data2010 <- read_csv(here::here("dataset", "2010-table-5.csv"),skip = 3)
data2011 <- read_csv(here::here("dataset", "2011-table-5.csv"),skip = 3)
data2012 <- read_csv(here::here("dataset", "2012-table-5.csv"),skip = 3)
data2013 <- read_csv(here::here("dataset", "2013-table-5.csv"),skip = 3)
data2014 <- read_csv(here::here("dataset", "2014-table-5.csv"),skip = 3)
data2015 <- read_csv(here::here("dataset", "2015-table-5.csv"),skip = 3)
data2016 <- read_csv(here::here("dataset", "2016-table-5.csv"),skip = 3)
data2017 <- read_csv(here::here("dataset", "2017-table-5.csv"),skip = 3)


data2010<- data2010 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2011<- data2011 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2012<- data2012 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2013<- data2013 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2014<- data2014 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2015<- data2015 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2016<- data2016 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2017<- data2017 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)

data2010 <- data2010[, colSums(is.na(data2010)) < nrow(data2010)]
data2011 <- data2011[, colSums(is.na(data2011)) < nrow(data2011)]
data2012 <- data2012[, colSums(is.na(data2012)) < nrow(data2012)]
data2013 <- data2013[, colSums(is.na(data2013)) < nrow(data2013)]
data2014 <- data2014[, colSums(is.na(data2014)) < nrow(data2014)]
data2015 <- data2015[, colSums(is.na(data2015)) < nrow(data2015)]
data2016 <- data2016[, colSums(is.na(data2016)) < nrow(data2016)]
data2017 <- data2017[, colSums(is.na(data2017)) < nrow(data2017)]

df_list <- list(data2010, data2011, data2012,data2013,data2014,data2015,data2016,data2017)
mergedf <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
mergedf$State <- gsub('[0-9., ]', '', mergedf$State)

mergedf<-mergedf %>%
  select(c(State,Robbery,Burglary)) %>%
  group_by(State) %>%
  summarize(meanRobbery = mean(Robbery),meanBurglary = mean(Burglary))
write.csv(mergedf,file=here::here("dataset/10-17CrimeAvg.csv"), row.names = FALSE)
save(mergedf, file = here::here("dataset/10-17CrimeAvg.RData"))

# year2000
data2000 <- read_csv(here::here("dataset", "2000.csv"),skip = 3)%>% 
  filter(Area!= 'Metropolitan Statistical Area'&
           Area!='Area actually reporting'&
           Area!='Estimated totals'&
           Area!='Cities outside metropolitan areas'&
           Area!='Rural'&
           Area!='State Total'&
           Area!='Total') %>% 
  select(c(Area,Robbery,Burglary))
data2000$Area <- gsub('Rate per 100,000 inhabitants', NA, data2000$Area) 
data2000 <- data2000 %>% 
  fill(Area) %>%
  drop_na(Robbery, Burglary)

# year2001
data2001 <- read_csv(here::here("dataset", "2001.csv"),skip = 4)%>% 
  filter(Area!= 'Metropolitan Statistical Area'&
           Area!='Area actually reporting'&
           Area!='Estimated totals'&
           Area!='Cities outside metropolitan areas'&
           Area!='Rural'&
           Area!='State Total'&
           Area!='Total') %>% 
  select(c(Area,Robbery,Burglary))
data2001$Area <- gsub('Rate per 100,000 inhabitants', NA, data2001$Area) 
data2001 <- data2001 %>% 
  fill(Area) %>%
  drop_na(Robbery, Burglary)

# year2002
data2002 <- read_csv(here::here("dataset", "2002.csv"),skip = 3)%>% 
  filter(Area!= 'Metropolitan Statistical Area'&
           Area!='Area actually reporting'&
           Area!='Estimated totals'&
           Area!='Cities outside metropolitan areas'&
           Area!='Rural'&
           Area!='State Total'&
           Area!='Total'&
           Area!='Estimated total') %>% 
  select(c(Area,Robbery,Burglary))
data2002$Area <- gsub('Rate per 100,000 inhabitants', NA, data2002$Area) 
data2002 <- data2002 %>% 
  fill(Area) %>%
  drop_na(Robbery, Burglary)

# year2003
data2003 <- read_csv(here::here("dataset", "2003.csv"),skip = 4)%>% 
  filter(Area!= 'Metropolitan Statistical Area'&
           Area!='Area actually reporting'&
           Area!='Estimated totals'&
           Area!='Cities outside metropolitan areas'&
           Area!='Rural'&
           Area!='State Total'&
           Area!='Total'&
           Area!='Nonmetropolitan counties'&
           Area!='Estimated total') %>% 
  select(c(Area,Robbery,Burglary))
data2003$Area <- gsub('Rate per 100,000 inhabitants', NA, data2003$Area) 
data2003 <- data2003 %>% 
  fill(Area) %>%
  drop_na(Robbery, Burglary)

# year2004 data is missing

# year2005
data2005<- read_excel(here::here("dataset", "2005-table-5.xls"),skip = 3) %>% 
  fill(State) %>% 
  fill(Area) %>% 
  filter(Area == 'State Total') %>% 
  filter(...3== 'Rate per 100,000 inhabitants') %>% 
  select(-Population,-...3)%>%
  select(State,Robbery,Burglary)
colnames(data2005)[1] = "Area"


# year2006
data2006<-read_excel(here::here("dataset", "2006-table-5.xls"),skip = 3) %>% 
  fill(State) %>% 
  fill(Area) %>% 
  filter(Area == 'State Total') %>% 
  filter(...3== 'Rate per 100,000 inhabitants') %>%
  select(-Population,-...3)%>% 
  select(State,Robbery,Burglary)
colnames(data2006)[1] = "Area"


# year2007
data2007<-read_excel(here::here("dataset", "2007-table-5.xls"),skip = 3) %>% 
  fill(State) %>% 
  fill(Area) %>% 
  filter(Area == 'State Total') %>%
  filter(...3== 'Rate per 100,000 inhabitants') %>% 
  select(-Population,-...3)%>% 
  select(State,Robbery,Burglary)
colnames(data2007)[1] = "Area"


# combine into 1 dataset 
df_list <- list(data2000, data2001, data2002,data2003,data2005,data2006,data2007)
mergedf <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
mergedf$Area <- gsub('[0-9., ]', '', mergedf$Area)


mergedf <- mergedf %>%
  group_by(Area) %>%
  summarize(meanRobbery = mean(as.numeric(Robbery)),meanBurglary = mean(Burglary))


write.csv(mergedf,file=here::here("dataset/00-07CrimeAvg.csv"), row.names = FALSE)
save(mergedf, file = here::here("dataset/00-07CrimAvg.RData"))

