library(tidyverse)
library(readxl)
## Primary Data: Commute Zone Population
## Import and Clean
od_data <- read_csv(here::here("dataset-ignore", "od.csv")) %>%
  separate(pool,
           into = c("race","income"),
           sep = -2
  ) %>%
  filter(n != -1) %>%
  filter(n_tot_o != -1) %>%
  filter(n_tot_d != -1) 
od_data[is.na(od_data)] <- 0
write.csv(od_data,file=here::here("dataset-ignore/od_data.csv"),row.names=FALSE)

#########BY STATE#########

## INCOME ##
state_inc <- od_data %>% 
  group_by(o_state_name,income) %>%
  summarize(inc_n = sum(n))
write.csv(state_inc,file=here::here("dataset/state_inc.csv"))
## LEAVE FROM STATE ##
leave <-od_data %>% 
  group_by(o_state_name) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  filter(o_state_name != d_state_name) %>% group_by(o_state_name) %>% 
  summarise(leave_pop = sum(n),total_pop = total, proportion = sum(n)/total) %>%
  distinct(o_state_name,.keep_all = T)
write_csv(leave, file = here::here("dataset", "leave.csv"))

## MOVE INTO STATE ##
movein <- od_data %>% 
  group_by(d_state_name) %>%
  mutate(total = sum(n)) %>%
  filter(o_state_name != d_state_name) %>% group_by(d_state_name) %>% 
  summarise(movein_pop = sum(n),total_pop = total,proportion = sum(n)/total) %>%
  distinct(d_state_name,.keep_all = T)
write_csv(movein, file = here::here("dataset", "movein.csv"))

###Sub-data: Origin from State(eg.MA)###
o_MA <- od_data %>% 
  filter(o_state_name == "Massachusetts") 
write_csv(o_MA, file = here::here("dataset", "o_MA.csv"))

##Complimentary Data: 
Uni <- read_delim(here::here('dataset','us-colleges-and-universities.csv'),delim=';') %>%
  filter(COUNTRY == 'USA') %>%
  select(NAME,CITY,STATE,NAICS_DESC) 
write_csv(Uni, file = here::here("dataset", "Uni.csv"))

StateUni <- Uni %>%
  group_by(NAICS_DESC,STATE) %>%
  summarize(n=n())%>%
  pivot_wider(names_from = NAICS_DESC, values_from = 1) %>%
  rename(Business='BUSINESS AND SECRETARIAL SCHOOLS',
         General='COLLEGES, UNIVERSITIES, AND PROFESSIONAL SCHOOLS',
         Computer='COMPUTER TRAINING',
         Cosme_Barber='COSMETOLOGY AND BARBER SCHOOLS',
         EduServ='EDUCATIONAL SUPPORT SERVICES',
         Arts='FINE ARTS SCHOOLS',
         Flight='FLIGHT TRAINING',
         Junior='JUNIOR COLLEGES',
         Other='OTHER TECHNICAL AND TRADE SCHOOLS') %>%
  pivot_longer(c(Business,General,Computer,Cosme_Barber,EduServ,Arts,Flight,Junior,Other),names_to = 'type',values_to='value') %>%
  drop_na() %>%
  select(STATE,type,n)
write_csv(StateUni, file = here::here("dataset", "StateUni.csv"))
  

##Secondary Data: Crime at AGE 16(average of 2000-200) and at AGE 26(average from 2010-2017)
data2010 <- read_csv(here::here("dataset/crime2010-2017", "2010-table-5.csv"),skip = 3)
data2011 <- read_csv(here::here("dataset/crime2010-2017", "2011-table-5.csv"),skip = 3)
data2012 <- read_csv(here::here("dataset/crime2010-2017", "2012-table-5.csv"),skip = 3)
data2013 <- read_csv(here::here("dataset/crime2010-2017", "2013-table-5.csv"),skip = 3)
data2014 <- read_csv(here::here("dataset/crime2010-2017", "2014-table-5.csv"),skip = 3)
data2015 <- read_csv(here::here("dataset/crime2010-2017", "2015-table-5.csv"),skip = 3)
data2016 <- read_csv(here::here("dataset/crime2010-2017", "2016-table-5.csv"),skip = 3)
data2017 <- read_csv(here::here("dataset/crime2010-2017", "2017-table-5.csv"),skip = 3)
data2010<- data2010 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2011<- data2011 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2012<- data2012 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2013<- data2013 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2014<- data2014 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2015<- data2015 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2016<- data2016 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2017<- data2017 %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)
data2010 <- data2010[, colSums(is.na(data2010)) < nrow(data2010)] %>% mutate(year = '2010')
data2011 <- data2011[, colSums(is.na(data2011)) < nrow(data2011)] %>% mutate(year = '2011')
data2012 <- data2012[, colSums(is.na(data2012)) < nrow(data2012)] %>% mutate(year = '2012')
data2013 <- data2013[, colSums(is.na(data2013)) < nrow(data2013)] %>% mutate(year = '2013')
data2014 <- data2014[, colSums(is.na(data2014)) < nrow(data2014)] %>% mutate(year = '2014')
data2015 <- data2015[, colSums(is.na(data2015)) < nrow(data2015)] %>% mutate(year = '2015')
data2016 <- data2016[, colSums(is.na(data2016)) < nrow(data2016)] %>% mutate(year = '2016')
data2017 <- data2017[, colSums(is.na(data2017)) < nrow(data2017)] %>% mutate(year = '2017')
df_list <- list(data2010, data2011, data2012,data2013,data2014,data2015,data2016,data2017)
mergedf <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
mergedf$State <- gsub('[0-9., ]', '', mergedf$State)
merge <- mergedf %>% select(c(State,Robbery,Burglary,year)) %>% arrange(year)
average_merge<-mergedf %>%select(c(State,Robbery,Burglary,year)) %>%group_by(State) %>%summarize(meanRobbery = mean(Robbery),meanBurglary = mean(Burglary))
write.csv(merge,file=here::here("dataset/10-17Crime.csv"), row.names = FALSE)
write.csv(average_merge,file=here::here("dataset/10-17AvgCrime.csv"), row.names = FALSE)
data2000 <- read_csv(here::here("dataset/crime2000-2007", "2000.csv"),skip = 3)%>% filter(!Area%in% c('Metropolitan Statistical Area','Area actually reporting','Estimated totals','Cities outside metropolitan areas','Rural','State Total','Total'))%>%select(c(Area,Robbery,Burglary))
data2000$Area <- gsub('Rate per 100,000 inhabitants', NA, data2000$Area) 
data2000 <- data2000 %>% fill(Area) %>% drop_na(Robbery, Burglary)%>% mutate(year='2000')
data2001 <- read_csv(here::here("dataset/crime2000-2007", "2001.csv"),skip = 4)%>% filter(!Area%in% c('Metropolitan Statistical Area','Area actually reporting','Estimated totals','Cities outside metropolitan areas','Rural','State Total','Total'))%>%select(c(Area,Robbery,Burglary))
data2001$Area <- gsub('Rate per 100,000 inhabitants', NA, data2001$Area) 
data2001 <- data2001 %>% fill(Area) %>%drop_na(Robbery, Burglary)%>% mutate(year='2001')
data2002 <- read_csv(here::here("dataset/crime2000-2007", "2002.csv"),skip = 3)%>% filter(!Area%in% c('Metropolitan Statistical Area','Area actually reporting','Estimated total','Cities outside metropolitan areas','Rural','State Total','Total'))%>%select(c(Area,Robbery,Burglary))
data2002$Area <- gsub('Rate per 100,000 inhabitants', NA, data2002$Area) 
data2002 <- data2002 %>% fill(Area) %>%drop_na(Robbery, Burglary)%>% mutate(year='2002')
data2003 <- read_csv(here::here("dataset/crime2000-2007", "2003.csv"),skip = 4)%>% filter(!Area%in% c('Metropolitan Statistical Area','Area actually reporting','Estimated total','Cities outside metropolitan areas','Nonmetropolitan counties','Rural','State Total','Total'))%>%select(c(Area,Robbery,Burglary))
data2003$Area <- gsub('Rate per 100,000 inhabitants', NA, data2003$Area) 
data2003$Area <- gsub('2', '', data2003$Area) 
data2003 <- data2003 %>% fill(Area) %>%drop_na(Robbery, Burglary)%>% mutate(year='2003')
# year2004 data is missing
data2005<- read_excel(here::here("dataset/crime2000-2007", "2005-table-5.xls"),skip = 3) %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)%>%select(State,Robbery,Burglary) %>% mutate(year='2005')
colnames(data2005)[1] = "Area"
data2006<-read_excel(here::here("dataset/crime2000-2007", "2006-table-5.xls"),skip = 3) %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>% filter(...3== 'Rate per 100,000 inhabitants') %>%select(-Population,-...3)%>% select(State,Robbery,Burglary) %>% mutate(year='2006')
colnames(data2006)[1] = "Area"
data2007<-read_excel(here::here("dataset/crime2000-2007", "2007-table-5.xls"),skip = 3) %>% fill(State) %>% fill(Area) %>% filter(Area == 'State Total') %>%filter(...3== 'Rate per 100,000 inhabitants') %>% select(-Population,-...3)%>% select(State,Robbery,Burglary) %>% mutate(year='2007')
colnames(data2007)[1] = "Area"
df_list <- list(data2000, data2001, data2002,data2003,data2005,data2006,data2007)
mergedf <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list) %>% arrange(year)
mergedf$Area <- gsub('[0-9., ]', '', mergedf$Area) 
average_merge <- mergedf %>%group_by(Area) %>%summarize(meanRobbery = mean(as.numeric(Robbery)),meanBurglary = mean(Burglary))
write.csv(mergedf,file=here::here("dataset/00-07Crime.csv"), row.names = FALSE)
write.csv(average_merge,file=here::here("dataset/00-07AvgCrime.csv"),row.names=FALSE)


## Combined State variables
uni <- read_csv(here::here("dataset/StateUni.csv")) %>%
  group_by(STATE) %>%
  mutate(state_sum = sum(n)) %>% 
  pivot_wider(names_from = type,values_from=n)
uni[is.na(uni)] <- 0
state <- st_read(here::here("dataset","cb_2019_us_state_20m/cb_2019_us_state_20m.shp")) %>%
  select(STUSPS,NAME) %>% rename(STATE = STUSPS)
state_uni <- inner_join(uni,state,by='STATE') %>%
  ungroup() %>%
  select(-'STATE')
movein_crime <- read_csv(here::here("dataset/10-17CrimeAvg.csv")) %>% rename(NAME = State)
leave_crime <- read_csv(here::here("dataset/00-07CrimeAvg.csv")) %>% rename(NAME = Area)
leave <- read_csv(here::here("dataset","leave.csv")) %>%
  select(-total_pop) %>%
  rename(NAME = o_state_name,leave_prop=proportion) 
movein <- read_csv(here::here("dataset","movein.csv")) %>%
  select(-total_pop) %>%
  rename(NAME = d_state_name,movein_prop=proportion)
state <- inner_join(state_uni,leave,by="NAME") %>%
  inner_join(movein,by="NAME") %>% ungroup() %>% mutate(NAME=toupper(NAME)) 
state$NAME <- gsub(' ','',state$NAME)
state <- state_full %>% inner_join(movein_crime,by='NAME') %>%
  rename(Robbery10=meanRobbery,Burglary10=meanBurglary) %>%
  inner_join(leave_crime,by='NAME') %>%
  rename(Robbey00=meanRobbery,Burglary00=meanBurglary) %>%
  select(-geometry)

write.csv(state,file=here::here("dataset/state.csv"),row.names=FALSE)
