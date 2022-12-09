library(tidyverse)

## STATE MODEL ##
leave <- read_csv(here::here('dataset','leave.csv')) %>%
  rename(State = o_state_name) %>% 
  mutate(State = toupper(State))
leave$State <- gsub(' ','',leave$State)
movein <- read_csv(here::here('dataset','movein.csv')) %>%
  rename(State = d_state_name) %>% 
  mutate(State = toupper(State))
movein$State <- gsub(' ','',movein$State)
## Merge&Plot ####### AVGCRIME
leave_crimeavg <- read_csv(here::here('dataset','00-07AvgCrime.csv')) %>%
  rename(State = Area) %>%
  inner_join(leave,by='State') 
leave_crimeavg %>%
  pivot_longer(c(meanRobbery,meanBurglary),names_to="crime",values_to="value") %>%
  ggplot(aes(x=value,y=leave_pop,color=crime)) +
  geom_point()
leave_crimeavg %>%
  pivot_longer(c(meanRobbery,meanBurglary),names_to="crime",values_to="value") %>%
  ggplot(aes(x=value,y=proportion,color=crime)) +
  geom_point()

movein_crimeavg <- read_csv(here::here('dataset','10-17AvgCrime.csv')) %>%
  inner_join(movein,by='State')
movein_crimeavg %>%
  pivot_longer(c(meanRobbery,meanBurglary),names_to="crime",values_to="value") %>%
  ggplot(aes(x=value,y=movein_pop,color=crime)) +
  geom_point()
movein_crimeavg %>%
  pivot_longer(c(meanRobbery,meanBurglary),names_to="crime",values_to="value") %>%
  ggplot(aes(x=value,y=proportion,color=crime)) +
  geom_point()

leave_crimeavg %>%
  ggplot(aes(x=meanRobbery,y=leave_pop))+
  geom_point()

###########FULL CRIME##########
leave_crime <- read_csv(here::here('dataset','00-07Crime.csv')) %>%
  rename(State=Area)%>%
  inner_join(leave,by='State')
leave_crime %>% 
  pivot_longer(c(Robbery,Burglary),names_to="crime",values_to="value")%>%
  ggplot(aes(x=value,y=leave_pop,color=crime)) +
  geom_point() +
  facet_grid(vars(year))
leave_crime %>%
  ggplot(aes(x=Robbery,y=proportion))+
  geom_point() +
  facet_grid(vars(year))

movein_crime <- read_csv(here::here('dataset','10-17Crime.csv')) %>%
  inner_join(movein,by='State')
movein_crime %>% 
  pivot_longer(c(Robbery,Burglary),names_to="crime",values_to="value")%>%
  ggplot(aes(x=value,y=proportion,color=crime)) +
  geom_point() +
  facet_grid(vars(year))
movein_crime %>% 
  ggplot(aes(x=Robbery,y=movein_pop)) +
  geom_point() +
  facet_grid(vars(year))

od_data <- read_csv(here::here('dataset-ignore','od_data.csv')) %>%
  mutate(leave = case_when(o_cz_name!=d_cz_name|o_state_name!=d_state_name ~ 1,
                           o_cz_name==d_cz_name&o_state_name==d_state_name ~ 0))

model <- glm(leave~income+race,data=od_data)

summary(model)