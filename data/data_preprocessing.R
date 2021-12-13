library(readr)
library(plyr)
library(dplyr)
library(tidyverse)
library(tibble)
library(ggplot2)
library(zoo)
library(reshape2)
library(patchwork)
library(ggridges)
library(tmap)            
library(tmaptools) 
library(tigris)          
library(sf)

day <- "2021-12-09"

us_vaccination_url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv'

us_cases_url <- 'https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv?raw=true'

us_death_url <- 'https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv?raw=true'

us_vaccination <- read_csv(url(us_vaccination_url))

us_cases <- read_csv(url(us_cases_url))

us_death <- read_csv(url(us_death_url))


non_state <- c("American Samoa","Bureau of Prisons","Dept of Defense","District of Columbia","Federated States of Micronesia",
               "Guam","Indian Health Svc","Long Term Care","Marshall Islands","Northern Mariana Islands","Puerto Rico","Republic of Palau",
               "United States","Veterans Health","Virgin Islands","Diamond Princess","Grand Princess")

state_vaccination_raw <- us_vaccination %>% filter(!location %in% non_state)

state_cases_raw <- us_cases %>% filter(!Province_State %in% non_state)

state_death_raw <- us_death %>% filter(!Province_State %in% non_state)

state_vaccination <- na.locf(state_vaccination_raw)

# state_vaccination_raw <- rename(state_vaccination_raw,c('location'='state'))
# state_cases_raw <- rename(state_cases_raw,c('Province_state'='state'))
# state_death_raw <- rename(state_death_raw,c('Province_state'='state'))

state_vaccination <- state_vaccination %>%
  rename(state=location) %>%
#  rename(c('location'='state')) %>%
  mutate(state=recode(state,'New York State' = 'New York')) %>%
  group_by(state) %>%
  mutate(vacc_diff = people_vaccinated - lag(people_vaccinated)) %>%
  mutate_at('vacc_diff', ~replace(., is.na(.), 0)) %>%
  mutate(vacc_diff = replace(vacc_diff, which(vacc_diff < 0), NA)) %>%     # replace negative diff with NA
  mutate(vacc_diff = na.locf(vacc_diff)) %>%
  mutate(fully_vacc_diff = people_fully_vaccinated - lag(people_fully_vaccinated)) %>%
  mutate_at('fully_vacc_diff', ~replace(., is.na(.), 0)) %>%
  mutate(fully_vacc_diff = replace(fully_vacc_diff, which(fully_vacc_diff < 0), NA)) %>%     # replace negative diff with NA
  mutate(fully_vacc_diff = na.locf(fully_vacc_diff))

state_cases <- state_cases_raw %>% 
  select(Province_State,12:dim(state_cases_raw)[2]) %>%
  group_by(Province_State) %>%
  summarise_all(list(sum)) %>%
  pivot_longer(!Province_State, names_to = 'date', values_to = 'cases') %>%
  mutate(date=as.Date(date, format="%m/%d/%y")) %>%
  rename(state=Province_State) %>%
#  rename(c('Province_State'='state')) %>%
  group_by(state) %>%
  mutate(case_diff = cases - lag(cases)) %>%         # calculate growth
  mutate_at(4, ~replace(., is.na(.), 0)) %>%    # remove NA of first day of diff
  mutate(case_diff = replace(case_diff, which(case_diff < 0), NA)) %>%     # replace negative diff with NA
  mutate(case_diff = na.locf(case_diff))

state_death <- state_death_raw %>% 
  select(Province_State,13:dim(state_cases_raw)[2]) %>%
  group_by(Province_State) %>%
  summarise_all(list(sum)) %>%
  pivot_longer(!Province_State, names_to = 'date', values_to = 'death') %>%
  mutate(date=as.Date(date, format="%m/%d/%y")) %>%
  rename(state=Province_State) %>%
#  rename(c('Province_State' = 'state')) %>%
  group_by(state) %>%
  mutate(death_diff = death - lag(death)) %>%         # calculate growth
  mutate_at(4, ~replace(., is.na(.), 0)) %>%    # remove NA of first day of diff
  mutate(death_diff = replace(death_diff, which(death_diff < 0), NA)) %>%     # replace negative diff with NA
  mutate(death_diff = na.locf(death_diff))

us_whole_vaccination <- us_vaccination[us_vaccination$location=='United States',]



