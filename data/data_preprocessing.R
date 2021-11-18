library(readr)
library(dplyr)
library(tidyverse)
library(tibble)
library(ggplot2)

us_vaccination_url <- 'https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv'

us_vaccination <- read_csv(url(us_vaccination_url))

non_state <- c("American Samoa","Bureau of Prisons","Dept of Defense","District of Columbia","Federated States of Micronesia",
               "Guam","Indian Health Svc","Long Term Care","Marshall Islands","Northern Mariana Islands","Puerto Rico","Republic of Palau",
               "United States","Veterans Health","Virgin Islands")

us_50_state_vaccination <- us_vaccination %>% filter(!location %in% non_state)

us_whole_vaccination <- us_vaccination[us_vaccination$location=='United States',]

