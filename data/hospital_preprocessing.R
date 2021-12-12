library(readr)
library(plyr)
library(dplyr)
library(tidyverse)
library(tibble)
library(ggplot2)
getOption('timeout')
options(timeout=200)


# COVID-19 hospital capacity

hospital_capacity_url<-'https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD'
hospital_capacity<-read_csv(url(hospital_capacity_url))

hospital_capacity=data_frame(hospital_capacity)
hospital_capacity[hospital_capacity==-999999]=2

selected_features<-c("collection_week","state","total_beds_7_day_sum",
                     "total_adult_patients_hospitalized_confirmed_covid_7_day_sum",
                     "total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum","total_icu_beds_7_day_sum")
hospital_capacity_selected<-hospital_capacity[,selected_features]
col_names<-c("colle_week","state","total_beds",
             "adu_conf",
             "ped_conf","total_icu_beds" )
names(hospital_capacity_selected)<-col_names

start_date_hos<-min(hospital_capacity_selected$colle_week)
  
end_date_hos<-max(hospital_capacity_selected$colle_week)

# COVID-19 case and death

case_death_yrl<-'https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD'
case_death<-read_csv(url(case_death_yrl))


## convert type of submission_date to date 
case_death$submission_date<-as.Date(case_death$submission_date, "%m/%d/%Y")
start_date_case<-min(case_death$submission_date)
end_date_case<-max(case_death$submission_date)

## select date which is in the same range as COVID-19 hospital capacity data-set

case_death_in_range <- case_death %>% filter(submission_date>=start_date_hos) %>% filter(submission_date<=end_date_hos)

## select features which are used for this analysis
case_death_selected<-case_death_in_range[,c("submission_date","state","tot_cases","tot_death")]

# total population by state
##source:https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm#
population<-read_csv('data/population.csv')
state_dict<-data.frame(State=c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware',
                               'District of Columbia','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa',
                               'Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota',
                               'Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico','New York',
                               'North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Puerto Rico','Rhode Island',
                               'South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Virgin Islands','Washington',
                               'West Virginia','Wisconsin','Wyoming'),
                       state_init=c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT',
                                    'NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','PR','RI','SC','SD','TN','TX','UT','VT','VA','VI','WA','WV','WI','WY'))

# replace the states name in population data-set with initial
population_state<-merge(population,state_dict,by="State")

selected_features2<-c("state_init","Pop")
population_state<-population_state[,selected_features2]
names(population_state)<-c("state","population")

# add total population column to the case_death_selected data-set
case_death_selected<-merge(case_death_selected,population_state,by="state")





