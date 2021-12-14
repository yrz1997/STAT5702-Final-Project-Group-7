
data <- read_csv("data/owid-covid-data.csv")

# missing value
dat <- data[,c(1:6,11,12,62)] 
map_dbl(dat,~sum(is.na(.x)))
dat_a <- dat %>% na.omit()


# a total number of COVID-19 cases 
total_cases_loc <- dat_a %>% group_by(location) %>%
  summarise(total_cases[length(total_cases)])



loc <- c("China", "India","Sweden", "Russia", "United Kingdom", "United States")
# find total cases
total_cases_loc <- total_cases_loc[total_cases_loc$location %in% loc, ] 
colnames(total_cases_loc)[2] <- "total_cases"


dat_a$date <- dat_a$date %>% ymd()
total_cases_per_day <- dat_a %>% group_by(location) %>% summarise(time_length(interval(date[1], date), "day")) 
colnames(total_cases_per_day)[2] <- "Days"
total_cases_per_day$new_cases <- dat_a$new_cases
total_cases_per_day$total_cases_per_million <- dat_a$total_cases_per_million
total_cases_per_day$new_cases_per_million <- dat_a$new_cases_per_million
total_cases_per_day <- total_cases_per_day[total_cases_per_day$location %in% loc, ] 

data_relation <- dat_a[, c("location", "total_cases_per_million", "life_expectancy")]
data_relation_uni <- data_relation %>% group_by(location) %>% summarise(total_cases_per_million[length(total_cases_per_million)])
data_relation_uni <- na.omit(data_relation_uni)
data_relation <- data_relation[which(data_relation$total_cases_per_million %in% data_relation_uni$`total_cases_per_million[length(total_cases_per_million)]`), ]
data_relation <- na.omit(data_relation)

