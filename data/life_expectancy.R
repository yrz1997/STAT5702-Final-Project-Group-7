
data <- read_csv("data/owid-covid-data.csv")

# missing value
dat <- data[,c(1:6,11,12,62)] 
map_dbl(dat,~sum(is.na(.x)))
dat_a <- dat %>% na.omit()

