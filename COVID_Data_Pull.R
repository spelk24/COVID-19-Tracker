# ----- Script to pull in full data each run ----- #
# NYT repository: https://github.com/nytimes/covid-19-data

library(tidyverse)
library(zoo)
# County
county_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
names(county_data) <- c("Date","County","State","FIPS","Cases","Deaths")

# Group by State
state_data <- county_data %>%
  group_by(Date, State) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths),
            .groups = "drop_last")

# Group by Country (US Only)
country_data <- county_data %>%
  group_by(Date) %>% 
  summarise(Cases = sum(Cases),
            Deaths = sum(Deaths),
            .groups = "drop_last")

# Run Date
today <- Sys.Date()