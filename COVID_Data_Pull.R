# ----- Script to pull in full data each run ----- #
# NYT repository: https://github.com/nytimes/covid-19-data

### --------------------- ###
#     --- Data Pull ---     #
### --------------------- ###

# Libs
library(tidyverse)
library(zoo)
library(tibbletime)
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

### --------------------- ###
#     --- Functions ---     #
### --------------------- ###
rolling14 <- rollify(mean, window = 14, na_value = 0) # rolling 14-day average

state_rolling_14 <- function(df, state) { # calculate rolling 14 data for a given state
  df <- df %>%
    filter(State == state) %>% 
    select(Date,Cases,Deaths) %>% 
    ungroup() %>% 
    mutate(New_Cases = Cases - lag(Cases, default = 0),
           New_Deaths = Deaths - lag(Deaths, default = 0)) %>% 
    replace_na(list(New_Cases = .[[1,2]],
                    New_Deaths = .[[1,3]])) %>%
    arrange(Date) %>% 
    ungroup() %>% 
    mutate(Rolling14Cases = round(rolling14(New_Cases)),
           Rolling14Deaths = round(rolling14(New_Deaths)),
           State = state)
  return(df)
}

### --------------------- ###
#     --- Data Prep ---     #
### --------------------- ###

daily_country_data <- country_data %>%
  arrange(Date) %>% 
  select(Date,Cases,Deaths) %>% 
  mutate(New_Cases = Cases - lag(Cases, default = 0),
         New_Deaths = Deaths - lag(Deaths, default = 0)) %>% 
  replace_na(list(New_Cases = .[[1,2]],
                  New_Deaths = .[[1,3]])) %>%
  arrange(Date) %>% 
  mutate(Rolling14Cases = round(rolling14(New_Cases)),
         Rolling14Deaths = round(rolling14(New_Deaths)),
         State = "All")

# Create mass dataset for all states and CW
states <- unique(state_data$State)

for (st in states) {
  temp_df <- state_rolling_14(state_data, st)
  daily_country_data <- bind_rows(daily_country_data,temp_df)
}

daily_country_data <- daily_country_data %>% arrange(State)