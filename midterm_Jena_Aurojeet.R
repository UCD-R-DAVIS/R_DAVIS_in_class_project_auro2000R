### R-Data Analysis & Visualization In Science ###
#### Midterm ###
### Aurojeet Jena ###

library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)

### Loading the Data ###

url <- "https://raw.githubusercontent.com/ucd-cepb/R-DAVIS/refs/heads/main/data/tyler_activity_laps_10-24.csv"
tyler_data <- read_csv(url)

### Data Filtering ###

running_laps <- tyler_data %>%
  filter(sport == "running") %>%
  filter(total_elapsed_time_s >= 60) %>%
  filter(minutes_per_mile < 10 & minutes_per_mile > 5) %>%
  mutate(
    pace_cat = case_when(
      minutes_per_mile < 6 ~ "fast",
      minutes_per_mile >= 6 & minutes_per_mile < 8 ~ "medium",
      TRUE ~ "slow"
    ),
    form = case_when(
      year == 2024 ~ "new form",
      TRUE ~ "old form"
    )
  )

# Grouping and Summarizing Average Steps per Minute by Form and Pace
avg_steps_summary <- running_laps %>%
  group_by(form, pace_cat) %>%
  summarize(avg_spm = mean(steps_per_minute, na.rm = TRUE)) %>%
  pivot_wider(id_cols = form, values_from = avg_spm, names_from = pace_cat) %>%
  select(form, slow, medium, fast)

# Displaying the result
print(avg_steps_summary)