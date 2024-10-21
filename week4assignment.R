### R-Data Analysis & Visualization In Science ###
#### Homework Week 4 ###
### Aurojeet Jena ###

setwd("~/R_GitHub/R_GitHub_Auro/R_DAVIS_in_class_project_auro2000R/data")

library(tidyverse)

#Reading dataset
surveys <- read_csv("portal_data_joined.csv")

# Subsetting surveys to keep rows with weight between 30 and 60, and print the first 6 rows
surveys_subset <- surveys %>%
  filter(weight >= 30 & weight <= 60)

head(surveys_subset)

#Creating a tibble showing the maximum weight for each species+sex combination
biggest_critters <- surveys %>%
  drop_na(weight) %>%  # Removing rows with NA in weight
  group_by(species, sex) %>%
  summarize(max_weight = max(weight)) %>%
  arrange(desc(max_weight))  # Sorting by maximum weight

print(biggest_critters)


#Exploring where NA weights are concentrated
na_weight_tally <- surveys %>%
  filter(is.na(weight)) %>%
  group_by(species, taxa, plot_id) %>%
  tally() %>%
  arrange(desc(n))

print(na_weight_tally)

#Removing rows where weight is NA and add a column with the average weight for each species+sex combination
surveys_avg_weight <- surveys %>%
  drop_na(weight) %>%
  group_by(species, sex) %>%
  mutate(avg_weight = mean(weight)) %>%
  select(species, sex, weight, avg_weight)  # Keeping only the relevant columns


#Adding a column "above_average" to indicate if the weight is above the average for the species+sex combination
surveys_avg_weight <- surveys_avg_weight %>%
  mutate(above_average = weight > avg_weight)

print(surveys_avg_weight)

