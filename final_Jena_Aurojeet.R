### R-Data Analysis & Visualization In Science ###
#### Final Assessment ###
### Aurojeet Jena ###

#Loading Libraries
library(readr)
library(dplyr)
library(lubridate)

# Reading the Data

url <- "https://raw.githubusercontent.com/UCD-R-DAVIS/R-DAVIS/refs/heads/main/data/tyler_activity_laps_12-6.csv"

tyler_laps <- read_csv(url)


# Filter the dataset to keep only rows where the sport is "running"
running_activities <- tyler_laps %>% 
  filter(sport == "running")

# Filter for normal running laps
normal_running <- running_activities %>%
  filter(
    minutes_per_mile >= 5, 
    minutes_per_mile <= 10, 
    total_elapsed_time_s > 60 
  )

# Add a "time_period" column to classify activities
normal_running <- normal_running %>%
  mutate(
    timestamp = ymd_hms(timestamp),
    time_period = case_when(
      year < 2024 ~ "pre-2024 running",
      year == 2024 & month(timestamp) >= 1 & month(timestamp) <= 6 ~ "rehab efforts (Jan-Jun 2024)",
      year == 2024 & month(timestamp) >= 7 ~ "activities (Jul-Dec 2024)"
    )
  )

# Group by the new time_period variable and inspect the results
grouped_data <- normal_running %>%
  group_by(time_period)

# Calculate speed as the reciprocal of minutes_per_mile
normal_running <- normal_running %>%
  mutate(speed = 1 / minutes_per_mile) # Speed in miles per minute

# Create the scatter plot with linear trendlines for each time period
ggplot(normal_running, aes(x = speed, y = steps_per_minute)) +
  geom_point(aes(color = time_period), size = 3, alpha = 0.7) +  # Scatter plot with color by time_period
  geom_smooth(aes(color = time_period), method = "lm", se = FALSE, size = 1.2) + # Linear trendlines for each time period
  labs(
    title = "SPM vs Speed by Lap with Linear Trendlines",
    x = "Speed (miles per minute)",
    y = "Steps Per Minute (SPM)"
  ) +
  theme_light() +  # Clean and light theme for the plot
  theme(
    text = element_text(size = 14),  # Increase font size for readability
    axis.title = element_text(size = 16, face = "bold"),  # Bold axis titles
    axis.text = element_text(size = 12),  # Larger axis labels
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)  # Bold and centered plot title
  ) +
  scale_color_manual(values = c("pre-2024 running" = "blue", 
                                "rehab efforts (Jan-Jun 2024)" = "green", 
                                "activities (Jul-Dec 2024)" = "red"))  # Custom discrete colors



# Convert timestamp to Date and filter for runs after July 1, 2024
normal_running <- normal_running %>%
  mutate(timestamp = as.POSIXct(timestamp),  # Ensure timestamp is in POSIXct format
         date = as.Date(timestamp)) %>%
  filter(date > as.Date("2024-07-01"))  # Select only laps after July 1, 2024


# Rank laps by date, creating lap numbers
normal_running <- normal_running %>%
  group_by(date) %>%
  mutate(lap_number = rank(timestamp)) %>%
  filter(lap_number <= 3)  # Keep only the first three laps per day


# Calculate speed (in miles per minute) from minutes_per_mile
normal_running <- normal_running %>%
  mutate(speed = 1 / minutes_per_mile)  # Speed in miles per minute


# Plot SPM vs Speed, with lap number shown on x-axis and speed as color
ggplot(normal_running, aes(x = lap_number, y = steps_per_minute)) +
  geom_point(aes(color = speed, size = speed), alpha = 0.7) +  # Scatter plot with speed affecting color and size
  labs(
    title = "SPM vs Speed by Lap (Post-Intervention)",
    x = "Lap Number",
    y = "Steps Per Minute (SPM)",
    color = "Speed (miles per min)",
    size = "Speed (miles per min)"
  ) +
  theme_light() +  # Clean theme for clarity
  theme(
    text = element_text(size = 14),  # Increase font size for readability
    axis.title = element_text(size = 16, face = "bold"),  # Bold axis titles
    axis.text = element_text(size = 12),  # Larger axis labels
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)  # Bold and centered title
  ) +
  scale_color_gradient(low = "blue", high = "red")  # Gradient color based on speed
