# Name: [Jessica Magalit]
# Date: 2025-02-24
# Purpose: This script reads COVID-19 data from a URL and processes it.

# Install and load necessary libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")

library(ggplot2)
library(dplyr)
library(readr)

# Read in the COVID-19 data
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
covid_data <- read_csv(url)

# Identify the 6 states with the most current cases
top_states <- covid_data %>%
  filter(date == max(date)) %>%  # Filter for the most recent date
  group_by(state) %>%
  summarise(total_cases = max(cases)) %>%
  top_n(6, total_cases) %>%
  pull(state)

# Filter the data for the 6 states
filtered_data <- covid_data %>%
  filter(state %in% top_states)

# Create a faceted line plot
plot1 <- ggplot(filtered_data, aes(x = as.Date(date), y = cases, color = state)) +
  geom_line() +
  labs(title = "COVID-19 Cases in the 6 States with Most Cases",
       x = "Date", y = "Cases") +
  facet_wrap(~ state) +
  theme_minimal()

# Save the plot to the img directory
ggsave("img/state_cases_plot.png", plot1)

# Calculate daily total cases in the USA
daily_cases <- covid_data %>%
  group_by(date) %>%
  summarise(daily_total_cases = sum(cases, na.rm = TRUE))

# Create a column plot of daily total cases in the USA
plot2 <- ggplot(daily_cases, aes(x = as.Date(date), y = daily_total_cases)) +
  geom_col() +
  labs(title = "Daily Total COVID-19 Cases in the USA",
       x = "Date", y = "Total Cases") +
  theme_minimal()

# Save the plot to the img directory
ggsave("img/daily_cases_plot.png", plot2)

