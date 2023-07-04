# data_preprocessing.R
library(dplyr)
library(tidyr)
library(zoo)
library(purrr)


preprocess_data <- function(data) {
  # Assuming 'data' is your data frame, 'age' is the age column, and 'value' is the value column
  data %>%
    # Group by 'age'
    group_by(age) %>%
    # Calculate the average 'd18O' and 'Ca2' for each 'age' and drop the grouping
    summarise(d18O = mean(d18O, na.rm = TRUE), Ca2 = mean(Ca2, na.rm = TRUE), .groups = "drop") %>%
    # Calculate the rolling mean for 'd18O' over a 625 point window, aligning to the right
    mutate(rolling_mean = rollmean(d18O, k = 625, fill = NA, align = "right")) %>%
    # Subtract the rolling mean from 'd18O'
    mutate(d18O = d18O - rolling_mean, Ca2 = -log(Ca2)) %>%
    # Remove the 'rolling_mean' column
    select(-rolling_mean) %>%
    # Filter data to keep only rows where 'age' is between 23000 and 110000
    filter(age >= 23000, age <= 110000) %>%
    # Create a new column with scaled age
    mutate(d18O = d18O - mean(d18O), Ca2 = Ca2 - mean(Ca2)) %>%
    arrange(desc(age)) 
}