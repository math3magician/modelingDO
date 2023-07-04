# data_loading.R
library(readxl)
library(httr)
library(tidyverse)

# Define the path and filename for saving the data file
data_file <- "data.csv"

# Check if the data file exists
if (file.exists(data_file)) {
  # Load the data from the existing file
  data <- read_csv(data_file)
} else {
  # Fetch the data from the online Excel file
  excel_url <- "https://www.iceandclimate.nbi.ku.dk/data/GICC05modelext_GRIP_and_GISP2_and_resampled_data_series_Seierstad_et_al._2014_version_10Dec2014-2.xlsx"
  res <- httr::GET(excel_url)
  excel_file <- tempfile(fileext = ".xlsx")
  writeBin(httr::content(res, as = "raw"), excel_file)
  
  # Read the data from the Excel file
  data <- read_excel(excel_file, sheet = 3, range = "A51:F12278") %>%
    slice(-1) %>%
    select(1, ncol(.) - 1, ncol(.)) %>%
    mutate_all(as.numeric) %>%
    drop_na() %>%
    rename(age = 1, d18O = 2, Ca2 = 3)
  
  # Clean up temporary file
  unlink(excel_file)
  
  # Save the data to the data file as CSV
  write_csv(data, data_file)
}
