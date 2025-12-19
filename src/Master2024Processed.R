# Load required packages
library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(lubridate)  # For handling dates
library(tidyr)      # For handling missing values efficiently
library(stringr)

# Read the CSV file
df <- read_csv("Data/MasterTemp2024.csv", na = c("", "NA")) # Seba: changed so it doesn't depend on local file directory but on the project's

# Convert Date and Plant_Date to Date format
df <- df %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"), 
    Plant_Date = as.Date(Plant_Date, format = "%Y-%m-%d"),
    
    # Calculate Days_After_Planting (only if both Date and Plant_Date are available)
    Days_After_Planting = if_else(!is.na(Date) & !is.na(Plant_Date), as.numeric(Date - Plant_Date), NA_real_)
  )

# Function to convert Fahrenheit to Celsius (handles missing values)
convert_f_to_c <- function(fahrenheit) {
  ifelse(!is.na(fahrenheit), (fahrenheit - 32) * 5/9, NA_real_)
}

# Apply conversion only where values exist
df <- df %>%
  mutate(
    LocMaxTempC = convert_f_to_c(LocMaxTempF),
    LocMinTempC = convert_f_to_c(LocMinTempF),
    
    PRISMMinTempC = convert_f_to_c(PRISMMinTempF),
    PRISMMaxTempC = convert_f_to_c(PRISMMaxTempF),
    
    StatMinTempC = convert_f_to_c(StatMinTempF),
    StatMaxTempC = convert_f_to_c(StatMaxTempF)
  )

calculate_days_to_head <- function(data) {
  result <- data.frame()
  
  # Get the list of cumulative columns (if needed for reference)
  cumul_cols <- grep("^cum_", names(data), value = TRUE)
  
  for (var in c("HeadM105", "HeadM206", "HeadM209", "HeadM210", "HeadM211", "HeadM401")) {
    # Identify the first "preHeading" and "Heading" dates
    head_dates <- data %>%
      filter(!!sym(var) == "Heading") %>%
      group_by(Location, Year) %>%
      summarize(
        first_head_date = min(Date, na.rm = TRUE),
        first_head_dap = min(Days_After_Planting, na.rm = TRUE),
        .groups = 'drop'
      )
    
    pre_dates <- data %>%
      filter(!!sym(var) == "preHeading") %>%
      group_by(Location, Year) %>%
      summarize(
        first_pre_date = min(Date, na.rm = TRUE),
        first_pre_dap = min(Days_After_Planting, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Join head_dates and pre_dates
    combined <- left_join(head_dates, pre_dates, by = c("Location", "Year"))
    
    # Calculate "Days to Heading"
    combined <- combined %>%
      mutate("{var}_DaysToHeading" := as.numeric(first_head_dap - first_pre_dap))
    
    # Merge back into original dataset, filling missing values across all rows
    data <- left_join(data, combined %>% select(Location, Year, ends_with("DaysToHeading")), by = c("Location", "Year")) %>%
      group_by(Location, Year) %>%
      fill(ends_with("DaysToHeading"), .direction = "downup") %>% # Fills NAs down and up within groups
      ungroup()
  }
  
  return(data)
}

# Apply the function to the dataframe
df <- calculate_days_to_head(df)

# Ensure the Outputs folder exists
output_dir <- "Outputs" # Seba: changed so it doesn't depend on local file directory but on the project's
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save the processed data in the Outputs folder
write_csv(df, file.path(output_dir, "MasterTemp2024_processed.csv"))

# Print message to confirm script completion
cat("Processing complete! File saved in 'Outputs/MasterTemp2024_processed.csv'\n")

