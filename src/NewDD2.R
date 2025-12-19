library(dplyr)
library(readr)
library(stats)

# Load dataset (Update file path)
df <- read_csv("Outputs/MasterTemp2024_processed.csv", na = c("", "NA")) # Seba: changed so it doesn't depend on local file directory but on the project's

# ✅ Exclude Specific Location-Year Pairs
df <- df %>%
  filter(!(Location == "Canal" & Year == 2021),
         !(Location == "Rehmann" & Year == 2023),
         !(Location == "BosworthRue" & Year == 2024),
         !(Location == "DelRio" & Year == 2024),
         !(Location == "Wylie" & Year == 2024))

# Ensure missing values and adjustments are applied correctly
df <- df %>%
  mutate(
    # Replace missing Local/Stat values with PRISM
    LocMinTempC = ifelse(is.na(LocMinTempC), PRISMMinTempC, LocMinTempC),
    LocMaxTempC = ifelse(is.na(LocMaxTempC), PRISMMaxTempC, LocMaxTempC),
    StatMinTempC = ifelse(is.na(StatMinTempC), PRISMMinTempC, StatMinTempC),
    StatMaxTempC = ifelse(is.na(StatMaxTempC), PRISMMaxTempC, StatMaxTempC),
    
    # Apply replacement rules for Local Data
    LocMinTempC = ifelse(MINDif_OUT == 1, PRISMMinTempC, LocMinTempC),
    LocMaxTempC = ifelse(MAXDif_OUT == 1, PRISMMaxTempC, LocMaxTempC)
  )

# Ensure the Outputs folder exists
output_dir <- "Outputs" # Seba: changed so it doesn't depend on local file directory but on the project's
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ✅ Function to calculate GDD
calculate_gdd <- function(df, min_temp_col, max_temp_col, source_name, T_l, T_opt, T_base = 10) {
  df <- df %>%
    mutate(
      # Apply lower and upper thresholds
      Tmin_adj = ifelse(.data[[min_temp_col]] < T_l, NA, .data[[min_temp_col]]),  # Ignore values below T_l
      Tmax_adj = pmin(.data[[max_temp_col]], T_opt),  # Cap Tmax at T_opt
      
      # Calculate GDD only when Tmin is above T_l
      GDD = ifelse(is.na(Tmin_adj), 0, pmax(((Tmax_adj + Tmin_adj) / 2) - T_base, 0))
    ) %>%
    
    group_by(Location, Year) %>%
    mutate(!!paste0(source_name, "_Cumulative_GDD") := cumsum(GDD)) %>%
    ungroup()
  
  return(df)
}

# ✅ Define varieties with heading data  # Seba: moved this here because it's needed for following loop
varieties <- c("HeadM105_DaysToHeading", "HeadM206_DaysToHeading", 
               "HeadM209_DaysToHeading", "HeadM210_DaysToHeading", 
               "HeadM211_DaysToHeading", "HeadM401_DaysToHeading")

# ✅ Step 1: **Loop through different T_l and T_opt threshold values**
T_l_values <- seq(10, 20, by = 1)   # Test lower thresholds
T_opt_values <- seq(25, 40, by = 1)  # Test upper thresholds

gdd_results <- data.frame()

for (T_l in T_l_values) { 
  for (T_opt in T_opt_values) {
    
    print(sprintf("🔍 Testing T_l = %.1f, T_opt = %.1f", T_l, T_opt))
    
    df_temp <- calculate_gdd(df, "LocMinTempC", "LocMaxTempC", "Local", T_l, T_opt)
    df_temp <- calculate_gdd(df_temp, "PRISMMinTempC", "PRISMMaxTempC", "PRISM", T_l, T_opt)
    df_temp <- calculate_gdd(df_temp, "StatMinTempC", "StatMaxTempC", "Stat", T_l, T_opt)
    
    df_final <- df_temp %>%
      select(Location, Year, Date, Days_After_Planting, 
             Local_Cumulative_GDD, PRISM_Cumulative_GDD, Stat_Cumulative_GDD, all_of(varieties)) %>%  # ✅ Include varieties
      mutate(T_l = T_l, T_opt = T_opt)  # ✅ Add threshold columns
    
    gdd_results <- bind_rows(gdd_results, df_final)
  }
}

# write_csv(gdd_results, "Outputs/GDD_VariedThresholds2.csv") # Seba: i deactivated this because the csv is too large and messes the push to GitHub
print("✅ GDD data for varied thresholds saved to 'Outputs/GDD_VariedThresholds2.csv'")

# ✅ Extract GDD values at heading for each variety using varied thresholds
gdd_heading_results <- data.frame()

for (var in varieties) {
  
  # ✅ Skip if column is missing
  if (!(var %in% colnames(gdd_results))) {
    print(sprintf("⚠️ Warning: Column %s not found in gdd_results. Skipping...", var))
    next
  }
  
  df_heading <- gdd_results %>%
    select(Location, Year, Days_After_Planting, 
           Local_Cumulative_GDD, PRISM_Cumulative_GDD, Stat_Cumulative_GDD, T_l, T_opt, all_of(var)) %>%
    rename(DaysToHeading = all_of(var)) %>%
    filter(!is.na(DaysToHeading)) %>%
    filter(Days_After_Planting == DaysToHeading)  # Extract GDD at heading
  
  # ✅ Check if df_heading is empty
  if (nrow(df_heading) == 0) {
    print(sprintf("⚠️ Warning: No matching data for %s in gdd_results.", var))
    next
  }
  
  df_heading <- df_heading %>%
    mutate(Variety = var) %>%
    select(Variety, Location, Year, DaysToHeading, T_l, T_opt, 
           Local_Cumulative_GDD, PRISM_Cumulative_GDD, Stat_Cumulative_GDD)
  
  gdd_heading_results <- bind_rows(gdd_heading_results, df_heading)
  
  # ✅ Save per-variety results (ensuring T_l and T_opt are included)
  write_csv(df_heading, sprintf("Outputs/GDD_At_Heading_%s.csv", var))
}

# ✅ Save full extracted GDD at heading **with T_l and T_opt values**
write_csv(gdd_heading_results, "Outputs/GDD_At_Heading_VariedThresholds2.csv")
print("✅ Extracted GDD values at heading saved to 'Outputs/GDD_At_Heading_VariedThresholds2.csv'")



# ✅ Functions to Compute Error Metrics
compute_rmse <- function(predicted, observed) {
  valid_data <- data.frame(predicted, observed) %>%
    filter(!is.na(predicted) & !is.na(observed))
  
  if (nrow(valid_data) == 0) return(NA)
  
  sqrt(mean((valid_data$predicted - valid_data$observed)^2, na.rm = TRUE))
}

compute_mbe <- function(predicted, observed) {
  valid_data <- data.frame(predicted, observed) %>%
    filter(!is.na(predicted) & !is.na(observed))
  
  if (nrow(valid_data) == 0) return(NA)
  
  mean(valid_data$predicted - valid_data$observed, na.rm = TRUE)
}

compute_mae <- function(predicted, observed) {
  valid_data <- data.frame(predicted, observed) %>%
    filter(!is.na(predicted) & !is.na(observed))
  
  if (nrow(valid_data) == 0) return(NA)
  
  mean(abs(valid_data$predicted - valid_data$observed), na.rm = TRUE)
}

# ✅ Initialize Separate DataFrames for PRISM and Station Comparisons
error_results_prism <- data.frame()
error_results_station <- data.frame()

for (var in varieties) {
  df_variety <- gdd_heading_results %>%
    filter(Variety == var)  # Filter for specific variety
  
  # for (yr in unique(df_variety$Year)) { # Seba: I'm removing this loop so the error functions (e.g. compute_mbe) use all years' GDD as data to calculate
  #   df_year <- df_variety %>%
  #     filter(Year == yr)  # Filter for specific year
    
    for (loc in unique(df_variety$Location)) { # Seba: previous to modification:     for (loc in unique(df_year$Location)) { 
      df_location <- df_variety %>% # Seba: previous to modification: df_location <- df_year 
        filter(Location == loc)  # Filter for specific location
      
      for (tl_val in unique(df_location$T_l)) {  
        for (topt_val in unique(df_location$T_opt)) {
          df_param <- df_location %>%
            filter(T_l == tl_val, T_opt == topt_val)  # Select specific temp params
          
          if (nrow(df_param) == 0) next  # Skip if no matching data
          
          # Extract key values
          DaysToHeading_value <- unique(df_param$DaysToHeading)
          Local_GDD_value <- unique(df_param$Local_Cumulative_GDD)
          PRISM_GDD_value <- unique(df_param$PRISM_Cumulative_GDD)
          Stat_GDD_value <- unique(df_param$Stat_Cumulative_GDD)
          
          # ✅ Compute RMSE, MBE, and MAE for Local vs PRISM
          rmse_local_prism <- compute_rmse(df_param$Local_Cumulative_GDD, df_param$PRISM_Cumulative_GDD)
          mbe_local_prism <- compute_mbe(df_param$Local_Cumulative_GDD, df_param$PRISM_Cumulative_GDD)
          mae_local_prism <- compute_mae(df_param$Local_Cumulative_GDD, df_param$PRISM_Cumulative_GDD)
          
          # ✅ Compute RMSE, MBE, and MAE for Local vs Station
          rmse_local_station <- compute_rmse(df_param$Local_Cumulative_GDD, df_param$Stat_Cumulative_GDD)
          mbe_local_station <- compute_mbe(df_param$Local_Cumulative_GDD, df_param$Stat_Cumulative_GDD)
          mae_local_station <- compute_mae(df_param$Local_Cumulative_GDD, df_param$Stat_Cumulative_GDD)
          
          # ✅ Store results for PRISM comparison
          error_results_prism <- bind_rows(error_results_prism, 
                                           data.frame(Variety = var, Location = loc,  # Seba: prev.: data.frame(Variety = var, Location = loc, Year = yr, 
                                                      # DaysToHeading = DaysToHeading_value, # Seba: removed 
                                                      T_l = tl_val, T_opt = topt_val,
                                                      # Local_Cumulative_GDD = Local_GDD_value, PRISM_Cumulative_GDD = PRISM_GDD_value, # Seba: removed 
                                                      Comparison = "Local vs PRISM", 
                                                      RMSE = as.numeric(rmse_local_prism), 
                                                      MBE = as.numeric(mbe_local_prism), 
                                                      MAE = as.numeric(mae_local_prism))) 
          
          # ✅ Store results for Station comparison
          error_results_station <- bind_rows(error_results_station, 
                                             data.frame(Variety = var, Location = loc, # Seba: prev.: data.frame(Variety = var, Location = loc, Year = yr,
                                                        # DaysToHeading = DaysToHeading_value, # Seba: removed 
                                                        T_l = tl_val, T_opt = topt_val,
                                                        # Local_Cumulative_GDD = Local_GDD_value, Stat_Cumulative_GDD = Stat_GDD_value, # Seba: removed 
                                                        Comparison = "Local vs Station", 
                                                        RMSE = as.numeric(rmse_local_station), 
                                                        MBE = as.numeric(mbe_local_station), 
                                                        MAE = as.numeric(mae_local_station))) 
        }
      }
    }
  }
# } # Seba: removed, as Year loop was removed

# ✅ Save error analysis results separately
# write_csv(error_results_prism, "Outputs/Error_Analysis_Local_vs_PRISM.csv") # Seba: these I used to store calculations before modifications
# write_csv(error_results_station, "Outputs/Error_Analysis_Local_vs_Station.csv")

write_csv(error_results_prism, "Outputs/Error_Analysis_Local_vs_PRISM_2.csv") # Seba: these are calculations after modifications.
write_csv(error_results_station, "Outputs/Error_Analysis_Local_vs_Station_2.csv") # Seba: these are calculations after modifications.


print("✅ Error analysis saved separately for PRISM and Station comparisons.")



# ✅ Load Error Analysis Data
df_prism <- read_csv("Outputs/Error_Analysis_Local_vs_PRISM_2.csv", na = c("", "NA"))
df_station <- read_csv("Outputs/Error_Analysis_Local_vs_Station_2.csv", na = c("", "NA"))

# ✅ Function to Find Best Parameters Based on RMSE Only
find_best_params_rmse <- function(df, comparison_type) {
  df_best <- df %>%
    filter(!is.na(RMSE)) %>%  # Remove NAs to prevent sorting errors
    group_by(Variety, Location) %>% # Seba: previous to modification:     group_by(Variety, Location, Year) %>%
    arrange(RMSE, .by_group = TRUE) %>%  # Sort by RMSE within each group
    slice(1) %>%  # Select the best row
    ungroup() %>%
    select(Variety, Location, T_l, T_opt, RMSE, MBE, MAE) %>% # Seba: previous to modification: select(Variety, Location, Year, T_l, T_opt, RMSE, MBE, MAE)
    mutate(Comparison = comparison_type, Approach = "RMSE Only")
  
  return(df_best)
}

# ✅ Function to Find Best Parameters Based on RMSE + MAE + MBE
find_best_params_balanced <- function(df, comparison_type) {
  df_best <- df %>%
    filter(!is.na(RMSE) & !is.na(MAE) & !is.na(MBE)) %>%  # Remove rows with NAs
    group_by(Variety, Location) %>% # Seba: removed Year as it conflicts with modified nested loops
    
    # Seba: lines added assuming you're aiming at min(MSE + MAE + MBE) as a criteria to find the best parameters
    mutate(sum_RMSE_MAE_MBE = RMSE + MAE + abs(MBE)) %>% 
    arrange(sum_RMSE_MAE_MBE, .by_group = TRUE) %>% 
    
    # arrange(RMSE, MAE, abs(MBE), .by_group = TRUE) %>%  # Sort with RMSE first, then MAE, then absolute MBE - Seba: replaced for previous arrange
    slice(1) %>%  # Select the best row
    ungroup() %>%
    select(Variety, Location, T_l, T_opt, RMSE, MBE, MAE, sum_RMSE_MAE_MBE) %>% # Seba: removed Year as it conflicts with modified nested loops
    mutate(Comparison = comparison_type, Approach = "RMSE + MAE + MBE")
  
  return(df_best)
}

# ✅ Find Best Parameters for Each Approach
best_rmse_prism <- find_best_params_rmse(df_prism, "Local vs PRISM")
best_balanced_prism <- find_best_params_balanced(df_prism, "Local vs PRISM")

best_rmse_station <- find_best_params_rmse(df_station, "Local vs Station")
best_balanced_station <- find_best_params_balanced(df_station, "Local vs Station")

# ✅ Combine Results for Comparison
best_params_comparison <- bind_rows(best_rmse_prism, best_balanced_prism,
                                    best_rmse_station, best_balanced_station)

# ✅ Save Results for Analysis
write_csv(best_params_comparison, "Outputs/Best_Temperature_Parameters_Comparison.csv")

print("✅ Best temperature parameters comparison saved to 'Outputs/Best_Temperature_Parameters_Comparison.csv'")



# ✅ Load Best Parameters from the Comparison File
df_best <- read_csv("Outputs/Best_Temperature_Parameters_Comparison.csv", na = c("", "NA"))

# ✅ Function to Get Best Parameters for Each Variety & Location (RMSE Only)
find_best_params_per_variety_location_rmse <- function(df) {
  df_best <- df %>%
    filter(Approach == "RMSE Only") %>%  # Use only RMSE method
    group_by(Variety, Location) %>%
    arrange(RMSE, .by_group = TRUE) %>%  # Sort within each group
    slice(1) %>%  # Take the best row
    ungroup() %>%
    select(Variety, Location, T_l, T_opt, RMSE, MBE, MAE, Comparison, Approach)
  
  return(df_best)
}

# ✅ Function to Get Best Parameters for Each Variety & Location (Balanced RMSE + MAE + MBE)
find_best_params_per_variety_location_balanced <- function(df) {
  df_best <- df %>%
    filter(Approach == "RMSE + MAE + MBE") %>%  # Use balanced method
    group_by(Variety, Location) %>%
    arrange(RMSE, MAE, abs(MBE), .by_group = TRUE) %>%  # Sort within each group
    slice(1) %>%  # Take the best row
    ungroup() %>%
    select(Variety, Location, T_l, T_opt, RMSE, MBE, MAE, Comparison, Approach)
  
  return(df_best)
}

# ✅ Find Best Parameters for Each Variety & Location
best_rmse_var_loc <- find_best_params_per_variety_location_rmse(df_best)
best_balanced_var_loc <- find_best_params_per_variety_location_balanced(df_best)

# ✅ Combine Results for Comparison
best_params_var_loc_comparison <- bind_rows(best_rmse_var_loc, best_balanced_var_loc)

# ✅ Save Results for Analysis
write_csv(best_params_var_loc_comparison, "Outputs/Best_Temperature_Parameters_Per_Variety_Location.csv")

print("✅ Best temperature parameters per variety & location saved to 'Outputs/Best_Temperature_Parameters_Per_Variety_Location.csv'")



# ✅ Load the best temperature parameters for each location & year
df_best_params <- read_csv("Outputs/Best_Temperature_Parameters_Comparison.csv")

# ✅ Compute the average best parameters for each variety & comparison type
average_best_params <- df_best_params %>%
  group_by(Variety, Comparison) %>%
  summarise(
    Avg_T_l = mean(T_l, na.rm = TRUE),  # Compute average lower threshold
    Avg_T_opt = mean(T_opt, na.rm = TRUE),  # Compute average upper threshold
    Avg_RMSE = mean(RMSE, na.rm = TRUE),  # Compute average RMSE
    Avg_MBE = mean(MBE, na.rm = TRUE),  # Compute average MBE
    Avg_MAE = mean(MAE, na.rm = TRUE),  # Compute average MAE
    SD_RMSE = sd(RMSE, na.rm = TRUE),  # Standard deviation of RMSE
    SD_MBE = sd(MBE, na.rm = TRUE),  # Standard deviation of MBE
    SD_MAE = sd(MAE, na.rm = TRUE)  # Standard deviation of MAE
  ) %>%
  ungroup()

# ✅ Save results to CSV
write_csv(average_best_params, "Outputs/Averaged_Best_Temperature_Parameters.csv")

print("✅ Averaged best temperature parameters saved to 'Outputs/Averaged_Best_Temperature_Parameters.csv'")