# Your original approach makes perfect sense statistically but as you rightly point out, minimizing RMSE favors narrow T-ranges, which artificially improves fit by reducing variation in GDD (even though biologically it may be incorrect). You're looking instead for a physiologically meaningful range — i.e., the true effective temperature interval where GDD contributes to rice development.
# 
# 🌱 Revised Goal (Biological Approach)
# You're now aiming to identify, for each rice variety, the pair of T_lower and T_upper thresholds where:
#   
#   GDD accumulation meaningfully contributes to development up to heading;
# 
# Beyond those thresholds, increasing or decreasing them further does not improve model performance — i.e., performance plateaus.
# 
# This leads to the idea of change-point or stability detection.
# 
# ✅ Step-by-Step Strategy
# We’ll look for points of stabilization in model performance (RMSE or R²) as a function of T_lower or T_upper.
# 
# 🧭 Main Idea:
#   For each variety:
#   
#   Fix T_upper, and vary T_lower: plot performance metric vs T_lower.
# 
# Fix T_lower, and vary T_upper: plot performance metric vs T_upper.
# 
# Then:
#   
#   Identify where the change in performance metric plateaus, i.e., slope approaches zero.
# 
# Use that point as the threshold beyond which temperatures no longer contribute to GDD accumulation.

# Approach 1: Detection via Slope Change: ####

library(dplyr)
library(Metrics)
library(tidyr)
library(purrr)
library(dplyr)
library(segmented)

# Step 1.1: Calculate RMSE per threshold combo per Variety
metrics_variety2 <- all_gdd_records %>%
        filter(!is.na(cum_gdd), !is.na(DaysToHeading)) %>%
        group_by(Variety, T_lower, T_upper) %>%
        summarise(
          RMSE = rmse(DaysToHeading, predict(lm(DaysToHeading ~ cum_gdd))),
          .groups = "drop"
        )

# Step 1.2: For each variety... ####
## 2.1. ...analyze how RMSE changes across T_lower (for fixed T_upper) ####

RMSE_changes_T_lower <- ggplot(
        metrics_variety %>% filter(T_upper == 35), 
        aes(x = T_lower, y = RMSE)
      ) +
        geom_line() +
        facet_wrap(~Variety) +
        labs(title = "RMSE vs T_lower for fixed T_upper = 35")

print(RMSE_changes_T_lower)

## 2.2. ...analyze how RMSE changes across T_upper(for fixed T_upper) ####

RMSE_changes_T_upper <- ggplot(
        metrics_variety %>% filter(T_lower == 15), 
        aes(x = T_upper, y = RMSE)
      ) +
        geom_line() +
        facet_wrap(~Variety) +
        labs(title = "RMSE vs T_upper for fixed T_lower = 15")

print(RMSE_changes_T_upper)

# Approach 2: Automate Plateau Detection ####

#--- PARAMETERS ---#
fixed_T_upper <- 30
fixed_T_lower <- 20
slope_thresh <- 0.01  # RMSE change below this = plateau

#--- FUNCTION TO DETECT PLATEAU POINT ---#
detect_plateau <- function(df, temp_col = "T_lower", metric_col = "RMSE", slope_thresh = 0.01) {
        df <- df %>% arrange(.data[[temp_col]])
        slopes <- abs(diff(df[[metric_col]]))
        idx <- which(slopes < slope_thresh)[1]
        if (!is.na(idx)) {
          return(df[[temp_col]][idx])
        } else {
          return(NA_real_)
        }
}

#--- 1. Calculate RMSE across all thresholds ---#
metrics_all_2ndapp <- all_gdd_records %>%
        filter(!is.na(cum_gdd), !is.na(DaysToHeading)) %>%
        group_by(Variety, T_lower, T_upper) %>%
        summarise(
          RMSE = rmse(DaysToHeading, predict(lm(DaysToHeading ~ cum_gdd))),
          .groups = "drop"
  )

#--- 2. Extract T_lower (RMSE vs T_lower, fixed T_upper) ---#
T_lower_df <- metrics_all_2ndapp %>%
        filter(T_upper == fixed_T_upper) %>%
        group_by(Variety) %>%
        nest() %>%
        mutate(
          T_lower_plateau = map_dbl(data, ~ detect_plateau(.x, "T_lower", "RMSE", slope_thresh))
        ) %>%
        dplyr::select(Variety, T_lower_plateau)

#--- 3. Extract T_upper (RMSE vs T_upper, fixed T_lower) ---#
T_upper_df <- metrics_all_2ndapp %>%
        filter(T_lower == fixed_T_lower) %>%
        group_by(Variety) %>%
        nest() %>%
        mutate(
          T_upper_plateau = map_dbl(data, ~ detect_plateau(.x, "T_upper", "RMSE", slope_thresh))
        ) %>%
        dplyr::select(Variety, T_upper_plateau)

#--- 4. Combine Results ---#
best_thresholds <- full_join(T_lower_df, T_upper_df, by = "Variety")

#--- 5. Output ---#
print(best_thresholds)

# Approach 3: Function to get breakpoint (e.g. for T_lower or T_upper) ####

get_piecewise_breakpoint <- function(data, x_var, y_var, guess = NULL) {
  data <- data %>% arrange(.data[[x_var]])
  lm_fit <- lm(reformulate(x_var, y_var), data = data)
  
  # Provide initial guess if not given
  if (is.null(guess)) {
    guess <- median(data[[x_var]], na.rm = TRUE)
  }
  
  seg_fit <- tryCatch({
    segmented(lm_fit, seg.Z = as.formula(paste0("~", x_var)), psi = list(. = guess))
  }, error = function(e) return(NULL))
  
  if (!is.null(seg_fit)) {
    bp <- summary(seg_fit)$psi[1, "Est."]
    return(bp)
  } else {
    return(NA_real_)
  }
}
# Apply to RMSE-vs-T_lower (fixed T_upper)

fixed_T_upper <- 35

T_lower_breakpoints <- metrics_all_2ndapp %>%
  filter(T_upper == fixed_T_upper) %>%
  group_by(Variety) %>%
  nest() %>%
  mutate(
    T_lower_opt = map_dbl(data, ~ get_piecewise_breakpoint(.x, "T_lower", "RMSE", guess = 117))
  ) %>%
  dplyr::select(Variety, T_lower_opt)

# Apply to RMSE-vs-T_upper (fixed T_lower)

fixed_T_lower <- 15

T_upper_breakpoints <- metrics_all_3rdapp %>%
  filter(T_lower == fixed_T_lower) %>%
  group_by(Variety) %>%
  nest() %>%
  mutate(
    T_upper_opt = map_dbl(data, ~ get_piecewise_breakpoint(.x, "T_upper", "RMSE"))
  ) %>%
  dplyr::select(Variety, T_upper_opt)

# Combine the Results

best_thresholds_piecewise <- full_join(T_lower_breakpoints, T_upper_breakpoints, by = "Variety")
print(best_thresholds_piecewise)

# 4. My approach ####

Seba_T_upp <- 35
Seba_T_low <- 15

Seba_df_low <- all_gdd_records %>% 
  filter(Variety == "M105",
         T_upper == Seba_T_upp) %>% 
  mutate(DTH_GDD = DaysToHeading / cum_gdd)

ggplot(Seba_df_low, aes(x = T_lower, y = DTH_GDD, color = Location, fill = Year)) + 
  geom_point()


Seba_df_upp <- all_gdd_records %>% 
  filter(Variety == "M105",
         T_lower == Seba_T_low) %>% 
  mutate(DTH_GDD = DaysToHeading / cum_gdd)

ggplot(Seba_df_upp, aes(x = T_upper, y = DTH_GDD, color = Location, fill = Year)) + 
  geom_point()

