
# ✅ Load required packages
library(tidyverse)
library(readr)
library(Metrics)
library(broom)

# ✅ Step 1: Load dataset and exclude unreliable trials
df_Jul25 <- read_csv("Outputs/MasterTemp2024_processed.csv", na = c("", "NA")) %>%
  filter(
    !(Location == "Canal" & Year == 2021),
    !(Location == "Rehmann" & Year == 2023),
    !(Location == "BosworthRue" & Year == 2024),
    !(Location == "DelRio"),
    !(Location == "Wylie" & Year == 2024)
  )

# ✅ Step 2: Define target varieties and heading columns
target_varieties <- c("M105", "M206", "M209", "M210", "M211")
heading_cols <- paste0("Head", target_varieties, "_DaysToHeading")

# ✅ Step 3: Reshape to long format
df_Jul25_long <- df_Jul25 %>%
  select(Location, Year, Date, Plant_Date, Days_After_Planting,
         PRISMMinTempC, PRISMMaxTempC, all_of(heading_cols)) %>%
  pivot_longer(cols = all_of(heading_cols),
               names_to = "Variety",
               values_to = "DaysToHeading") %>%
  mutate(Variety = gsub("^Head(.*)_DaysToHeading$", "\\1", Variety)) %>%
  filter(Variety %in% target_varieties, !is.na(DaysToHeading))

# ✅ Step 4: Stratified split based on DTH (by Location/Year/Variety)
set.seed(123)
split_assignments <- df_Jul25_long %>%
  group_by(Location, Year, Variety) %>%
  summarise(DaysToHeading = first(DaysToHeading), .groups = "drop") %>%
  mutate(row_id = row_number()) %>%
  mutate(split = case_when(
    row_id <= 0.7 * n() ~ "train",
    row_id <= 0.9 * n() ~ "calibration",
    TRUE ~ "test"
  )) %>%
  select(Location, Year, Variety, split)

df_Jul25_long <- df_Jul25_long %>%
  left_join(split_assignments, by = c("Location", "Year", "Variety"))

# ✅ Step 5: GDD Calculation Function
calculate_gdd_at_heading <- function(data, t_lower, t_upper, t_base = 10) {
  data %>%
    mutate(
      GDD = case_when(
        PRISMMaxTempC <= t_lower ~ 0,                                   # No accumulation: both temps too low
        PRISMMinTempC >= t_upper ~ t_upper - t_base,                    # Fully saturated day
        TRUE ~ ((pmin(PRISMMaxTempC, t_upper) + pmax(PRISMMinTempC, t_lower)) / 2 - t_base)
      ),
      GDD = pmax(GDD, 0)
    ) %>%
    group_by(Location, Year, Variety) %>%
    arrange(Date) %>%
    mutate(cum_gdd = cumsum(GDD)) %>%
    ungroup() %>%
    filter(Days_After_Planting == DaysToHeading) %>%
    select(Location, Year, Variety, DaysToHeading, cum_gdd)
}

# ✅ Step 6: Grid search over temperature thresholds
grid <- expand.grid(
  T_lower = seq(10, 18, 0.1),
  T_upper = seq(20, 40, 0.1)
) %>%
  filter(T_lower < T_upper)

#------------------------------------------------------------------------
#### Seba_check 1: checking daily GDD calculation (pre-cumulative) ######
#------------------------------------------------------------------------

# Check selecting just one day from a specific location
# e.g. BosworthRue ; 2022-07-18

Check_GDDLocDate <- df_Jul25_long %>% 
  select(-Variety, -DaysToHeading, -split) %>% 
  filter(
    Location == "BosworthRue",
    Date == "2022-07-18"
  ) 

Check_GDDLocDate_grid <- merge(Check_GDDLocDate[1,] , grid, all = TRUE) # Check_GDDLocDate[1,]  because it contains identical repeated rows

GDD_check <- Check_GDDLocDate_grid %>%
    mutate(
      GDD = case_when(
        PRISMMaxTempC <= T_lower ~ 0,                                   # No accumulation: both temps too low
        PRISMMinTempC >= T_upper ~ T_upper - 10,                        # Fully saturated day
        TRUE ~ ((pmin(PRISMMaxTempC, T_upper) + pmax(PRISMMinTempC, T_lower)) / 2 - 10)), # min and max forces a conservative (short) temp range
      GDD = pmax(GDD, 0))

# Check result: OK
#------------------------------------------------------------------------

# ✅ Step X: Export GDD values at heading for all threshold combos
all_gdd_records <- map_dfr(1:nrow(grid), function(i) {
  row <- grid[i, ]
  gdd_data <- calculate_gdd_at_heading(df_Jul25_long, row$T_lower, row$T_upper)
  
  if (nrow(gdd_data) == 0) return(NULL)
  
  gdd_data %>%
    mutate(T_lower = row$T_lower, T_upper = row$T_upper)
})

# ✅ Save detailed GDD data for visualization
# write_csv(all_gdd_records, "Outputs/GDD_AtHeading_AllThresholds.csv") # Seba: commented out for efficiency

#### Individual Threshold Plots Per Variety ####
# 
# library(tidyverse) # Seba: commented out for efficiency
# library(Metrics)

# gdd_all <- read_csv("Outputs/GDD_AtHeading_AllThresholds.csv") %>% # Seba: commented out for efficiency
#   mutate(
#     threshold_label = paste0("T_low=", T_lower, ", T_up=", T_upper)
#   )

gdd_all <- all_gdd_records %>% # Seba: same output as passing through the csv file
  mutate(
    threshold_label = paste0("T_low=", T_lower, ", T_up=", T_upper)
  )

output_dir <- "Outputs/Plots_Per_Threshold"
dir.create(output_dir, showWarnings = FALSE)

# Prepare combos
unique_combos <- gdd_all %>%
  distinct(Variety, T_lower, T_upper) %>%
  arrange(Variety, T_lower, T_upper)

# Open Pdf device once
pdf("Outputs/GDD_vs_DTH_AllThresholds.pdf", width = 7, height = 5)

# Loop for both PNG and Pdf ####
for (i in seq_len(nrow(unique_combos))) {
  this_row <- unique_combos[i, ]
  variety <- this_row$Variety
  t_low <- this_row$T_lower
  t_up <- this_row$T_upper
  
  df_Jul25_subset <- gdd_all %>%
    filter(Variety == variety, T_lower == t_low, T_upper == t_up)
  
  if (nrow(df_Jul25_subset) == 0 || all(is.na(df_Jul25_subset$cum_gdd))) next
  
  mean_gdd <- mean(df_Jul25_subset$cum_gdd, na.rm = TRUE)
  
  # Compute regression metrics
  lm_fit <- lm(DaysToHeading ~ cum_gdd, data = df_Jul25_subset)
  r2_val <- summary(lm_fit)$r.squared
  rmse_val <- rmse(df_Jul25_subset$DaysToHeading, predict(lm_fit))
  
  # Plot
  p <- ggplot(df_Jul25_subset, aes(x = cum_gdd, y = DaysToHeading)) +
    geom_point(alpha = 0.7, color = "steelblue") +
    geom_smooth(method = "lm", se = FALSE, color = "darkred", size = 1) +
    geom_vline(xintercept = mean_gdd, linetype = "dashed", color = "gray40") +
    annotate("text", x = mean_gdd, y = max(df_Jul25_subset$DaysToHeading, na.rm = TRUE),
             label = paste0("Mean GDD = ", round(mean_gdd, 1)),
             hjust = -0.1, vjust = -1, size = 3.5, color = "gray20") +
    labs(
      title = paste0("Variety: ", variety,
                     " | T_low=", t_low, ", T_up=", t_up,
                     "\nRMSE = ", round(rmse_val, 2), ", R² = ", round(r2_val, 3)),
      x = "Cumulative GDD at Heading",
      y = "Days to Heading"
    ) +
    theme_minimal()
  
  # Save PNG
  ggsave(
    filename = file.path(output_dir, paste0("GDD_vs_DTH_", variety, "_", t_low, "_", t_up, ".png")),
    plot = p,
    width = 7,
    height = 5
  )
  
  # Print to Pdf_Jul25
  print(p)
}

# Close Pdf_Jul25 device
dev.off()


#### Individual Plots Varieties Combined ####

library(tidyverse)
library(Metrics)

# 📂 Load GDD results
gdd_all <- read_csv("Outputs/GDD_AtHeading_AllThresholds.csv")

# 📁 Output directories
output_dir <- "Outputs/Plots_CombinedVarieties_PerThreshold"
dir.create(output_dir, showWarnings = FALSE)

# 📌 Unique temperature threshold combinations
unique_thresholds <- gdd_all %>%
  distinct(T_lower, T_upper) %>%
  arrange(T_lower, T_upper)

# 🎨 Generate one plot per threshold combo (with all varieties)
for (i in seq_len(nrow(unique_thresholds))) {
  row <- unique_thresholds[i, ]
  t_low <- row$T_lower
  t_up <- row$T_upper
  
  df_Jul25_subset <- gdd_all %>%
    filter(T_lower == t_low, T_upper == t_up)
  
  if (nrow(df_Jul25_subset) == 0 || all(is.na(df_Jul25_subset$cum_gdd))) next
  
  # 📊 Model: global fit
  fit <- lm(DaysToHeading ~ cum_gdd, data = df_Jul25_subset)
  r2 <- summary(fit)$r.squared
  rmse_val <- rmse(df_Jul25_subset$DaysToHeading, predict(fit))
  mean_gdd <- mean(df_Jul25_subset$cum_gdd, na.rm = TRUE)
  
  p <- ggplot(df_Jul25_subset, aes(x = cum_gdd, y = DaysToHeading, color = Variety)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
    geom_vline(xintercept = mean_gdd, linetype = "dashed", color = "gray40") +
    annotate("text", x = mean_gdd, y = max(df_Jul25_subset$DaysToHeading, na.rm = TRUE),
             label = paste0("Mean GDD = ", round(mean_gdd, 1)),
             hjust = -0.1, vjust = -1, size = 3.5, color = "gray20") +
    labs(
      title = paste0("GDD vs DTH (All Varieties)\nT_low = ", t_low, ", T_up = ", t_up,
                     " | RMSE = ", round(rmse_val, 2),
                     ", R² = ", round(r2, 3)),
      x = "Cumulative GDD at Heading",
      y = "Days to Heading"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Save PNG
  ggsave(
    filename = file.path(output_dir, paste0("GDD_AllVarieties_T", t_low, "_", t_up, ".png")),
    plot = p, width = 8, height = 6
  )
}

# 📄 Combine all plots into a Pdf
pdf_Jul25("Outputs/GDD_AllVarieties_Thresholds_Combined.pdf", width = 8, height = 6)

for (i in seq_len(nrow(unique_thresholds))) {
  row <- unique_thresholds[i, ]
  t_low <- row$T_lower
  t_up <- row$T_upper
  
  df_Jul25_subset <- gdd_all %>%
    filter(T_lower == t_low, T_upper == t_up)
  
  if (nrow(df_Jul25_subset) == 0 || all(is.na(df_Jul25_subset$cum_gdd))) next
  
  fit <- lm(DaysToHeading ~ cum_gdd, data = df_Jul25_subset)
  r2 <- summary(fit)$r.squared
  rmse_val <- rmse(df_Jul25_subset$DaysToHeading, predict(fit))
  mean_gdd <- mean(df_Jul25_subset$cum_gdd, na.rm = TRUE)
  
  p <- ggplot(df_Jul25_subset, aes(x = cum_gdd, y = DaysToHeading, color = Variety)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black", size = 1) +
    geom_vline(xintercept = mean_gdd, linetype = "dashed", color = "gray40") +
    annotate("text", x = mean_gdd, y = max(df_Jul25_subset$DaysToHeading, na.rm = TRUE),
             label = paste0("Mean GDD = ", round(mean_gdd, 1)),
             hjust = -0.1, vjust = -1, size = 3.5, color = "gray20") +
    labs(
      title = paste0("GDD vs DTH (All Varieties)\nT_low = ", t_low, ", T_up = ", t_up,
                     " | RMSE = ", round(rmse_val, 2),
                     ", R² = ", round(r2, 3)),
      x = "Cumulative GDD at Heading",
      y = "Days to Heading"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
}

dev.off()




# Seba: Metrics ####

# library(tidyverse) # Seba: commented out, already in script preamble
# library(Metrics)

# gdd_all <- read_csv("Outputs/GDD_AtHeading_AllThresholds.csv")  # Seba: commented out for efficiency. "all_gdd_records" is the same, "gdd_all" replaced hereon

# Calculate metrics 

## Seba: a) per variety ####

metrics_variety <- all_gdd_records %>%
  filter(!is.na(cum_gdd), !is.na(DaysToHeading)) %>%
  group_by(Variety, T_lower, T_upper) %>%
  summarise(
    RMSE = rmse(DaysToHeading, predict(lm(DaysToHeading ~ cum_gdd))),
    R2 = summary(lm(DaysToHeading ~ cum_gdd))$r.squared,
    .groups = "drop"
  )

# Best by RMSE
best_rmse <- metrics_variety %>%
  group_by(Variety) %>%
  slice_min(RMSE, n = 1) %>%
  ungroup()

# Best by R²
best_r2 <- metrics_variety %>%
  group_by(Variety) %>%
  slice_max(R2, n = 1) %>%
  ungroup()

# Combined rank (RMSE low + R2 high)
best_combo <- metrics_variety %>%
  group_by(Variety) %>%
  mutate(
    rank_rmse = rank(RMSE),
    rank_r2 = rank(desc(R2)),
    combo_score = rank_rmse + rank_r2
  ) %>%
  slice_min(combo_score, n = 1) %>%
  ungroup()

## Seba: b) Combined across all varieties ####

metrics_all_combined <- all_gdd_records %>%
  filter(!is.na(cum_gdd), !is.na(DaysToHeading)) %>%
  group_by(T_lower, T_upper) %>%
  summarise(
    RMSE = rmse(DaysToHeading, predict(lm(DaysToHeading ~ cum_gdd))),
    R2 = summary(lm(DaysToHeading ~ cum_gdd))$r.squared,
    .groups = "drop"
  )

best_all_rmse <- slice_min(metrics_all_combined, RMSE, n = 1)
best_all_r2   <- slice_max(metrics_all_combined, R2, n = 1)
best_all_combo <- metrics_all_combined %>%
  mutate(
    rank_rmse = rank(RMSE),
    rank_r2 = rank(desc(R2)),
    combo_score = rank_rmse + rank_r2
  ) %>%
  slice_min(combo_score, n = 1)

# Export
# write_csv(best_rmse, "Outputs/BestThresholds_PerVariety_RMSE.csv") # Seba: commented out for efficiency
# write_csv(best_r2, "Outputs/BestThresholds_PerVariety_R2.csv")
# write_csv(best_combo, "Outputs/BestThresholds_PerVariety_Combined.csv")
# 
# write_csv(best_all_rmse, "Outputs/BestThresholds_AllVarieties_RMSE.csv")
# write_csv(best_all_r2, "Outputs/BestThresholds_AllVarieties_R2.csv")
# write_csv(best_all_combo, "Outputs/BestThresholds_AllVarieties_Combined.csv")



# library(tidyverse) # Seba: commented out, already in script preamble
# library(Metrics)
library(progress)

# gdd_all <- read_csv("Outputs/GDD_AtHeading_AllThresholds.csv")  # Seba: commented out for efficiency. "all_gdd_records" is the same, "gdd_all" replaced hereon

unique_combos <- all_gdd_records %>%
  distinct(Variety, T_lower, T_upper)

# Create progress bar
pb <- progress_bar$new(
  format = "  Calculating [:bar] :percent ETA: :eta",
  total = nrow(unique_combos),
  clear = FALSE,
  width = 60
)

# Function to compute RMSE and R²
calculate_metrics <- function(df) {
  if (nrow(df) < 2 || anyNA(df$cum_gdd) || anyNA(df$DaysToHeading)) {
    return(tibble(RMSE = NA, R2 = NA))
  }
  model <- lm(DaysToHeading ~ cum_gdd, data = df)
  pred <- predict(model, newdata = df)
  tibble(RMSE = rmse(df$DaysToHeading, pred),
         R2 = summary(model)$r.squared)
}

# Loop with progress bar
metrics_results <- map_dfr(seq_len(nrow(unique_combos)), function(i) {
  pb$tick()
  row <- unique_combos[i, ]
  subset <- all_gdd_records %>%
    filter(Variety == row$Variety,
           T_lower == row$T_lower,
           T_upper == row$T_upper)
  metrics <- calculate_metrics(subset)
  bind_cols(row, metrics)
})

# Save output
write_csv(metrics_results, "Outputs/GDD_Metrics_FullGrid.csv")


# library(tidyverse) # Seba: commented out, already in script preamble
# library(Metrics)

# 📂 Load already-calculated GDD values
# all_gdd_records <- read_csv("Outputs/GDD_AtHeading_AllThresholds.csv") # Seba: commented out for efficiency. all_gdd_records is the same, replaced hereon

# ✅ Calculate RMSE and R² per Variety and Threshold Combo
metrics_variety <- all_gdd_records %>%
  filter(!is.na(cum_gdd), !is.na(DaysToHeading)) %>%
  group_by(Variety, T_lower, T_upper) %>%
  summarise(
    RMSE = tryCatch(rmse(DaysToHeading, predict(lm(DaysToHeading ~ cum_gdd))),
                    error = function(e) NA),
    R2 = tryCatch(summary(lm(DaysToHeading ~ cum_gdd))$r.squared,
                  error = function(e) NA),
    .groups = "drop"
  )

write_csv(metrics_variety, "Outputs/Metrics_PerVariety_AllThresholds.csv")

# ✅ Calculate RMSE and R² for all varieties combined (per threshold)
metrics_combined <- all_gdd_records %>%
  filter(!is.na(cum_gdd), !is.na(DaysToHeading)) %>%
  group_by(T_lower, T_upper) %>%
  summarise(
    RMSE = tryCatch(rmse(DaysToHeading, predict(lm(DaysToHeading ~ cum_gdd))),
                    error = function(e) NA),
    R2 = tryCatch(summary(lm(DaysToHeading ~ cum_gdd))$r.squared,
                  error = function(e) NA),
    .groups = "drop"
  )

write_csv(metrics_combined, "Outputs/Metrics_AllVarieties_Combined.csv")

library(writexl)

# 📄 Write per-variety metrics
write_xlsx(metrics_variety, "Outputs/Metrics_PerVariety_AllThresholds.xlsx")

# 📄 Write combined-variety metrics
write_xlsx(metrics_combined, "Outputs/Metrics_AllVarieties_Combined.xlsx")





# Lewis analysis 18/07/2025 ####

# New GDD formula, without conditions:
calculate_gdd_at_heading <- function(data, t_lower, t_upper, t_base = 10) {
  data %>%
    filter(PRISMMinTempC <= PRISMMaxTempC) %>%
    mutate(
      capped_tmin = pmax(PRISMMinTempC, t_lower),
      capped_tmax = pmin(PRISMMaxTempC, t_upper),
      t_avg = (capped_tmin + capped_tmax) / 2,
      GDD = pmax(t_avg - t_base, 0)
    ) %>%
    group_by(Location, Year, Variety) %>%
    arrange(Date) %>%
    mutate(cum_gdd = cumsum(GDD)) %>%
    ungroup() %>%
    filter(Days_After_Planting == DaysToHeading) %>%
    mutate(T_lower = t_lower, T_upper = t_upper) %>%  # ✅ Add here
    select(Location, Year, Variety, DaysToHeading, cum_gdd, T_lower, T_upper)  # ✅ Select known columns
}

# 1) Mean GDD for each Variety × (T_lower, T_upper) combo
mean_by_threshold <- all_gdd_records %>%
  group_by(Variety, T_lower, T_upper) %>%
  filter(!is.na(cum_gdd)) %>% # Added by Seba
  dplyr::summarise(mean_gdd = mean(cum_gdd), .groups = "drop")

# 2) Mean GDD for each Variety across all temperature combos
mean_overall <- all_gdd_records %>%
  group_by(Variety) %>%
  filter(!is.na(cum_gdd)) %>% # Added by Seba
  dplyr::summarise(mean_gdd = mean(cum_gdd), .groups = "drop")

# 3) Find threshold combo closest to the overall mean, by Variety
best_closest_to_mean <- mean_by_threshold %>%
  # join on overall mean
  left_join(
    mean_overall %>% rename(mean_overall_gdd = mean_gdd),
    by = "Variety"
  ) %>% 
  # compute absolute difference
  mutate(diff = abs(mean_gdd - mean_overall_gdd)) %>%
  # pick the single best per variety
  group_by(Variety) %>%
  slice_min(diff, with_ties = FALSE) %>%
  ungroup()
