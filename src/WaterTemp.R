# water_minmax_daily.R
# Purpose: From multiple WaterTempF readings per day/site, produce daily min/max in F and C.
# Output columns: Location, Date, Year, County, WaterMinTempF, WaterMaxTempF, WaterMinTempC, WaterMaxTempC

# ===== 0) Setup =====
# Adjust if needed
setwd("/Users/lewisdaniel/R Folder/LinquistLab/WaterTemp")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(janitor)
  library(stringr)
})

if (!dir.exists("output")) dir.create("output")

# ===== 1) Load =====
dat <- readr::read_csv("MasterWaterRange.csv", show_col_types = FALSE, na = c("", "NA", "NaN"))

# Drop empty/unneeded columns like "Unnamed: n"
dat <- dat %>% remove_empty("cols") %>% select(-matches("^Unnamed"))

# ===== 2) Parse and normalize =====
dat <- dat %>%
  mutate(
    # Parse Date assuming m/d/yy or m/d/yyyy
    Date = suppressWarnings(lubridate::mdy(Date)),
    # Trim/standardize text
    County = str_to_title(str_squish(County)),
    Location = str_to_title(str_squish(Location)),
    # If Year missing, derive from Date
    Year = ifelse(is.na(Year) & !is.na(Date), year(Date), Year),
    # Ensure numeric temperature
    WaterTempF = suppressWarnings(as.numeric(WaterTempF))
  )

# ===== 3) Daily min/max by Location-Date-Year-County =====
minmax <- dat %>%
  filter(!is.na(WaterTempF), !is.na(Date)) %>%
  group_by(Location, Date, Year, County) %>%
  summarize(
    WaterMinTempF = min(WaterTempF, na.rm = TRUE),
    WaterMaxTempF = max(WaterTempF, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    WaterMinTempC = (WaterMinTempF - 32) * 5/9,
    WaterMaxTempC = (WaterMaxTempF - 32) * 5/9
  ) %>%
  # Reorder to requested column order
  select(Location, Date, Year, County, WaterMinTempF, WaterMaxTempF, WaterMinTempC, WaterMaxTempC) %>%
  arrange(Location, County, Date)

# ===== 4) Write output =====
out_file <- "Outputs/water_daily_minmax.csv"
readr::write_csv(minmax, out_file)

cat("Wrote: ", out_file, "\n",
    "Rows: ", nrow(minmax), "\n", sep = "")







# merge_water_minmax_into_master.R
# Purpose: Add daily min/max water temps (from MasterWaterRange.csv) to MasterTemp2024_repaired.csv
# Output: output/MasterTemp2024_withWaterDailyMinMax.csv
# Adds columns: WaterMinTempF, WaterMaxTempF, WaterMinTempC, WaterMaxTempC

# ===== 0) Setup =====
setwd("/Users/lewisdaniel/R Folder/LinquistLab/WaterTemp")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(janitor)
})

if (!dir.exists("output")) dir.create("output")

# Helper to build robust join keys (trim + lowercase)
norm_key <- function(x) x %>% stringr::str_squish() %>% stringr::str_to_lower()

# ===== 1) Load files =====
master <- readr::read_csv("MasterTemp2024_repaired.csv", show_col_types = FALSE, na = c("", "NA", "NaN"))
water  <- readr::read_csv("MasterWaterRange.csv",        show_col_types = FALSE, na = c("", "NA", "NaN"))

# Drop fully empty / "Unnamed" columns in water
water <- water %>% janitor::remove_empty("cols") %>% select(-matches("^Unnamed"))

# ===== 2) Parse types =====
# Master: Date/Plant_Date appear as ISO yyyy-mm-dd already; ensure Date class
master <- master %>%
  mutate(
    Date = suppressWarnings(lubridate::ymd(Date)),
    Plant_Date = suppressWarnings(lubridate::ymd(Plant_Date))
  )

# Water: Date likely m/d/yy -> parse with mdy; ensure numeric temps
water <- water %>%
  mutate(
    Date = suppressWarnings(lubridate::mdy(Date)),
    WaterTempF = suppressWarnings(as.numeric(WaterTempF))
  )

# ===== 3) Build normalized join keys on BOTH datasets =====
master_keys <- master %>%
  transmute(
    .rowid = row_number(),
    Location_key = norm_key(Location),
    County_key   = norm_key(County),
    Date_key     = as.Date(Date),
    Year         = Year
  )

water_prepped <- water %>%
  # keep only rows with valid date and temp
  filter(!is.na(Date), !is.na(WaterTempF)) %>%
  mutate(
    Location_key = norm_key(Location),
    County_key   = norm_key(County),
    Date_key     = as.Date(Date),
    # if Year missing in water, derive from date
    Year = ifelse(is.na(Year) & !is.na(Date_key), year(Date_key), Year)
  )

# ===== 4) Compute daily min/max from water by join keys =====
water_minmax <- water_prepped %>%
  group_by(Location_key, County_key, Date_key, Year) %>%
  summarize(
    WaterMinTempF = min(WaterTempF, na.rm = TRUE),
    WaterMaxTempF = max(WaterTempF, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    WaterMinTempC = (WaterMinTempF - 32) * 5/9,
    WaterMaxTempC = (WaterMaxTempF - 32) * 5/9
  )

# ===== 5) Join back to master using normalized keys, preserve original columns =====
master_with_keys <- master %>%
  mutate(
    Location_key = norm_key(Location),
    County_key   = norm_key(County),
    Date_key     = as.Date(Date)
  ) %>%
  left_join(
    water_minmax,
    by = c("Location_key", "County_key", "Date_key", "Year")
  ) %>%
  select(-Location_key, -County_key, -Date_key)

# ===== 6) Write output =====
out_file <- "Outputs/MasterTemp2024_withWaterDailyMinMax.csv"
readr::write_csv(master_with_keys, out_file)

# ===== 7) Console summary =====
n_master <- nrow(master)
n_joined <- sum(!is.na(master_with_keys$WaterMinTempF) | !is.na(master_with_keys$WaterMaxTempF))
message("Wrote: ", out_file)
message("Rows in master: ", n_master)
message("Rows with matched water min/max: ", n_joined)
message("Added columns: WaterMinTempF, WaterMaxTempF, WaterMinTempC, WaterMaxTempC")




# plot_water_vs_DAP_outputs.R
# Saves ALL plots to 'Outputs/' as BOTH .png and .pdf

# ===== 0) Setup =====
setwd("/Users/lewisdaniel/R Folder/LinquistLab/WaterTemp")

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(lubridate)
  library(stringr); library(janitor); library(ggplot2)
})

# Create Outputs folder
if (!dir.exists("Outputs")) dir.create("Outputs")

norm_key <- function(x) x %>% str_squish() %>% str_to_lower()

# ===== 1) Load master (prefer premerged if available) =====
premerged_path <- "output/MasterTemp2024_withWaterDailyMinMax.csv"  # if you used earlier merge script
use_premerged <- file.exists(premerged_path)

master <- if (use_premerged) {
  message("Reading premerged: ", premerged_path)
  read_csv(premerged_path, show_col_types = FALSE)
} else {
  read_csv("MasterTemp2024_repaired.csv", show_col_types = FALSE)
}

master <- master %>%
  mutate(
    Date = suppressWarnings(ymd(Date)),
    Plant_Date = suppressWarnings(ymd(Plant_Date))
  )

# ===== 2) Ensure WaterMinTempC/WaterMaxTempC exist; derive if not =====
need_water <- !all(c("WaterMinTempC","WaterMaxTempC") %in% names(master))

if (need_water) {
  message("Water columns not found; deriving from MasterWaterRange.csv ...")
  water <- read_csv("MasterWaterRange.csv", show_col_types = FALSE, na = c("", "NA", "NaN")) %>%
    remove_empty("cols") %>% select(-matches("^Unnamed")) %>%
    mutate(
      Date = suppressWarnings(mdy(Date)),
      WaterTempF = suppressWarnings(as.numeric(WaterTempF))
    ) %>% filter(!is.na(Date), !is.na(WaterTempF)) %>%
    mutate(
      Location_key = norm_key(Location),
      County_key   = norm_key(County),
      Date_key     = as.Date(Date),
      Year = ifelse(is.na(Year) & !is.na(Date_key), year(Date_key), Year)
    )
  
  water_minmax <- water %>%
    group_by(Location_key, County_key, Date_key, Year) %>%
    summarize(
      WaterMinTempF = min(WaterTempF, na.rm = TRUE),
      WaterMaxTempF = max(WaterTempF, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      WaterMinTempC = (WaterMinTempF - 32) * 5/9,
      WaterMaxTempC = (WaterMaxTempF - 32) * 5/9
    ) %>%
    select(Location_key, County_key, Date_key, Year, WaterMinTempC, WaterMaxTempC)
  
  master <- master %>%
    mutate(
      Location_key = norm_key(Location),
      County_key   = norm_key(County),
      Date_key     = as.Date(Date)
    ) %>%
    left_join(water_minmax, by = c("Location_key","County_key","Date_key","Year")) %>%
    select(-Location_key, -County_key, -Date_key)
}

# ===== 3) Apply filters =====
df <- master %>%
  filter((is.na(MINDif_OUT) | MINDif_OUT != 1),
         (is.na(MAXDif_OUT) | MAXDif_OUT != 1)) %>%
  filter(Location != "DelRio") %>%
  filter(!(Location == "ErdmanGallagher" & Year == 2017)) %>%
  filter(!is.na(WaterMinTempC), !is.na(WaterMaxTempC), !is.na(Days_After_Planting)) %>%
  mutate(WaterAvgTempC = (WaterMinTempC + WaterMaxTempC)/2)

# ===== 4) Aggregations vs DAP =====
# A) County × Year × DAP means
county_year_dap <- df %>%
  group_by(Year, County, Days_After_Planting) %>%
  summarize(
    MinC = mean(WaterMinTempC, na.rm = TRUE),
    AvgC = mean(WaterAvgTempC, na.rm = TRUE),
    MaxC = mean(WaterMaxTempC, na.rm = TRUE),
    .groups = "drop"
  )

# B) Year × DAP means (all counties)
year_dap <- df %>%
  group_by(Year, Days_After_Planting) %>%
  summarize(
    MinC = mean(WaterMinTempC, na.rm = TRUE),
    AvgC = mean(WaterAvgTempC, na.rm = TRUE),
    MaxC = mean(WaterMaxTempC, na.rm = TRUE),
    .groups = "drop"
  )

# C) Overall DAP means (all years & counties)
overall_dap <- df %>%
  group_by(Days_After_Planting) %>%
  summarize(
    MinC = mean(WaterMinTempC, na.rm = TRUE),
    AvgC = mean(WaterAvgTempC, na.rm = TRUE),
    MaxC = mean(WaterMaxTempC, na.rm = TRUE),
    .groups = "drop"
  )

# ===== 5) Save helper: PNG + PDF in Outputs/ =====
save_both <- function(plot, base_filename, width = 10, height = 6) {
  png_path <- file.path("Outputs", paste0(base_filename, ".png"))
  pdf_path <- file.path("Outputs", paste0(base_filename, ".pdf"))
  ggsave(png_path, plot = plot, width = width, height = height, dpi = 150)
  ggsave(pdf_path, plot = plot, width = width, height = height)
  message("Saved: ", basename(png_path), " and ", basename(pdf_path))
}

# ===== 6) (1) Per-year, faceted by County =====
make_year_facet_plot <- function(dat, metric, title_suffix) {
  dat %>%
    select(Year, County, Days_After_Planting, Value = all_of(metric)) %>%
    ggplot(aes(x = Days_After_Planting, y = Value)) +
    geom_line() +
    facet_wrap(~ County, scales = "free_y") +
    labs(x = "Days After Planting (DAP)", y = paste0(title_suffix, " (°C)")) +
    theme_minimal()
}

years <- sort(unique(county_year_dap$Year))
for (yy in years) {
  dat_y <- county_year_dap %>% filter(Year == yy)
  
  p_min <- make_year_facet_plot(dat_y, "MinC", "Water Minimum") +
    ggtitle(paste0("Water Minimum vs DAP — Year ", yy))
  save_both(p_min, paste0("water_DAP_byCounty_Min_year_", yy))
  
  p_avg <- make_year_facet_plot(dat_y, "AvgC", "Water Average") +
    ggtitle(paste0("Water Average vs DAP — Year ", yy))
  save_both(p_avg, paste0("water_DAP_byCounty_Avg_year_", yy))
  
  p_max <- make_year_facet_plot(dat_y, "MaxC", "Water Maximum") +
    ggtitle(paste0("Water Maximum vs DAP — Year ", yy))
  save_both(p_max, paste0("water_DAP_byCounty_Max_year_", yy))
}

# ===== 7) (2) All counties combined — by Year =====
p_year_min <- ggplot(year_dap, aes(x = Days_After_Planting, y = MinC, group = Year, color = factor(Year))) +
  geom_line() +
  labs(title = "Water Minimum (°C) vs DAP — Year-wise Mean (All Counties)",
       x = "Days After Planting (DAP)", y = "Minimum (°C)", color = "Year") +
  theme_minimal()
save_both(p_year_min, "water_DAP_Min_byYear_allCounties")

p_year_avg <- ggplot(year_dap, aes(x = Days_After_Planting, y = AvgC, group = Year, color = factor(Year))) +
  geom_line() +
  labs(title = "Water Average (°C) vs DAP — Year-wise Mean (All Counties)",
       x = "Days After Planting (DAP)", y = "Average (°C)", color = "Year") +
  theme_minimal()
save_both(p_year_avg, "water_DAP_Avg_byYear_allCounties")

p_year_max <- ggplot(year_dap, aes(x = Days_After_Planting, y = MaxC, group = Year, color = factor(Year))) +
  geom_line() +
  labs(title = "Water Maximum (°C) vs DAP — Year-wise Mean (All Counties)",
       x = "Days After Planting (DAP)", y = "Maximum (°C)", color = "Year") +
  theme_minimal()
save_both(p_year_max, "water_DAP_Max_byYear_allCounties")

# ===== 8) (3) Overall — all years & counties mean vs DAP =====
overall_long <- overall_dap %>%
  pivot_longer(cols = c(MinC, AvgC, MaxC), names_to = "Metric", values_to = "ValueC") %>%
  mutate(Metric = factor(Metric, levels = c("MinC","AvgC","MaxC"),
                         labels = c("Minimum (°C)","Average (°C)","Maximum (°C)")))

p_overall <- ggplot(overall_long, aes(x = Days_After_Planting, y = ValueC)) +
  geom_line() +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "Water Temperature vs DAP — Grand Means (All Years, All Counties)",
       x = "Days After Planting (DAP)", y = "Temperature (°C)") +
  theme_minimal()
save_both(p_overall, "water_DAP_grand_means")

message("All plots saved to the 'Outputs/' folder.")








# plot_water_DAP_grand_means_warmup.R
# Grand-mean WATER temperature (°C) vs DAP
# X-axis from 0..150, but lines begin after an auto-detected warm-up cutoff (or fixed if you set it).
# Saves PNG + PDF to Outputs/.

# ===== 0) Setup =====
setwd("/Users/lewisdaniel/R Folder/LinquistLab/WaterTemp")

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(lubridate)
  library(stringr); library(janitor); library(ggplot2); library(zoo)
})

if (!dir.exists("Outputs")) dir.create("Outputs")

norm_key <- function(x) x %>% str_squish() %>% str_to_lower()

# ===== User options =====
# If you want a fixed warm-up cutoff (e.g., always 5), set FIXED_START to a number; otherwise use NA to auto-detect.
FIXED_START <- NA_real_  # e.g., set to 5 to force start at DAP=5
EARLY_WINDOW <- 20       # search for the early low spike within DAP <= 20
ROLL_K <- 3              # smoothing window for early AvgC curve
END_DAP <- 150

# ===== 1) Load master (prefer premerged if available) =====
premerged_path <- "output/MasterTemp2024_withWaterDailyMinMax.csv"
use_premerged <- file.exists(premerged_path)

master <- if (use_premerged) {
  message("Reading premerged: ", premerged_path)
  read_csv(premerged_path, show_col_types = FALSE)
} else {
  read_csv("MasterTemp2024_repaired.csv", show_col_types = FALSE)
}

master <- master %>%
  mutate(
    Date = suppressWarnings(ymd(Date)),
    Plant_Date = suppressWarnings(ymd(Plant_Date))
  )

# ===== 2) Ensure WaterMinTempC/WaterMaxTempC exist; derive from raw water if needed =====
need_water <- !all(c("WaterMinTempC","WaterMaxTempC") %in% names(master))
if (need_water) {
  message("Water columns not found; deriving from MasterWaterRange.csv ...")
  water <- read_csv("MasterWaterRange.csv", show_col_types = FALSE, na = c("", "NA", "NaN")) %>%
    remove_empty("cols") %>% select(-matches("^Unnamed")) %>%
    mutate(
      Date = suppressWarnings(mdy(Date)),
      WaterTempF = suppressWarnings(as.numeric(WaterTempF))
    ) %>% filter(!is.na(Date), !is.na(WaterTempF)) %>%
    mutate(
      Location_key = norm_key(Location),
      County_key   = norm_key(County),
      Date_key     = as.Date(Date),
      Year = ifelse(is.na(Year) & !is.na(Date_key), year(Date_key), Year)
    )
  
  water_minmax <- water %>%
    group_by(Location_key, County_key, Date_key, Year) %>%
    summarize(
      WaterMinTempF = min(WaterTempF, na.rm = TRUE),
      WaterMaxTempF = max(WaterTempF, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      WaterMinTempC = (WaterMinTempF - 32) * 5/9,
      WaterMaxTempC = (WaterMaxTempF - 32) * 5/9
    ) %>%
    select(Location_key, County_key, Date_key, Year, WaterMinTempC, WaterMaxTempC)
  
  master <- master %>%
    mutate(
      Location_key = norm_key(Location),
      County_key   = norm_key(County),
      Date_key     = as.Date(Date)
    ) %>%
    left_join(water_minmax, by = c("Location_key","County_key","Date_key","Year")) %>%
    select(-Location_key, -County_key, -Date_key)
}

# ===== 3) Apply requested filters =====
df <- master %>%
  filter((is.na(MINDif_OUT) | MINDif_OUT != 1),
         (is.na(MAXDif_OUT) | MAXDif_OUT != 1)) %>%
  filter(Location != "DelRio") %>%
  filter(!(Location == "ErdmanGallagher" & Year == 2017)) %>%
  filter(!is.na(WaterMinTempC), !is.na(WaterMaxTempC), !is.na(Days_After_Planting)) %>%
  mutate(WaterAvgTempC = (WaterMinTempC + WaterMaxTempC)/2)

# ===== 4) Grand-mean vs DAP =====
overall_dap <- df %>%
  group_by(Days_After_Planting) %>%
  summarize(
    MinC = mean(WaterMinTempC, na.rm = TRUE),
    AvgC = mean(WaterAvgTempC, na.rm = TRUE),
    MaxC = mean(WaterMaxTempC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Days_After_Planting)

# ===== 5) Determine start_dap (warm-up cutoff) =====
if (is.na(FIXED_START)) {
  early <- overall_dap %>% filter(Days_After_Planting <= EARLY_WINDOW)
  if (nrow(early) > 0) {
    early <- early %>%
      mutate(AvgC_smooth = zoo::rollmean(AvgC, k = ROLL_K, fill = "extend", align = "center"))
    min_idx <- which.min(early$AvgC_smooth)
    auto_start <- early$Days_After_Planting[min_idx]
  } else {
    auto_start <- 5
  }
  start_dap <- max(5, auto_start)
} else {
  start_dap <- FIXED_START
}

# Clip for line drawing, but keep x-axis from 0..END_DAP
clipped <- overall_dap %>%
  filter(Days_After_Planting >= start_dap, Days_After_Planting <= END_DAP)

# ===== 6) Plot & Save (PNG + PDF) =====
save_both <- function(plot, base_filename, width = 10, height = 6) {
  png_path <- file.path("Outputs", paste0(base_filename, ".png"))
  pdf_path <- file.path("Outputs", paste0(base_filename, ".pdf"))
  ggsave(png_path, plot = plot, width = width, height = height, dpi = 150)
  ggsave(pdf_path, plot = plot, width = width, height = height)
  message("Saved: ", basename(png_path), " and ", basename(pdf_path))
}

# Long format for faceting (lines only after start_dap)
clipped_long <- clipped %>%
  tidyr::pivot_longer(cols = c(MinC, AvgC, MaxC), names_to = "Metric", values_to = "ValueC") %>%
  dplyr::mutate(Metric = factor(Metric, levels = c("MinC","AvgC","MaxC"),
                                labels = c("Minimum","Average","Maximum")))

# Plot: axis shown from 0..END_DAP, with no shading and no vertical line
p <- ggplot(clipped_long, aes(x = Days_After_Planting, y = ValueC)) +
  geom_line() +
  facet_wrap(~ Metric, scales = "free_y") +
  coord_cartesian(xlim = c(0, END_DAP)) +
  labs(
    title = paste0("Water Temperature vs DAP — Grand Means (lines start at DAP ", start_dap, ", axis begins at 0)"),
    x = "Days After Planting (DAP)",
    y = "Temperature (°C)"
  ) +
  theme_minimal()

save_both(p, paste0("water_DAP_grand_means_warmup_start_", start_dap, "_axis_0_to_", END_DAP))




# ===== 6) Plot & Save (PNG + PDF) — NO TITLE =====
save_both <- function(plot, base_filename, width = 10, height = 6) {
  png_path <- file.path("Outputs", paste0(base_filename, ".png"))
  pdf_path <- file.path("Outputs", paste0(base_filename, ".pdf"))
  ggsave(png_path, plot = plot, width = width, height = height, dpi = 150)
  ggsave(pdf_path, plot = plot, width = width, height = height)
  message("Saved: ", basename(png_path), " and ", basename(pdf_path))
}

# Long format for faceting (lines only after start_dap)
clipped_long <- clipped %>%
  tidyr::pivot_longer(cols = c(MinC, AvgC, MaxC), names_to = "Metric", values_to = "ValueC") %>%
  dplyr::mutate(Metric = factor(Metric, levels = c("MinC","AvgC","MaxC"),
                                labels = c("Minimum","Average","Maximum")))

# No title, axis still 0..END_DAP, lines begin at start_dap
p <- ggplot(clipped_long, aes(x = Days_After_Planting, y = ValueC)) +
  geom_line() +
  facet_wrap(~ Metric, scales = "free_y") +
  coord_cartesian(xlim = c(0, END_DAP)) +
  labs(x = "Days After Planting (DAP)", y = "Temperature (°C)") +
  theme_minimal()

save_both(p, paste0("water_DAP_grand_means_warmup_start_", start_dap, "_axis_0_to_", END_DAP, "_notitle"))





# ===== 6) Plot & Save (PNG + PDF) — STACKED (Min, Max, Avg) & NO TITLE =====
save_both <- function(plot, base_filename, width = 10, height = 8) {
  png_path <- file.path("Outputs", paste0(base_filename, ".png"))
  pdf_path <- file.path("Outputs", paste0(base_filename, ".pdf"))
  ggsave(png_path, plot = plot, width = width, height = height, dpi = 150)
  ggsave(pdf_path, plot = plot, width = width, height = height)
  message("Saved: ", basename(png_path), " and ", basename(pdf_path))
}

# Long format for faceting (lines only after start_dap)
clipped_long <- clipped %>%
  tidyr::pivot_longer(cols = c(MinC, AvgC, MaxC), names_to = "Metric", values_to = "ValueC") %>%
  # Set labels & enforced top-to-bottom order: Minimum, Maximum, Average
  dplyr::mutate(Metric = factor(Metric,
                                levels = c("MinC", "MaxC", "AvgC"),
                                labels = c("Minimum (°C)", "Maximum (°C)", "Average (°C)")))

# Stacked facets (one column), axis 0..END_DAP, no shading, no vertical line, no title
p <- ggplot(clipped_long, aes(x = Days_After_Planting, y = ValueC)) +
  geom_line() +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  coord_cartesian(xlim = c(0, END_DAP)) +
  labs(x = "Days After Planting (DAP)", y = "Temperature (°C)") +
  theme_minimal()

save_both(p, paste0("water_DAP_grand_means_warmup_start_", start_dap, "_axis_0_to_", END_DAP, "_stacked_notitle"))




# === ADD-ON: SD & SE ribbon plots (append at end of your current script) ===

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })

# Ensure Outputs folder and a save helper exist
if (!dir.exists("Outputs")) dir.create("Outputs")
if (!exists("save_both")) {
  save_both <- function(plot, base_filename, width = 10, height = 8) {
    png_path <- file.path("Outputs", paste0(base_filename, ".png"))
    pdf_path <- file.path("Outputs", paste0(base_filename, ".pdf"))
    ggsave(png_path, plot = plot, width = width, height = height, dpi = 150)
    ggsave(pdf_path, plot = plot, width = width, height = height)
    message("Saved: ", basename(png_path), " and ", basename(pdf_path))
  }
}

# 1) Compute stats vs DAP (across all years & counties) from your existing df
stats_dap <- df %>%
  group_by(Days_After_Planting) %>%
  summarize(
    Min_mean = mean(WaterMinTempC, na.rm = TRUE),
    Min_sd   = sd(WaterMinTempC,  na.rm = TRUE),
    Min_n    = sum(!is.na(WaterMinTempC)),
    Avg_mean = mean((WaterMinTempC + WaterMaxTempC)/2, na.rm = TRUE),
    Avg_sd   = sd((WaterMinTempC + WaterMaxTempC)/2,  na.rm = TRUE),
    Avg_n    = sum(!is.na(WaterMinTempC) & !is.na(WaterMaxTempC)),
    Max_mean = mean(WaterMaxTempC, na.rm = TRUE),
    Max_sd   = sd(WaterMaxTempC,  na.rm = TRUE),
    Max_n    = sum(!is.na(WaterMaxTempC)),
    .groups = "drop"
  ) %>%
  mutate(
    Min_se = Min_sd / sqrt(pmax(Min_n, 1)),
    Avg_se = Avg_sd / sqrt(pmax(Avg_n, 1)),
    Max_se = Max_sd / sqrt(pmax(Max_n, 1))
  ) %>%
  arrange(Days_After_Planting)

# 2) Clip to start at your warm-up cutoff and end at END_DAP
stats_clip <- stats_dap %>%
  filter(Days_After_Planting >= start_dap, Days_After_Planting <= END_DAP)

# 3) Build long frames in stacked order: Minimum (top), Maximum (middle), Average (bottom)
sd_long <- bind_rows(
  stats_clip %>% transmute(DAP = Days_After_Planting, Metric = "Minimum",
                           y = Min_mean, ymin = Min_mean - Min_sd, ymax = Min_mean + Min_sd),
  stats_clip %>% transmute(DAP = Days_After_Planting, Metric = "Maximum",
                           y = Max_mean, ymin = Max_mean - Max_sd, ymax = Max_mean + Max_sd),
  stats_clip %>% transmute(DAP = Days_After_Planting, Metric = "Average",
                           y = Avg_mean, ymin = Avg_mean - Avg_sd, ymax = Avg_mean + Avg_sd)
) %>% mutate(Metric = factor(Metric, levels = c("Minimum","Maximum","Average")))

se_long <- bind_rows(
  stats_clip %>% transmute(DAP = Days_After_Planting, Metric = "Minimum",
                           y = Min_mean, ymin = Min_mean - Min_se, ymax = Min_mean + Min_se),
  stats_clip %>% transmute(DAP = Days_After_Planting, Metric = "Maximum",
                           y = Max_mean, ymin = Max_mean - Max_se, ymax = Max_mean + Max_se),
  stats_clip %>% transmute(DAP = Days_After_Planting, Metric = "Average",
                           y = Avg_mean, ymin = Avg_mean - Avg_se, ymax = Avg_mean + Avg_se)
) %>% mutate(Metric = factor(Metric, levels = c("Minimum","Maximum","Average")))

# 4) Plot SD ribbon (mean ± SD) — no title, stacked, axis 0..END_DAP
p_sd <- ggplot(sd_long, aes(x = DAP, y = y)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.15) +
  geom_line() +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  coord_cartesian(xlim = c(0, END_DAP)) +
  labs(x = "Days After Planting (DAP)", y = "Temperature (°C)") +
  theme_minimal()
save_both(p_sd, paste0("water_DAP_grand_means_sdRibbon_start_", start_dap, "_axis0to", END_DAP))

# 5) Plot SE ribbon (mean ± SE) — no title, stacked, axis 0..END_DAP
#    (swap to 95% CI by replacing ymin/ymax in se_long with mean ± 1.96*SE if desired)
p_se <- ggplot(se_long, aes(x = DAP, y = y)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.15) +
  geom_line() +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  coord_cartesian(xlim = c(0, END_DAP)) +
  labs(x = "Days After Planting (DAP)", y = "Temperature (°C)") +
  theme_minimal()
save_both(p_se, paste0("water_DAP_grand_means_seRibbon_start_", start_dap, "_axis0to", END_DAP))




# === ADD-ON: FULL SEASON (INCLUDE EARLY DAYS) — SD & SE ribbons ===
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })
if (!dir.exists("Outputs")) dir.create("Outputs")
if (!exists("save_both")) {
  save_both <- function(plot, base_filename, width = 10, height = 8) {
    png_path <- file.path("Outputs", paste0(base_filename, ".png"))
    pdf_path <- file.path("Outputs", paste0(base_filename, ".pdf"))
    ggsave(png_path, plot = plot, width = width, height = height, dpi = 150)
    ggsave(pdf_path, plot = plot, width = width, height = height)
    message("Saved: ", basename(png_path), " and ", basename(pdf_path))
  }
}

# 1) Stats vs DAP across all years & counties (NO clipping; includes first 5 days)
stats_full <- df %>%
  group_by(Days_After_Planting) %>%
  summarize(
    Min_mean = mean(WaterMinTempC, na.rm = TRUE),
    Min_sd   = sd(WaterMinTempC,  na.rm = TRUE),
    Min_n    = sum(!is.na(WaterMinTempC)),
    Avg_mean = mean((WaterMinTempC + WaterMaxTempC)/2, na.rm = TRUE),
    Avg_sd   = sd((WaterMinTempC + WaterMaxTempC)/2,  na.rm = TRUE),
    Avg_n    = sum(!is.na(WaterMinTempC) & !is.na(WaterMaxTempC)),
    Max_mean = mean(WaterMaxTempC, na.rm = TRUE),
    Max_sd   = sd(WaterMaxTempC,  na.rm = TRUE),
    Max_n    = sum(!is.na(WaterMaxTempC)),
    .groups = "drop"
  ) %>%
  mutate(
    Min_se = Min_sd / sqrt(pmax(Min_n, 1)),
    Avg_se = Avg_sd / sqrt(pmax(Avg_n, 1)),
    Max_se = Max_sd / sqrt(pmax(Max_n, 1))
  ) %>%
  arrange(Days_After_Planting) %>%
  filter(Days_After_Planting >= 0, Days_After_Planting <= END_DAP)

# 2) Build long frames (stack order: Minimum, Maximum, Average)
sd_full <- bind_rows(
  stats_full %>% transmute(DAP = Days_After_Planting, Metric = "Minimum",
                           y = Min_mean, ymin = Min_mean - Min_sd, ymax = Min_mean + Min_sd),
  stats_full %>% transmute(DAP = Days_After_Planting, Metric = "Maximum",
                           y = Max_mean, ymin = Max_mean - Max_sd, ymax = Max_mean + Max_sd),
  stats_full %>% transmute(DAP = Days_After_Planting, Metric = "Average",
                           y = Avg_mean, ymin = Avg_mean - Avg_sd, ymax = Avg_mean + Avg_sd)
) %>% mutate(Metric = factor(Metric, levels = c("Minimum","Maximum","Average")))

se_full <- bind_rows(
  stats_full %>% transmute(DAP = Days_After_Planting, Metric = "Minimum",
                           y = Min_mean, ymin = Min_mean - Min_se, ymax = Min_mean + Min_se),
  stats_full %>% transmute(DAP = Days_After_Planting, Metric = "Maximum",
                           y = Max_mean, ymin = Max_mean - Max_se, ymax = Max_mean + Max_se),
  stats_full %>% transmute(DAP = Days_After_Planting, Metric = "Average",
                           y = Avg_mean, ymin = Avg_mean - Avg_se, ymax = Avg_mean + Avg_se)
) %>% mutate(Metric = factor(Metric, levels = c("Minimum","Maximum","Average")))

# 3) Plot & save (NO title; axis 0..END_DAP; includes first 5 days)
p_sd <- ggplot(sd_full, aes(x = DAP, y = y)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.15) +
  geom_line() +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  coord_cartesian(xlim = c(0, END_DAP)) +
  labs(x = "Days After Planting (DAP)", y = "Temperature (°C)") +
  theme_minimal()
save_both(p_sd, paste0("water_DAP_grand_means_SD_full_includeEarly_axis0to", END_DAP))

p_se <- ggplot(se_full, aes(x = DAP, y = y)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.15) +
  geom_line() +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  coord_cartesian(xlim = c(0, END_DAP)) +
  labs(x = "Days After Planting (DAP)", y = "Temperature (°C)") +
  theme_minimal()
save_both(p_se, paste0("water_DAP_grand_means_SE_full_includeEarly_axis0to", END_DAP))




# ===== BETWEEN-YEAR SD & SE RIBBONS (match PRISM–Local method) =====
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })

START_DAP <- 0
END_DAP   <- 150
USE_CI95  <- FALSE  # set TRUE for 95% CI = ±1.96*SE

# 1) Per-Year × DAP means (treat each year as one replicate)
by_year <- df %>%
  filter(Days_After_Planting >= START_DAP, Days_After_Planting <= END_DAP) %>%
  group_by(Year, Days_After_Planting) %>%
  summarise(
    Min_mean = mean(WaterMinTempC, na.rm = TRUE),
    Avg_mean = mean((WaterMinTempC + WaterMaxTempC)/2, na.rm = TRUE),
    Max_mean = mean(WaterMaxTempC, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Across-year summaries at each DAP (SE = SD/sqrt(n_years))
summary_years <- by_year %>%
  group_by(Days_After_Planting) %>%
  summarise(
    Min_mu = mean(Min_mean, na.rm = TRUE),
    Min_sd = sd(  Min_mean, na.rm = TRUE),
    Min_n  = dplyr::n(),
    Min_se = Min_sd / sqrt(pmax(Min_n, 1)),
    Avg_mu = mean(Avg_mean, na.rm = TRUE),
    Avg_sd = sd(  Avg_mean, na.rm = TRUE),
    Avg_n  = dplyr::n(),
    Avg_se = Avg_sd / sqrt(pmax(Avg_n, 1)),
    Max_mu = mean(Max_mean, na.rm = TRUE),
    Max_sd = sd(  Max_mean, na.rm = TRUE),
    Max_n  = dplyr::n(),
    Max_se = Max_sd / sqrt(pmax(Max_n, 1)),
    .groups = "drop"
  ) %>%
  arrange(Days_After_Planting) %>%
  rename(DAP = Days_After_Planting)

# 3) Long frames for plotting (stack order: Minimum, Maximum, Average)
sd_long <- bind_rows(
  summary_years %>% transmute(DAP, Metric = "Minimum", y = Min_mu,
                              ymin = Min_mu - Min_sd, ymax = Min_mu + Min_sd),
  summary_years %>% transmute(DAP, Metric = "Maximum", y = Max_mu,
                              ymin = Max_mu - Max_sd, ymax = Max_mu + Max_sd),
  summary_years %>% transmute(DAP, Metric = "Average", y = Avg_mu,
                              ymin = Avg_mu - Avg_sd, ymax = Avg_mu + Avg_sd)
) %>% mutate(Metric = factor(Metric, levels = c("Minimum","Maximum","Average")))

se_long <- bind_rows(
  summary_years %>% transmute(DAP, Metric = "Minimum", y = Min_mu,
                              half = if (USE_CI95) 1.96*Min_se else Min_se,
                              ymin = Min_mu - half, ymax = Min_mu + half),
  summary_years %>% transmute(DAP, Metric = "Maximum", y = Max_mu,
                              half = if (USE_CI95) 1.96*Max_se else Max_se,
                              ymin = Max_mu - half, ymax = Max_mu + half),
  summary_years %>% transmute(DAP, Metric = "Average", y = Avg_mu,
                              half = if (USE_CI95) 1.96*Avg_se else Avg_se,
                              ymin = Avg_mu - half, ymax = Avg_mu + half)
) %>%
  select(-half) %>%
  mutate(Metric = factor(Metric, levels = c("Minimum","Maximum","Average")))

# 4) Plotters (stacked facets, no forced y-limits → no ribbon clipping)
p_water_sd_year <- ggplot(sd_long, aes(x = DAP, y = y)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.15) +
  geom_line() +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  coord_cartesian(xlim = c(START_DAP, END_DAP)) +
  labs(x = "Days After Planting (DAP)", y = "Temperature (°C)") +
  theme_minimal()

p_water_se_year <- ggplot(se_long, aes(x = DAP, y = y)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.15) +
  geom_line() +
  facet_wrap(~ Metric, ncol = 1, scales = "free_y") +
  coord_cartesian(xlim = c(START_DAP, END_DAP)) +
  labs(x = "Days After Planting (DAP)", y = "Temperature (°C)") +
  theme_minimal()

# 5) Save (reuses save_both() already defined above)
save_both(p_water_sd_year,
          paste0("water_DAP_betweenYear_SD_axis0to", END_DAP), width = 10, height = 8)
save_both(p_water_se_year,
          paste0("water_DAP_betweenYear_", if (USE_CI95) "CI95" else "SE", "_axis0to", END_DAP),
          width = 10, height = 8)

message("✓ Water SD/SE ribbons now use between-year variability to match PRISM–Local.")
