# ===============================================================
# FINAL PROJECT SCRIPT (single Outputs/ folder)
#   • Self-contained: computes all Tl/Tu from MasterTemp2024_repaired.csv
#   • Compares PRISM vs Local temps for rice heading date models
#   • Writes everything to Outputs/ with FINAL_ prefixes
#   • Rounded thresholds are FIXED: Local(11,34), PRISM(12,33)
# ===============================================================

# ---- Working directory ----
setwd("/Users/lewisdaniel/R Folder/LinquistLab/PRISMLocalGDD")

suppressPackageStartupMessages({
  library(tidyverse); library(glue); library(readr); library(purrr)
})

# ---- Output folder (single) ----
OUT_DIR <- "Outputs"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 0) Load master + basic filters ----
master_path <- "MasterTemp2024_repaired.csv"
stopifnot(file.exists(master_path))

raw <- read_csv(master_path, col_types = cols(Date = col_date())) %>%
  filter(!(Location == "Canal"       & Year == 2021),
         !(Location == "Rehmann"     & Year == 2023),
         !(Location == "BosworthRue" & Year == 2024),
         !(Location == "DelRio"),
         !(Location == "Wylie"       & Year == 2024)) %>%
  mutate(across(c(PRISMMinTempC, PRISMMaxTempC,
                  LocMinTempC,  LocMaxTempC), as.numeric))

# ---- 0b) Local temps with PRISM fallback (effective Tmin/Tmax) ----
df_local_base <- raw %>%
  mutate(
    Tmin_useC = if_else(MINDif_OUT == 1 | is.na(LocMinTempC), PRISMMinTempC, LocMinTempC),
    Tmax_useC = if_else(MAXDif_OUT == 1 | is.na(LocMaxTempC), PRISMMaxTempC, LocMaxTempC)
  )

# ---- 0c) Long frames (Local+fallback and PRISM) ----
target_varieties <- c("M105","M206","M209","M210","M211")
heading_cols     <- paste0("Head", target_varieties, "_DaysToHeading")

df_long_local <- df_local_base %>%
  pivot_longer(all_of(heading_cols), names_to="VarietyKey", values_to="DaysToHeading",
               values_drop_na = TRUE) %>%
  mutate(Variety = sub("^Head(.*)_DaysToHeading$", "\\1", VarietyKey)) %>%
  select(-VarietyKey) %>%
  filter(Variety %in% target_varieties) %>%
  select(Location, Year, Date, Plant_Date, Days_After_Planting,
         Variety, DaysToHeading, Tmin_useC, Tmax_useC)

df_long_prism <- raw %>%
  pivot_longer(all_of(heading_cols), names_to="VarietyKey", values_to="DaysToHeading",
               values_drop_na = TRUE) %>%
  mutate(Variety = sub("^Head(.*)_DaysToHeading$", "\\1", VarietyKey)) %>%
  select(-VarietyKey) %>%
  filter(Variety %in% target_varieties) %>%
  select(Location, Year, Date, Plant_Date, Days_After_Planting,
         Variety, DaysToHeading, PRISMMinTempC, PRISMMaxTempC)

# ===============================================================
# A) FIGURES — PRISM minus Local temperature differences by DAP
#     • One PDF per metric (Min, Max, Avg) + a 3-panel summary
#     • Style: SE ribbons and min–max bands, zero reference line
#     • Uses raw Local sensors vs PRISM (no fallback)
# ===============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
  library(patchwork)   # for the 3-panel composite
  library(readr)
})

# ---- CONFIG ---------------------------------------------------------------
UNIT <- "C"  # change to "F" if you want Fahrenheit figures

# ---- helpers --------------------------------------------------------------
c_to_f <- function(x) x*9/5 + 32
lab_unit <- function(unit) if (toupper(unit) == "F") "°F" else "°C"

# When converting DIFFERENCES, scale by 9/5 (since Δ°F = 1.8 * Δ°C)
scale_diff_for_unit <- function(x_c, unit) if (toupper(unit) == "F") x_c*9/5 else x_c

# ---- load & prep: use the already-filtered `raw` --------------------------
df <- raw

need <- c("Year","Days_After_Planting","LocMinTempC","LocMaxTempC",
          "PRISMMinTempC","PRISMMaxTempC","MINDif_OUT","MAXDif_OUT")
stopifnot(all(need %in% names(df)))

df <- df %>%
  transmute(
    Year,
    DAP   = Days_After_Planting,
    PrismMinC = PRISMMinTempC,
    PrismMaxC = PRISMMaxTempC,
    # Knock out local temps only where their OWN flag says they're bad
    LocalMinC = ifelse(MINDif_OUT == 1, NA_real_, LocMinTempC),
    LocalMaxC = ifelse(MAXDif_OUT == 1, NA_real_, LocMaxTempC),
    # Averages (°C)
    PrismAvgC = (PRISMMinTempC + PRISMMaxTempC) / 2,
    LocalAvgC = ifelse(MINDif_OUT == 1 | MAXDif_OUT == 1,
                       NA_real_, (LocMinTempC + LocMaxTempC) / 2)
  ) %>%
  mutate(
    # Differences (°C). NAs propagate only for the affected metric(s)
    dMin_C = PrismMinC - LocalMinC,  # NA when MINDif_OUT == 1
    dMax_C = PrismMaxC - LocalMaxC,  # NA when MAXDif_OUT == 1
    dAvg_C = PrismAvgC - LocalAvgC   # NA when either flag == 1
  ) %>%
  filter(DAP >= 0, DAP <= 160) %>%
  select(Year, DAP, dMin_C, dMax_C, dAvg_C)

# Build a tidy long frame of differences
d_long <- df %>%
  pivot_longer(cols = c(dMin_C, dMax_C, dAvg_C),
               names_to = "Metric", values_to = "Diff_C") %>%
  mutate(
    Metric = recode(Metric,
                    dMin_C = "Minimum",
                    dMax_C = "Maximum",
                    dAvg_C = "Average"),
    Diff   = scale_diff_for_unit(Diff_C, UNIT)  # convert only the difference
  )

# ---- average across seasons + SE ribbons ---------------------------------
# 1) First average within each Year×DAP (if multiple rows per day),
# 2) then average across Years; SE across Years for the ribbon.
by_year <- d_long %>%
  group_by(Metric, Year, DAP) %>%
  summarise(Diff_mean_year = mean(Diff, na.rm = TRUE), .groups = "drop")

summary_across_years <- by_year %>%
  group_by(Metric, DAP) %>%
  summarise(
    mean_diff = mean(Diff_mean_year, na.rm = TRUE),
    sd_diff   = sd(Diff_mean_year,   na.rm = TRUE),
    n_years   = dplyr::n(),
    se_diff   = sd_diff / sqrt(n_years),
    .groups   = "drop"
  )

# ---- plotting function for single-panel figs ------------------------------
plot_metric <- function(metric_name) {
  dat <- summary_across_years %>% filter(Metric == metric_name)
  n_seasons <- max(dat$n_years, na.rm = TRUE)
  
  ggplot(dat, aes(x = DAP, y = mean_diff)) +
    geom_ribbon(aes(ymin = mean_diff - se_diff, ymax = mean_diff + se_diff),
                alpha = 0.18) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(
      limits = c(-4, 4),
      breaks = seq(-4, 4, by = 1)
    ) +
    labs(
      title = glue("PRISM - Local {metric_name} Temperature"),
      subtitle = glue("Mean across seasons ± 1 SE (n = {n_seasons} seasons)"),
      x = "Days After Planting (DAP)",
      y = glue("Difference ({lab_unit(UNIT)})")
    ) +
    theme_classic(base_size = 13) +
    theme(
      plot.title   = element_text(face = "bold"),
      plot.subtitle= element_text(size = 10)
    )
}

p_min <- plot_metric("Minimum")
p_max <- plot_metric("Maximum")
p_avg <- plot_metric("Average")

# ---- save individual PDFs -------------------------------------------------
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Min_{UNIT}.pdf")), p_min, width = 6.5, height = 4.25)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Max_{UNIT}.pdf")), p_max, width = 6.5, height = 4.25)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Avg_{UNIT}.pdf")), p_avg, width = 6.5, height = 4.25)

# --- 3-panel composite (Y label on middle, X label on bottom) --------------

# Top panel: no axis labels
p_min_combo <- p_min +
  labs(title = NULL, subtitle = NULL, x = NULL, y = NULL) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Middle panel: Y label only
p_max_combo <- p_max +
  labs(
    title = NULL, subtitle = NULL,
    x = NULL,
    y = glue("Difference ({lab_unit(UNIT)})")
  ) +
  theme(
    axis.title.x = element_blank()
  )

# Bottom panel: X label only
p_avg_combo <- p_avg +
  labs(
    title = NULL, subtitle = NULL,
    x = "Days After Planting (DAP)",
    y = NULL
  ) +
  theme(
    axis.title.y = element_blank()
  )

p_combo <- (p_min_combo / p_max_combo / p_avg_combo) +
  plot_annotation(
    title = "PRISM - Local Min, Max, and Average Temperature Differences",
    theme = theme(
      plot.title = element_text(hjust = 0.5)  # centered, not bold
    )
  )

ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_3panel_{UNIT}.pdf")),
       p_combo, width = 7.5, height = 10.5)

ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_3panel_{UNIT}.png")),
       p_combo, width = 7.5, height = 10.5, dpi = 300)


message("✓ Wrote: ",
        file.path(OUT_DIR, glue("PRISMminusLocal_Min_{UNIT}.pdf")), ", ",
        file.path(OUT_DIR, glue("PRISMminusLocal_Max_{UNIT}.pdf")), ", ",
        file.path(OUT_DIR, glue("PRISMminusLocal_Avg_{UNIT}.pdf")), ", ",
        file.path(OUT_DIR, glue("PRISMminusLocal_3panel_{UNIT}.pdf")))




# ======================= ADD-ON: Min–Max ribbon figures =======================
# Place this AFTER the block where `by_year` is created.

# 1) Summarize across seasons to get mean curve + min/max band
summary_range <- by_year %>%
  dplyr::group_by(Metric, DAP) %>%
  dplyr::summarise(
    mean_diff = mean(Diff_mean_year, na.rm = TRUE),
    min_diff  = min (Diff_mean_year, na.rm = TRUE),
    max_diff  = max (Diff_mean_year, na.rm = TRUE),
    n_years   = dplyr::n(),
    .groups   = "drop"
  )

# 2) Plotter for min–max ribbon
plot_metric_range <- function(metric_name) {
  dat <- summary_range %>% dplyr::filter(Metric == metric_name)
  ggplot(dat, aes(x = DAP, y = mean_diff)) +
    geom_ribbon(aes(ymin = min_diff, ymax = max_diff), alpha = 0.18) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = glue("PRISM − Local {metric_name} Temperature"),
      subtitle = glue("Mean curve with min–max band (n = {max(dat$n_years, na.rm = TRUE)} seasons)"),
      x = "Days After Planting (DAP)",
      y = glue("Difference ({lab_unit(UNIT)})")
    ) +
    theme_classic(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10)
    )
}

# 3) Build the three range plots
p_min_rng <- plot_metric_range("Minimum")
p_max_rng <- plot_metric_range("Maximum")
p_avg_rng <- plot_metric_range("Average")

# 4) Save (new filenames; originals untouched)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Min_{UNIT}_Range.pdf")), p_min_rng, width = 6.5, height = 4.25)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Max_{UNIT}_Range.pdf")), p_max_rng, width = 6.5, height = 4.25)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Avg_{UNIT}_Range.pdf")), p_avg_rng, width = 6.5, height = 4.25)

# 5) Three-panel composite for the range version
#    (panel titles + Y on middle, X on bottom + unified y-scale)

title_style <- theme(
  plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 4))
)

# helper: common y-scale
rng_yscale <- scale_y_continuous(limits = c(-10, 10),
                                 breaks = seq(-10, 10, by = 2.5),
                                 expand = expansion(mult = c(0, 0)))

# Top: "Minimum", no axis labels
p_min_rng_combo <- p_min_rng +
  rng_yscale +
  labs(title = "Minimum", subtitle = NULL, x = NULL, y = NULL) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) + title_style

# Middle: "Maximum", Y label only
p_max_rng_combo <- p_max_rng +
  rng_yscale +
  labs(
    title = "Maximum", subtitle = NULL,
    x = NULL,
    y = glue("Difference ({lab_unit(UNIT)})")
  ) +
  theme(
    axis.title.x = element_blank()
  ) + title_style

# Bottom: "Average", X label only
p_avg_rng_combo <- p_avg_rng +
  rng_yscale +
  labs(
    title = "Average", subtitle = NULL,
    x = "Days After Planting (DAP)",
    y = NULL
  ) +
  theme(
    axis.title.y = element_blank()
  ) + title_style

# Compose without a global title
p_combo_range <- (p_min_rng_combo / p_max_rng_combo / p_avg_rng_combo)

ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_3panel_{UNIT}_Range.pdf")),
       p_combo_range, width = 7.5, height = 10.5)





# ===============================================================
# B) Cumulative PRISM - Local temperature differences by DAP
#     • Cumulative sum of mean daily differences over the season
#     • Separate curves for Min, Max, and Average
# ===============================================================

# --- small helper: centered rolling mean smoother -------------------------
roll_mean <- function(x, k = 5) {
  if (length(x) < k) return(x)
  sm <- stats::filter(x, rep(1/k, k), sides = 2)
  as.numeric(sm)
}

# Build cumulative sums of mean differences (per Metric) + smoothing
summary_cum <- summary_across_years %>%
  group_by(Metric) %>%
  arrange(DAP, .by_group = TRUE) %>%
  mutate(
    cum_diff_raw = cumsum(mean_diff),
    cum_diff_sm  = roll_mean(cum_diff_raw, k = 5),
    # use smoothed where available, otherwise fall back to raw
    cum_diff     = ifelse(is.na(cum_diff_sm), cum_diff_raw, cum_diff_sm)
  ) %>%
  ungroup()

# Helper: single-panel cumulative plot
plot_cum_metric <- function(metric_name) {
  dat <- summary_cum %>% filter(Metric == metric_name)
  
  ggplot(dat, aes(x = DAP, y = cum_diff)) +
    geom_line(linewidth = 0.8, color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_y_continuous(
      limits = c(-250, 250),
      breaks = seq(-250, 250, by = 100)
    ) +
    labs(
      title = glue("Cumulative PRISM - Local {metric_name} Temperature Difference"),
      x = "Days After Planting (DAP)",
      y = glue("Cumulative difference ({lab_unit(UNIT)}·days)")
    ) +
    theme_classic(base_size = 13) +
    theme(
      plot.title  = element_text(face = "bold"),
    )
}

p_cum_min <- plot_cum_metric("Minimum")
p_cum_max <- plot_cum_metric("Maximum")
p_cum_avg <- plot_cum_metric("Average")

# Save individual cumulative PDFs
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Cumulative_Min_{UNIT}.pdf")),
       p_cum_min, width = 6.5, height = 4.25)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Cumulative_Max_{UNIT}.pdf")),
       p_cum_max, width = 6.5, height = 4.25)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Cumulative_Avg_{UNIT}.pdf")),
       p_cum_avg, width = 6.5, height = 4.25)

# --- 3-panel cumulative composite (Y on middle, X on bottom) --------------

# Top: no labels
p_cum_min_combo <- p_cum_min +
  labs(title = NULL, x = NULL, y = NULL) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Middle: Y only
p_cum_max_combo <- p_cum_max +
  labs(
    title = NULL,
    x = NULL,
    y = glue("Cumulative difference ({lab_unit(UNIT)}·days)")
  ) +
  theme(
    axis.title.x = element_blank()
  )

# Bottom: X only
p_cum_avg_combo <- p_cum_avg +
  labs(
    title = NULL,
    x = "Days After Planting (DAP)",
    y = NULL
  ) +
  theme(
    axis.title.y = element_blank()
  )

p_cum_combo <- (p_cum_min_combo / p_cum_max_combo / p_cum_avg_combo) +
  plot_annotation(
    title = "Cumulative PRISM - Local Min, Max, and Average Temperature Differences",
    theme = theme(
      plot.title = element_text(hjust = 0.5)  # centered, not bold
    )
  )

ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Cumulative_3panel_{UNIT}.pdf")),
       p_cum_combo, width = 7.5, height = 10.5)

ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Cumulative_3panel_{UNIT}.png")),
       p_cum_combo, width = 7.5, height = 10.5, dpi = 300)




# ===================== SE & SD versions (titles per panel; stop at 150 DAP) =====================
suppressPackageStartupMessages({ library(ggplot2); library(glue); library(patchwork); library(dplyr) })

# Generic plotter: choose band = "SE" or "SD"
plot_metric_band <- function(metric_name, band = c("SE","SD")) {
  band <- match.arg(band)
  dat <- summary_across_years %>%
    dplyr::filter(Metric == metric_name, DAP <= 150)
  n_seasons <- max(dat$n_years, na.rm = TRUE)
  
  hw <- if (band == "SE") dat$se_diff else dat$sd_diff
  subtxt <- if (band == "SE")
    glue("Mean across seasons ± 1 SE (n = {n_seasons} seasons)")
  else
    glue("Mean across seasons ± 1 SD (n = {n_seasons} seasons)")
  
  ggplot(dat, aes(x = DAP, y = mean_diff)) +
    geom_ribbon(aes(ymin = mean_diff - hw, ymax = mean_diff + hw), alpha = 0.18) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    # keep x clipping as-is
    scale_x_continuous(limits = c(0, 150), expand = expansion(mult = c(0, 0))) +
    # DO NOT set y limits here (that trims); just set breaks…
    scale_y_continuous(breaks = seq(-4, 4, by = 1)) +
    # …and zoom the viewport instead so the ribbon isn’t truncated
    coord_cartesian(ylim = c(-4, 4)) +
    labs(
      title    = metric_name,
      subtitle = subtxt,
      x = "Days After Planting (DAP)",
      y = glue("Difference ({lab_unit(UNIT)})")
    ) +
    theme_classic(base_size = 13) +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "plain"),
      plot.subtitle = element_text(size = 10)
    )
}


# Build singles
p_min_SE <- plot_metric_band("Minimum", "SE")
p_max_SE <- plot_metric_band("Maximum", "SE")
p_avg_SE <- plot_metric_band("Average", "SE")

p_min_SD <- plot_metric_band("Minimum", "SD")
p_max_SD <- plot_metric_band("Maximum", "SD")
p_avg_SD <- plot_metric_band("Average", "SD")

# Save singles
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Min_{UNIT}_SE.pdf")), p_min_SE, width = 6.5, height = 4.25)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Max_{UNIT}_SE.pdf")), p_max_SE, width = 6.5, height = 4.25)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Avg_{UNIT}_SE.pdf")), p_avg_SE, width = 6.5, height = 4.25)

ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Min_{UNIT}_SD.pdf")), p_min_SD, width = 6.5, height = 4.25)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Max_{UNIT}_SD.pdf")), p_max_SD, width = 6.5, height = 4.25)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_Avg_{UNIT}_SD.pdf")), p_avg_SD, width = 6.5, height = 4.25)

# ---------- 3-panel composites (no overall title; panel titles kept) ----------
title_style <- theme(plot.title = element_text(hjust = 0.5, face = "plain", margin = margin(b = 4)))

mk_combo_no_overall <- function(p_top, p_mid, p_bot) {
  p_top_c <- p_top + labs(subtitle = NULL, x = NULL, y = NULL) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + title_style
  p_mid_c <- p_mid + labs(subtitle = NULL, x = NULL,
                          y = glue("Difference ({lab_unit(UNIT)})")) +
    theme(axis.title.x = element_blank()) + title_style
  p_bot_c <- p_bot + labs(subtitle = NULL,
                          x = "Days After Planting (DAP)", y = NULL) +
    theme(axis.title.y = element_blank()) + title_style
  (p_top_c / p_mid_c / p_bot_c) # << no plot_annotation() -> no overall title
}

p_combo_SE <- mk_combo_no_overall(p_min_SE, p_max_SE, p_avg_SE)
p_combo_SD <- mk_combo_no_overall(p_min_SD, p_max_SD, p_avg_SD)

# Save 3-panel composites
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_3panel_{UNIT}_SE.pdf")),
       p_combo_SE, width = 7.5, height = 10.5)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_3panel_{UNIT}_SD.pdf")),
       p_combo_SD, width = 7.5, height = 10.5)

# Optional PNGs
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_3panel_{UNIT}_SE.png")),
       p_combo_SE, width = 7.5, height = 10.5, dpi = 300)
ggsave(file.path(OUT_DIR, glue("PRISMminusLocal_3panel_{UNIT}_SD.png")),
       p_combo_SD, width = 7.5, height = 10.5, dpi = 300)

message("✓ Wrote SE & SD daily-difference figures with panel titles and DAP≤150.")





# ---- 1) Split: stratified 70/20/10 by (Location × DTH quartile) ----
make_even_split <- function(df_long_like, seed){
  set.seed(seed)
  env_tbl <- df_long_like %>%
    group_by(Location, Year, Variety) %>%
    summarise(DTH = first(DaysToHeading), .groups = "drop") %>%
    mutate(DTH_bin = ntile(DTH, 4))
  make_labels <- function(n){
    n_tr <- floor(0.70*n); n_cal <- floor(0.20*n); n_val <- n - n_tr - n_cal
    sample(c(rep("Train", n_tr), rep("Calibrate", n_cal), rep("Validate", n_val)))
  }
  env_split <- env_tbl %>%
    group_by(Location, DTH_bin) %>%
    mutate(Set = sample(make_labels(n()))) %>%
    ungroup() %>%
    select(Location, Year, Variety, Set)
  df_long_like %>% inner_join(env_split, by = c("Location","Year","Variety"))
}

# ---- 2) Temperature response + helpers ----
daily_gdd_cols <- function(tmin, tmax, Tb, Tl, Tu){
  tmin_c <- pmax(tmin, Tl); tmax_c <- pmin(tmax, Tu)
  pmax((tmin_c + tmax_c)/2 - Tb, 0)
}
daily_dd_base_cols <- function(tmin, tmax, Tb){
  pmax((tmin + tmax)/2 - Tb, 0)
}
predict_env_gdd_cols <- function(d, min_col, max_col, Tl, Tu, Tb, Greq){
  inc <- daily_gdd_cols(d[[min_col]], d[[max_col]], Tb, Tl, Tu)
  inc[is.na(inc)] <- 0
  hit <- which(cumsum(inc) >= Greq)[1]
  if (is.na(hit)) NA_real_ else d$Days_After_Planting[hit]
}
metric_from_preds <- function(obs, pred){
  rmse <- sqrt(mean((pred - obs)^2, na.rm = TRUE))
  bias <- mean(pred - obs, na.rm = TRUE)
  sse  <- sum((pred - obs)^2, na.rm = TRUE)
  sst  <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  r2   <- ifelse(sst == 0, NA_real_, 1 - sse/sst)
  c(RMSE=rmse, Bias=bias, R2=r2)
}

# ---- 3) Per-var OPT Tl/Tu (CV inside TRAIN) ----
run_gdd_pervar_generic <- function(split_df, min_col, max_col,
                                   varieties, lambda = 0.2, Tb = 10, restarts = 6,
                                   lower = c(Tl=8, Tu=30), upper = c(Tl=18, Tu=45)){
  map_dfr(varieties, function(v){
    env_train <- split_df %>% filter(Variety==v, Set=="Train") %>% group_split(Location,Year)
    if (!length(env_train)) return(tibble())
    folds <- sample(rep(1:5, length.out = length(env_train)))
    obj_fun <- function(par){
      Tl <- par[1]; Tu <- par[2]; if (Tl >= Tu) return(1e9)
      mean(map_dbl(unique(folds), function(k){
        tr <- which(folds!=k); te <- which(folds==k)
        G_train <- map_dbl(env_train[tr], function(d){
          dth <- unique(d$DaysToHeading)
          sum(daily_gdd_cols(d[[min_col]][d$Days_After_Planting<=dth],
                             d[[max_col]][d$Days_After_Planting<=dth], Tb, Tl, Tu))
        })
        Greq <- mean(G_train, na.rm=TRUE); if (!is.finite(Greq) || Greq<=0) return(1e8)
        preds <- map_dbl(env_train[te], function(d)
          predict_env_gdd_cols(d, min_col, max_col, Tl, Tu, Tb, Greq))
        obs <- map_dbl(env_train[te], \(d) unique(d$DaysToHeading))
        if (anyNA(preds)) return(1e8)
        m <- metric_from_preds(obs, preds)
        unname(m["RMSE"] + lambda*abs(m["Bias"]))
      }))
    }
    best <- list(value=Inf, par=c(NA_real_, NA_real_))
    starts <- cbind(runif(restarts, 9, 14), runif(restarts, 32, 40))
    for (j in seq_len(restarts)){
      fit <- optim(starts[j,], obj_fun, method="L-BFGS-B",
                   lower=lower, upper=upper, control=list(maxit=600))
      if (fit$value < best$value) best <- fit
    }
    Greq <- map_dbl(env_train, function(d){
      dth <- unique(d$DaysToHeading)
      sum(daily_gdd_cols(d[[min_col]][d$Days_After_Planting<=dth],
                         d[[max_col]][d$Days_After_Planting<=dth], Tb, best$par[1], best$par[2]))
    }) %>% mean(na.rm=TRUE)
    tibble(Variety=v, Tl=best$par[1], Tu=best$par[2], GDDreq=Greq)
  })
}

# ---- 4) Global tuned Tl/Tu (pooled across varieties; CV on TRAIN envs) ----
run_gdd_global_generic <- function(split_df, min_col, max_col,
                                   lambda=0.2, Tb=10, restarts=10,
                                   lower=c(Tl=8, Tu=30), upper=c(Tl=18, Tu=45)){
  train_envs <- split_df %>% filter(Set=="Train") %>% group_split(Variety, Location, Year)
  if (!length(train_envs)) return(tibble(Tl=NA_real_, Tu=NA_real_))
  folds <- sample(rep(1:5, length.out = length(train_envs)))
  obj_fun <- function(par){
    Tl <- par[1]; Tu <- par[2]; if (Tl >= Tu) return(1e9)
    mean(map_dbl(unique(folds), function(k){
      tr <- which(folds!=k); te <- which(folds==k)
      tr_list <- train_envs[tr]
      vars    <- unique(map_chr(tr_list, ~ unique(.x$Variety)))
      greq <- setNames(rep(NA_real_, length(vars)), vars)
      for (v in vars){
        ets <- keep(tr_list, ~ unique(.x$Variety)==v)
        if (!length(ets)) next
        greq[v] <- mean(map_dbl(ets, function(d){
          dth <- unique(d$DaysToHeading)
          sum(daily_gdd_cols(d[[min_col]][d$Days_After_Planting<=dth],
                             d[[max_col]][d$Days_After_Planting<=dth], Tb, Tl, Tu))
        }), na.rm=TRUE)
      }
      te_list <- train_envs[te]
      pred <- map_dfr(te_list, function(d){
        v <- unique(d$Variety); g <- greq[[v]]
        if (!is.finite(g)) return(tibble())
        tibble(Obs=unique(d$DaysToHeading),
               Pred=predict_env_gdd_cols(d, min_col, max_col, Tl, Tu, Tb, g))
      }) %>% drop_na()
      if (!nrow(pred)) return(1e8)
      m <- metric_from_preds(pred$Obs, pred$Pred)
      unname(m["RMSE"] + lambda*abs(m["Bias"]))
    }))
  }
  best <- list(value=Inf, par=c(NA_real_, NA_real_))
  starts <- cbind(runif(restarts, 9, 14), runif(restarts, 32, 40))
  for (j in seq_len(restarts)){
    fit <- optim(starts[j,], obj_fun, method="L-BFGS-B",
                 lower=lower, upper=upper, control=list(maxit=800))
    if (fit$value < best$value) best <- fit
  }
  tibble(Tl=best$par[1], Tu=best$par[2])
}

# ---- 5) Metrics builders ----
pooled_metrics_base_generic <- function(split_df, min_col, max_col, Tb = 10, set = "Validate", label){
  envs_tr <- split_df %>% filter(Set=="Train") %>% group_split(Variety, Location, Year)
  vars    <- sort(unique(split_df$Variety))
  greq <- setNames(rep(NA_real_, length(vars)), vars)
  for (v in vars){
    ets <- purrr::keep(envs_tr, ~ unique(.x$Variety)==v)
    if (!length(ets)) next
    greq[v] <- mean(purrr::map_dbl(ets, function(d){
      dth <- unique(d$DaysToHeading)
      sum(daily_dd_base_cols(d[[min_col]][d$Days_After_Planting<=dth],
                             d[[max_col]][d$Days_After_Planting<=dth], Tb))
    }), na.rm=TRUE)
  }
  envs_tg <- split_df %>% filter(Set==set) %>% group_split(Variety, Location, Year)
  pred <- purrr::map_dfr(envs_tg, function(d){
    v <- unique(d$Variety); g <- greq[[v]]
    if (!is.finite(g)) return(tibble())
    inc <- daily_dd_base_cols(d[[min_col]], d[[max_col]], Tb)
    hit <- which(cumsum(inc) >= g)[1]
    tibble(Obs=unique(d$DaysToHeading), Pred=ifelse(is.na(hit), NA_real_, d$Days_After_Planting[hit]))
  }) %>% drop_na()
  if (!nrow(pred)) return(tibble(Model=label, Set=set, RMSE=NA, Bias=NA, R2=NA, n=0L))
  m <- metric_from_preds(pred$Obs, pred$Pred)
  tibble(Model=label, Set=set, RMSE=m["RMSE"], Bias=m["Bias"], R2=m["R2"], n=nrow(pred))
}

pooled_metrics_universal_generic <- function(split_df, min_col, max_col, Tl, Tu, Tb = 10, set = "Validate", label){
  vars <- sort(unique(split_df$Variety))
  greq <- setNames(rep(NA_real_, length(vars)), vars)
  for (v in vars){
    ets <- split_df %>% filter(Variety==v, Set=="Train") %>% group_split(Location, Year)
    if (!length(ets)) next
    greq[v] <- mean(purrr::map_dbl(ets, function(d){
      dth <- unique(d$DaysToHeading)
      sum(daily_gdd_cols(d[[min_col]][d$Days_After_Planting<=dth],
                         d[[max_col]][d$Days_After_Planting<=dth], Tb, Tl, Tu))
    }), na.rm=TRUE)
  }
  envs_tg <- split_df %>% filter(Set==set) %>% group_split(Variety, Location, Year)
  pred <- purrr::map_dfr(envs_tg, function(d){
    v <- unique(d$Variety); g <- greq[[v]]
    if (!is.finite(g)) return(tibble())
    tibble(Obs=unique(d$DaysToHeading),
           Pred=predict_env_gdd_cols(d, min_col, max_col, Tl, Tu, Tb, g))
  }) %>% drop_na()
  if (!nrow(pred)) return(tibble(Model=label, Set=set, RMSE=NA, Bias=NA, R2=NA, n=0L))
  m <- metric_from_preds(pred$Obs, pred$Pred)
  tibble(Model=label, Set=set, RMSE=m["RMSE"], Bias=m["Bias"], R2=m["R2"], n=nrow(pred))
}

pooled_metrics_pervar_generic <- function(split_df, min_col, max_col, Tb = 10, set = "Validate", label){
  vars <- sort(unique(split_df$Variety))
  fit  <- run_gdd_pervar_generic(split_df, min_col, max_col, vars, lambda = 0.2, Tb = Tb, restarts = 6)
  envs <- split_df %>% filter(Set==set) %>% group_split(Variety, Location, Year)
  all_pred <- map_dfr(envs, function(d){
    v <- unique(d$Variety); row <- fit %>% filter(Variety==v)
    if (!nrow(row)) return(tibble())
    tibble(Obs=unique(d$DaysToHeading),
           Pred=predict_env_gdd_cols(d, min_col, max_col, row$Tl, row$Tu, Tb, row$GDDreq))
  }) %>% drop_na()
  if (!nrow(all_pred)) return(tibble(Model=label, Set=set, RMSE=NA, Bias=NA, R2=NA, n=0L))
  m <- metric_from_preds(all_pred$Obs, all_pred$Pred)
  tibble(Model=label, Set=set, RMSE=m["RMSE"], Bias=m["Bias"], R2=m["R2"], n=nrow(all_pred))
}

# ---- 6) Utilities: validate and coerce Tl/Tu pairs ----
.valid_pair <- function(x){
  if (is.null(x)) return(FALSE)
  if (!all(c("Tl","Tu") %in% names(x))) return(FALSE)
  y <- suppressWarnings(as.numeric(x[c("Tl","Tu")]))
  all(is.finite(y))
}
.coerce_pair <- function(x, default=c(Tl=12, Tu=34)){
  if (.valid_pair(x)) {
    y <- as.numeric(x[c("Tl","Tu")]); names(y) <- c("Tl","Tu"); return(y)
  } else {
    return(as.numeric(default))
  }
}

# ---- 7) Tl/Tu sources computed INSIDE this script (NO rounded here) ----
compute_threshold_menu <- function(split_df, dataset=c("Local","PRISM")){
  dataset <- match.arg(dataset)
  if (dataset == "Local"){
    min_col <- "Tmin_useC"; max_col <- "Tmax_useC"
  } else {
    min_col <- "PRISMMinTempC"; max_col <- "PRISMMaxTempC"
  }
  vars <- sort(unique(split_df$Variety))
  pervar <- run_gdd_pervar_generic(split_df, min_col, max_col, vars, lambda=0.2, Tb=10, restarts=6)
  
  mean_pair_raw <- c(Tl = mean(pervar$Tl, na.rm=TRUE), Tu = mean(pervar$Tu, na.rm=TRUE))
  mean_pair <- .coerce_pair(mean_pair_raw)
  
  tuned_tbl  <- run_gdd_global_generic(split_df, min_col, max_col, lambda=0.2, Tb=10, restarts=10)
  tuned_raw  <- c(Tl = tuned_tbl$Tl[1], Tu = tuned_tbl$Tu[1])
  tuned_pair <- if (.valid_pair(tuned_raw)) .coerce_pair(tuned_raw) else mean_pair
  
  list(mean=mean_pair, tuned=tuned_pair, pervar_table=pervar)
}

# ---- 8) Metrics for a complete dataset (Base, Universal, Per-var) ----
metrics_for_split_dataset <- function(split_df, dataset = c("Local","PRISM"),
                                      TlTu_menu, Tb_grid = 7:10,
                                      set_for_eval = "Validate") {
  dataset <- match.arg(dataset)
  if (dataset == "Local") {
    min_col <- "Tmin_useC";    max_col <- "Tmax_useC"; tag <- "(Local)"
  } else {
    min_col <- "PRISMMinTempC"; max_col <- "PRISMMaxTempC"; tag <- "(PRISM)"
  }
  
  # Base model: no Tl/Tu
  base_rows <- map_dfr(
    Tb_grid,
    ~ pooled_metrics_base_generic(
      split_df, min_col, max_col,
      Tb   = .x,
      set  = set_for_eval,                               # ⭐ NEW
      label = glue("Base Tb={.x} {tag}")
    )
  )
  
  # Universal Tl/Tu variants (mean, tuned, rounded)
  uni_named <- list(
    Local_mean    = TlTu_menu$Local$mean,
    Local_tuned   = TlTu_menu$Local$tuned,
    Local_rounded = TlTu_menu$Local$rounded,   # fixed (11,33)
    PRISM_mean    = TlTu_menu$PRISM$mean,
    PRISM_tuned   = TlTu_menu$PRISM$tuned,
    PRISM_rounded = TlTu_menu$PRISM$rounded   # fixed (12,34)
  )
  uni_named <- purrr::imap(uni_named, ~ .coerce_pair(.x))
  
  uni_rows <- imap_dfr(uni_named, function(pair, nm){
    map_dfr(
      Tb_grid,
      ~ pooled_metrics_universal_generic(
        split_df, min_col, max_col,
        Tl   = pair["Tl"], Tu = pair["Tu"],
        Tb   = .x,
        set  = set_for_eval,                              # ⭐ NEW
        label = glue("Universal Tl/Tu ({gsub('_',' ',nm)}, Tb={.x})")
      )
    )
  })
  
  # OPT per-var Tl/Tu
  pervar_rows <- map_dfr(
    Tb_grid,
    ~ pooled_metrics_pervar_generic(
      split_df, min_col, max_col,
      Tb   = .x,
      set  = set_for_eval,                                # ⭐ NEW
      label = glue("OPT per-var ({dataset}, Tb={.x})")
    )
  )
  
  bind_rows(base_rows, uni_rows, pervar_rows) %>%
    mutate(J = RMSE + 0.2 * abs(Bias))
}

# ---- 9) SINGLE RUN (seed = 1001): thresholds + evaluation ----
SEED_SINGLE <- 1001
split_local_single <- make_even_split(df_long_local, seed=SEED_SINGLE)
split_prism_single <- make_even_split(df_long_prism, seed=SEED_SINGLE)

menu_local  <- compute_threshold_menu(split_local_single, dataset="Local")
menu_prism  <- compute_threshold_menu(split_prism_single, dataset="PRISM")

# ---- Fixed "rounded" values (your compromise) ----
Local_fixed_round <- c(Tl=11, Tu=34)
PRISM_fixed_round <- c(Tl=12, Tu=33)

TlTu_menu <- list(
  Local = list(
    mean    = menu_local$mean,
    tuned   = menu_local$tuned,
    rounded = Local_fixed_round
  ),
  PRISM = list(
    mean    = menu_prism$mean,
    tuned   = menu_prism$tuned,
    rounded = PRISM_fixed_round
  )
)

# Build and write Tl/Tu summary
tl_summary <- tibble(
  Dataset = rep(c("Local","PRISM"), each=3),
  Variant = rep(c("mean","tuned","rounded"), times=2),
  Tl = as.numeric(c(TlTu_menu$Local$mean["Tl"],   TlTu_menu$Local$tuned["Tl"],   TlTu_menu$Local$rounded["Tl"],
                    TlTu_menu$PRISM$mean["Tl"],   TlTu_menu$PRISM$tuned["Tl"],   TlTu_menu$PRISM$rounded["Tl"])),
  Tu = as.numeric(c(TlTu_menu$Local$mean["Tu"],   TlTu_menu$Local$tuned["Tu"],   TlTu_menu$Local$rounded["Tu"],
                    TlTu_menu$PRISM$mean["Tu"],   TlTu_menu$PRISM$tuned["Tu"],   TlTu_menu$PRISM$rounded["Tu"])),
  Source = c("mean of per-var Local fits (seed 1001 TRAIN)",
             "global tuned on Local (seed 1001 TRAIN)",
             "fixed rounded compromise (Tl=11, Tu=34)",
             "mean of per-var PRISM fits (seed 1001 TRAIN)",
             "global tuned on PRISM (seed 1001 TRAIN)",
             "fixed rounded compromise (Tl=12, Tu=33)")
) %>% mutate(Tl=round(Tl,2), Tu=round(Tu,2))

write_csv(tl_summary, file.path(OUT_DIR,"FINAL_TlTu_summary_seed1001.csv"))
print(tl_summary)

# ---- 9a) CALIBRATION STEP: choose best model/Tb using Set == "Calibrate" ----

# Local dataset: performance on CALIBRATION set
single_local_cal <- metrics_for_split_dataset(
  split_local_single,
  dataset      = "Local",
  TlTu_menu    = TlTu_menu,
  Tb_grid      = 7:10,
  set_for_eval = "Calibrate"          # ⭐ use the Cal set here
) %>%
  arrange(J)

# PRISM dataset: performance on CALIBRATION set
single_prism_cal <- metrics_for_split_dataset(
  split_prism_single,
  dataset      = "PRISM",
  TlTu_menu    = TlTu_menu,
  Tb_grid      = 7:10,
  set_for_eval = "Calibrate"
) %>%
  arrange(J)

# Best (lowest J) model/Tb per dataset, according to CALIBRATION set
best_local_from_cal <- single_local_cal %>%
  filter(!is.na(J)) %>% 
  slice(1) %>%
  mutate(Dataset = "Local")

best_prism_from_cal <- single_prism_cal %>%
  filter(!is.na(J)) %>%
  slice(1) %>%
  mutate(Dataset = "PRISM")

# Save full calibration grids + the best choices
write_csv(single_local_cal,
          file.path(OUT_DIR, "FINAL_Local_CALIBRATION_allModels_byJ_low2high.csv"))
write_csv(single_prism_cal,
          file.path(OUT_DIR, "FINAL_PRISM_CALIBRATION_allModels_byJ_low2high.csv"))

best_from_cal <- bind_rows(best_local_from_cal, best_prism_from_cal)
write_csv(best_from_cal,
          file.path(OUT_DIR, "FINAL_bestModels_from_CALIBRATION.csv"))
print(best_from_cal)

single_local <- metrics_for_split_dataset(split_local_single, dataset="Local", TlTu_menu=TlTu_menu) %>%
  arrange(J) %>% mutate(Dataset="Local", run=1L, seed=SEED_SINGLE)
single_prism <- metrics_for_split_dataset(split_prism_single, dataset="PRISM", TlTu_menu=TlTu_menu) %>%
  arrange(J) %>% mutate(Dataset="PRISM", run=1L, seed=SEED_SINGLE)

single_both <- bind_rows(single_local, single_prism) %>% arrange(Dataset, J)
write_csv(single_both, file.path(OUT_DIR,"FINAL_COMPARE_Local_vs_PRISM_SINGLErun_byJ_low2high.csv"))
print(single_both, n=Inf)

# ---- 10) FIVE RUNS (seeds = 1001:1005) ----
N_RUNS <- 5
SEEDS  <- 1001:(1000 + N_RUNS)

# ---- 10a) Tl/Tu summaries for all seeds (mean & tuned) ----
tl_all_seeds <- purrr::map_dfr(SEEDS, function(s) {
  # Split once per dataset for this seed
  df_split_loc  <- make_even_split(df_long_local,  seed = s)
  df_split_pris <- make_even_split(df_long_prism,  seed = s)
  
  menu_loc <- compute_threshold_menu(df_split_loc,  dataset = "Local")
  menu_pri <- compute_threshold_menu(df_split_pris, dataset = "PRISM")
  
  tibble(
    seed    = s,
    Dataset = c("Local", "Local", "PRISM", "PRISM"),
    Variant = c("mean", "tuned", "mean", "tuned"),
    Tl      = c(menu_loc$mean["Tl"],  menu_loc$tuned["Tl"],
                menu_pri$mean["Tl"],  menu_pri$tuned["Tl"]),
    Tu      = c(menu_loc$mean["Tu"],  menu_loc$tuned["Tu"],
                menu_pri$mean["Tu"],  menu_pri$tuned["Tu"])
  )
}) %>%
  mutate(
    Tl = round(as.numeric(Tl), 2),
    Tu = round(as.numeric(Tu), 2)
  ) %>%
  arrange(Dataset, Variant, seed)

write_csv(
  tl_all_seeds,
  file.path(OUT_DIR, "FINAL_TlTu_summary_allSeeds_mean_tuned.csv")
)
print(tl_all_seeds, n = Inf)

# ---- Tl/Tu averages over seeds (per Dataset × Variant) ----
tl_all_seeds_summary <- tl_all_seeds %>%
  group_by(Dataset, Variant) %>%
  summarise(
    n_seeds = dplyr::n(),
    Tl_mean = mean(Tl, na.rm = TRUE),
    Tl_sd   = sd(Tl,   na.rm = TRUE),
    Tu_mean = mean(Tu, na.rm = TRUE),
    Tu_sd   = sd(Tu,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Tl_mean = round(Tl_mean, 2),
    Tl_sd   = round(Tl_sd,   2),
    Tu_mean = round(Tu_mean, 2),
    Tu_sd   = round(Tu_sd,   2)
  )

write_csv(
  tl_all_seeds_summary,
  file.path(OUT_DIR, "FINAL_TlTu_allSeeds_MEAN_byDatasetVariant.csv")
)

print(tl_all_seeds_summary)


# For each seed, compute metrics on Train, Calibrate, and Validate
all_runs_local <- map2_dfr(SEEDS, seq_along(SEEDS), function(s, i){
  df_split_loc  <- make_even_split(df_long_local,  seed = s)
  df_split_pris <- make_even_split(df_long_prism,  seed = s)
  
  menu_loc <- compute_threshold_menu(df_split_loc,  dataset = "Local")
  menu_pri <- compute_threshold_menu(df_split_pris, dataset = "PRISM")
  
  menu <- list(
    Local = list(mean = menu_loc$mean, tuned = menu_loc$tuned, rounded = Local_fixed_round),
    PRISM = list(mean = menu_pri$mean, tuned = menu_pri$tuned, rounded = PRISM_fixed_round)
  )
  
  # Loop over Train / Calibrate / Validate for this seed
  map_dfr(c("Train","Calibrate","Validate"), function(set_nm){
    metrics_for_split_dataset(
      df_split_loc,
      dataset      = "Local",
      TlTu_menu    = menu,
      Tb_grid      = 7:10,
      set_for_eval = set_nm
    ) %>%
      mutate(
        Dataset = "Local",
        run     = i,
        seed    = s,
        Set     = set_nm
      )
  })
})

all_runs_prism <- map2_dfr(SEEDS, seq_along(SEEDS), function(s, i){
  df_split_pris <- make_even_split(df_long_prism, seed = s)
  df_split_loc  <- make_even_split(df_long_local, seed = s)
  
  menu_pri <- compute_threshold_menu(df_split_pris, dataset = "PRISM")
  menu_loc <- compute_threshold_menu(df_split_loc,  dataset = "Local")
  
  menu <- list(
    Local = list(mean = menu_loc$mean, tuned = menu_loc$tuned, rounded = Local_fixed_round),
    PRISM = list(mean = menu_pri$mean, tuned = menu_pri$tuned, rounded = PRISM_fixed_round)
  )
  
  map_dfr(c("Train","Calibrate","Validate"), function(set_nm){
    metrics_for_split_dataset(
      df_split_pris,
      dataset      = "PRISM",
      TlTu_menu    = menu,
      Tb_grid      = 7:10,
      set_for_eval = set_nm
    ) %>%
      mutate(
        Dataset = "PRISM",
        run     = i,
        seed    = s,
        Set     = set_nm
      )
  })
})

all_runs_both <- bind_rows(all_runs_local, all_runs_prism)

# 5-run summary by Dataset × Set × Model
summary_5runs_both <- all_runs_both %>%
  group_by(Dataset, Set, Model) %>%
  summarise(
    RMSE_mean = mean(RMSE, na.rm = TRUE),
    RMSE_sd   = sd(RMSE,   na.rm = TRUE),
    Bias_mean = mean(Bias, na.rm = TRUE),
    Bias_sd   = sd(Bias,   na.rm = TRUE),
    R2_mean   = mean(R2,   na.rm = TRUE),
    R2_sd     = sd(R2,     na.rm = TRUE),
    J_mean    = mean(J,    na.rm = TRUE),
    J_sd      = sd(J,      na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  arrange(Dataset, Set, J_mean)

# Save the full per-run table + the per-set summaries
write_csv(
  all_runs_both,
  file.path(OUT_DIR,"FINAL_COMPARE_Local_vs_PRISM_5runs_perRun_allSets.csv")
)
write_csv(
  summary_5runs_both,
  file.path(OUT_DIR,"FINAL_COMPARE_Local_vs_PRISM_5runs_bySet_byJ_low2high.csv")
)
print(summary_5runs_both, n = Inf)

# ---- 10b) Export three clean tables: Train / Calibrate / Validate ----

summary_train_5 <- summary_5runs_both %>%
  filter(Set == "Train") %>%
  arrange(Dataset, J_mean)

summary_cal_5 <- summary_5runs_both %>%
  filter(Set == "Calibrate") %>%
  arrange(Dataset, J_mean)

summary_val_5 <- summary_5runs_both %>%
  filter(Set == "Validate") %>%
  arrange(Dataset, J_mean)

write_csv(
  summary_train_5,
  file.path(OUT_DIR, "FINAL_Train_5runs_byJ_low2high.csv")
)
write_csv(
  summary_cal_5,
  file.path(OUT_DIR, "FINAL_Calibrate_5runs_byJ_low2high.csv")
)
write_csv(
  summary_val_5,
  file.path(OUT_DIR, "FINAL_Validate_5runs_byJ_low2high.csv")
)

# ---- 10c) Shared Tl/Tu = 11/33 sensitivity (Validate set only) ----
# Uses the same SEEDS and split logic, but forces BOTH datasets
# to share Tl=11, Tu=33 for the Universal model.

shared_Tl <- 11
shared_Tu <- 33
Tb_grid_shared <- 7:10

shared_11_33_perRun <- purrr::map2_dfr(SEEDS, seq_along(SEEDS), function(s, i) {
  # Split Local and PRISM once per seed
  split_loc  <- make_even_split(df_long_local,  seed = s)
  split_pris <- make_even_split(df_long_prism,  seed = s)
  
  purrr::map_dfr(Tb_grid_shared, function(tb_val) {
    # Local with shared Tl/Tu
    loc_row <- pooled_metrics_universal_generic(
      split_df   = split_loc,
      min_col    = "Tmin_useC",
      max_col    = "Tmax_useC",
      Tl         = shared_Tl,
      Tu         = shared_Tu,
      Tb         = tb_val,
      set        = "Validate",
      label      = glue("Shared Tl/Tu (11/33), Tb={tb_val}")
    ) %>%
      mutate(Dataset = "Local")
    
    # PRISM with shared Tl/Tu
    pri_row <- pooled_metrics_universal_generic(
      split_df   = split_pris,
      min_col    = "PRISMMinTempC",
      max_col    = "PRISMMaxTempC",
      Tl         = shared_Tl,
      Tu         = shared_Tu,
      Tb         = tb_val,
      set        = "Validate",
      label      = glue("Shared Tl/Tu (11/33), Tb={tb_val}")
    ) %>%
      mutate(Dataset = "PRISM")
    
    bind_rows(loc_row, pri_row) %>%
      mutate(
        seed = s,
        run  = i,
        Tb   = tb_val,
        J    = RMSE + 0.2 * abs(Bias)
      )
  })
})

# Save per-run table
write_csv(
  shared_11_33_perRun,
  file.path(OUT_DIR, "FINAL_Validate_5runs_SHARED_11_33_perRun.csv")
)

# Summarise over seeds: Dataset × Tb (all Shared 11/33)
shared_11_33_summary <- shared_11_33_perRun %>%
  group_by(Dataset, Tb, Model) %>%
  summarise(
    n_seeds   = dplyr::n(),
    RMSE_mean = mean(RMSE, na.rm = TRUE),
    RMSE_sd   = sd(RMSE,   na.rm = TRUE),
    Bias_mean = mean(Bias, na.rm = TRUE),
    Bias_sd   = sd(Bias,   na.rm = TRUE),
    R2_mean   = mean(R2,   na.rm = TRUE),
    R2_sd     = sd(R2,     na.rm = TRUE),
    J_mean    = mean(J,    na.rm = TRUE),
    J_sd      = sd(J,      na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    RMSE_mean = round(RMSE_mean, 2),
    RMSE_sd   = round(RMSE_sd,   2),
    Bias_mean = round(Bias_mean, 2),
    Bias_sd   = round(Bias_sd,   2),
    R2_mean   = round(R2_mean,   2),
    R2_sd     = round(R2_sd,     2),
    J_mean    = round(J_mean,    2),
    J_sd      = round(J_sd,      2)
  ) %>%
  arrange(Dataset, Tb, J_mean)

write_csv(
  shared_11_33_summary,
  file.path(OUT_DIR, "FINAL_Validate_5runs_SHARED_11_33_summary.csv")
)

print(shared_11_33_summary, n = Inf)

# ---- 10d) Integrate Shared Tl/Tu (11/33) into 5-run comparisons ----
# We take the existing all_runs_both and simply append the
# per-run Shared(11/33) metrics (Validate set only).

all_runs_with_shared <- bind_rows(
  all_runs_both,
  shared_11_33_perRun %>%
    mutate(Set = "Validate")  # ensure Set column is present/consistent
)

# 5-run summary INCLUDING Shared(11/33) as another Model
summary_5runs_with_shared <- all_runs_with_shared %>%
  group_by(Dataset, Set, Model) %>%
  summarise(
    RMSE_mean = mean(RMSE, na.rm = TRUE),
    RMSE_sd   = sd(RMSE,   na.rm = TRUE),
    Bias_mean = mean(Bias, na.rm = TRUE),
    Bias_sd   = sd(Bias,   na.rm = TRUE),
    R2_mean   = mean(R2,   na.rm = TRUE),
    R2_sd     = sd(R2,     na.rm = TRUE),
    J_mean    = mean(J,    na.rm = TRUE),
    J_sd      = sd(J,      na.rm = TRUE),
    n_runs    = dplyr::n(),
    .groups   = "drop"
  ) %>%
  arrange(Dataset, Set, J_mean)

# Save the full with-shared summary
write_csv(
  summary_5runs_with_shared,
  file.path(OUT_DIR, "FINAL_5runs_bySet_byJ_withSHARED11_33.csv")
)

# Optional: a Validate-only table that directly compares *all* models, incl. Shared(11/33)
summary_val_with_shared <- summary_5runs_with_shared %>%
  filter(Set == "Validate") %>%
  arrange(Dataset, J_mean)

write_csv(
  summary_val_with_shared,
  file.path(OUT_DIR, "FINAL_Validate_5runs_byJ_withSHARED11_33.csv")
)

print(summary_val_with_shared, n = Inf)

# ===============================================================
# 10e) Build per-run Validation predictions for ALL families
#      -> FINAL_predictions_perRun.csv
#      Families:
#        • Base (no Tl/Tu)
#        • Universal tuned (per-dataset Tl/Tu)
#        • Universal rounded (Local 11/33, PRISM 12/34)
#        • OPT per-var (Tl/Tu and GDDreq per variety)
#      Tb grid = 7:10, seeds = SEEDS (1001:1005)
# ===============================================================

# ---- helpers (Validation predictions) -------------------------------------

compute_greq_base <- function(split_df, min_col, max_col, Tb) {
  vars    <- sort(unique(split_df$Variety))
  envs_tr <- split_df %>%
    dplyr::filter(Set == "Train") %>%
    dplyr::group_split(Variety, Location, Year)
  
  greq <- setNames(rep(NA_real_, length(vars)), vars)
  for (v in vars) {
    ets <- purrr::keep(envs_tr, ~ unique(.x$Variety) == v)
    if (!length(ets)) next
    greq[v] <- mean(purrr::map_dbl(ets, function(d) {
      dth <- unique(d$DaysToHeading)
      sum(daily_dd_base_cols(
        d[[min_col]][d$Days_After_Planting <= dth],
        d[[max_col]][d$Days_After_Planting <= dth],
        Tb
      ))
    }), na.rm = TRUE)
  }
  greq
}

compute_greq_universal <- function(split_df, min_col, max_col, Tl, Tu, Tb) {
  vars    <- sort(unique(split_df$Variety))
  envs_tr <- split_df %>%
    dplyr::filter(Set == "Train") %>%
    dplyr::group_split(Variety, Location, Year)
  
  greq <- setNames(rep(NA_real_, length(vars)), vars)
  for (v in vars) {
    ets <- purrr::keep(envs_tr, ~ unique(.x$Variety) == v)
    if (!length(ets)) next
    greq[v] <- mean(purrr::map_dbl(ets, function(d) {
      dth <- unique(d$DaysToHeading)
      sum(daily_gdd_cols(
        d[[min_col]][d$Days_After_Planting <= dth],
        d[[max_col]][d$Days_After_Planting <= dth],
        Tb, Tl, Tu
      ))
    }), na.rm = TRUE)
  }
  greq
}

predict_validate_base <- function(split_df, min_col, max_col, Tb, greq_vec,
                                  dataset_label, family_label, model_label,
                                  seed, run_id) {
  envs_val <- split_df %>%
    dplyr::filter(Set == "Validate") %>%
    dplyr::group_split(Variety, Location, Year)
  
  purrr::map_dfr(envs_val, function(d) {
    v   <- unique(d$Variety)
    loc <- unique(d$Location)
    yr  <- unique(d$Year)
    g   <- greq_vec[[v]]
    if (!is.finite(g)) return(tibble())
    
    inc <- daily_dd_base_cols(d[[min_col]], d[[max_col]], Tb)
    hit <- which(cumsum(inc) >= g)[1]
    pred_dap <- ifelse(is.na(hit), NA_real_, d$Days_After_Planting[hit])
    
    tibble(
      Obs      = unique(d$DaysToHeading),
      Pred     = pred_dap,
      Variety  = v,
      Location = loc,
      Year     = yr,
      Dataset  = dataset_label,
      Family   = family_label,
      Model    = model_label,
      Tb       = Tb,
      seed     = seed,
      run      = run_id
    )
  }) %>%
    dplyr::filter(!is.na(Obs) & !is.na(Pred))
}

predict_validate_universal <- function(split_df, min_col, max_col, Tl, Tu, Tb,
                                       greq_vec, dataset_label, family_label,
                                       model_label, seed, run_id) {
  envs_val <- split_df %>%
    dplyr::filter(Set == "Validate") %>%
    dplyr::group_split(Variety, Location, Year)
  
  purrr::map_dfr(envs_val, function(d) {
    v   <- unique(d$Variety)
    loc <- unique(d$Location)
    yr  <- unique(d$Year)
    g   <- greq_vec[[v]]
    if (!is.finite(g)) return(tibble())
    
    tibble(
      Obs      = unique(d$DaysToHeading),
      Pred     = predict_env_gdd_cols(d, min_col, max_col, Tl, Tu, Tb, g),
      Variety  = v,
      Location = loc,
      Year     = yr,
      Dataset  = dataset_label,
      Family   = family_label,
      Model    = model_label,
      Tb       = Tb,
      seed     = seed,
      run      = run_id
    )
  }) %>%
    dplyr::filter(!is.na(Obs) & !is.na(Pred))
}

predict_validate_opt <- function(split_df, min_col, max_col, Tb, pervar_tbl,
                                 dataset_label, family_label, model_label,
                                 seed, run_id) {
  envs_val <- split_df %>%
    dplyr::filter(Set == "Validate") %>%
    dplyr::group_split(Variety, Location, Year)
  
  purrr::map_dfr(envs_val, function(d) {
    v   <- unique(d$Variety)
    loc <- unique(d$Location)
    yr  <- unique(d$Year)
    row <- pervar_tbl %>% dplyr::filter(Variety == v)
    if (!nrow(row)) return(tibble())
    
    tibble(
      Obs      = unique(d$DaysToHeading),
      Pred     = predict_env_gdd_cols(
        d, min_col, max_col,
        Tl   = row$Tl,
        Tu   = row$Tu,
        Tb   = Tb,
        Greq = row$GDDreq
      ),
      Variety  = v,
      Location = loc,
      Year     = yr,
      Dataset  = dataset_label,
      Family   = family_label,
      Model    = model_label,
      Tb       = Tb,
      seed     = seed,
      run      = run_id
    )
  }) %>%
    dplyr::filter(!is.na(Obs) & !is.na(Pred))
}

# ---- build predictions for one dataset & seed -----------------------------

Tb_grid_pred <- 7:10

build_preds_for_dataset <- function(split_df, dataset_label, menu_ds,
                                    min_col, max_col, seed, run_id) {
  out <- list()
  
  # dataset-specific rounded thresholds (unchanged)
  if (dataset_label == "Local") {
    Tl_r <- Local_fixed_round["Tl"]; Tu_r <- Local_fixed_round["Tu"]
  } else {
    Tl_r <- PRISM_fixed_round["Tl"]; Tu_r <- PRISM_fixed_round["Tu"]
  }
  
  tuned_pair <- menu_ds$tuned
  
  for (tb in Tb_grid_pred) {
    # 1) Base
    greq_base <- compute_greq_base(split_df, min_col, max_col, Tb = tb)
    base_pred <- predict_validate_base(
      split_df      = split_df,
      min_col       = min_col,
      max_col       = max_col,
      Tb            = tb,
      greq_vec      = greq_base,
      dataset_label = dataset_label,
      family_label  = "Base",
      model_label   = glue("Base Tb={tb} ({dataset_label})"),
      seed          = seed,
      run_id        = run_id
    )
    
    # 2) Universal tuned
    greq_tuned <- compute_greq_universal(
      split_df = split_df, min_col = min_col, max_col = max_col,
      Tl = tuned_pair["Tl"], Tu = tuned_pair["Tu"], Tb = tb
    )
    uni_tuned <- predict_validate_universal(
      split_df      = split_df, min_col = min_col, max_col = max_col,
      Tl            = tuned_pair["Tl"], Tu = tuned_pair["Tu"],
      Tb            = tb, greq_vec = greq_tuned,
      dataset_label = dataset_label,
      family_label  = "Universal tuned",
      model_label   = glue("Universal tuned (Tb={tb}, {dataset_label})"),
      seed          = seed, run_id = run_id
    )
    
    # 3) Universal rounded
    greq_round <- compute_greq_universal(
      split_df = split_df, min_col = min_col, max_col = max_col,
      Tl = Tl_r, Tu = Tu_r, Tb = tb
    )
    uni_round <- predict_validate_universal(
      split_df      = split_df, min_col = min_col, max_col = max_col,
      Tl            = Tl_r, Tu = Tu_r,
      Tb            = tb, greq_vec = greq_round,
      dataset_label = dataset_label,
      family_label  = "Universal rounded",
      model_label   = glue("Universal rounded (Tb={tb}, {dataset_label})"),
      seed          = seed, run_id = run_id
    )
    
    # 4) OPT per-var  —— RECOMPUTE PER TB (this is the fix)
    vars <- sort(unique(split_df$Variety))
    pervar_tbl_tb <- run_gdd_pervar_generic(
      split_df, min_col, max_col,
      varieties = vars, lambda = 0.2, Tb = tb, restarts = 6
    )
    
    opt_pred <- predict_validate_opt(
      split_df      = split_df,
      min_col       = min_col,
      max_col       = max_col,
      Tb            = tb,
      pervar_tbl    = pervar_tbl_tb,   # use the per-Tb table
      dataset_label = dataset_label,
      family_label  = "OPT",
      model_label   = glue("OPT per-var (Tb={tb}, {dataset_label})"),
      seed          = seed,
      run_id        = run_id
    )
    
    out[[length(out) + 1]] <- dplyr::bind_rows(base_pred, uni_tuned, uni_round, opt_pred)
  }
  
  dplyr::bind_rows(out)
}

# ---- loop over seeds & datasets to build prediction cache -----------------

predictions_perRun <- purrr::map2_dfr(SEEDS, seq_along(SEEDS), function(s, i) {
  # Split once per dataset for this seed
  split_loc  <- make_even_split(df_long_local,  seed = s)
  split_pris <- make_even_split(df_long_prism,  seed = s)
  
  # Tl/Tu menus per dataset for this seed
  menu_loc <- compute_threshold_menu(split_loc,  dataset = "Local")
  menu_pri <- compute_threshold_menu(split_pris, dataset = "PRISM")
  
  # Local: min/max use Tmin_useC/Tmax_useC
  preds_loc <- build_preds_for_dataset(
    split_df     = split_loc,
    dataset_label = "Local",
    menu_ds      = menu_loc,
    min_col      = "Tmin_useC",
    max_col      = "Tmax_useC",
    seed         = s,
    run_id       = i
  )
  
  # PRISM: min/max use PRISMMinTempC/PRISMMaxTempC
  preds_pri <- build_preds_for_dataset(
    split_df     = split_pris,
    dataset_label = "PRISM",
    menu_ds      = menu_pri,
    min_col      = "PRISMMinTempC",
    max_col      = "PRISMMaxTempC",
    seed         = s,
    run_id       = i
  )
  
  dplyr::bind_rows(preds_loc, preds_pri)
})

# Save prediction cache used by Section 11
pred_cache_fp <- file.path(OUT_DIR, "FINAL_predictions_perRun.csv")
readr::write_csv(predictions_perRun, pred_cache_fp)
message("✓ Wrote prediction cache: ", normalizePath(pred_cache_fp))

# ===============================================================
# 11) FIGURES — Obs vs Pred (TRAIN / CALIBRATE / VALIDATE), 5 seeds pooled
#       Panels: Base / Universal tuned / Universal rounded / OPT × Tb=7..10
#       Train & Calibrate: compute predictions here (per-Tb OPT refit)
#       Validate: read the cache from Part 10e (FINAL_predictions_perRun.csv)
# ===============================================================

suppressPackageStartupMessages({
  library(ggplot2); library(readr); library(glue)
  library(dplyr); library(purrr); library(tidyr); library(stringr)
})

# ---- 11.1 Generic predictors for arbitrary Set ("Train" / "Calibrate") ----
predict_set_base <- function(split_df, min_col, max_col, Tb, greq_vec,
                             dataset_label, family_label, model_label,
                             set_nm, seed, run_id) {
  envs <- split_df %>% dplyr::filter(Set == set_nm) %>% dplyr::group_split(Variety, Location, Year)
  purrr::map_dfr(envs, function(d) {
    v <- unique(d$Variety); g <- greq_vec[[v]]; if (!is.finite(g)) return(tibble())
    inc <- daily_dd_base_cols(d[[min_col]], d[[max_col]], Tb)
    hit <- which(cumsum(inc) >= g)[1]
    tibble(
      Obs  = unique(d$DaysToHeading),
      Pred = ifelse(is.na(hit), NA_real_, d$Days_After_Planting[hit]),
      Variety  = v, Location = unique(d$Location), Year = unique(d$Year),
      Dataset  = dataset_label, Family = family_label, Model = model_label,
      Tb = Tb, seed = seed, run = run_id, Set = set_nm
    )
  }) %>% dplyr::filter(is.finite(Obs) & is.finite(Pred))
}

predict_set_universal <- function(split_df, min_col, max_col, Tl, Tu, Tb, greq_vec,
                                  dataset_label, family_label, model_label,
                                  set_nm, seed, run_id) {
  envs <- split_df %>% dplyr::filter(Set == set_nm) %>% dplyr::group_split(Variety, Location, Year)
  purrr::map_dfr(envs, function(d) {
    v <- unique(d$Variety); g <- greq_vec[[v]]; if (!is.finite(g)) return(tibble())
    tibble(
      Obs  = unique(d$DaysToHeading),
      Pred = predict_env_gdd_cols(d, min_col, max_col, Tl, Tu, Tb, g),
      Variety  = v, Location = unique(d$Location), Year = unique(d$Year),
      Dataset  = dataset_label, Family = family_label, Model = model_label,
      Tb = Tb, seed = seed, run = run_id, Set = set_nm
    )
  }) %>% dplyr::filter(is.finite(Obs) & is.finite(Pred))
}

predict_set_opt <- function(split_df, min_col, max_col, Tb, pervar_tbl,
                            dataset_label, family_label, model_label,
                            set_nm, seed, run_id) {
  envs <- split_df %>% dplyr::filter(Set == set_nm) %>% dplyr::group_split(Variety, Location, Year)
  purrr::map_dfr(envs, function(d) {
    v <- unique(d$Variety)
    row <- pervar_tbl %>% dplyr::filter(Variety == v)
    if (!nrow(row)) return(tibble())
    tibble(
      Obs  = unique(d$DaysToHeading),
      Pred = predict_env_gdd_cols(d, min_col, max_col,
                                  Tl = row$Tl, Tu = row$Tu, Tb = Tb, Greq = row$GDDreq),
      Variety  = v, Location = unique(d$Location), Year = unique(d$Year),
      Dataset  = dataset_label, Family = family_label, Model = model_label,
      Tb = Tb, seed = seed, run = run_id, Set = set_nm
    )
  }) %>% dplyr::filter(is.finite(Obs) & is.finite(Pred))
}

# ---- 11.2 Build predictions for a dataset & set ("Train" / "Calibrate") ---
Tb_grid_pred <- 7:10

build_preds_for_dataset_set <- function(split_df, dataset_label, menu_ds,
                                        min_col, max_col, seed, run_id, set_nm) {
  out <- list()
  # dataset-specific rounded thresholds
  if (dataset_label == "Local") {
    Tl_r <- Local_fixed_round["Tl"]; Tu_r <- Local_fixed_round["Tu"]
  } else {
    Tl_r <- PRISM_fixed_round["Tl"]; Tu_r <- PRISM_fixed_round["Tu"]
  }
  tuned_pair <- menu_ds$tuned
  
  for (tb in Tb_grid_pred) {
    # Base greq (from TRAIN only)
    greq_base <- compute_greq_base(split_df, min_col, max_col, Tb = tb)
    
    base_pred <- predict_set_base(
      split_df, min_col, max_col, Tb = tb, greq_vec = greq_base,
      dataset_label = dataset_label, family_label = "Base",
      model_label = glue("Base Tb={tb} ({dataset_label})"),
      set_nm = set_nm, seed = seed, run_id = run_id
    )
    
    # Universal tuned
    greq_tuned <- compute_greq_universal(split_df, min_col, max_col,
                                         Tl = tuned_pair["Tl"], Tu = tuned_pair["Tu"], Tb = tb)
    uni_tuned <- predict_set_universal(
      split_df, min_col, max_col, Tl = tuned_pair["Tl"], Tu = tuned_pair["Tu"], Tb = tb,
      greq_vec = greq_tuned,
      dataset_label = dataset_label, family_label = "Universal tuned",
      model_label = glue("Universal tuned (Tb={tb}, {dataset_label})"),
      set_nm = set_nm, seed = seed, run_id = run_id
    )
    
    # Universal rounded
    greq_round <- compute_greq_universal(split_df, min_col, max_col, Tl = Tl_r, Tu = Tu_r, Tb = tb)
    uni_round <- predict_set_universal(
      split_df, min_col, max_col, Tl = Tl_r, Tu = Tu_r, Tb = tb, greq_vec = greq_round,
      dataset_label = dataset_label, family_label = "Universal rounded",
      model_label = glue("Universal rounded (Tb={tb}, {dataset_label})"),
      set_nm = set_nm, seed = seed, run_id = run_id
    )
    
    # OPT per-var (refit per Tb)
    vars <- sort(unique(split_df$Variety))
    pervar_tbl_tb <- run_gdd_pervar_generic(split_df, min_col, max_col,
                                            varieties = vars, lambda = 0.2, Tb = tb, restarts = 6)
    opt_pred <- predict_set_opt(
      split_df, min_col, max_col, Tb = tb, pervar_tbl = pervar_tbl_tb,
      dataset_label = dataset_label, family_label = "OPT",
      model_label = glue("OPT per-var (Tb={tb}, {dataset_label})"),
      set_nm = set_nm, seed = seed, run_id = run_id
    )
    
    out[[length(out) + 1]] <- dplyr::bind_rows(base_pred, uni_tuned, uni_round, opt_pred)
  }
  dplyr::bind_rows(out)
}

# ---- 11.3 TRAIN/CALIBRATE predictions (VALIDATE comes from cache) ---------
pred_cache_val_fp <- file.path(OUT_DIR, "FINAL_predictions_perRun.csv")
stopifnot(file.exists(pred_cache_val_fp))
preds_validate <- readr::read_csv(pred_cache_val_fp, show_col_types = FALSE)

predictions_train <- purrr::map2_dfr(SEEDS, seq_along(SEEDS), function(s, i) {
  split_loc  <- make_even_split(df_long_local,  seed = s)
  split_pris <- make_even_split(df_long_prism,  seed = s)
  menu_loc <- compute_threshold_menu(split_loc,  dataset = "Local")
  menu_pri <- compute_threshold_menu(split_pris, dataset = "PRISM")
  
  preds_loc <- build_preds_for_dataset_set(split_loc,  "Local", menu_loc,
                                           "Tmin_useC",      "Tmax_useC",      s, i, set_nm = "Train")
  preds_pri <- build_preds_for_dataset_set(split_pris, "PRISM", menu_pri,
                                           "PRISMMinTempC",  "PRISMMaxTempC",  s, i, set_nm = "Train")
  dplyr::bind_rows(preds_loc, preds_pri)
})

predictions_cal <- purrr::map2_dfr(SEEDS, seq_along(SEEDS), function(s, i) {
  split_loc  <- make_even_split(df_long_local,  seed = s)
  split_pris <- make_even_split(df_long_prism,  seed = s)
  menu_loc <- compute_threshold_menu(split_loc,  dataset = "Local")
  menu_pri <- compute_threshold_menu(split_pris, dataset = "PRISM")
  
  preds_loc <- build_preds_for_dataset_set(split_loc,  "Local", menu_loc,
                                           "Tmin_useC",      "Tmax_useC",      s, i, set_nm = "Calibrate")
  preds_pri <- build_preds_for_dataset_set(split_pris, "PRISM", menu_pri,
                                           "PRISMMinTempC",  "PRISMMaxTempC",  s, i, set_nm = "Calibrate")
  dplyr::bind_rows(preds_loc, preds_pri)
})

# ---- 11.4 Write TRAIN/CALIBRATE caches for reproducibility ----------------
pred_train_fp <- file.path(OUT_DIR, "FINAL_predictions_perRun_TRAIN.csv")
pred_cal_fp   <- file.path(OUT_DIR, "FINAL_predictions_perRun_CALIBRATE.csv")
readr::write_csv(predictions_train, pred_train_fp)
readr::write_csv(predictions_cal,   pred_cal_fp)
message("✓ Wrote TRAIN prediction cache: ", normalizePath(pred_train_fp))
message("✓ Wrote CALIBRATE prediction cache: ", normalizePath(pred_cal_fp))

# ---- 11.5 Shared facet metric + plotting helper ---------------------------
facet_metrics <- function(df_subset){
  df_subset %>%
    dplyr::group_by(Family, Tb) %>%
    dplyr::summarise(
      RMSE = sqrt(mean((Pred - Obs)^2, na.rm = TRUE)),
      Bias = mean(Pred - Obs, na.rm = TRUE),
      R2   = {
        sse <- sum((Pred - Obs)^2, na.rm = TRUE)
        sst <- sum((Obs - mean(Obs, na.rm = TRUE))^2, na.rm = TRUE)
        ifelse(sst == 0, NA_real_, 1 - sse/sst)
      },
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(c(RMSE, Bias, R2), ~ round(.x, 2)))
}

plot_obs_pred_byset <- function(pred_tbl, ds_label, set_label, title_suffix){
  df <- pred_tbl %>%
    dplyr::filter(Dataset == ds_label) %>%
    dplyr::mutate(
      Family  = factor(Family, levels = c("Base","Universal tuned","Universal rounded","OPT")),
      Tb      = factor(Tb,     levels = c(7,8,9,10)),
      Variety = factor(Variety, levels = target_varieties)
    )
  ann <- facet_metrics(df)
  xr <- range(df$Obs,  na.rm = TRUE)
  yr <- range(df$Pred, na.rm = TRUE)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(Obs, Pred, shape = Variety)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.6) +
    ggplot2::geom_point(alpha = 0.35, size = 1.6,
                        position = ggplot2::position_jitter(width = 0.2, height = 0.2)) +
    ggplot2::facet_grid(Family ~ Tb, drop = FALSE,
                        labeller = ggplot2::labeller(Tb = function(x) paste0("Tb=", x))) +
    ggplot2::scale_shape_manual(
      breaks = target_varieties,
      values = c(M105=16, M206=17, M209=15, M210=3, M211=0),
      drop = FALSE
    ) +
    ggplot2::geom_text(
      data = ann, inherit.aes = FALSE,
      ggplot2::aes(x = xr[1], y = yr[2],
                   label = paste0("RMSE=", RMSE, "\nBias=", Bias, "\nR²=", R2)),
      hjust = -0.02, vjust = 1.02, size = 3
    ) +
    ggplot2::coord_cartesian(xlim = xr, ylim = yr, clip = "on") +
    ggplot2::labs(
      title = glue("Observed vs Predicted ({title_suffix}) — pooled across 5 runs [{ds_label}]"),
      x = "Observed DTH (days)", y = "Predicted DTH (days)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text.y = ggplot2::element_text(face = "bold"),
      strip.text.x = ggplot2::element_text(face = "bold"),
      plot.title   = ggplot2::element_text(hjust = 0.5),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
  
  # Keep Validate filenames consistent with earlier outputs
  out_name <- if (toupper(title_suffix) == "VALIDATE")
    glue("FINAL_{ds_label}_allRuns_obsVpred.pdf")
  else
    glue("FINAL_{ds_label}_{toupper(title_suffix)}_allRuns_obsVpred.pdf")
  
  out_pdf <- file.path(OUT_DIR, out_name)
  ggplot2::ggsave(filename = out_pdf, plot = p, width = 11, height = 8.5, units = "in", dpi = 300)
  message("✓ Wrote ", title_suffix, " figure: ", normalizePath(out_pdf))
}

# ---- 11.6 Plot ALL three sets (Local & PRISM) -----------------------------
# Train
plot_obs_pred_byset(predictions_train, "Local", "TRAIN", "TRAIN")
plot_obs_pred_byset(predictions_train, "PRISM", "TRAIN", "TRAIN")

# Calibrate
plot_obs_pred_byset(predictions_cal, "Local", "CALIBRATE", "CALIBRATE")
plot_obs_pred_byset(predictions_cal, "PRISM", "CALIBRATE", "CALIBRATE")

# Validate (from cache)
# Derive Family/Tb/Dataset if missing (robust to CSV variations)
preds_validate_clean <- preds_validate %>%
  mutate(
    Family = case_when(
      str_detect(Model, regex("^Base\\b", ignore_case = TRUE)) ~ "Base",
      str_detect(Model, regex("\\bUniversal tuned\\b",   ignore_case = TRUE)) ~ "Universal tuned",
      str_detect(Model, regex("\\bUniversal rounded\\b", ignore_case = TRUE)) ~ "Universal rounded",
      str_detect(Model, regex("^OPT\\b",  ignore_case = TRUE)) ~ "OPT",
      TRUE ~ Family    # keep if already present
    ),
    Tb = coalesce(Tb, readr::parse_number(Model)),
    Dataset = coalesce(
      Dataset,
      case_when(
        str_detect(Model, regex("\\bPRISM\\b", ignore_case = TRUE)) ~ "PRISM",
        str_detect(Model, regex("\\bLocal\\b", ignore_case = TRUE)) ~ "Local",
        TRUE ~ Dataset
      )
    )
  ) %>%
  filter(!is.na(Family), Tb %in% 7:10, !is.na(Dataset))

plot_obs_pred_byset(preds_validate_clean, "Local", "VALIDATE", "VALIDATE")
plot_obs_pred_byset(preds_validate_clean, "PRISM", "VALIDATE", "VALIDATE")

# ===============================================================
# 11B) FIGURES — Universal rounded (Tb = 7):
#      Local(11/34) vs PRISM(12/33), side-by-side
#      One plot each for TRAIN, CALIBRATE, VALIDATE (5 seeds pooled)
#      (Place this before Section 11C)
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(glue); library(tidyr); library(stringr)
})

# --- Helper: pooled metrics per Dataset for annotations --------------------
ann_metrics_by_dataset <- function(df){
  df %>%
    group_by(Dataset) %>%
    summarise(
      RMSE = sqrt(mean((Pred - Obs)^2, na.rm = TRUE)),
      Bias = mean(Pred - Obs, na.rm = TRUE),
      R2   = {
        sse <- sum((Pred - Obs)^2, na.rm = TRUE)
        sst <- sum((Obs - mean(Obs, na.rm = TRUE))^2, na.rm = TRUE)
        ifelse(sst == 0, NA_real_, 1 - sse/sst)
      },
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    mutate(across(c(RMSE, Bias, R2), ~ round(.x, 2)))
}

# --- Helper: shared two-panel plot (Local vs PRISM) ------------------------
plot_rounded_tb7_byset <- function(pred_tbl, set_label){
  df <- pred_tbl %>%
    dplyr::filter(Family == "Universal rounded", Tb == 7) %>%
    dplyr::mutate(
      Variety = factor(Variety, levels = target_varieties),
      Dataset = factor(Dataset, levels = c("Local","PRISM"))
    )
  
  # Fixed symmetric limits for both axes
  lims <- c(70, 105)
  
  # Annotations per dataset (place in top-right corner of lims box)
  ann <- ann_metrics_by_dataset(df) %>%
    dplyr::mutate(
      x_pos = lims[1] + 1.5,  # a little right of the left edge
      y_pos = lims[2] - 1.5   # a little below the top edge
    )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Obs, y = Pred, shape = Variety)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.6) +
    ggplot2::geom_point(alpha = 0.4, size = 1.6,
                        position = ggplot2::position_jitter(width = 0.2, height = 0.2)) +
    ggplot2::facet_wrap(~ Dataset, nrow = 1) +
    ggplot2::scale_shape_manual(
      breaks = target_varieties,
      values = c(M105 = 16, M206 = 17, M209 = 15, M210 = 3, M211 = 0),
      drop = FALSE
    ) +
    ggplot2::geom_text(
      data = ann, inherit.aes = FALSE,
      ggplot2::aes(x = x_pos, y = y_pos,
                   label = paste0("RMSE=", RMSE,
                                  "\nBias=", Bias,
                                  "\nR²=", R2)),
      hjust = -0.02, vjust = 1.02, size = 3
    ) +
    # >>> fixed x/y ranges + no padding + 1:1 aspect <<<
    ggplot2::scale_x_continuous(limits = lims, expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::scale_y_continuous(limits = lims, expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = glue("Observed vs Predicted DTH — Universal rounded (Tb=7) [{set_label}]"),
      subtitle = "Local Tl/Tu = 11/34 vs PRISM Tl/Tu = 12/33 (5 seeds pooled)",
      x = "Observed DTH (days)", y = "Predicted DTH (days)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text   = ggplot2::element_text(face = "bold"),
      plot.title   = ggplot2::element_text(hjust = 0.5),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
  
  out_pdf <- file.path(OUT_DIR, glue("FINAL_Rounded_Tb7_{toupper(set_label)}_Local_vs_PRISM.pdf"))
  out_png <- file.path(OUT_DIR, glue("FINAL_Rounded_Tb7_{toupper(set_label)}_Local_vs_PRISM.png"))
  ggsave(out_pdf, p, width = 8.5, height = 5.5, units = "in", dpi = 300)
  ggsave(out_png, p, width = 8.5, height = 5.5, units = "in", dpi = 300, bg = "white")
  message("✓ Wrote Universal rounded Tb=7 plot (", set_label, "): ", normalizePath(out_pdf))
}



# --- Build TRAIN/CALIBRATE sources from the objects already created in 11 ----
# predictions_train and predictions_cal were built in Section 11.3
rounded_train <- predictions_train %>%
  filter(Family == "Universal tuned" | Family == "Universal rounded") %>% # keep safe if minor naming variations
  mutate(Family = ifelse(str_detect(Model, regex("Universal rounded", ignore_case = TRUE)), 
                         "Universal rounded", Family)) %>%
  filter(Family == "Universal rounded", Tb == 7)

rounded_cal <- predictions_cal %>%
  filter(Family == "Universal tuned" | Family == "Universal rounded") %>%
  mutate(Family = ifelse(str_detect(Model, regex("Universal rounded", ignore_case = TRUE)), 
                         "Universal rounded", Family)) %>%
  filter(Family == "Universal rounded", Tb == 7)

# --- VALIDATE source from the cache made in 10e / cleaned in 11.6 ----------
rounded_val <- preds_validate_clean %>%
  filter(Family == "Universal rounded", Tb == 7)

# --- Make the three plots ---------------------------------------------------
plot_rounded_tb7_byset(rounded_train, "Train")
plot_rounded_tb7_byset(rounded_cal,   "Calibrate")
plot_rounded_tb7_byset(rounded_val,   "Validate")




# --- 11B-extra) VALIDATE side-by-side (Universal rounded, Tb=7) — NO TITLE ---
suppressPackageStartupMessages({ library(ggplot2); library(glue); library(dplyr) })

# If rounded_val doesn't exist yet, build it from the validate cache cleaned in 11.6
if (!exists("rounded_val")) {
  rounded_val <- preds_validate_clean %>%
    dplyr::filter(Family == "Universal rounded", Tb == 7)
}

# Shared limits
# Fixed symmetric limits
lims <- c(70, 105)

ann_val <- rounded_val %>%
  dplyr::group_by(Dataset) %>%
  dplyr::summarise(
    RMSE = sqrt(mean((Pred - Obs)^2, na.rm = TRUE)),
    Bias = mean(Pred - Obs, na.rm = TRUE),
    R2   = {
      sse <- sum((Pred - Obs)^2, na.rm = TRUE)
      sst <- sum((Obs - mean(Obs, na.rm = TRUE))^2, na.rm = TRUE)
      ifelse(sst == 0, NA_real_, 1 - sse/sst)
    },
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    RMSE = round(RMSE, 2),
    Bias = round(Bias, 2),
    R2   = round(R2,   2),
    x_pos = lims[1] + 1.5,
    y_pos = lims[2] - 1.5
  )

p_val_notitle <- ggplot2::ggplot(
  rounded_val %>%
    dplyr::mutate(
      Variety = factor(Variety, levels = target_varieties),
      Dataset = factor(Dataset, levels = c("Local","PRISM"))
    ),
  ggplot2::aes(x = Obs, y = Pred, shape = Variety)
) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.6) +
  ggplot2::geom_point(alpha = 0.4, size = 1.6,
                      position = ggplot2::position_jitter(width = 0.2, height = 0.2)) +
  ggplot2::facet_wrap(~ Dataset, nrow = 1) +
  ggplot2::scale_shape_manual(
    breaks = target_varieties,
    values = c(M105 = 16, M206 = 17, M209 = 15, M210 = 3, M211 = 0),
    drop = FALSE
  ) +
  ggplot2::geom_text(
    data = ann_val, inherit.aes = FALSE,
    ggplot2::aes(x = x_pos, y = y_pos,
                 label = paste0("RMSE=", RMSE,
                                "\nBias=", Bias,
                                "\nR²=", R2)),
    hjust = -0.02, vjust = 1.02, size = 3
  ) +
  # >>> fixed x/y ranges + no padding + 1:1 aspect <<<
  ggplot2::scale_x_continuous(limits = lims, expand = ggplot2::expansion(mult = c(0, 0))) +
  ggplot2::scale_y_continuous(limits = lims, expand = ggplot2::expansion(mult = c(0, 0))) +
  ggplot2::coord_equal() +
  ggplot2::labs(title = NULL, subtitle = NULL,
                x = "Observed DTH (days)", y = "Predicted DTH (days)") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.text   = ggplot2::element_text(face = "bold"),
    axis.title.x = ggplot2::element_text(face = "bold"),
    axis.title.y = ggplot2::element_text(face = "bold"),
    legend.position = "right"
  )


out_pdf <- file.path(OUT_DIR, "FINAL_Rounded_Tb7_VALIDATE_Local_vs_PRISM_NOTITLE.pdf")
out_png <- file.path(OUT_DIR, "FINAL_Rounded_Tb7_VALIDATE_Local_vs_PRISM_NOTITLE.png")
ggplot2::ggsave(out_pdf, p_val_notitle, width = 8.5, height = 5.5, units = "in", dpi = 300)
ggplot2::ggsave(out_png, p_val_notitle, width = 8.5, height = 5.5, units = "in", dpi = 300, bg = "white")
message("✓ Wrote Universal rounded Tb=7 VALIDATE (no-title): ", normalizePath(out_pdf))





# ===============================================================
# 11C) FIGURES — Shared Tl/Tu = 11/33 across TRAIN / CALIBRATE / VALIDATE
#       • Universal (shared) thresholds for BOTH datasets
#       • Tb grid = 7:10; 5 seeds pooled; OPT not used here
#       • Facets: (Family="Shared 11/33") × Tb
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(glue)
  library(readr); library(tidyr); library(stringr); library(ggplot2)
})

# ---- settings --------------------------------------------------------------
Tl_shared <- 11
Tu_shared <- 33
Tb_grid_shared <- 7:10

# ---- helpers (greq on TRAIN only; universal shared Tl/Tu) ------------------
compute_greq_universal_shared <- function(split_df, min_col, max_col, Tb, Tl=Tl_shared, Tu=Tu_shared){
  vars    <- sort(unique(split_df$Variety))
  envs_tr <- split_df %>% dplyr::filter(Set=="Train") %>% dplyr::group_split(Variety, Location, Year)
  greq <- setNames(rep(NA_real_, length(vars)), vars)
  for (v in vars){
    ets <- purrr::keep(envs_tr, ~ unique(.x$Variety)==v)
    if (!length(ets)) next
    greq[v] <- mean(purrr::map_dbl(ets, function(d){
      dth <- unique(d$DaysToHeading)
      sum(daily_gdd_cols(
        d[[min_col]][d$Days_After_Planting<=dth],
        d[[max_col]][d$Days_After_Planting<=dth],
        Tb, Tl, Tu
      ))
    }), na.rm=TRUE)
  }
  greq
}

predict_set_universal_shared <- function(split_df, min_col, max_col, Tb, greq_vec,
                                         dataset_label, set_nm, seed, run_id){
  envs <- split_df %>% dplyr::filter(Set == set_nm) %>% dplyr::group_split(Variety, Location, Year)
  purrr::map_dfr(envs, function(d){
    v <- unique(d$Variety); loc <- unique(d$Location); yr <- unique(d$Year)
    g <- greq_vec[[v]]; if (!is.finite(g)) return(tibble())
    tibble(
      Obs      = unique(d$DaysToHeading),
      Pred     = predict_env_gdd_cols(d, min_col, max_col, Tl=Tl_shared, Tu=Tu_shared, Tb=Tb, Greq=g),
      Variety  = v,
      Location = loc,
      Year     = yr,
      Dataset  = dataset_label,
      Family   = "Shared 11/33",
      Model    = glue("Shared Tl/Tu (11/33), Tb={Tb}"),
      Tb       = Tb,
      seed     = seed,
      run      = run_id,
      Set      = set_nm
    )
  }) %>% tidyr::drop_na(Obs, Pred)
}

# ---- build per-seed predictions for all sets (Local + PRISM) --------------
preds_shared_allsets <- purrr::map2_dfr(SEEDS, seq_along(SEEDS), function(s, i){
  # per-seed splits
  split_loc  <- make_even_split(df_long_local,  seed = s)
  split_pris <- make_even_split(df_long_prism,  seed = s)
  
  # column names
  min_loc  <- "Tmin_useC";      max_loc  <- "Tmax_useC"
  min_pris <- "PRISMMinTempC";  max_pris <- "PRISMMaxTempC"
  
  purrr::map_dfr(Tb_grid_shared, function(tb_val){
    # greq from TRAIN once per dataset & Tb
    greq_loc  <- compute_greq_universal_shared(split_loc,  min_loc,  max_loc,  Tb=tb_val)
    greq_pris <- compute_greq_universal_shared(split_pris, min_pris, max_pris, Tb=tb_val)
    
    purrr::map_dfr(c("Train","Calibrate","Validate"), function(set_nm){
      bind_rows(
        predict_set_universal_shared(split_loc,  min_loc,  max_loc,  Tb=tb_val, greq_vec=greq_loc,
                                     dataset_label="Local", set_nm=set_nm, seed=s, run_id=i),
        predict_set_universal_shared(split_pris, min_pris, max_pris, Tb=tb_val, greq_vec=greq_pris,
                                     dataset_label="PRISM", set_nm=set_nm, seed=s, run_id=i)
      )
    })
  })
})

# Save cache (handy for re-plotting quickly)
shared_allsets_fp <- file.path(OUT_DIR, "FINAL_predictions_perRun_SHARED_11_33_allSets.csv")
readr::write_csv(preds_shared_allsets, shared_allsets_fp)
message("✓ Wrote Shared(11/33) all-sets cache: ", normalizePath(shared_allsets_fp))

# ---- metrics + plotting helper (same look as Part 11) ---------------------
facet_metrics_shared <- function(df_subset){
  df_subset %>%
    dplyr::group_by(Family, Tb) %>%
    dplyr::summarise(
      RMSE = sqrt(mean((Pred - Obs)^2, na.rm = TRUE)),
      Bias = mean(Pred - Obs, na.rm = TRUE),
      R2   = {
        sse <- sum((Pred - Obs)^2, na.rm = TRUE)
        sst <- sum((Obs - mean(Obs, na.rm = TRUE))^2, na.rm = TRUE)
        ifelse(sst == 0, NA_real_, 1 - sse/sst)
      },
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(c(RMSE, Bias, R2), ~ round(.x, 2)))
}

plot_obs_pred_shared_byset <- function(pred_tbl, ds_label, set_label){
  df <- pred_tbl %>%
    dplyr::filter(Dataset == ds_label, Set == set_label) %>%
    dplyr::mutate(
      Family  = factor(Family, levels = c("Shared 11/33")),
      Tb      = factor(Tb,     levels = c(7,8,9,10)),
      Variety = factor(Variety, levels = target_varieties)
    )
  ann <- facet_metrics_shared(df)
  xr <- range(df$Obs,  na.rm = TRUE)
  yr <- range(df$Pred, na.rm = TRUE)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(Obs, Pred, shape = Variety)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.6) +
    ggplot2::geom_point(alpha = 0.35, size = 1.6,
                        position = ggplot2::position_jitter(width = 0.2, height = 0.2)) +
    ggplot2::facet_grid(Family ~ Tb, drop = FALSE,
                        labeller = ggplot2::labeller(Tb = function(x) paste0("Tb=", x))) +
    ggplot2::scale_shape_manual(
      breaks = target_varieties,
      values = c(M105=16, M206=17, M209=15, M210=3, M211=0),
      drop = FALSE
    ) +
    ggplot2::geom_text(
      data = ann, inherit.aes = FALSE,
      ggplot2::aes(x = xr[1], y = yr[2],
                   label = paste0("RMSE=", RMSE, "\nBias=", Bias, "\nR²=", R2)),
      hjust = -0.02, vjust = 1.02, size = 3
    ) +
    ggplot2::coord_cartesian(xlim = xr, ylim = yr, clip = "on") +
    ggplot2::labs(
      title = glue("Observed vs Predicted — Shared Tl/Tu = 11/33 ({set_label}) [{ds_label}]"),
      x = "Observed DTH (days)", y = "Predicted DTH (days)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text.y = ggplot2::element_text(face = "bold"),
      strip.text.x = ggplot2::element_text(face = "bold"),
      plot.title   = ggplot2::element_text(hjust = 0.5),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
  
  out_pdf <- file.path(OUT_DIR, glue("FINAL_{ds_label}_SHARED11_33_{toupper(set_label)}_obsVpred.pdf"))
  ggplot2::ggsave(filename = out_pdf, plot = p, width = 11, height = 8.5, units = "in", dpi = 300)
  message("✓ Wrote Shared(11/33) {set_label} figure: ", normalizePath(out_pdf))
}

# ---- build all 6 plots (Local/PRISM × Train/Calibrate/Validate) ----------
plot_obs_pred_shared_byset(preds_shared_allsets, "Local", "Train")
plot_obs_pred_shared_byset(preds_shared_allsets, "PRISM", "Train")
plot_obs_pred_shared_byset(preds_shared_allsets, "Local", "Calibrate")
plot_obs_pred_shared_byset(preds_shared_allsets, "PRISM", "Calibrate")
plot_obs_pred_shared_byset(preds_shared_allsets, "Local", "Validate")
plot_obs_pred_shared_byset(preds_shared_allsets, "PRISM", "Validate")




# ===============================================================
# 11B-SWAP) OBS vs PRED — Universal (Tb=7) with SWAPPED Tl/Tu
#   • Local uses PRISM thresholds 12/33
#   • PRISM uses Local thresholds 11/34
#   • Uses TRAIN-only greq per variety; predicts Train/Calibrate/Validate
#   • Outputs:
#       - FINAL_predictions_perRun_SWAPPED_Rounded_Tb7_allSets.csv
#       - FINAL_SWAPPED_Rounded_Tb7_metrics_bySet.csv
#       - FINAL_RoundedSWAP_Tb7_VALIDATE_Local_vs_PRISM_NOTITLE.pdf
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(glue); library(tidyr); library(stringr); library(ggplot2); library(readr)
})

# ---- Fixed SWAP thresholds and Tb -----------------------------------------
Tl_local_SWAP  <- 12; Tu_local_SWAP  <- 33  # Local uses PRISM thresholds
Tl_prism_SWAP  <- 11; Tu_prism_SWAP  <- 34  # PRISM uses Local thresholds
Tb_SWAP        <- 7
lims_fixed     <- c(70, 105)                # fixed axes for the plots

# ---- Helpers (TRAIN-only greq and set predictors; reuse names for clarity) -
compute_greq_universal_trainOnly <- function(split_df, min_col, max_col, Tl, Tu, Tb) {
  vars    <- sort(unique(split_df$Variety))
  envs_tr <- split_df %>% dplyr::filter(Set == "Train") %>% dplyr::group_split(Variety, Location, Year)
  greq <- setNames(rep(NA_real_, length(vars)), vars)
  for (v in vars) {
    ets <- purrr::keep(envs_tr, ~ unique(.x$Variety) == v)
    if (!length(ets)) next
    greq[v] <- mean(purrr::map_dbl(ets, function(d) {
      dth <- unique(d$DaysToHeading)
      # daily_gdd_cols defined earlier in your script; keep the call signature
      sum(daily_gdd_cols(
        d[[min_col]][d$Days_After_Planting <= dth],
        d[[max_col]][d$Days_After_Planting <= dth],
        Tb, Tl, Tu
      ))
    }), na.rm = TRUE)
  }
  greq
}

predict_set_universal_swapped <- function(split_df, min_col, max_col, Tl, Tu, Tb,
                                          greq_vec, dataset_label, set_nm, seed, run_id) {
  envs <- split_df %>% dplyr::filter(Set == set_nm) %>% dplyr::group_split(Variety, Location, Year)
  purrr::map_dfr(envs, function(d) {
    v   <- unique(d$Variety); loc <- unique(d$Location); yr <- unique(d$Year)
    g   <- greq_vec[[v]]; if (!is.finite(g)) return(tibble())
    tibble(
      Obs      = unique(d$DaysToHeading),
      Pred     = predict_env_gdd_cols(d, min_col, max_col, Tl, Tu, Tb, g),
      Variety  = v, Location = loc, Year = yr,
      Dataset  = dataset_label,
      Family   = "Universal SWAP",
      Model    = glue("Universal SWAP (Tl/Tu={Tl}/{Tu}, Tb={Tb})"),
      Tb       = Tb, seed = seed, run = run_id, Set = set_nm
    )
  }) %>% tidyr::drop_na(Obs, Pred)
}

# ---- Build per-seed predictions for Train/Calibrate/Validate --------------
swapped_preds_all <- purrr::map2_dfr(SEEDS, seq_along(SEEDS), function(s, i) {
  split_loc  <- make_even_split(df_long_local,  seed = s)  # has Tmin_useC/Tmax_useC (with PRISM fallback)
  split_pris <- make_even_split(df_long_prism,  seed = s)
  
  # columns
  min_loc <- "Tmin_useC";      max_loc <- "Tmax_useC"
  min_pri <- "PRISMMinTempC";  max_pri <- "PRISMMaxTempC"
  
  # TRAIN-only greq for each dataset under SWAP thresholds
  greq_loc  <- compute_greq_universal_trainOnly(split_loc,  min_loc,  max_loc,  Tl = Tl_local_SWAP, Tu = Tu_local_SWAP, Tb = Tb_SWAP)
  greq_pris <- compute_greq_universal_trainOnly(split_pris, min_pri,  max_pri,  Tl = Tl_prism_SWAP, Tu = Tu_prism_SWAP, Tb = Tb_SWAP)
  
  # Predict each Set
  purrr::map_dfr(c("Train","Calibrate","Validate"), function(set_nm) {
    bind_rows(
      predict_set_universal_swapped(split_loc,  min_loc, max_loc, Tl_local_SWAP, Tu_local_SWAP, Tb_SWAP,
                                    greq_vec = greq_loc,  dataset_label = "Local", set_nm = set_nm, seed = s, run_id = i),
      predict_set_universal_swapped(split_pris, min_pri,  max_pri, Tl_prism_SWAP, Tu_prism_SWAP, Tb_SWAP,
                                    greq_vec = greq_pris, dataset_label = "PRISM", set_nm = set_nm, seed = s, run_id = i)
    )
  })
})

# Cache the per-run predictions
out_swapped_preds <- file.path(OUT_DIR, "FINAL_predictions_perRun_SWAPPED_Rounded_Tb7_allSets.csv")
readr::write_csv(swapped_preds_all, out_swapped_preds)
message("✓ Wrote: ", normalizePath(out_swapped_preds))

# ---- Metrics by Dataset × Set (pooled over runs) --------------------------
swapped_metrics <- swapped_preds_all %>%
  group_by(Dataset, Set) %>%
  summarise(
    RMSE = sqrt(mean((Pred - Obs)^2, na.rm = TRUE)),
    Bias = mean(Pred - Obs, na.rm = TRUE),
    R2   = {
      sse <- sum((Pred - Obs)^2, na.rm = TRUE)
      sst <- sum((Obs - mean(Obs, na.rm = TRUE))^2, na.rm = TRUE)
      ifelse(sst == 0, NA_real_, 1 - sse/sst)
    },
    n    = n(),
    .groups = "drop"
  ) %>%
  mutate(across(c(RMSE, Bias, R2), ~ round(.x, 2)))

out_swapped_metrics <- file.path(OUT_DIR, "FINAL_SWAPPED_Rounded_Tb7_metrics_bySet.csv")
readr::write_csv(swapped_metrics, out_swapped_metrics)
print(swapped_metrics)
message("✓ Wrote: ", normalizePath(out_swapped_metrics))

# ---- OBS vs PRED side-by-side plots for all Sets (NO TITLE) ----------------
make_swapped_plot <- function(preds_all, set_nm,
                              lims = lims_fixed,
                              x_annot = 75, y_annot = 100) {
  df_set <- preds_all %>%
    dplyr::filter(Set == set_nm) %>%
    dplyr::mutate(
      Variety = factor(Variety, levels = target_varieties),
      Dataset = factor(Dataset, levels = c("Local","PRISM"))
    )
  
  ann_set <- df_set %>%
    dplyr::group_by(Dataset) %>%
    dplyr::summarise(
      RMSE = sqrt(mean((Pred - Obs)^2, na.rm = TRUE)),
      Bias = mean(Pred - Obs, na.rm = TRUE),
      R2   = {
        sse <- sum((Pred - Obs)^2, na.rm = TRUE)
        sst <- sum((Obs - mean(Obs, na.rm = TRUE))^2, na.rm = TRUE)
        ifelse(sst == 0, NA_real_, 1 - sse/sst)
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      RMSE = round(RMSE, 2), Bias = round(Bias, 2), R2 = round(R2, 2),
      x_pos = x_annot, y_pos = y_annot
    )
  
  p <- ggplot2::ggplot(df_set, ggplot2::aes(Obs, Pred, shape = Variety)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.6) +
    ggplot2::geom_point(alpha = 0.4, size = 1.6,
                        position = ggplot2::position_jitter(width = 0.2, height = 0.2)) +
    ggplot2::facet_wrap(~ Dataset, nrow = 1) +
    ggplot2::scale_shape_manual(
      breaks = target_varieties,
      values = c(M105 = 16, M206 = 17, M209 = 15, M210 = 3, M211 = 0),
      drop = FALSE
    ) +
    ggplot2::geom_text(
      data = ann_set, inherit.aes = FALSE,
      ggplot2::aes(x = x_pos, y = y_pos,
                   label = paste0("RMSE=", RMSE, "\nBias=", Bias, "\nR²=", R2)),
      hjust = 0, vjust = 1, lineheight = 1.1, size = 3
    ) +
    ggplot2::scale_x_continuous(limits = lims, expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::scale_y_continuous(limits = lims, expand = ggplot2::expansion(mult = c(0, 0))) +
    ggplot2::coord_equal() +
    ggplot2::labs(title = NULL, subtitle = NULL,
                  x = "Observed DTH (days)", y = "Predicted DTH (days)") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text   = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(face = "bold"),
      axis.title.y = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
  
  stub <- paste0("FINAL_RoundedSWAP_Tb7_", toupper(set_nm), "_Local_vs_PRISM_NOTITLE")
  out_pdf <- file.path(OUT_DIR, paste0(stub, ".pdf"))
  out_png <- file.path(OUT_DIR, paste0(stub, ".png"))
  ggplot2::ggsave(out_pdf, p, width = 8.5, height = 5.5, units = "in", dpi = 300)
  ggplot2::ggsave(out_png, p, width = 8.5, height = 5.5, units = "in", dpi = 300, bg = "white")
  message("✓ Wrote SWAP obs-vs-pred (no-title, ", set_nm, "): ", normalizePath(out_pdf))
}

# Make all three
purrr::walk(c("Train","Calibrate","Validate"),
            ~ make_swapped_plot(swapped_preds_all, .x))





# ===============================================================
# 11C-metrics) Tables — Shared Tl/Tu = 11/33 metrics (Local vs PRISM)
#   • Per Dataset × Set × Tb
#   • Overall per Dataset × Set (pooled Tb)
#   • Best Tb per Dataset × Set by J = RMSE + 0.2*|Bias|
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(glue)
})

# Load from memory if present; else from the cache written in 11C
if (!exists("preds_shared_allsets")) {
  shared_allsets_fp <- file.path(OUT_DIR, "FINAL_predictions_perRun_SHARED_11_33_allSets.csv")
  stopifnot(file.exists(shared_allsets_fp))
  preds_shared_allsets <- readr::read_csv(shared_allsets_fp, show_col_types = FALSE)
}

# ---- metric helper ---------------------------------------------------------
metrics_tbl <- function(df) {
  df %>%
    summarise(
      RMSE = sqrt(mean((Pred - Obs)^2, na.rm = TRUE)),
      Bias = mean(Pred - Obs, na.rm = TRUE),
      R2   = {
        sse <- sum((Pred - Obs)^2, na.rm = TRUE)
        sst <- sum((Obs - mean(Obs, na.rm = TRUE))^2, na.rm = TRUE)
        ifelse(sst == 0, NA_real_, 1 - sse/sst)
      },
      n    = dplyr::n(),
      .groups = "drop_last"
    )
}

# ---- 1) Per Dataset × Set × Tb --------------------------------------------
shared_by_tb <- preds_shared_allsets %>%
  group_by(Dataset, Set, Tb) %>%
  do(metrics_tbl(.)) %>%         # compute metrics within each group
  ungroup() %>%
  mutate(
    J    = RMSE + 0.2*abs(Bias),
    RMSE = round(RMSE, 2),
    Bias = round(Bias, 2),
    R2   = round(R2,   2),
    J    = round(J,    2)
  ) %>%
  arrange(Dataset, Set, Tb)

out_by_tb <- file.path(OUT_DIR, "FINAL_Shared11_33_metrics_bySet_byTb.csv")
readr::write_csv(shared_by_tb, out_by_tb)
message("✓ Wrote: ", normalizePath(out_by_tb))

# ---- 2) Overall per Dataset × Set (pooled over Tb) ------------------------
shared_overall <- preds_shared_allsets %>%
  group_by(Dataset, Set) %>%
  do(metrics_tbl(.)) %>%
  ungroup() %>%
  mutate(
    J    = RMSE + 0.2*abs(Bias),
    RMSE = round(RMSE, 2),
    Bias = round(Bias, 2),
    R2   = round(R2,   2),
    J    = round(J,    2)
  ) %>%
  arrange(Dataset, Set)

out_overall <- file.path(OUT_DIR, "FINAL_Shared11_33_metrics_bySet_OVERALL.csv")
readr::write_csv(shared_overall, out_overall)
message("✓ Wrote: ", normalizePath(out_overall))

# ---- 3) Best Tb per Dataset × Set by J ------------------------------------
best_tb <- shared_by_tb %>%
  group_by(Dataset, Set) %>%
  arrange(J, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(Dataset, Set, Tb, RMSE, Bias, R2, J, n)

out_best <- file.path(OUT_DIR, "FINAL_Shared11_33_bestTb_bySet.csv")
readr::write_csv(best_tb, out_best)
message("✓ Wrote: ", normalizePath(out_best))

# Optional: print to console
print(shared_overall)
print(best_tb)

# ===============================================================
# 12) FIGURES — Obs vs Pred (Validation):
#       Base vs Universal rounded vs Shared Tl/Tu (11/33), Tb=7
#       Recomputes Validation predictions for these 3 families
#       using the existing SEEDS and stratified splits
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr); library(purrr); library(glue); library(ggplot2); library(readr); library(tidyr); library(stringr)
})

# ---- Settings for this figure --------------------------------------------
Tb_shared   <- 7        # Tb used for all three families in this figure
Tl_local_r  <- 11       # Local rounded Tl
Tu_local_r  <- 34       # Local rounded Tu
Tl_prism_r  <- 12       # PRISM rounded Tl
Tu_prism_r  <- 33       # PRISM rounded Tu
Tl_shared   <- 11       # Shared Tl for both data sources
Tu_shared   <- 33       # Shared Tu for both data sources

# Small helper: GDDreq per variety for a universal Tl/Tu
compute_greq_universal <- function(split_df, min_col, max_col, Tl, Tu, Tb) {
  vars <- sort(unique(split_df$Variety))
  envs_tr <- split_df %>% filter(Set == "Train") %>% group_split(Variety, Location, Year)
  greq <- setNames(rep(NA_real_, length(vars)), vars)
  for (v in vars) {
    ets <- purrr::keep(envs_tr, ~ unique(.x$Variety) == v)
    if (!length(ets)) next
    greq[v] <- mean(purrr::map_dbl(ets, function(d) {
      dth <- unique(d$DaysToHeading)
      sum(daily_gdd_cols(
        d[[min_col]][d$Days_After_Planting <= dth],
        d[[max_col]][d$Days_After_Planting <= dth],
        Tb, Tl, Tu
      ))
    }), na.rm = TRUE)
  }
  greq
}

# Helper: GDDreq per variety for Base (no Tl/Tu)
compute_greq_base <- function(split_df, min_col, max_col, Tb) {
  vars <- sort(unique(split_df$Variety))
  envs_tr <- split_df %>% filter(Set == "Train") %>% group_split(Variety, Location, Year)
  greq <- setNames(rep(NA_real_, length(vars)), vars)
  for (v in vars) {
    ets <- purrr::keep(envs_tr, ~ unique(.x$Variety) == v)
    if (!length(ets)) next
    greq[v] <- mean(purrr::map_dbl(ets, function(d) {
      dth <- unique(d$DaysToHeading)
      sum(daily_dd_base_cols(
        d[[min_col]][d$Days_After_Planting <= dth],
        d[[max_col]][d$Days_After_Planting <= dth],
        Tb
      ))
    }), na.rm = TRUE)
  }
  greq
}

# Helper: build per-observation predictions on VALIDATE set
predict_validate_universal <- function(split_df, min_col, max_col, Tl, Tu, Tb, greq_vec,
                                       dataset_label, family_label, model_label, seed, run_id) {
  envs_val <- split_df %>% filter(Set == "Validate") %>% group_split(Variety, Location, Year)
  purrr::map_dfr(envs_val, function(d) {
    v   <- unique(d$Variety)
    loc <- unique(d$Location)
    yr  <- unique(d$Year)
    g   <- greq_vec[[v]]
    if (!is.finite(g)) return(tibble())
    tibble(
      Obs      = unique(d$DaysToHeading),
      Pred     = predict_env_gdd_cols(d, min_col, max_col, Tl, Tu, Tb, g),
      Variety  = v,
      Location = loc,
      Year     = yr,
      Dataset  = dataset_label,
      Family   = family_label,
      Model    = model_label,
      Tb       = Tb,
      seed     = seed,
      run      = run_id
    )
  }) %>% drop_na(Obs, Pred)
}

predict_validate_base <- function(split_df, min_col, max_col, Tb, greq_vec,
                                  dataset_label, family_label, model_label, seed, run_id) {
  envs_val <- split_df %>% filter(Set == "Validate") %>% group_split(Variety, Location, Year)
  purrr::map_dfr(envs_val, function(d) {
    v   <- unique(d$Variety)
    loc <- unique(d$Location)
    yr  <- unique(d$Year)
    g   <- greq_vec[[v]]
    if (!is.finite(g)) return(tibble())
    inc <- daily_dd_base_cols(d[[min_col]], d[[max_col]], Tb)
    hit <- which(cumsum(inc) >= g)[1]
    pred_dap <- ifelse(is.na(hit), NA_real_, d$Days_After_Planting[hit])
    tibble(
      Obs      = unique(d$DaysToHeading),
      Pred     = pred_dap,
      Variety  = v,
      Location = loc,
      Year     = yr,
      Dataset  = dataset_label,
      Family   = family_label,
      Model    = model_label,
      Tb       = Tb,
      seed     = seed,
      run      = run_id
    )
  }) %>% drop_na(Obs, Pred)
}

# ---- Build prediction table for all seeds, VALIDATE set only ---------------
preds_fig12 <- purrr::map2_dfr(SEEDS, seq_along(SEEDS), function(s, i) {
  
  # Make splits for this seed
  split_loc  <- make_even_split(df_long_local,  seed = s)
  split_pris <- make_even_split(df_long_prism,  seed = s)
  
  # Column names per dataset
  min_loc  <- "Tmin_useC";      max_loc  <- "Tmax_useC"
  min_pris <- "PRISMMinTempC";  max_pris <- "PRISMMaxTempC"
  
  # ---- 1) Base (no Tl/Tu), Tb = Tb_shared --------------------------------
  greq_base_loc  <- compute_greq_base(split_loc,  min_loc,  max_loc,  Tb_shared)
  greq_base_pris <- compute_greq_base(split_pris, min_pris, max_pris, Tb_shared)
  
  base_loc <- predict_validate_base(
    split_df      = split_loc,
    min_col       = min_loc,
    max_col       = max_loc,
    Tb            = Tb_shared,
    greq_vec      = greq_base_loc,
    dataset_label = "Local",
    family_label  = "Base",
    model_label   = glue("Base (Tb={Tb_shared})"),
    seed          = s,
    run_id        = i
  )
  
  base_pris <- predict_validate_base(
    split_df      = split_pris,
    min_col       = min_pris,
    max_col       = max_pris,
    Tb            = Tb_shared,
    greq_vec      = greq_base_pris,
    dataset_label = "PRISM",
    family_label  = "Base",
    model_label   = glue("Base (Tb={Tb_shared})"),
    seed          = s,
    run_id        = i
  )
  
  # ---- 2) Universal rounded (dataset-specific Tl/Tu), Tb = Tb_shared ------
  # Local rounded: 11 / 33
  greq_loc_round <- compute_greq_universal(
    split_df = split_loc,
    min_col  = min_loc,
    max_col  = max_loc,
    Tl       = Tl_local_r,
    Tu       = Tu_local_r,
    Tb       = Tb_shared
  )
  
  uni_loc_round <- predict_validate_universal(
    split_df      = split_loc,
    min_col       = min_loc,
    max_col       = max_loc,
    Tl            = Tl_local_r,
    Tu            = Tu_local_r,
    Tb            = Tb_shared,
    greq_vec      = greq_loc_round,
    dataset_label = "Local",
    family_label  = "Universal rounded",
    model_label   = glue("Universal rounded (Tl/Tu={Tl_local_r}/{Tu_local_r}, Tb={Tb_shared})"),
    seed          = s,
    run_id        = i
  )
  
  # PRISM rounded: 12 / 34
  greq_pris_round <- compute_greq_universal(
    split_df = split_pris,
    min_col  = min_pris,
    max_col  = max_pris,
    Tl       = Tl_prism_r,
    Tu       = Tu_prism_r,
    Tb       = Tb_shared
  )
  
  uni_pris_round <- predict_validate_universal(
    split_df      = split_pris,
    min_col       = min_pris,
    max_col       = max_pris,
    Tl            = Tl_prism_r,
    Tu            = Tu_prism_r,
    Tb            = Tb_shared,
    greq_vec      = greq_pris_round,
    dataset_label = "PRISM",
    family_label  = "Universal rounded",
    model_label   = glue("Universal rounded (Tl/Tu={Tl_prism_r}/{Tu_prism_r}, Tb={Tb_shared})"),
    seed          = s,
    run_id        = i
  )
  
  # ---- 3) Universal SHARED Tl/Tu = 11/33 for both datasets ----------------
  greq_loc_shared <- compute_greq_universal(
    split_df = split_loc,
    min_col  = min_loc,
    max_col  = max_loc,
    Tl       = Tl_shared,
    Tu       = Tu_shared,
    Tb       = Tb_shared
  )
  
  uni_loc_shared <- predict_validate_universal(
    split_df      = split_loc,
    min_col       = min_loc,
    max_col       = max_loc,
    Tl            = Tl_shared,
    Tu            = Tu_shared,
    Tb            = Tb_shared,
    greq_vec      = greq_loc_shared,
    dataset_label = "Local",
    family_label  = "Shared 11/33",
    model_label   = glue("Shared Tl/Tu (11/33, Tb={Tb_shared})"),
    seed          = s,
    run_id        = i
  )
  
  greq_pris_shared <- compute_greq_universal(
    split_df = split_pris,
    min_col  = min_pris,
    max_col  = max_pris,
    Tl       = Tl_shared,
    Tu       = Tu_shared,
    Tb       = Tb_shared
  )
  
  uni_pris_shared <- predict_validate_universal(
    split_df      = split_pris,
    min_col       = min_pris,
    max_col       = max_pris,
    Tl            = Tl_shared,
    Tu            = Tu_shared,
    Tb            = Tb_shared,
    greq_vec      = greq_pris_shared,
    dataset_label = "PRISM",
    family_label  = "Shared 11/33",
    model_label   = glue("Shared Tl/Tu (11/33, Tb={Tb_shared})"),
    seed          = s,
    run_id        = i
  )
  
  dplyr::bind_rows(
    base_loc, base_pris,
    uni_loc_round, uni_pris_round,
    uni_loc_shared, uni_pris_shared
  )
})

# Save these predictions in case you want them later
write_csv(
  preds_fig12,
  file.path(OUT_DIR, glue("FINAL_Validate_preds_FIG12_Base_vsRounded_vsShared_Tb{Tb_shared}.csv"))
)

# ---- Compute pooled metrics per Dataset × Family for annotations ----------
ann_fig12 <- preds_fig12 %>%
  group_by(Dataset, Family) %>%
  summarise(
    RMSE = sqrt(mean((Pred - Obs)^2, na.rm = TRUE)),
    Bias = mean(Pred - Obs, na.rm = TRUE),
    R2   = {
      sse <- sum((Pred - Obs)^2, na.rm = TRUE)
      sst <- sum((Obs - mean(Obs, na.rm = TRUE))^2, na.rm = TRUE)
      ifelse(sst == 0, NA_real_, 1 - sse/sst)
    },
    n    = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(across(c(RMSE, Bias, R2), ~ round(.x, 2)))

# ---- Plotting helper for this specific figure -----------------------------
plot_obs_pred_fig12 <- function(ds) {
  df <- preds_fig12 %>%
    filter(Dataset == ds) %>%
    mutate(
      Variety = factor(Variety, levels = target_varieties),
      Family  = factor(Family,
                       levels = c("Base", "Universal rounded", "Shared 11/33"))
    )
  
  ann <- ann_fig12 %>%
    filter(Dataset == ds) %>%
    mutate(Family = factor(Family, levels = levels(df$Family)))
  
  xr <- range(df$Obs,  na.rm = TRUE)
  yr <- range(df$Pred, na.rm = TRUE)
  
  p <- ggplot(df, aes(x = Obs, y = Pred, shape = Variety)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.6) +
    geom_point(alpha = 0.35, size = 1.6,
               position = position_jitter(width = 0.2, height = 0.2)) +
    facet_grid(Family ~ .) +
    scale_shape_manual(
      breaks = target_varieties,
      values = c(M105 = 16,  # circle
                 M206 = 17,  # triangle
                 M209 = 15,  # square
                 M210 = 3,   # plus
                 M211 = 0),  # open square
      drop = FALSE
    ) +
    geom_text(
      data = ann, inherit.aes = FALSE,
      aes(x = xr[1], y = yr[2],
          label = paste0("RMSE=", RMSE,
                         "\nBias=", Bias,
                         "\nR²=", R2)),
      hjust = -0.02, vjust = 1.02, size = 3
    ) +
    coord_cartesian(xlim = xr, ylim = yr, clip = "on") +
    labs(
      title = glue("Observed vs Predicted DTH (Validation, Tb={Tb_shared}) — {ds}"),
      subtitle = "Base vs Universal rounded vs Shared Tl/Tu (11/33)",
      x = "Observed DTH (days)",
      y = "Predicted DTH (days)"
    ) +
    theme_bw() +
    theme(
      strip.text.y   = element_text(face = "bold"),
      plot.title     = element_text(hjust = 0.5),
      axis.title.x   = element_text(face = "bold"),
      axis.title.y   = element_text(face = "bold"),
      legend.position = "right"
    )
  
  out_pdf <- file.path(OUT_DIR, glue("FINAL_{ds}_FIG12_Base_vsRounded_vsShared_Tb{Tb_shared}.pdf"))
  ggsave(out_pdf, p, width = 7.5, height = 10.5, units = "in", dpi = 300)
  message("✓ Wrote figure (FIG12): ", normalizePath(out_pdf))
}

plot_obs_pred_fig12("Local")
plot_obs_pred_fig12("PRISM")

# ===============================================================
# 12a) FIGURE — Shared Tl/Tu = 11/33 only:
#       Local vs PRISM side by side (Validation, Tb = Tb_shared)
#       Uses preds_fig12 from Part 12
# ===============================================================

# Filter to Shared 11/33 family only
preds_fig12_shared <- preds_fig12 %>%
  dplyr::filter(Family == "Shared 11/33") %>%
  dplyr::mutate(
    Variety  = factor(Variety, levels = target_varieties),
    Dataset  = factor(Dataset, levels = c("Local", "PRISM"))
  )

# Compute per-dataset metrics for annotation
ann_fig12a <- preds_fig12_shared %>%
  dplyr::group_by(Dataset) %>%
  dplyr::summarise(
    RMSE = sqrt(mean((Pred - Obs)^2, na.rm = TRUE)),
    Bias = mean(Pred - Obs, na.rm = TRUE),
    R2   = {
      sse <- sum((Pred - Obs)^2, na.rm = TRUE)
      sst <- sum((Obs - mean(Obs, na.rm = TRUE))^2, na.rm = TRUE)
      ifelse(sst == 0, NA_real_, 1 - sse/sst)
    },
    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dplyr::across(c(RMSE, Bias, R2), ~ round(.x, 2))
  )

# Shared axis limits across both panels
xr_shared <- range(preds_fig12_shared$Obs,  na.rm = TRUE)
yr_shared <- range(preds_fig12_shared$Pred, na.rm = TRUE)

# Add positions for annotation text
ann_fig12a <- ann_fig12a %>%
  dplyr::mutate(
    x_pos = xr_shared[1],
    y_pos = yr_shared[2]
  )

# Plot: Local vs PRISM side by side
p_fig12a <- ggplot(preds_fig12_shared, aes(x = Obs, y = Pred, shape = Variety)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, linewidth = 0.6) +
  geom_point(alpha = 0.4, size = 1.6,
             position = position_jitter(width = 0.2, height = 0.2)) +
  facet_wrap(~ Dataset, nrow = 1) +
  scale_shape_manual(
    breaks = target_varieties,
    values = c(
      M105 = 16,  # circle
      M206 = 17,  # triangle
      M209 = 15,  # square
      M210 = 3,   # plus
      M211 = 0    # open square
    ),
    drop = FALSE
  ) +
  geom_text(
    data = ann_fig12a,
    inherit.aes = FALSE,
    aes(x = x_pos, y = y_pos,
        label = paste0("RMSE=", RMSE,
                       "\nBias=", Bias,
                       "\nR²=", R2)),
    hjust = -0.02,
    vjust = 1.02,
    size  = 3
  ) +
  coord_cartesian(xlim = xr_shared, ylim = yr_shared, clip = "on") +
  labs(
    title = glue("Observed vs Predicted DTH (Validation, Tb={Tb_shared}) — Shared Tl/Tu = 11/33"),
    subtitle = "Local vs PRISM side by side (5 seeds pooled)",
    x = "Observed DTH (days)",
    y = "Predicted DTH (days)"
  ) +
  theme_bw() +
  theme(
    strip.text   = element_text(face = "bold"),
    plot.title   = element_text(hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "right"
  )

out_pdf_12a <- file.path(
  OUT_DIR,
  glue("FINAL_FIG12a_Shared11_33_Local_vs_PRISM_Tb{Tb_shared}.pdf")
)

ggsave(out_pdf_12a, p_fig12a, width = 8.5, height = 5.5, units = "in", dpi = 300)
message("✓ Wrote figure (FIG12a): ", normalizePath(out_pdf_12a))

# ===============================================================
# 12a-alt) FIGURE — Shared Tl/Tu = 11/33 (No Title / No Subtitle)
#          Local vs PRISM side by side (Validation, Tb = Tb_shared)
# ===============================================================

p_fig12a_notitle <- p_fig12a +
  labs(title = NULL, subtitle = NULL)

out_pdf_12a_notitle <- file.path(
  OUT_DIR,
  glue("FINAL_FIG12a_Shared11_33_Local_vs_PRISM_Tb{Tb_shared}_NOTITLE.pdf")
)

ggsave(out_pdf_12a_notitle, p_fig12a_notitle,
       width = 8.5, height = 5.5, units = "in", dpi = 300)

message("✓ Wrote figure (FIG12a no-title version): ",
        normalizePath(out_pdf_12a_notitle))

# ===============================================================
# 13) Shared Tl/Tu = 11/33 cumulative GDD vs DAP
#       • Shared Tl=11, Tu=33, Tb=7 for BOTH Local and PRISM
#       • Overlay and ΔGDD curves across all environments
#       • By-county faceted plots with min–max bands + mean GDD@heading
# ===============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
})

# ---- 13.0 Settings & helpers ---------------------------------------------

shared_Tl <- 11
shared_Tu <- 33
shared_Tb <- 7

# Cumulative that keeps NA on missing days but resumes afterward
cumsum_with_na_breaks <- function(x) {
  s <- 0
  out <- numeric(length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      out[i] <- NA_real_
    } else {
      s <- s + x[i]
      out[i] <- s
    }
  }
  out
}

# Last non-NA cumulative value at/before target DAP
last_obs_upto <- function(x, daps, target) {
  i <- which(daps <= target & is.finite(x))
  if (length(i)) x[max(i)] else NA_real_
}

# Daily GDD with Tl/Tu caps (already conceptually used earlier)
daily_gdd_shared <- function(tmin, tmax, Tb, Tl, Tu) {
  tmin_c <- pmax(tmin, Tl)
  tmax_c <- pmin(tmax, Tu)
  pmax((tmin_c + tmax_c)/2 - Tb, 0)
}

# ---- 13.0b Overall mean heading date (across all varieties/envs) ---------
overall_mean_DTH <- raw %>%
  dplyr::select(dplyr::all_of(heading_cols)) %>%
  tidyr::pivot_longer(dplyr::everything(), values_to = "DTH") %>%
  dplyr::summarise(mean_DTH = mean(DTH, na.rm = TRUE)) %>%
  dplyr::pull(mean_DTH)

message(glue("Overall mean heading date (DAP) ≈ {round(overall_mean_DTH, 1)}"))

# ---- 13.1 Build base daily GDD table (Local vs PRISM) --------------------
# Uses raw Local temps, with flagged Local points set to NA (no PRISM fallback)

df_gdd <- raw %>%
  mutate(
    LocMinTempC_clean = ifelse(MINDif_OUT == 1, NA_real_, LocMinTempC),
    LocMaxTempC_clean = ifelse(MAXDif_OUT == 1, NA_real_, LocMaxTempC)
  ) %>%
  transmute(
    Location,
    Year,
    County,
    Date,
    DAP = Days_After_Planting,
    PRISMMinTempC,
    PRISMMaxTempC,
    LocMinTempC = LocMinTempC_clean,
    LocMaxTempC = LocMaxTempC_clean
  )

# ---- 13.2 Daily increments + cumulative GDD (shared 11/33) ---------------

df_gdd_inc <- df_gdd %>%
  mutate(
    inc_PRISM = daily_gdd_shared(PRISMMinTempC, PRISMMaxTempC,
                                 Tb = shared_Tb, Tl = shared_Tl, Tu = shared_Tu),
    inc_LOCAL = daily_gdd_shared(LocMinTempC,   LocMaxTempC,
                                 Tb = shared_Tb, Tl = shared_Tl, Tu = shared_Tu)
  ) %>%
  arrange(Location, Year, DAP) %>%
  group_by(Location, Year) %>%
  mutate(
    cum_PRISM = cumsum_with_na_breaks(inc_PRISM),
    cum_LOCAL = cumsum_with_na_breaks(inc_LOCAL)
  ) %>%
  ungroup()

# ---- 13.3 Overlay: mean ± range across all environments ------------------

acc_shared <- bind_rows(
  df_gdd_inc %>%
    group_by(DAP) %>%
    summarise(
      mean_cum = mean(cum_PRISM, na.rm = TRUE),
      min_cum  = min (cum_PRISM, na.rm = TRUE),
      max_cum  = max (cum_PRISM, na.rm = TRUE),
      n_envs   = sum(is.finite(cum_PRISM)),
      Source   = "PRISM",
      .groups  = "drop"
    ),
  df_gdd_inc %>%
    group_by(DAP) %>%
    summarise(
      mean_cum = mean(cum_LOCAL, na.rm = TRUE),
      min_cum  = min (cum_LOCAL, na.rm = TRUE),
      max_cum  = max (cum_LOCAL, na.rm = TRUE),
      n_envs   = sum(is.finite(cum_LOCAL)),
      Source   = "LOCAL",
      .groups  = "drop"
    )
) %>%
  mutate(Method = "Shared Tl=11 Tu=33 Tb=7")

rng_main <- range(c(acc_shared$min_cum, acc_shared$max_cum), na.rm = TRUE)
pad_main <- diff(range(rng_main)) * 0.05
y_lim_main <- c(rng_main[1] - pad_main, rng_main[2] + pad_main)

p_shared_overlay <- ggplot(acc_shared,
                           aes(DAP, mean_cum,
                               linetype = Source,
                               color    = Source,
                               fill     = Source)) +
  geom_ribbon(aes(ymin = min_cum, ymax = max_cum),
              alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1) +
  # NEW: overall mean heading date line
  geom_vline(xintercept = overall_mean_DTH,
             linetype = "dashed",
             linewidth = 0.6) +
  coord_cartesian(ylim = y_lim_main) +
  labs(
    title    = "Average cumulative GDD vs DAP — Shared Tl=11 Tu=33 Tb=7",
    subtitle = glue("Mean with min–max band across environments (PRISM vs Local, independent accumulation)\nVertical line = overall mean heading date (DTH ≈ {round(overall_mean_DTH, 1)} DAP)"),
    x = "Days After Planting (DAP)",
    y = "Cumulative GDD (°C·d)",
    linetype = "Source",
    color    = "Source",
    fill     = "Source"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(
  file.path(OUT_DIR, "FINAL_Shared11_33_Overlay_PRISM_LOCAL_RANGE.pdf"),
  p_shared_overlay, width = 8.6, height = 5.4
)

# ---- 13.3A Overlay: mean ± range across all environments NO TITLE ------------------

p_shared_overlay_notitle <- ggplot(acc_shared,
                                   aes(DAP, mean_cum,
                                       linetype = Source,
                                       color    = Source,
                                       fill     = Source)) +
  geom_ribbon(aes(ymin = min_cum, ymax = max_cum),
              alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = overall_mean_DTH,
             linetype = "dashed",
             linewidth = 0.6) +
  coord_cartesian(ylim = y_lim_main) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Days After Planting (DAP)",
    y = "Cumulative GDD (°C·d)",
    linetype = "Source",
    color    = "Source",
    fill     = "Source"
  ) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.margin = margin(5, 5, 5, 5)  # keeps spacing clean
  )

ggsave(
  file.path(OUT_DIR, "FINAL_Shared11_33_Overlay_PRISM_LOCAL_RANGE_notitle.pdf"),
  p_shared_overlay_notitle,
  width = 8.6, height = 5.4
)

message("✓ Wrote no-title overlay figure.")

# ---- 13.4 ΔGDD (PRISM − LOCAL) with min–max band -------------------------

delta_shared <- df_gdd_inc %>%
  mutate(delta = cum_PRISM - cum_LOCAL) %>%
  group_by(DAP) %>%
  summarise(
    mean_delta = mean(delta, na.rm = TRUE),
    min_delta  = min (delta, na.rm = TRUE),
    max_delta  = max (delta, na.rm = TRUE),
    n_envs     = sum(is.finite(delta)),
    .groups    = "drop"
  )

p_shared_delta <- ggplot(delta_shared, aes(DAP, mean_delta)) +
  geom_ribbon(aes(ymin = min_delta, ymax = max_delta), alpha = 0.18) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1) +
  # NEW: overall mean heading date line
  geom_vline(xintercept = overall_mean_DTH,
             linetype = "dashed",
             linewidth = 0.6) +
  labs(
    title    = "Cumulative Delta GDD (PRISM − Local) — Shared Tl=11 Tu=33 Tb=7",
    subtitle = glue("Mean with min–max band across environments (independent accumulation)\nVertical line = overall mean heading date (DTH ≈ {round(overall_mean_DTH, 1)} DAP)"),
    x = "Days After Planting (DAP)",
    y = "Delta GDD (°C·d)"
  ) +
  theme_classic(base_size = 12)

ggsave(
  file.path(OUT_DIR, "FINAL_Shared11_33_Delta_RANGE.pdf"),
  p_shared_delta, width = 8.6, height = 5.4
)

# ---- 13.4A ΔGDD (PRISM − LOCAL) — no title/subtitle, no range ribbon ----

delta_shared_simple <- delta_shared %>%
  dplyr::filter(DAP >= 0, DAP <= 160)

p_shared_delta_notitle_noribbon <- ggplot(delta_shared_simple, aes(DAP, mean_delta)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = overall_mean_DTH,
             linetype = "dashed",
             linewidth = 0.6) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "Days After Planting (DAP)",
    y = "Delta GDD (°C·d)"   
  ) +
  theme_classic(base_size = 12)

ggsave(
  file.path(OUT_DIR, "FINAL_Shared11_33_Delta_SIMPLE_notitle.pdf"),
  p_shared_delta_notitle_noribbon,
  width = 8.6, height = 5.4
)

message("✓ Wrote simple ΔGDD figure without title/subtitle/range ribbon (0–160 DAP).")

# ---- 13.5 DTH table (per Location × Year × Variety) ----------------------

dth_long_shared <- raw %>%
  select(Location, Year, County, all_of(heading_cols)) %>%
  pivot_longer(all_of(heading_cols),
               names_to  = "VarietyKey",
               values_to = "DTH") %>%
  mutate(
    Variety = sub("^Head(.*)_DaysToHeading$", "\\1", VarietyKey)
  ) %>%
  filter(Variety %in% target_varieties) %>%
  group_by(Location, Year, County, Variety) %>%
  summarise(
    DTH = suppressWarnings(first(na.omit(DTH))),
    .groups = "drop"
  ) %>%
  filter(is.finite(DTH))

# ---- 13.6 GDD@heading per environment (Shared 11/33) ---------------------

env_shared <- df_gdd_inc %>%
  select(Location, Year, County, DAP, cum_PRISM, cum_LOCAL) %>%
  tidyr::crossing(Variety = target_varieties) %>%
  left_join(dth_long_shared,
            by = c("Location", "Year", "County", "Variety")) %>%
  filter(is.finite(DTH)) %>%
  group_by(County, Location, Year, Variety) %>%
  summarise(
    DTH       = first(DTH),
    GDD_PRISM = last_obs_upto(cum_PRISM, DAP, DTH),
    GDD_LOCAL = last_obs_upto(cum_LOCAL, DAP, DTH),
    .groups   = "drop"
  ) %>%
  filter(is.finite(GDD_PRISM), is.finite(GDD_LOCAL), is.finite(DTH))

# ---- 13.7 County-level marker stats (mean ± SE) --------------------------

marker_shared <- env_shared %>%
  group_by(County) %>%
  summarise(
    mean_DTH   = mean(DTH),
    se_DTH     = sd  (DTH) / sqrt(sum(is.finite(DTH))),
    mean_PRISM = mean(GDD_PRISM),
    se_PRISM   = sd  (GDD_PRISM) / sqrt(sum(is.finite(GDD_PRISM))),
    mean_LOCAL = mean(GDD_LOCAL),
    se_LOCAL   = sd  (GDD_LOCAL) / sqrt(sum(is.finite(GDD_LOCAL))),
    .groups    = "drop"
  ) %>%
  pivot_longer(
    cols          = c(mean_PRISM, se_PRISM, mean_LOCAL, se_LOCAL),
    names_to      = c(".value", "Source"),
    names_pattern = "(mean|se)_(PRISM|LOCAL)"
  ) %>%
  rename(mean_GDD = mean, se_GDD = se)

county_counts_shared <- raw %>%
  distinct(County, Year) %>%
  count(County, name = "n_years")

marker_shared <- marker_shared %>%
  left_join(county_counts_shared, by = "County") %>%
  mutate(CountyLab = glue("{County} (n={n_years})"))

# ---- 13.8 By-county min–max ribbons (Shared 11/33) -----------------------

acc_shared_co <- bind_rows(
  df_gdd_inc %>%
    group_by(County, DAP) %>%
    summarise(
      mean_cum = mean(cum_PRISM, na.rm = TRUE),
      min_cum  = min (cum_PRISM, na.rm = TRUE),
      max_cum  = max (cum_PRISM, na.rm = TRUE),
      Source   = "PRISM",
      .groups  = "drop"
    ),
  df_gdd_inc %>%
    group_by(County, DAP) %>%
    summarise(
      mean_cum = mean(cum_LOCAL, na.rm = TRUE),
      min_cum  = min (cum_LOCAL, na.rm = TRUE),
      max_cum  = max (cum_LOCAL, na.rm = TRUE),
      Source   = "LOCAL",
      .groups  = "drop"
    )
) %>%
  left_join(county_counts_shared, by = "County") %>%
  mutate(CountyLab = glue("{County} (n={n_years})"))

# ---- 13.9 Plot: by-county with mean heading markers + vertical lines ------

p_shared_byCounty <- ggplot(
  acc_shared_co,
  aes(DAP, mean_cum,
      linetype = Source,
      color    = Source,
      fill     = Source)
) +
  geom_ribbon(aes(ymin = min_cum, ymax = max_cum),
              alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.9) +
  # vertical mean DTH line per county
  geom_vline(
    data = marker_shared,
    inherit.aes = FALSE,
    aes(xintercept = mean_DTH),
    linetype = "dashed",
    linewidth = 0.5
  ) +
  # points at mean GDD@heading
  geom_point(
    data        = marker_shared,
    inherit.aes = FALSE,
    aes(x = mean_DTH, y = mean_GDD, shape = Source, color = Source),
    size = 2.8
  ) +
  # vertical error bars in GDD dimension
  geom_errorbar(
    data        = marker_shared,
    inherit.aes = FALSE,
    aes(x = mean_DTH,
        ymin = mean_GDD - se_GDD,
        ymax = mean_GDD + se_GDD,
        color = Source),
    width = 0
  ) +
  facet_wrap(~ CountyLab, scales = "free_y") +
  labs(
    title = "Cumulative GDD by County — Shared Tl=11 Tu=33 Tb=7",
    subtitle = "Mean with min–max band across Years; dashed line = mean heading date (DTH), points = mean GDD@heading (±SE)",
    x = "Days After Planting (DAP)",
    y = "Cumulative GDD (°C·d)",
    linetype = "Source",
    color    = "Source",
    fill     = "Source",
    shape    = "Source"
  ) +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(OUT_DIR, "FINAL_Shared11_33_ByCounty_RANGE_withMeanHeading.pdf"),
  p_shared_byCounty, width = 12, height = 9
)


message("✓ Part 13 complete: Shared 11/33 cumulative GDD figures written to Outputs/")




# ===============================================================
# 13R) Overlay — ROUNDED dataset-specific thresholds with Local fallback
#       Local: Tl=11, Tu=34 (uses Tmin_useC/Tmax_useC with PRISM fallback)
#       PRISM: Tl=12, Tu=33
#       Base temperature Tb = 7
#       Output: mean curve + min–max ribbon for each source
# ===============================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(glue) })

# ---- Settings --------------------------------------------------------------
Tl_local_r  <- 11; Tu_local_r  <- 34
Tl_prism_r  <- 12; Tu_prism_r  <- 33
Tb_round    <- 7

# ---- Make sure we have fallback Local temps available ---------------------
# You defined df_local_base earlier with Tmin_useC/Tmax_useC
stopifnot(exists("df_local_base"))

# ---- Build a compact daily table with PRISM and Local(fallback) -----------
df_round_base <- df_local_base %>%
  transmute(
    Location, Year, County, Date,
    DAP = Days_After_Planting,
    PRISMMinTempC, PRISMMaxTempC,
    Tmin_useC, Tmax_useC
  )

# ---- Helper: capped daily GDD ---------------------------------------------
daily_gdd_capped <- function(tmin, tmax, Tb, Tl, Tu){
  tmin_c <- pmax(tmin, Tl)
  tmax_c <- pmin(tmax, Tu)
  pmax((tmin_c + tmax_c)/2 - Tb, 0)
}

# If you didn't run Section 13 above, re-define this small helper:
if (!exists("cumsum_with_na_breaks")) {
  cumsum_with_na_breaks <- function(x) {
    s <- 0; out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (is.na(x[i])) out[i] <- NA_real_ else { s <- s + x[i]; out[i] <- s }
    }
    out
  }
}

# ---- Daily increments + cumulative GDD (rounded thresholds) ----------------
df_round_inc <- df_round_base %>%
  mutate(
    inc_PRISM = daily_gdd_capped(PRISMMinTempC, PRISMMaxTempC,
                                 Tb = Tb_round, Tl = Tl_prism_r, Tu = Tu_prism_r),
    inc_LOCAL = daily_gdd_capped(Tmin_useC,     Tmax_useC,
                                 Tb = Tb_round, Tl = Tl_local_r, Tu = Tu_local_r)
  ) %>%
  arrange(Location, Year, DAP) %>%
  group_by(Location, Year) %>%
  mutate(
    cum_PRISM = cumsum_with_na_breaks(inc_PRISM),
    cum_LOCAL = cumsum_with_na_breaks(inc_LOCAL)
  ) %>%
  ungroup()

# ---- Daily increments + cumulative GDD (rounded thresholds) ----------------
df_round_inc <- df_round_base %>%
  mutate(
    inc_PRISM = daily_gdd_capped(PRISMMinTempC, PRISMMaxTempC,
                                 Tb = Tb_round, Tl = Tl_prism_r, Tu = Tu_prism_r),
    inc_LOCAL = daily_gdd_capped(Tmin_useC,     Tmax_useC,
                                 Tb = Tb_round, Tl = Tl_local_r, Tu = Tu_local_r)
  ) %>%
  arrange(Location, Year, DAP) %>%
  group_by(Location, Year) %>%
  mutate(
    cum_PRISM = cumsum_with_na_breaks(inc_PRISM),
    cum_LOCAL = cumsum_with_na_breaks(inc_LOCAL)
  ) %>%
  ungroup() %>%
  filter(DAP >= 0, DAP <= 155)   # <= move THIS up, before summarising

# ---- Across-environment summary by DAP (mean & min–max) -------------------
acc_rounded <- bind_rows(
  df_round_inc %>%
    group_by(DAP) %>%
    summarise(
      mean_cum = mean(cum_PRISM, na.rm = TRUE),
      min_cum  = min (cum_PRISM, na.rm = TRUE),
      max_cum  = max (cum_PRISM, na.rm = TRUE),
      n_envs   = sum(is.finite(cum_PRISM)),
      Source   = "PRISM",
      .groups  = "drop"
    ),
  df_round_inc %>%
    group_by(DAP) %>%
    summarise(
      mean_cum = mean(cum_LOCAL, na.rm = TRUE),
      min_cum  = min (cum_LOCAL, na.rm = TRUE),
      max_cum  = max (cum_LOCAL, na.rm = TRUE),
      n_envs   = sum(is.finite(cum_LOCAL)),
      Source   = "LOCAL",
      .groups  = "drop"
    )
)

# Recompute y-lims from the clipped data
# ---- Plot limits -----------------------------------------------------------
rng_main <- range(c(acc_rounded$min_cum, acc_rounded$max_cum), na.rm = TRUE)
y_upper  <- rng_main[2] + diff(rng_main) * 0.05  # small headroom
x_upper  <- max(acc_rounded$DAP, na.rm = TRUE)   # or set to 150/153 if you prefer

# ---- Overlay plot (titled) ------------------------------------------------
p_rounded_overlay <- ggplot(acc_rounded,
                            aes(DAP, mean_cum, linetype=Source, color=Source, fill=Source)) +
  geom_ribbon(aes(ymin = min_cum, ymax = max_cum), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = overall_mean_DTH, linetype = "dashed", linewidth = 0.6) +
  # >>> make axes meet at the corner: start both at 0, no padding <<<
  scale_x_continuous(limits = c(0, x_upper), expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = c(0, y_upper), expand = expansion(mult = c(0, 0))) +
  labs(
    title    = glue("Average cumulative GDD vs DAP — Rounded thresholds (Tb={Tb_round})"),
    subtitle = glue("Local Tl/Tu={Tl_local_r}/{Tu_local_r} (with PRISM fallback) vs PRISM Tl/Tu={Tl_prism_r}/{Tu_prism_r}\nMean curve with min–max band; dashed line = overall mean DTH (≈ {round(overall_mean_DTH,1)} DAP)"),
    x = "Days After Planting (DAP)",
    y = "Cumulative GDD (°C·d)",
    linetype = "Source", color = "Source", fill = "Source"
  ) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(OUT_DIR, "FINAL_Rounded_Overlay_PRISM_LOCAL_RANGE.pdf"),
       p_rounded_overlay, width = 8.6, height = 5.4)

# ---- Overlay plot (NO TITLE / NO SUBTITLE) --------------------------------
p_rounded_overlay_notitle <- p_rounded_overlay + labs(title=NULL, subtitle=NULL)

ggsave(file.path(OUT_DIR, "FINAL_Rounded_Overlay_PRISM_LOCAL_RANGE_notitle.pdf"),
       p_rounded_overlay_notitle, width = 8.6, height = 5.4)

message("✓ Wrote: ",
        normalizePath(file.path(OUT_DIR, "FINAL_Rounded_Overlay_PRISM_LOCAL_RANGE.pdf")))
message("✓ Wrote: ",
        normalizePath(file.path(OUT_DIR, "FINAL_Rounded_Overlay_PRISM_LOCAL_RANGE_notitle.pdf")))




# ===============================================================
# 13R-delta) ΔGDD (PRISM − Local) — ROUNDED thresholds, SIMPLE, no title
#   • Local uses Tmin_useC/Tmax_useC (PRISM fallback)
#   • PRISM uses PRISMMinTempC/PRISMMaxTempC
#   • Tl/Tu: Local 11/34, PRISM 12/33; Tb = 7
#   • Clipped to 0–160 DAP
#   • Output: FINAL_Rounded_Delta_SIMPLE_notitle.pdf
# ===============================================================

# Build delta from the rounded-threshold cumulative curves made in 13R
delta_rounded <- df_round_inc %>%
  dplyr::mutate(delta = cum_PRISM - cum_LOCAL) %>%
  dplyr::group_by(DAP) %>%
  dplyr::summarise(mean_delta = mean(delta, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(DAP >= 0, DAP <= 153)

p_delta_rounded_simple <- ggplot2::ggplot(delta_rounded, ggplot2::aes(DAP, mean_delta)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_vline(xintercept = overall_mean_DTH, linetype = "dashed", linewidth = 0.6) +
  ggplot2::scale_x_continuous(limits = c(0, 153), expand = ggplot2::expansion(mult = c(0, 0))) +
  ggplot2::labs(
    title = NULL, subtitle = NULL,
    x = "Days After Planting (DAP)",
    y = "Delta GDD (°C·d)"
  ) +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::coord_cartesian(ylim = c(-25, 75))

ggplot2::ggsave(
  file.path(OUT_DIR, "FINAL_Rounded_Delta_SIMPLE_notitle.pdf"),
  p_delta_rounded_simple, width = 8.6, height = 5.4
)




# ===============================================================
# 13R-SWAP) Overlay — thresholds SWAPPED between sources
#       PRISM uses Local Tl/Tu (11/34) ; Local uses PRISM Tl/Tu (12/33)
#       Local still uses PRISM fallback (Tmin_useC/Tmax_useC)
#       Tb = 7; Output: overlay + simple delta (no title)
#       Place this right after Section 13R
# ===============================================================

suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(glue) })

# ---- Reuse settings from 13R ----------------------------------------------
Tl_local_r  <- 11; Tu_local_r  <- 34
Tl_prism_r  <- 12; Tu_prism_r  <- 33
Tb_round    <- 7

stopifnot(exists("df_local_base"))  # needs Tmin_useC/Tmax_useC (with fallback)

# Compact daily table (same as 13R)
df_swap_base <- df_local_base %>%
  transmute(
    Location, Year, County, Date,
    DAP = Days_After_Planting,
    PRISMMinTempC, PRISMMaxTempC,
    Tmin_useC, Tmax_useC
  )

# Capped daily GDD (reuse if already defined)
if (!exists("daily_gdd_capped")) {
  daily_gdd_capped <- function(tmin, tmax, Tb, Tl, Tu){
    tmin_c <- pmax(tmin, Tl)
    tmax_c <- pmin(tmax, Tu)
    pmax((tmin_c + tmax_c)/2 - Tb, 0)
  }
}

# Cum-sum helper (reuse if needed)
if (!exists("cumsum_with_na_breaks")) {
  cumsum_with_na_breaks <- function(x) {
    s <- 0; out <- numeric(length(x))
    for (i in seq_along(x)) {
      if (is.na(x[i])) out[i] <- NA_real_ else { s <- s + x[i]; out[i] <- s }
    }
    out
  }
}

# ---- Daily increments + cumulative GDD (SWAPPED Tl/Tu) ---------------------
# PRISM gets (11/34); LOCAL gets (12/33)
df_swap_inc <- df_swap_base %>%
  mutate(
    inc_PRISM_sw = daily_gdd_capped(PRISMMinTempC, PRISMMaxTempC,
                                    Tb = Tb_round, Tl = Tl_local_r, Tu = Tu_local_r),
    inc_LOCAL_sw = daily_gdd_capped(Tmin_useC,     Tmax_useC,
                                    Tb = Tb_round, Tl = Tl_prism_r, Tu = Tu_prism_r)
  ) %>%
  arrange(Location, Year, DAP) %>%
  group_by(Location, Year) %>%
  mutate(
    cum_PRISM_sw = cumsum_with_na_breaks(inc_PRISM_sw),
    cum_LOCAL_sw = cumsum_with_na_breaks(inc_LOCAL_sw)
  ) %>%
  ungroup() %>%
  filter(DAP >= 0, DAP <= 155)   # clip like 13R

# ---- Across-environment summary by DAP (mean & min–max) -------------------
acc_swapped <- bind_rows(
  df_swap_inc %>%
    group_by(DAP) %>%
    summarise(
      mean_cum = mean(cum_PRISM_sw, na.rm = TRUE),
      min_cum  = min (cum_PRISM_sw, na.rm = TRUE),
      max_cum  = max (cum_PRISM_sw, na.rm = TRUE),
      n_envs   = sum(is.finite(cum_PRISM_sw)),
      Source   = "PRISM (uses 11/34)",
      .groups  = "drop"
    ),
  df_swap_inc %>%
    group_by(DAP) %>%
    summarise(
      mean_cum = mean(cum_LOCAL_sw, na.rm = TRUE),
      min_cum  = min (cum_LOCAL_sw, na.rm = TRUE),
      max_cum  = max (cum_LOCAL_sw, na.rm = TRUE),
      n_envs   = sum(is.finite(cum_LOCAL_sw)),
      Source   = "LOCAL (uses 12/33)",
      .groups  = "drop"
    )
)

# Mean DTH line (reuse if you didn’t keep it in memory)
if (!exists("overall_mean_DTH")) {
  overall_mean_DTH <- raw %>%
    select(all_of(heading_cols)) %>%
    tidyr::pivot_longer(everything(), values_to = "DTH") %>%
    summarise(mean_DTH = mean(DTH, na.rm = TRUE)) %>%
    pull(mean_DTH)
}

# Axis limits: start at 0 to anchor axes in the corner (like the rounded plot)
x_upper  <- max(acc_swapped$DAP, na.rm = TRUE)
rng_main <- range(c(acc_swapped$min_cum, acc_swapped$max_cum), na.rm = TRUE)
y_upper  <- rng_main[2] + diff(rng_main)*0.05

# ---- Overlay plot (NO TITLE / NO SUBTITLE, SWAP) --------------------------
p_swapped_overlay_notitle <- ggplot(acc_swapped,
                                    aes(DAP, mean_cum, linetype=Source, color=Source, fill=Source)) +
  geom_ribbon(aes(ymin = min_cum, ymax = max_cum), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = overall_mean_DTH, linetype = "dashed", linewidth = 0.6) +
  scale_x_continuous(limits = c(0, x_upper), expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(limits = c(0, y_upper), expand = expansion(mult = c(0, 0))) +
  labs(title = NULL, subtitle = NULL,
       x = "Days After Planting (DAP)", y = "Cumulative GDD (°C·d)",
       linetype = "Source", color = "Source", fill = "Source") +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(OUT_DIR, "FINAL_RoundedSWAP_Overlay_PRISM_LOCAL_RANGE_notitle.pdf"),
       p_swapped_overlay_notitle, width = 8.6, height = 5.4)

# ---- Delta (PRISM – LOCAL) from swapped curves (simple, no ribbon) --------
delta_swapped <- df_swap_inc %>%
  mutate(delta_sw = cum_PRISM_sw - cum_LOCAL_sw) %>%
  group_by(DAP) %>%
  summarise(mean_delta = mean(delta_sw, na.rm = TRUE), .groups = "drop")

p_swapped_delta_simple <- ggplot(delta_swapped, aes(DAP, mean_delta)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = overall_mean_DTH, linetype = "dashed", linewidth = 0.6) +
  scale_x_continuous(limits = c(0, x_upper), expand = expansion(mult = c(0, 0))) +
  # keep y free; or set e.g. c(-25, 75) if you want symmetry:
  # scale_y_continuous(limits = c(-25, 75), expand = expansion(mult = c(0, 0))) +
  labs(title = NULL, subtitle = NULL,
       x = "Days After Planting (DAP)",
       y = "Delta GDD (PRISM – Local, °C·d)") +
  theme_classic(base_size = 12)

ggsave(file.path(OUT_DIR, "FINAL_RoundedSWAP_Delta_SIMPLE_notitle.pdf"),
       p_swapped_delta_simple, width = 8.6, height = 5.4)

message("✓ Wrote SWAP overlay and delta: ",
        normalizePath(file.path(OUT_DIR, "FINAL_RoundedSWAP_Overlay_PRISM_LOCAL_RANGE_notitle.pdf")), " ; ",
        normalizePath(file.path(OUT_DIR, "FINAL_RoundedSWAP_Delta_SIMPLE_notitle.pdf")))






# ===============================================================
# Box & Whisker — GDD at heading by Variety
#   • Panel A: Rounded (Local 11/34, PRISM 12/33)
#   • Panel B: Shared 11/33 (both datasets 11/33)
#   • PRISM & LOCAL plotted together for each panel
#   • Matches stylization used in BoxWhisker_GDDatHeading_byVariety_Universal.pdf
# ===============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(glue); library(readr); library(purrr)
})

# --- SETTINGS ---------------------------------------------------------------
Tb_box        <- 7
Tl_local_r    <- 11; Tu_local_r  <- 34   # dataset-specific rounded (Local)
Tl_prism_r    <- 12; Tu_prism_r  <- 33   # dataset-specific rounded (PRISM)
Tl_shared     <- 11; Tu_shared    <- 33   # shared 11/33 for both

# Ensure Outputs/ exists
if (!exists("OUT_DIR")) OUT_DIR <- "Outputs"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Ensure consistent variety order
if (!exists("target_varieties")) {
  target_varieties <- c("M105","M206","M209","M210","M211")
}

# --- HELPERS ---------------------------------------------------------------
# Daily GDD with caps
if (!exists("daily_gdd_cols")) {
  daily_gdd_cols <- function(tmin, tmax, Tb, Tl, Tu){
    tmin_c <- pmax(tmin, Tl); tmax_c <- pmin(tmax, Tu)
    pmax((tmin_c + tmax_c)/2 - Tb, 0)
  }
}

# Sum GDD to heading (DTH) for one environment (single Variety×Loc×Year)
gdd_to_heading_env <- function(d, min_col, max_col, Tl, Tu, Tb) {
  dth <- unique(d$DaysToHeading)
  if (!is.finite(dth)) return(NA_real_)
  inc <- daily_gdd_cols(
    tmin = d[[min_col]][d$Days_After_Planting <= dth],
    tmax = d[[max_col]][d$Days_After_Planting <= dth],
    Tb   = Tb, Tl = Tl, Tu = Tu
  )
  sum(inc, na.rm = TRUE)
}

# Build per-environment GDD@heading table for one dataset and one Tl/Tu pair
# Returns rows: Source (LOCAL/PRISM), Variety, Year, Location, GDD
build_gdd_env_table <- function(df_long_like, source_nm, min_col, max_col, Tl, Tu, Tb = Tb_box) {
  df_long_like %>%
    group_by(Variety, Location, Year) %>%
    group_map(~ tibble(
      Variety  = unique(.x$Variety),
      Location = unique(.x$Location),
      Year     = unique(.x$Year),
      GDD      = gdd_to_heading_env(.x, min_col = min_col, max_col = max_col,
                                    Tl = Tl, Tu = Tu, Tb = Tb),
      Source   = source_nm
    ), .keep = TRUE) %>%
    list_rbind() %>%
    filter(is.finite(GDD))
}

# From per-env table -> per-year means (+ range across years for error bars)
prep_boxplot_inputs <- function(gdd_env_tbl) {
  # Per Year (across locations) for each Variety×Source
  year_means <- gdd_env_tbl %>%
    group_by(Variety, Source, Year) %>%
    summarise(GDD = mean(GDD, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Variety = factor(Variety, levels = target_varieties),
      Source  = factor(Source,  levels = c("LOCAL","PRISM"))
    )
  
  # Range across seasons for each Variety×Source (for error bars)
  range_by_var <- year_means %>%
    group_by(Variety, Source) %>%
    summarise(
      ymin = min(GDD, na.rm = TRUE),
      ymax = max(GDD, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(year_means = year_means, range_by_var = range_by_var)
}

# Plotter (matches your previous stylization exactly)
make_boxplot_single <- function(year_means, range_by_var, main_title, file_stub) {
  pos <- position_dodge(width = 0.70)
  
  p_box <- ggplot(year_means, aes(x = Variety, y = GDD, fill = Source)) +
    # min–max error bars (across seasons) behind the boxes
    geom_errorbar(
      data = range_by_var,
      aes(x = Variety, ymin = ymin, ymax = ymax, color = Source, group = Source),
      position = pos, width = 0.18, linewidth = 0.6, alpha = 0.9, inherit.aes = FALSE
    ) +
    # grouped boxplots (distribution across seasons)
    geom_boxplot(position = pos, width = 0.60, outlier.shape = 21, outlier.size = 1.8, alpha = 0.85) +
    # jitter the per-Year points for transparency
    geom_jitter(
      position = position_jitterdodge(jitter.width = 0.10, dodge.width = 0.70),
      size = 1.2, alpha = 0.45
    ) +
    labs(
      title = main_title,
      subtitle = NULL,
      x = "Variety", y = "GDD at heading (°C·d)", fill = "Source", color = "Source"
    ) +
    theme_classic(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
  
  # Save (titled)
  pdf_path <- file.path(OUT_DIR, glue("{file_stub}.pdf"))
  png_path <- file.path(OUT_DIR, glue("{file_stub}.png"))
  ggsave(pdf_path, p_box, width = 10.5, height = 6.8)
  ggsave(png_path, p_box, width = 10.5, height = 6.8, units = "in", dpi = 300, bg = "white")
  
  # Save (no-title)
  p_box_notitle <- p_box + labs(title = NULL, subtitle = NULL)
  pdf_path_nt <- file.path(OUT_DIR, glue("{file_stub}_NOTITLE.pdf"))
  png_path_nt <- file.path(OUT_DIR, glue("{file_stub}_NOTITLE.png"))
  ggsave(pdf_path_nt, p_box_notitle, width = 10.5, height = 6.8)
  ggsave(png_path_nt, p_box_notitle, width = 10.5, height = 6.8, units = "in", dpi = 300, bg = "white")
  
  message("✓ Wrote: ", normalizePath(pdf_path))
  message("✓ Wrote: ", normalizePath(png_path))
  message("✓ Wrote: ", normalizePath(pdf_path_nt))
  message("✓ Wrote: ", normalizePath(png_path_nt))
}

# --- PANEL A: ROUNDED (Local 11/34, PRISM 12/33) ---------------------------
gdd_local_round <- build_gdd_env_table(
  df_long_like = df_long_local,
  source_nm    = "LOCAL",
  min_col      = "Tmin_useC",
  max_col      = "Tmax_useC",
  Tl = Tl_local_r, Tu = Tu_local_r, Tb = Tb_box
)

gdd_prism_round <- build_gdd_env_table(
  df_long_like = df_long_prism,
  source_nm    = "PRISM",
  min_col      = "PRISMMinTempC",
  max_col      = "PRISMMaxTempC",
  Tl = Tl_prism_r, Tu = Tu_prism_r, Tb = Tb_box
)

gdd_round_both <- dplyr::bind_rows(gdd_local_round, gdd_prism_round)
inputs_round   <- prep_boxplot_inputs(gdd_round_both)

make_boxplot_single(
  year_means   = inputs_round$year_means,
  range_by_var = inputs_round$range_by_var,
  main_title   = glue("GDD to Heading by Variety — PRISM vs LOCAL (Rounded Tl/Tu: Local {Tl_local_r}/{Tu_local_r}, PRISM {Tl_prism_r}/{Tu_prism_r}, Tb={Tb_box})"),
  file_stub    = "FINAL_BoxWhisker_GDDatHeading_byVariety_Rounded"
)

# --- PANEL B: SHARED 11/33 (both datasets 11/33) ---------------------------
gdd_local_shared <- build_gdd_env_table(
  df_long_like = df_long_local,
  source_nm    = "LOCAL",
  min_col      = "Tmin_useC",
  max_col      = "Tmax_useC",
  Tl = Tl_shared, Tu = Tu_shared, Tb = Tb_box
)

gdd_prism_shared <- build_gdd_env_table(
  df_long_like = df_long_prism,
  source_nm    = "PRISM",
  min_col      = "PRISMMinTempC",
  max_col      = "PRISMMaxTempC",
  Tl = Tl_shared, Tu = Tu_shared, Tb = Tb_box
)

gdd_shared_both <- dplyr::bind_rows(gdd_local_shared, gdd_prism_shared)
inputs_shared   <- prep_boxplot_inputs(gdd_shared_both)

make_boxplot_single(
  year_means   = inputs_shared$year_means,
  range_by_var = inputs_shared$range_by_var,
  main_title   = glue("GDD to Heading by Variety — PRISM vs LOCAL (Shared Tl/Tu: {Tl_shared}/{Tu_shared}, Tb={Tb_box})"),
  file_stub    = "FINAL_BoxWhisker_GDDatHeading_byVariety_Shared11_33"
)

# Optional sanity check in console
message(glue("Rounded: LOCAL n={nrow(gdd_local_round)}, PRISM n={nrow(gdd_prism_round)}"))
message(glue("Shared : LOCAL n={nrow(gdd_local_shared)}, PRISM n={nrow(gdd_prism_shared)}"))




# --- Average DAP on (or near) July 4 ---

suppressPackageStartupMessages({ library(dplyr); library(lubridate); library(readr); library(stringr) })

# Use same master + filters you already apply
master_path <- "MasterTemp2024_repaired.csv"
stopifnot(file.exists(master_path))

raw0 <- read_csv(master_path, col_types = cols(Date = col_date())) %>%
  filter(!(Location == "Canal"       & Year == 2021),
         !(Location == "Rehmann"     & Year == 2023),
         !(Location == "BosworthRue" & Year == 2024),
         !(Location == "DelRio"),
         !(Location == "Wylie"       & Year == 2024)) %>%
  mutate(Date = as.Date(Date))

# Helper: pick rows on July 4, or nearest within ±2 days if exact date is missing
TOL_DAYS <- 2  # set to 0 for exact-only matches

# Exact July 4 first
july4_exact <- raw0 %>%
  filter(month(Date) == 7, day(Date) == 4) %>%
  select(Year, Location, Date, DAP = Days_After_Planting)

# If some Year×Location lack an exact 7/4, grab nearest within ±TOL_DAYS once per Year×Location
nearest_if_needed <- raw0 %>%
  filter(month(Date) == 7) %>%                           # July only to keep it sane
  mutate(day_in_july = day(Date),
         target_day   = 4L,
         abs_diff     = abs(day_in_july - target_day)) %>%
  filter(abs_diff <= TOL_DAYS) %>%
  arrange(Year, Location, abs_diff, Date) %>%
  group_by(Year, Location) %>%
  slice(1) %>%
  ungroup() %>%
  select(Year, Location, Date, DAP = Days_After_Planting)

# Prefer exact matches; fall back to nearest for pairs (Year, Location) missing exact 7/4
have_exact_keys <- july4_exact %>% mutate(key = paste(Year, Location)) %>% pull(key)

july4_best <- bind_rows(
  july4_exact,
  nearest_if_needed %>%
    mutate(key = paste(Year, Location)) %>%
    filter(!(key %in% have_exact_keys)) %>%
    select(-key)
)

# Summaries
by_year <- july4_best %>%
  group_by(Year) %>%
  summarize(mean_DAP = mean(DAP, na.rm = TRUE),
            n_sites  = dplyr::n(),
            .groups = "drop")

overall_pooled <- july4_best %>%
  summarize(mean_DAP = mean(DAP, na.rm = TRUE),
            sd_DAP   = sd(DAP, na.rm = TRUE),
            n        = dplyr::n()) %>%
  mutate(se = sd_DAP / sqrt(pmax(n, 1)),
         ci95_low  = mean_DAP - 1.96*se,
         ci95_high = mean_DAP + 1.96*se)

overall_year_mean <- by_year %>%
  summarize(mean_of_year_means = mean(mean_DAP, na.rm = TRUE),
            sd_year_means      = sd(mean_DAP, na.rm = TRUE),
            n_years            = dplyr::n()) %>%
  mutate(se = sd_year_means / sqrt(pmax(n_years, 1)),
         ci95_low  = mean_of_year_means - 1.96*se,
         ci95_high = mean_of_year_means + 1.96*se)

# Print results
message("Per-year mean DAP on/near July 4:")
print(by_year)

message("\nOverall pooled (all observations):")
print(overall_pooled)

message("\nMean of year-means (each year weighted equally):")
print(overall_year_mean)





# max_dap_export.R
# Create a per-Location × Year × County table of maximum DAP (and an estimated harvest date)
# PLUS summary stats for n_days (min/max/mean/median)

# --- Setup (edit setwd if you like) ---
# setwd("/Users/lewisdaniel/R Folder/LinquistLab/PRISMLocalGDD")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
  library(glue)
  library(tidyr)
})

# --- I/O ---
IN_FILE  <- "MasterTemp2024_repaired.csv"
OUT_DIR  <- "Outputs"
OUT_FILE <- file.path(OUT_DIR, "MaxDAP_by_Location_Year_County.csv")
OUT_SUM  <- file.path(OUT_DIR, "MaxDAP_summary.csv")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

stopifnot(file.exists(IN_FILE))

# --- Load & standardize dates/numbers ---
master <- readr::read_csv(IN_FILE, show_col_types = FALSE) %>%
  mutate(
    Date       = suppressWarnings(lubridate::ymd(Date)),
    Plant_Date = suppressWarnings(lubridate::ymd(Plant_Date)),
    Days_After_Planting = suppressWarnings(as.numeric(Days_After_Planting))
  )

# --- Match your usual exclusions (same as your PRISM–Local script) ---
master_filt <- master %>%
  filter(!(Location == "Canal"       & Year == 2021),
         !(Location == "Rehmann"     & Year == 2023),
         !(Location == "BosworthRue" & Year == 2024),
         !(Location == "DelRio"),
         !(Location == "Wylie"       & Year == 2024))

# --- Build Max DAP table ---
max_dap_tbl <- master_filt %>%
  filter(!is.na(Days_After_Planting), !is.na(Date)) %>%
  group_by(Location, Year, County) %>%
  summarise(
    Plant_Date_min = suppressWarnings(min(Plant_Date, na.rm = TRUE)),
    Last_Date      = suppressWarnings(max(Date,       na.rm = TRUE)),
    Max_DAP        = suppressWarnings(max(Days_After_Planting, na.rm = TRUE)),
    n_days         = dplyr::n_distinct(Date),
    .groups = "drop"
  ) %>%
  mutate(
    Est_Harvest_Date = ifelse(is.finite(Max_DAP) & !is.na(Plant_Date_min),
                              as.character(as.Date(Plant_Date_min) + Max_DAP),
                              NA_character_)
  ) %>%
  select(Location, Year, County, Plant_Date = Plant_Date_min, Last_Date,
         Max_DAP, n_days, Est_Harvest_Date) %>%
  arrange(Year, County, Location)

# --- Write main CSV ---
readr::write_csv(max_dap_tbl, OUT_FILE)

# --- Console summary for Max_DAP (as before) ---
rng_dap <- range(max_dap_tbl$Max_DAP, na.rm = TRUE)
message("Wrote: ", OUT_FILE)
message("Rows: ", nrow(max_dap_tbl))
message(glue("Max_DAP summary -> min: {rng_dap[1]}, mean: {round(mean(max_dap_tbl$Max_DAP, na.rm=TRUE),2)}, max: {rng_dap[2]}"))

# --- NEW: n_days summary (range, mean, median) ---
n_rng     <- range(max_dap_tbl$n_days, na.rm = TRUE)
n_mean    <- mean(max_dap_tbl$n_days, na.rm = TRUE)
n_median  <- stats::median(max_dap_tbl$n_days, na.rm = TRUE)

message(glue("n_days summary   -> min: {n_rng[1]}, mean: {round(n_mean,2)}, median: {n_median}, max: {n_rng[2]}"))

# Optional: also write a compact summary CSV
summary_tbl <- tibble::tibble(
  metric = c("n_days"),
  min    = n_rng[1],
  mean   = n_mean,
  median = n_median,
  max    = n_rng[2]
)

readr::write_csv(summary_tbl, OUT_SUM)
message("Wrote summary: ", OUT_SUM)

# (Optional) If you also want these by year:
# by_year <- max_dap_tbl %>%
#   group_by(Year) %>%
#   summarise(
#     n_days_min = min(n_days, na.rm = TRUE),
#     n_days_mean = mean(n_days, na.rm = TRUE),
#     n_days_median = median(n_days, na.rm = TRUE),
#     n_days_max = max(n_days, na.rm = TRUE),
#     .groups = "drop"
#   )
# readr::write_csv(by_year, file.path(OUT_DIR, "MaxDAP_n_days_byYear.csv"))
# message("Wrote by-year summary: ", file.path(OUT_DIR, "MaxDAP_n_days_byYear.csv"))
