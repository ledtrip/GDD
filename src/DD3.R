
###############################################################################
# 0.  Packages & parallel plan  ----------------------------------------------
###############################################################################
library(tidyverse)
library(furrr)          # for parallel grid search
library(patchwork)      # for multi-panel plots
plan(multisession, workers = parallel::detectCores() - 1)

###############################################################################
# 1.  Load the cleaned master dataset  +  BAD-TRIAL FILTER  -------------------
###############################################################################
master <- read_csv("Outputs/MasterTemp2024_repaired.csv",
                   col_types = cols(Date = col_date())) %>%
  # ── filter out unreliable site-years ────────────────────────────────────
  filter(
    !(Location == "Canal"        & Year == 2021),
    !(Location == "Rehmann"      & Year == 2023),
    !(Location == "BosworthRue"  & Year == 2024),
    !(Location == "DelRio"),                # drop all years of DelRio
    !(Location == "Wylie"        & Year == 2024),
    # add Canal 2023 here too if you decide it’s an outlier:
    !(Location == "Canal" & Year == 2023)
  )

# Inspect NA status (should be zero for all four PRISM cols)
master %>% summarise(across(starts_with("PRISM"), ~ sum(is.na(.x))))

###############################################################################
# 2.  Reshape to daily-long + variety-long format  ----------------------------
###############################################################################
target_varieties <- c("M105", "M206", "M209", "M210", "M211")

df_long <- master %>%
  # keep daily weather columns + the five heading-date columns
  select(Location, Year, Date, Plant_Date, Days_After_Planting,
         PRISMMinTempC, PRISMMaxTempC,
         starts_with("Head")) %>%
  pivot_longer(
    cols      = matches("^Head.*_DaysToHeading$"),
    names_to  = "VarietyKey",
    values_to = "DaysToHeading",
    values_drop_na = TRUE
  ) %>%
  mutate(Variety = sub("^Head(.*)_DaysToHeading$", "\\1", VarietyKey)) %>%
  filter(Variety %in% target_varieties)

###############################################################################
# 3.  One-stop GDD helper  ----------------------------------------------------
###############################################################################
calculate_gdd_at_heading <- function(data,
                                     t_lower,
                                     t_upper,
                                     t_base = 10) {
  data %>%
    mutate(
      Tmin_cap = pmax(PRISMMinTempC, t_lower),
      Tmax_cap = pmin(PRISMMaxTempC, t_upper),
      T_avg    = (Tmin_cap + Tmax_cap) / 2,
      GDD      = pmax(T_avg - t_base, 0)
    ) %>%
    group_by(Location, Year, Variety) %>%
    arrange(Date) %>%
    mutate(cum_gdd = cumsum(GDD)) %>%
    ungroup() %>%
    filter(Days_After_Planting == DaysToHeading) %>%
    mutate(T_lower = t_lower,
           T_upper = t_upper) %>%
    select(Location, Year, Variety, DaysToHeading, cum_gdd,
           T_lower,  T_upper)
}

###############################################################################
# 4.  Threshold search – keep both SD and CV  ---------------------------------
###############################################################################
# 4A.  Reference window (optional)
fixed_Tl <- 11
fixed_Tu <- 33
gdd_fixed <- calculate_gdd_at_heading(df_long, fixed_Tl, fixed_Tu)

# 4B.  Grid search (Tu floor = 32 °C)
grid <- expand.grid(T_lower = seq(10, 18, 0.2),
                    T_upper = seq(25, 40, 0.2)) %>%
  filter(T_lower < T_upper)

metric_tbl <- future_map_dfr(1:nrow(grid), \(i) {
  with(grid[i, ], {
    g <- calculate_gdd_at_heading(df_long, T_lower, T_upper)
    if (nrow(g) == 0) return(NULL)
    
    g %>%
      group_by(Variety) %>%
      summarise(mean_gdd = mean(cum_gdd),
                sd_gdd   = sd(cum_gdd),
                cv_gdd   = sd_gdd / mean_gdd,
                .groups  = "drop") %>%
      mutate(T_lower = T_lower,
             T_upper = T_upper)
  })
})

best_by_sd <- metric_tbl %>%
  group_by(Variety) %>% slice_min(sd_gdd, with_ties = FALSE) %>% ungroup()

best_by_cv <- metric_tbl %>%
  group_by(Variety) %>% slice_min(cv_gdd, with_ties = FALSE) %>% ungroup()

###############################################################################
# 5.  Summary tables  ---------------------------------------------------------
###############################################################################
summary_fixed <- gdd_fixed %>%
  group_by(Variety) %>%
  summarise(mean_gdd = mean(cum_gdd),
            sd_gdd   = sd(cum_gdd),
            cv_gdd   = sd_gdd / mean_gdd,
            .groups  = "drop")

print(summary_fixed)
print(best_by_sd)
print(best_by_cv)

###############################################################################
# 6.  Visualisation helper  ---------------------------------------------------
###############################################################################
plot_panel <- function(var, Tl, Tu) {
  gdat <- calculate_gdd_at_heading(df_long %>% filter(Variety == var), Tl, Tu)
  s <- gdat %>% summarise(mean_DTH = mean(DaysToHeading),
                          sd_DTH   = sd(DaysToHeading),
                          mean_GDD = mean(cum_gdd),
                          sd_GDD   = sd(cum_gdd))
  ggplot(gdat, aes(cum_gdd, DaysToHeading)) +
    geom_point(size = 1.5, alpha = 0.7) +
    geom_segment(data = s,
                 aes(x = mean_GDD, xend = mean_GDD,
                     y = mean_DTH - sd_DTH,
                     yend = mean_DTH + sd_DTH)) +
    geom_errorbarh(data = s, inherit.aes = FALSE,
                   aes(y = mean_DTH,
                       xmin = mean_GDD - sd_GDD,
                       xmax = mean_GDD + sd_GDD),
                   height = 0.15) +
    geom_point(data = s, aes(mean_GDD, mean_DTH),
               shape = 3, stroke = 1.1, size = 3) +
    geom_vline(xintercept = s$mean_GDD, linetype = "dashed") +
    labs(title = var,
         subtitle = glue::glue("Tl = {Tl} °C  |  Tu = {Tu} °C"),
         x = "Cumulative GDD (°C·day)",
         y = "Days to Heading") +
    theme_minimal(base_size = 11)
}

###############################################################################
# 6A. Fixed-window plots ------------------------------------------------------
plots_fixed <- map(target_varieties,
                   ~ plot_panel(.x, fixed_Tl, fixed_Tu))

fig_fixed <- wrap_plots(plots_fixed, ncol = 3) +
  plot_annotation(title = "DTH vs GDD  (Tl = 11 °C, Tu = 33 °C)")

print(fig_fixed)
ggsave("Outputs/DTH_vs_GDD_fixed_11_33.pdf", fig_fixed, width = 12, height = 8)

###############################################################################
# 6B. SD-optimised plots ------------------------------------------------------
plots_sd <- best_by_sd %>%
  select(Variety, T_lower, T_upper) %>%    
  pmap(\(Variety, T_lower, T_upper)
       plot_panel(Variety, T_lower, T_upper))

fig_sd <- wrap_plots(plots_sd, ncol = 3) +
  plot_annotation(title = "DTH vs GDD  (thresholds optimised for lowest SD)")

print(fig_sd)
ggsave("Outputs/DTH_vs_GDD_lowestSD.pdf", fig_sd, width = 12, height = 8)

###############################################################################
# 6C. CV-optimised plots ------------------------------------------------------
plots_cv <- pmap(best_by_cv,
                 \(Variety, mean_gdd, sd_gdd, cv_gdd, T_lower, T_upper)
                 plot_panel(Variety, T_lower, T_upper))

fig_cv <- wrap_plots(plots_cv, ncol = 3) +
  plot_annotation(title = "DTH vs GDD  (thresholds optimised for lowest CV)")

print(fig_cv)
ggsave("Outputs/DTH_vs_GDD_lowestCV.pdf", fig_cv, width = 12, height = 8)

###############################################################################
# 7.  Sharifi-style optimisation  ---------------------------------------------
###############################################################################
library(glue)

# --- helper: triangular daily TT --------------------------------------------
tri_tt <- function(tmin, tmax, Tb, Tl, Topt, Tu) {
  tmin_cap <- pmax(tmin,  Tl)
  tmax_cap <- pmin(tmax, Tu)
  tavg     <- (tmin_cap + tmax_cap) / 2
  
  # piecewise triangular response
  pmax(0, pmin(1,
               (tavg - Tb) / (Topt - Tb),        # rising limb
               (Tu  - tavg) / (Tu   - Topt)))    # falling limb
}

# --- helper: predict heading for one env -------------------------------------
predict_heading_env <- function(subdat, Tl, Topt, Tu, Tb, TTreq) {
  TTd  <- tri_tt(subdat$PRISMMinTempC, subdat$PRISMMaxTempC,
                 Tb, Tl, Topt, Tu)
  cum  <- cumsum(TTd)
  hit  <- which(cum >= TTreq)[1]
  subdat$Days_After_Planting[hit]
}

# --- optimiser objective: RMSE + λ|bias| -------------------------------------
obj_fun <- function(par, env_list, folds, lambda = 0.2, Tb = 10) {
  Tl   <- par[1];  Topt <- par[2];  Tu <- par[3]
  
  # ---- guard rails ---------------------------------------------------------
  if (!(Tl < Topt && Topt < Tu)) return(1e9)        # keep order sensible
  if (any(!is.finite(par)))         return(1e9)     # no NAN / Inf params
  
  # ---- k-fold CV -----------------------------------------------------------
  fold_loss <- map_dbl(unique(folds), \(k) {
    
    test_ids  <- which(folds == k)
    train_ids <- which(folds != k)
    
    # ----- estimate TTreq on training set -----------------------------------
    TT_train <- map_dbl(env_list[train_ids], \(d) {
      TTd <- tri_tt(d$PRISMMinTempC, d$PRISMMaxTempC, Tb, Tl, Topt, Tu)
      sum(TTd)
    })
    
    TTreq <- mean(TT_train, na.rm = TRUE)
    if (!is.finite(TTreq) || TTreq <= 0) return(1e8)  # bad TTreq → large loss
    
    # ----- predict on test set ---------------------------------------------
    preds <- map_dbl(env_list[test_ids], \(d) {
      p <- predict_heading_env(d, Tl, Topt, Tu, Tb, TTreq)
      ifelse(is.finite(p), p, NA_real_)
    })
    
    obs <- map_dbl(env_list[test_ids],
                   \(d) unique(d$DaysToHeading))
    
    # if any NA in preds → big penalty
    if (anyNA(preds)) return(1e8)
    
    rmse <- sqrt(mean((preds - obs)^2))
    bias <- mean(preds - obs)
    
    rmse + lambda * abs(bias)
  })
  
  loss <- mean(fold_loss)
  
  if (!is.finite(loss)) loss <- 1e9           # final safeguard
  loss
}

# --- run per variety ----------------------------------------------------------
set.seed(123)
varieties <- target_varieties
Sharifi_res <- map_dfr(varieties, function(v) {
  
  env_list <- df_long %>%
    filter(Variety == v) %>%
    group_split(Location, Year)
  
  folds <- sample(rep(1:5, length.out = length(env_list)))
  
  # reasonable start values
  par_start <- c(Tl = 12, Topt = 32, Tu = 36)
  
  fit <- optim(par      = par_start,
               fn       = obj_fun,
               env_list = env_list,
               folds    = folds,
               method   = "L-BFGS-B",
               lower    = c( 8, 28, 31),   # Tl_min, Topt_min, Tu_min
               upper    = c(18, 35, 45),   # Tl_max, Topt_max, Tu_max
               control  = list(maxit = 600))
  
  print(fit$convergence)   # 0 means success
  print(fit$par)           # Tl, Topt, Tu
  
  # final TTreq estimated on all envs
  TTreq_final <- env_list %>%
    map_dbl(~ {
      TTd <- tri_tt(.x$PRISMMinTempC, .x$PRISMMaxTempC,
                    Tb = 10, Tl = fit$par[1], Topt = fit$par[2], Tu = fit$par[3])
      sum(TTd)
    }) %>% mean()
  
  # observed vs predicted scatter
  pred_df <- imap_dfr(env_list, \(d, idx) {
    tibble(Env = idx,
           Obs = unique(d$DaysToHeading),
           Pred = predict_heading_env(d,
                                      Tl = fit$par[1],
                                      Topt = fit$par[2],
                                      Tu = fit$par[3],
                                      Tb = 10,
                                      TTreq = TTreq_final))
  })
  
  rmse <- sqrt(mean((pred_df$Pred - pred_df$Obs)^2, na.rm = TRUE))
  bias <- mean(pred_df$Pred - pred_df$Obs, na.rm = TRUE)
  
  p <- ggplot(pred_df, aes(Obs, Pred)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = glue("{v} – Sharifi fit"),
         subtitle = glue("Tl={round(fit$par[1],1)}  Topt={round(fit$par[2],1)}  ",
                         "Tu={round(fit$par[3],1)}  TTreq={round(TTreq_final,1)}\n",
                         "RMSE={round(rmse,2)}  Bias={round(bias,2)}"),
         x = "Observed DTH", y = "Predicted DTH") +
    theme_minimal()
  
  ggsave(glue("Outputs/Sharifi_{v}.pdf"), p, width = 6, height = 5)
  
  tibble(Variety = v,
         Tl      = round(fit$par[1], 2),
         Topt    = round(fit$par[2], 2),
         Tu      = round(fit$par[3], 2),
         TTreq   = round(TTreq_final, 1),
         RMSE    = round(rmse, 2),
         Bias    = round(bias, 2))
})

# save table
write_csv(Sharifi_res, "Outputs/Sharifi_Optima_Table.csv")
print(Sharifi_res)
