# PI Analysis - Jan 2026 ####

library(dplyr)
library(tidyverse)
library(ggplot2)

# 1. Master data frame 2024 ####

PI_2024 <- read_csv("Outputs/MasterTemp2024_repaired.csv",
                    col_types = cols(Date = col_date())) %>%
          # ── filter out unreliable site-years ────────────────────────────────────
          filter(
            Year == 2024,
            !(Location == "BosworthRue" ),
            !(Location == "DelRio"),      # drop all years of DelRio
            !(Location == "Wylie")        
          )

# Re-shape to have a Variety column (heading information is transported to a Heading_status column)

# 2. GDD calculation ####

## 2.1. Setting GDD function ####

GDD_function <- function(df,
                            min_temp_col,
                            max_temp_col,
                            source_name,
                            T_l,
                            T_opt,
                            T_base = 10) {
  
          df %>%
            mutate(
              Tmin_adj = ifelse(.data[[min_temp_col]] < T_l, NA, .data[[min_temp_col]]),
              Tmax_adj = pmin(.data[[max_temp_col]], T_opt),
              GDD = ifelse(
                is.na(Tmin_adj),
                0,
                pmax(((Tmax_adj + Tmin_adj) / 2) - T_base, 0)
              )
            ) %>%
            # 🔑 CRITICAL PART
            group_by(Location, Variety) %>%
            arrange(Date, .by_group = TRUE) %>%
            mutate(
              !!paste0(source_name, "_Cumulative_GDD") := cumsum(GDD)
            ) %>%
            ungroup()
}
        
## 2.2. Calculating GDD from Loc and PRISM data ####

# LOC:
PI_long <- GDD_function(
          df = PI_long,
          min_temp_col = "LocMinTempC",
          max_temp_col = "LocMaxTempC",
          source_name  = "Loc",
          T_l    = 12,
          T_opt  = 33,
          T_base = 7
)

# PRISM:
PI_long <- GDD_function(
          df = PI_long,
          min_temp_col = "PRISMMinTempC",
          max_temp_col = "PRISMMaxTempC",
          source_name  = "PRISM",
          T_l    = 12,
          T_opt  = 33,
          T_base = 7
)

# 3. Method plots ####

# 3.1. Cumulative GDD 

GDD_PI_LocPRISM_1 <- ggplot(
          PI_long,
          aes(
            x = Loc_Cumulative_GDD,
            y = PRISM_Cumulative_GDD
          )
        ) +
          # trajectory through time
          geom_path(alpha = 0.6, linewidth = 0.6) +
          
          # daily points
          geom_point(alpha = 0.5, size = 1) +
          
          # highlight PI date
          geom_point(
            data = PI_long %>% filter(Date == PI_Observed),
            color = "red",
            size = 2
          ) +
          
          # 1:1 reference line
          geom_abline(
            slope = 1,
            intercept = 0,
            linetype = "dashed"
          ) +
          
          # facets
          facet_grid(
            Location ~ Variety,
            scales = "free"
          ) +
          
          # labels
          labs(
            x = "Cumulative GDD (Loc temperatures, °C)",
            y = "Cumulative GDD (PRISM temperatures, °C)",
            title = "Cumulative Growing Degree Days: Loc vs PRISM",
            subtitle = "Trajectories shown through time; red points indicate observed PI"
          ) +
          
          # theme
          theme_bw() +
          theme(
            strip.text = element_text(size = 9),
            panel.grid.minor = element_blank(),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 8)
          )

ggsave("Outputs/GDD_PI_LocPRISM_1.pdf", plot = GDD_PI_LocPRISM_1, width = 12, height   = 8, units = "in")

# 4. Variety and location plots ####

GDD_at_PI <- PI_long %>%
  group_by(Location, Variety) %>%
  filter(Date == PI_Observed) %>%
  ungroup()

GDD_at_PI_longGDD <- GDD_at_PI %>%
  pivot_longer(
    cols = c(Loc_Cumulative_GDD, PRISM_Cumulative_GDD),
    names_to = "GDD_source",
    values_to = "Cumulative_GDD"
  ) %>%
  mutate(
    GDD_source = recode(
      GDD_source,
      "Loc_Cumulative_GDD"   = "LOC",
      "PRISM_Cumulative_GDD" = "PRISM"
    )
  )

## 4.1. Plot per variety ####

mean_variety <- GDD_at_PI_longGDD %>%
  group_by(Variety, GDD_source) %>%
  summarise(
    mean_GDD = mean(Cumulative_GDD, na.rm = TRUE),
    .groups = "drop"
  )

GDD_PI_Var <- ggplot(GDD_at_PI_longGDD,
       aes(x = Variety, y = Cumulative_GDD)) +
  geom_point(
    aes(color = Location),
    position = position_jitter(width = 0.15),
    size = 2,
    alpha = 0.8
  ) +
  geom_point(
    data = mean_variety,
    aes(y = mean_GDD),
    color = "black",
    size = 4,
    shape = 18
  ) +
  geom_text(
    data = mean_variety,
    aes(y = mean_GDD, label = round(mean_GDD, 0)),
    vjust = -1,
    size = 3
  ) +
  facet_wrap(~ GDD_source) +
  labs(
    title = "Cumulative GDD at PI by Variety",
    x = "Variety",
    y = "Cumulative GDD"
  ) +
  theme_bw()

ggsave("Outputs/GDD_PI_Var.pdf", plot = GDD_PI_Var, width = 13, height   = 4, units = "in")

## 4.2. Plot per Location ####

mean_location <- GDD_at_PI_longGDD %>%
  group_by(Location, GDD_source) %>%
  summarise(
    mean_GDD = mean(Cumulative_GDD, na.rm = TRUE),
    .groups = "drop"
  )

GDD_PI_Loc <- ggplot(GDD_at_PI_longGDD,
       aes(x = Location, y = Cumulative_GDD)) +
  geom_point(
    aes(color = Variety),
    position = position_jitter(width = 0.15),
    size = 2,
    alpha = 0.8
  ) +
  geom_point(
    data = mean_location,
    aes(y = mean_GDD),
    color = "black",
    size = 4,
    shape = 18
  ) +
  geom_text(
    data = mean_location,
    aes(y = mean_GDD, label = round(mean_GDD, 0)),
    vjust = -1,
    size = 3
  ) +
  facet_wrap(~ GDD_source) +
  labs(
    title = "Cumulative GDD at PI by Location",
    x = "Location",
    y = "Cumulative GDD"
  ) +
  theme_bw()

ggsave("Outputs/GDD_PI_Loc.pdf", plot = GDD_PI_Loc, width = 13, height   = 4, units = "in")