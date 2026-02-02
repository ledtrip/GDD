#### Maps ####

library(sf)
library(tigris)   # pulls official US Census shapefiles
library(dplyr)
library(ggplot2)
library(tmap)     # for interactive maps

# 1. Base map ####

# 1.1. Getting counties as shape files (sf) ####

options(tigris_use_cache = TRUE)

ca_counties <- counties( # downloads county boundaries for California
  state = "CA",
  year = 2022,
  cb = TRUE
) |> 
  st_transform(3310)  # takes the output of counties() and: (i) reprojects it to EPSG:3310 (California Albers); (ii) this ensures distances & buffers are in meters

# select specific counties:

# List of counties to check spelling:
# ca_counties %>%
#        st_drop_geometry() %>%
#        distinct(NAME) %>%
#        arrange(NAME)

target_counties <- c(
  "Yuba",
  "Colusa",
  "San Joaquin",
  "Yolo",         # no South Yolo within tigris' counties()
  "Butte",        # no South Butte within tigris' counties()
  "Sutter",
  "Yolo",         # no North Yolo within tigris' counties() 
  "Butte",        # no North Butte within tigris' counties()
  "Glenn"
 )

ca_sel <- ca_counties %>%
  filter(NAME %in% target_counties)

county_labels <- ca_sel %>% # to include county labels
  st_point_on_surface()

# Plot the counties ##

Plot_counties <- ggplot() +
  geom_sf(data = ca_sel, fill = "grey90", color = "grey30") +
  geom_sf_text(
    data = county_labels,
    aes(label = NAME),
    size = 3
  ) +
  theme_minimal()

print(Plot_counties)
