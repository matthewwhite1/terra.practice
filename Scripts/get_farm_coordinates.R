library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Get farms coordinates
farms <- read.csv("Data_Clean/farms.csv")
farms_coords <- tidygeocoder::geocode(farms[, -1], address = address)
farms_coords_valid <- farms_coords |>
  filter(!is.na(lat))

# Write coordinates
write.csv(farms_coords_valid, "Data_Clean/farms_coords.csv")

# Read coordinates
farms_coords_valid <- read.csv("Data_Clean/farms_coords.csv")

# Load in sap day raster
loca_sap_prop <- terra::rast("../ACCESS-CM2_run_1_proportion.tif")
loca_sap_prop <- terra::shift(loca_sap_prop, dx = -360)

# Extract sap day proportions at farm locations
farms_locations <- terra::vect(data.frame(lon = farms_coords_valid$long, lat = farms_coords_valid$lat))
farms_props <- terra::extract(loca_sap_prop, farms_locations)
names(farms_props)[1] <- "Farm"
farms_props$Farm <- farms_coords_valid$farm
farms_props <- farms_props |>
  dplyr::mutate(lon = farms_coords_valid$long, lat = farms_coords_valid$lat) |>
  dplyr::filter(!is.na(`1950`))

# Calculate sens slope for each location
farms_props$significant <- rep(0, nrow(farms_props))
for (i in 1:nrow(farms_props)) {
  slope <- trend::sens.slope(as.numeric(farms_props[i, 2:152]))
  if (slope$p.value < 0.05) {
    if (slope$estimates < 0) {
      farms_props$significant[i] <- -1
    } else {
      farms_props$significant[i] <- 1
    }
  }
}

# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
north_america <- world %>%
  filter(region_un == "Americas", name %in% c("United States of America", "Canada"))
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Convert farms to sf
farms_sf <- st_as_sf(farms_props, coords = c("lon", "lat"), crs = 4326)

# Get eco regions
eco_regions <- read_sf("Data_Clean/NA_Eco_Level3/NA_CEC_Eco_Level3.shp")

# Join sf objects
farms_sf <- st_transform(farms_sf, st_crs(eco_regions))
farms_joined <- st_join(farms_sf, eco_regions, join = st_within)

# Calculate mean proportion by region
farm_sig_mean <- farms_joined |>
  st_drop_geometry() |>
  group_by(NA_L3NAME) |>
  summarize(sig_mean = mean(significant))

# Rejoin back into eco regions table
eco_regions_joined <- right_join(eco_regions, farm_sig_mean, by = "NA_L3NAME")

# Plot
ggplot() +
  geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
  geom_sf(data = eco_regions_joined, mapping = aes(fill = sig_mean)) +
  geom_sf(data = farms_sf, color = "black", size = 1.5) +
  coord_sf(xlim = c(-98, -60), ylim = c(32, 53), expand = FALSE) +
  scale_fill_viridis_c(name = "Significance Proportion", option = "plasma") +
  theme_minimal() +
  labs(
    # title = "Significance of Sens Slope at Maple Farms",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.22))
