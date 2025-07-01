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
farms_props$significant <- rep(FALSE, nrow(farms_props))
for (i in 1:nrow(farms_props)) {
  slope <- trend::sens.slope(as.numeric(farms_props[i, 2:152]))
  if (slope$p.value < 0.05) {
    farms_props$significant[i] <- TRUE
  }
}

## ChatGPT did most of the following:
# Get North America map
world <- ne_countries(scale = "medium", returnclass = "sf")
north_america <- world %>%
  filter(region_un == "Americas", name %in% c("United States of America", "Canada"))
us_states <- ne_states(country = "United States of America", returnclass = "sf")
canada_provinces <- ne_states(country = "Canada", returnclass = "sf")

# Convert farms to sf
farms_sf <- st_as_sf(farms_props, coords = c("lon", "lat"), crs = 4326)

# Plot
ggplot() +
  geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
  geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +  # State borders
  geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) + # Canada borders
  geom_sf(data = farms_sf, aes(shape = significant, color = significant), size = 2) +
  scale_shape_manual(values = c(1, 17)) +  # Circle for not sig, triangle for sig
  scale_color_manual(values = c("blue", "red")) +
  coord_sf(xlim = c(-130, -60), ylim = c(35, 55), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Significance of Sens Slope at Maple Farms",
    subtitle = "Red triangles = significant trend, Blue circles = not significant",
    x = "Longitude",
    y = "Latitude",
    shape = "Significant",
    color = "Significant"
  )
