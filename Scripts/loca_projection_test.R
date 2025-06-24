library(terra)
library(tidyverse)
library(trend)
library(sf)
library(patchwork)

# Calculate ideal sap days for first run of first LOCA2 model
loca_rast <- loca_t_rast("D:/Data/LOCA2/ACCESS-CM2/0p0625deg/r1i1p1f1")
k_upper <- 2.2 + 273.15
k_lower <- -1.1 + 273.15
loca_sap <- sap_day(loca_rast$tmax, loca_rast$tmin, k_upper, k_lower)

# Shift longitude to [-180, 180]
loca_sap$proportion <- terra::shift(loca_sap$proportion, dx = -360)
loca_sap$sum <- terra::shift(loca_sap$sum, dx = -360)

# Plot with point at Fresh Air Fund maple farm
fresh_air <- terra::vect(data.frame(lon = -73.899167, lat = 41.534167))
us_states <- sf::read_sf("Data_Clean/US_State_Lines/cb_2018_us_state_500k.shp")
rapp_lats <- c(37.011, 38.231, 41.625, 42.532, 43.734, 48.431)
rapp_lons <- c(-82.676, -79.658, -87.081, -72.190, -72.249, -70.688)
rapp_locations <- terra::vect(data.frame(lon = rapp_lons, lat = rapp_lats))

plot(loca_sap$proportion[[1]])
lines(us_states)
plot(fresh_air, add = TRUE, col = "red")
plot(rapp_locations, add = TRUE, col = "orange")

### Proportion
# Fresh Air Fund proportion
fresh_air_props <- terra::extract(loca_sap$proportion, fresh_air)
fresh_air_props <- fresh_air_props[, -1]
fresh_air_props_df <- fresh_air_props |>
  pivot_longer(everything(), names_to = "Year", values_to = "Mean") |>
  mutate(Year = as.numeric(Year))
fresh_air_props_df$Period <- c(rep("Historical", 65), rep("Future", 86))

ggplot(fresh_air_props_df, aes(Year, Mean, color = Period)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 10)) +
  ylab("Proportion") +
  scale_color_manual(values = c("blue", "red"), breaks = c("Historical", "Future")) +
  ggtitle("Proportion of Ideal Maple Syrup Tapping Days for Fresh Air Fund Maple Farm")

sens.slope(fresh_air_props_df$Mean) # p-value = 1.806e-08

# Rapp locations proportion
rapp_props <- terra::extract(loca_sap$proportion, rapp_locations)
sites <- c("Divide Ridge", "Southernmost Maple",
           "Indiana Dunes National Lakeshore", "Harvard Forest",
           "Dartmouth Organic Farm", "Quebec - Northern range")
names(rapp_props)[1] <- "Site"
rapp_props$Site <- sites
rapp_props_df <- rapp_props |>
  pivot_longer(-Site, names_to = "Year", values_to = "Mean") |>
  mutate(Year = as.numeric(Year))
rapp_props_df$Period <- rep(c(rep("Historical", 65), rep("Future", 86)), 6)

# Plot all locations together
gs <- vector("list", 6)
for(i in 1:6) {
  gs[[i]] <- filter(rapp_props_df, Site == sites[i]) |>
    ggplot(aes(Year, Mean, color = Period)) +
    geom_line() +
    theme_bw() +
    scale_y_continuous(breaks = seq(0, 0.3, by = 0.05), limits = c(0, 0.3)) +
    scale_color_manual(values = c("blue", "red"), breaks = c("Historical", "Future")) +
    ggtitle(sites[i])
}
(gs[[1]] + gs[[2]] + gs[[3]]) /
  (gs[[4]] + gs[[5]] + gs[[6]]) +
  plot_layout(guides = "collect")

ggplot(rapp_props_df, aes(Year, Mean, color = Period)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 25)) +
  scale_y_continuous("Proportion", breaks = seq(0, 0.3, by = 0.05), limits = c(0, 0.3)) +
  scale_color_manual(values = c("blue", "red"), breaks = c("Historical", "Future")) +
  facet_wrap(~ Site) +
  ggtitle("Proportion of Ideal Sap Days at Different Maple Sites") +
  theme(plot.title = element_text(hjust = 0.5))



# Export onto SSD
writeRaster(loca_sap$proportion, "D:/Data/LOCA_prop_test.tif", overwrite = TRUE)
writeRaster(loca_sap$sum, "D:/Data/LOCA_sum_test.tif", overwrite = TRUE)

# Read back into R
loca_sap <- list(proportion = 0, sum = 0)
loca_sap$proportion <- terra::rast("D:/Data/LOCA_prop_test.tif")
loca_sap$sum <- terra::rast("D:/Data/LOCA_sum_test.tif")
