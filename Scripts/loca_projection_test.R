library(terra)
library(tidyverse)
library(trend)

loca_rast <- loca_t_rast("D:/Data/LOCA2/ACCESS-CM2/0p0625deg/r1i1p1f1")
k_upper <- 2.2 + 273.15
k_lower <- -1.1 + 273.15
loca_sap <- sap_day(loca_rast$tmax, loca_rast$tmin, k_upper, k_lower)

### Proportion
# Propbably add this line to loca_t_rast function?
crs(loca_sap$proportion) <- "NAD83"

# This kinda works, seems to maybe make a copy of the original but also keeps the original
loca_sap$proportion <- project(loca_sap$proportion, "+proj=longlat +datum=WGS84")

fresh_air <- terra::vect(data.frame(lon = -73.899167, lat = 41.534167))
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

### Sum
crs(loca_sap$sum) <- "NAD83"
loca_sap$sum <- project(loca_sap$sum, "+proj=longlat +datum=WGS84")

fresh_air_sum <- terra::extract(loca_sap$sum, fresh_air)
fresh_air_sum <- fresh_air_sum[, -1]
fresh_air_sum_df <- fresh_air_sum |>
  pivot_longer(everything(), names_to = "Year", values_to = "Sum") |>
  mutate(Year = as.numeric(Year))
fresh_air_sum_df$Period <- c(rep("Historical", 65), rep("Future", 86))

ggplot(fresh_air_sum_df, aes(Year, Sum, color = Period)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(1950, 2100, by = 10)) +
  scale_color_manual(values = c("blue", "red"), breaks = c("Historical", "Future")) +
  ggtitle("Sum of Ideal Maple Syrup Tapping Days for Fresh Air Fund Maple Farm")

sens.slope(fresh_air_sum_df$Sum) # p-value = 2.135e-08


# Export onto SSD
writeRaster(loca_sap$proportion, "D:/Data/LOCA_prop_test.tif")
writeRaster(loca_sap$sum, "D:/Data/LOCA_sum_test.tif")
