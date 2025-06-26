# Get farms coordinates
farms <- read.csv("Data_Clean/farms.csv")
farms_coords <- tidygeocoder::geocode(farms[, -1], address = address)
farms_coords_valid <- farms_coords |>
  filter(!is.na(lat))

# Write coordinates
write.csv(farms_coords_valid, "Data_Clean/farms_coords.csv")

# Load in sap day raster
loca_sap <- list(proportion = 0, sum = 0)
loca_sap$proportion <- terra::rast("D:/Data/LOCA_prop_test.tif")
loca_sap$sum <- terra::rast("D:/Data/LOCA_sum_test.tif")

# Extract sap day proportions at farm locations
farms_locations <- terra::vect(data.frame(lon = farms_coords_valid$long, lat = farms_coords_valid$lat))
farms_props <- terra::extract(loca_sap$proportion, farms_locations)
names(farms_props)[1] <- "Farm"
farms_props$Farm <- farms_coords_valid$farm
