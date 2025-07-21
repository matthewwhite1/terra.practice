library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(terra)

# Define stuff
models <- c("ACCESS-CM2", "ACCESS-ESM1-5")
scenarios <- c("ssp245", "ssp370", "ssp585")
model_names <- c()
scenario_names <- c()
distances <- c()

# Load in PRISM sap day proportion raster
prism_rast <- terra::rast("D:/Data/PRISM_prop.tif")

# Define US
us <- ne_countries(scale = "medium", returnclass = "sf", country = "United States of America")
us_vect <- vect(us)

# Get PRISM mean vector
us_prism <- project(us_vect, crs(prism_rast))
prism_us <- mask(crop(prism_rast, us_prism), us_prism)
prism_mean <- global(prism_us, fun = "mean", na.rm = TRUE)

# For each model...
for (i in 1:2) {
  # For each scenario...
  for (j in 1:3) {
    # Load in LOCA2 sap day proportion raster
    loca_rast <- terra::rast(paste0("D:/Data/LOCA2/", models[i], "_run1_", scenarios[j], "_prop.tif")) |>
      terra::shift(dx = -360)

    # Subset LOCA2 raster by prism years
    loca_rast <- loca_rast[[names(loca_rast) %in% as.character(1981:2020)]]

    # Mask rasters to be just the CONUS
    us_loca <- project(us_vect, crs(loca_rast))
    loca_us <- mask(crop(loca_rast, us_loca), us_loca)

    # Create yearly mean vectors
    loca_mean <- global(loca_us, fun = "mean", na.rm = TRUE)

    # Conduct Kolmogorov-Smirnov test
    dist <- ks.test(prism_mean$mean, loca_mean$mean)$statistic

    # Add things to vectors
    model_names <- c(model_names, models[i])
    scenario_names <- c(scenario_names, scenarios[j])
    distances <- c(distances, dist)
  }
}

# Create data frame
models_comparison <- data.frame(model = model_names, scenario = scenario_names, distance = distances)
write.csv(models_comparison, "Data_Clean/models_comparison.csv")
