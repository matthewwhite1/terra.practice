#' @export
get_sens_farms <- function(farms_coords, shapefile, variable) {
  # Load in sap day raster
  loca_sap_prop <- terra::rast("../loca_sap_weighted.tif")

  # Extract sap day proportions at farm locations
  farms_props <- terra::extract(loca_sap_prop, farms_coords)
  elevation <- terra::rast("Data_Clean/elevation.LOCA_2016-04-02.nc")
  elevation_values <- terra::extract(elevation, farms_coords)$Elevation
  farms_props$elevation <- elevation_values
  farms_sf <- sf::st_sf(farms_props, geometry = farms_coords$geometry) |>
    tidyr::drop_na()

  # Calculate sens slope for each location
  farms_sf$significant <- rep(0, nrow(farms_sf))
  for (i in 1:nrow(farms_sf)) {
    slope <- trend::sens.slope(as.numeric(sf::st_drop_geometry(farms_sf[i, 2:152])))
    if (slope$p.value < 0.05) {
      if (slope$estimates < 0) {
        farms_sf$significant[i] <- -1
      } else {
        farms_sf$significant[i] <- 1
      }
    }
  }

  # Join sf objects
  farms_sf <- sf::st_transform(farms_sf, sf::st_crs(shapefile))
  farms_joined <- sf::st_join(farms_sf, shapefile, join = sf::st_within)

  # Calculate mean proportion by region
  farm_sig_mean <- farms_joined |>
    sf::st_drop_geometry() |>
    dplyr::group_by(!!rlang::sym(variable)) |>
    dplyr::summarize(sig_mean = mean(significant),
                     elev_mean = mean(elevation))
  names(farm_sig_mean) <- stringr::str_remove_all(names(farm_sig_mean), "\"")

  # Rejoin back into shapefile table
  shapefile_joined <- dplyr::right_join(shapefile, farm_sig_mean, by = variable)

  # Return thing to be plotted
  shapefile_joined
}

farms_coords <- read_csv("Data_Clean/farms_coords.csv")
farms_coords <- st_as_sf(farms_coords, coords = c("long", "lat"), crs = 4326)
shapefile <- read_sf("Data_Clean/NA_Eco_Level3/NA_CEC_Eco_Level3.shp")
variable <- names(shapefile)[2]
