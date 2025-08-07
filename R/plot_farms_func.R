#' @export
plot_farms <- function(farms_coords, shapefile) {
  # Load in sap day raster
  loca_sap_prop <- terra::rast("../loca_sap_weighted.tif")

  # Extract sap day proportions at farm locations
  farms_locations <- terra::vect(data.frame(lon = farms_coords$long, lat = farms_coords$lat))
  farms_props <- terra::extract(loca_sap_prop, farms_locations)
  elevation <- terra::rast("Data_Clean/elevation.LOCA_2016-04-02.nc")
  elevation_values <- terra::extract(elevation, farms_locations)$Elevation
  farms_props$elevation <- elevation_values
  names(farms_props)[1] <- "Farm"
  farms_props$Farm <- farms_coords$farm
  farms_props <- farms_props |>
    dplyr::mutate(lon = farms_coords$long, lat = farms_coords$lat) |>
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
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  north_america <- world |>
    dplyr::filter(region_un == "Americas", name %in% c("United States of America", "Canada"))
  us_states <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
  canada_provinces <- rnaturalearth::ne_states(country = "Canada", returnclass = "sf")

  # Convert farms to sf
  farms_sf <- sf::st_as_sf(farms_props, coords = c("lon", "lat"), crs = 4326)

  # Join sf objects
  farms_sf <- sf::st_transform(farms_sf, sf::st_crs(shapefile))
  farms_joined <- sf::st_join(farms_sf, shapefile, join = sf::st_within)

  # Calculate mean proportion by region
  farm_sig_mean <- farms_joined |>
    sf::st_drop_geometry() |>
    dplyr::group_by(NA_L3NAME) |>
    dplyr::summarize(sig_mean = mean(significant),
                     elev_mean = mean(elevation))

  # Rejoin back into shapefile table
  shapefile_joined <- dplyr::right_join(shapefile, farm_sig_mean, by = "NA_L3NAME")

  # Ecoregions plot
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = north_america, fill = "grey95", color = "black", size = 0.2) +
    ggplot2::geom_sf(data = us_states, fill = NA, color = "darkgray", size = 0.3) +
    ggplot2::geom_sf(data = canada_provinces, fill = NA, color = "darkgray", size = 0.3) +
    ggplot2::geom_sf(data = shapefile_joined, mapping = ggplot2::aes(fill = sig_mean)) +
    ggplot2::geom_sf(data = farms_sf, color = "black", size = 1.5) +
    ggplot2::coord_sf(xlim = c(-99, -59), ylim = c(32, 53), expand = FALSE) +
    ggplot2::scale_fill_viridis_c(name = "Significance Proportion", option = "plasma") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      # title = "Significance of Sens Slope at Maple Farms",
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::theme(legend.position = "inside",
                  legend.position.inside = c(0.9, 0.22))
}
