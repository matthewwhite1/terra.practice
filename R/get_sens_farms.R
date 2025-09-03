#' Get the Sen's slope estimate and p-value given an sf dataframe of farm locations
#'
#' This function takes an sf dataframe of (assumed) farm coordinates. Then, it
#' extracts the time series of yearly sap day proportions for each farm. Sen's
#' slope is calculated for each time series. The slope estimate and p-value are
#' added as columns to the extracted data frame.
#'
#' @param farms_coords An sf dataframe containing farms and their coordinates.
#' @param sap_prop A terra SpatRaster containing yearly sap day proportion
#'   rasters, probably calculated from [sap_day()].
#'
#' @return An sf dataframe containing the time series of sap day proportions
#'   for each year, the geometry, the Sen's slope estimate, and the Sen's slope
#'   p-value.
#'
#' @export
get_sens_farms <- function(farms_coords, sap_prop) {
  # Error checking
  if (!any(class(farms_coords) == "sf") || !any(class(farms_coords) == "data.frame")) {
    stop("farms_coords must have both class sf and data.frame.")
  } else if (class(sap_prop) != "SpatRaster") {
    stop("sap_prop must have class SpatRaster.")
  }

  # Extract sap day proportions at farm locations
  farms_props <- terra::extract(sap_prop, farms_coords)
  elevation <- terra::rast("Data_Clean/elevation.LOCA_2016-04-02.nc")
  elevation_values <- terra::extract(elevation, farms_coords)$Elevation
  farms_props$elevation <- elevation_values
  farms_sf <- sf::st_sf(farms_props, geometry = farms_coords$geometry) |>
    tidyr::drop_na() |>
    dplyr::select(-ID)
  num_cols <- seq(1, ncol(farms_sf) - 2)

  # Initialize empty vectors
  farms_sf$sens_estimate <- rep(0, nrow(farms_sf))
  farms_sf$sens_p_value <- rep(0, nrow(farms_sf))

  # Calculate the Sen's slope for every farm
  for (i in seq_len(nrow(farms_sf))) {
    slope <- trend::sens.slope(as.numeric(sf::st_drop_geometry(farms_sf[i, num_cols])))
    farms_sf$sens_estimate[i] <- slope$estimates
    farms_sf$sens_p_value[i] <- slope$p.value
  }

  # Return sf dataframe
  farms_sf
}
