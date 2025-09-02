#' Get an sf data frame of maple farms' sen's slope grouped by shapefile variable
#'
#' This function takes an sf dataframe of (assumed) farm coordinates. Then, it
#' extracts the time series of yearly sap day proportions for each farm. Sen's
#' slope is calculated for each time series, and a significance variable is
#' created that is 1 if the slope is significant-positive, -1 if the slope is
#' negative-significant, and 0 if the slope is not significant. Then, the
#' farms are grouped by variable in the shapefile, and the average of the
#' significance value is taken for each group. A dataframe is returned that
#' contains the significance averages for each group and the corresponding
#' shapefile boundaries - this can be plotted with something like ggplot.
#'
#' @param farms_coords An sf dataframe containing farms and their coordinates.
#' @param shapefile An sf dataframe containing geographical shape boundaries
#' @param variable A character vector of length 1 containing the name of the
#'   variable within the shapefile dataframe to group by.
#' @param sap_prop A terra SpatRaster containing yearly sap day proportion
#'   rasters.
#'
#' @return An sf dataframe that contains the significance averages for each group
#'   and the corresponding shapefile boundaries - this can be plotted with
#'   something like ggplot.
#'
#' @export
get_sens_farms <- function(farms_coords, shapefile, variable, sap_prop) {
  # Error checking
  if (!any(class(farms_coords) == "sf") || !any(class(farms_coords) == "data.frame")) {
    stop("farms_coords must have both class sf and data.frame.")
  } else if (!any(class(shapefile) == "sf") || !any(class(shapefile) == "data.frame")) {
    stop("shapefile must have both class sf and data.frame.")
  } else if (!is.character(variable) || length(variable) != 1) {
    stop("variable must be a character vector of length 1.")
  } else if (!any(names(shapefile) == variable)) {
    stop("variable must be the name of a column in the shapefile.")
  } else if (class(sap_prop) != "SpatRaster") {
    stop("sap_prop must have class SpatRaster.")
  }

  # Extract sap day proportions at farm locations
  farms_props <- terra::extract(sap_prop, farms_coords)
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

# farms_coords <- read_csv("Data_Clean/farms_coords.csv")
# farms_coords <- st_as_sf(farms_coords, coords = c("long", "lat"), crs = 4326)
# shapefile <- read_sf("Data_Clean/NA_Eco_Level3/NA_CEC_Eco_Level3.shp")
# variable <- names(shapefile)[2]
# sap_prop <- terra::rast("../loca_sap_weighted.tif")
#
# farms_coords <- read_csv("Data_Clean/farms_coords.csv")
# farms_coords <- st_as_sf(farms_coords, coords = c("long", "lat"), crs = 4326)
# shapefile <- counties(cb = TRUE, year = 2020)
# variable <- "GEOID"
# sap_prop <- terra::rast("../loca_sap_weighted.tif")
