#' @export
sap_day <- function(tmax_rast, tmin_rast, t_upper = 2.2, t_lower = -1.1) {
  # Extract years for subsetting
  dates <- terra::time(tmax_rast)
  years <- as.integer(stringr::str_extract(dates, "[[:digit:]]{4}"))
  unique_years <- sort(unique(years))

  # Initialize empty lists
  sap_prop_list <- vector("list", length(unique_years))
  sap_sum_list <- vector("list", length(unique_years))

  # For each year...
  for (i in seq_along(unique_years)) {
    # Subset rasters by year
    year_layers <- which(years == unique_years[i])
    tmax_year <- tmax_rast[[year_layers]]
    tmin_year <- tmin_rast[[year_layers]]

    # Logical statement with temperature threshold
    sap_day <- tmax_year > t_upper & tmin_year < t_lower

    # Calculate proportion of sap days
    sap_prop_year <- terra::app(sap_day, mean)

    # Calculate sum of sap days
    sap_sum_year <- terra::app(sap_day, sum)

    # Put rasters in lists
    sap_prop_list[[i]] <- sap_prop_year
    sap_sum_list[[i]] <- sap_sum_year

    # Print year for progress
    message(paste0("Successfully calculated proportion and sum of sap days for year ", unique_years[i]))
  }

  # Combine rasters into one with layer for each year
  sap_prop <- terra::rast(sap_prop_list)
  sap_sum <- terra::rast(sap_sum_list)

  # Set year names
  names(sap_prop) <- unique_years
  names(sap_sum) <- unique_years

  # Return list of final rasters
  list(proportion = sap_prop, sum = sap_sum)
}
