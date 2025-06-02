#' @export
create_sap_day_rast_rcpp <- function() {
  # Check ordering of year folders
  year_folders <- list.files("Data_Raw/PRISM_Sap_Seasons_Data/", full.names = TRUE)
  years <- as.integer(stringr::str_extract(year_folders, "[[:digit:]]{4}"))
  if (!all(sort(years) == years)) {
    year_folders <- year_folders[order(years)]
  }

  # Within each folder:
  # - Check ordering of dates
  # - Load in two rasters
  # - Make logical statement with desired temps
  # - Use app() with mean() to find proportion
  # - Load that into ith entry of the list
  sap_prop_list <- list()
  for (i in seq_along(years)) {
    # Read in files
    files <- list.files(year_folders[i], full.names = TRUE)
    tmax_files <- files[stringr::str_detect(files, "tmax.*bil$")]
    tmin_files <- files[stringr::str_detect(files, "tmin.*bil$")]

    # Check ordering of dates
    tmax_dates <- stringr::str_extract(tmax_files, "[[:digit:]]{8}") |>
      as_date()
    if (!all(sort(tmax_dates) == tmax_dates)) {
      tmax_files <- tmax_files[order(tmax_dates)]
    }
    tmin_dates <- stringr::str_extract(tmin_files, "[[:digit:]]{8}") |>
      as_date()
    if (!all(sort(tmin_dates) == tmin_dates)) {
      tmin_files <- tmin_files[order(tmin_dates)]
    }

    # Load in rasters
    tmax_rast <- terra::rast(tmax_files)
    tmin_rast <- terra::rast(tmin_files)

    # Extract raster values
    tmax_vals <- as.matrix(terra::rast(tmax_rast))
    tmin_vals <- as.matrix(terra::rast(tmin_rast))

    # Use rcpp helper function to find proportion
    sap_prop <- sap_day_rast_helper(tmax_vals, tmin_vals)

    # Convert proportion back into raster from vector
    sap_prop_rast <- terra::rast(tmax_files[1])
    terra::values(sap_prop_rast) <- sap_prop

    # Load into list
    sap_prop_list[i] <- sap_prop_rast
  }

  # Combine all rasters into one raster with 30 layers
  sap_day_prop <- terra::rast(sap_prop_list)

  # Write final raster file
  terra::writeRaster(sap_day_prop, "Data_Clean/sap_day_prop.tif", overwrite = TRUE)
}
