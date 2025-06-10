#' @export
prism_t_rast <- function(filepath, folders = TRUE) {
  # Check if directory exists
  if (!dir.exists(filepath)) {
    stop("Given directory does not exist.")
  }

  # If there are folders in this directory
  if (folders) {
    # Check ordering of year folders
    year_folders <- list.files(filepath, full.names = TRUE)
    years <- as.integer(stringr::str_extract(year_folders, "[[:digit:]]{4}"))
    if (!all(sort(years) == years)) {
      year_folders <- year_folders[order(years)]
      years <- sort(years)
    }

    # Stop if any folders don't have a year in their name
    if (any(is.na(years))) {
      stop("At least one folder does not have a year in its name.")
    }

    # Initialize empty lists
    tmax_list <- list()
    tmin_list <- list()

    # For each year folder...
    for (i in seq_along(years)) {
      # Read in files
      t_files <- list.files(year_folders[i], full.names = TRUE)
      tmax_files <- t_files[stringr::str_detect(files, "tmax.*bil$")]
      tmin_files <- t_files[stringr::str_detect(files, "tmin.*bil$")]

      # Skip if tmax and tmin have different amount of files
      if (length(tmax_files) != length(tmin_files)) {
        warning(paste0("Different amount of files between tmax and tmin for year ", years[i]))
        next
      }

      # Check ordering of dates
      tmax_dates <- stringr::str_extract(tmax_files, "[[:digit:]]{8}") |>
        lubridate::as_date()
      if (!all(sort(tmax_dates) == tmax_dates)) {
        tmax_files <- tmax_files[order(tmax_dates)]
      }
      tmin_dates <- stringr::str_extract(tmin_files, "[[:digit:]]{8}") |>
        lubridate::as_date()
      if (!all(sort(tmin_dates) == tmin_dates)) {
        tmin_files <- tmin_files[order(tmin_dates)]
      }

      # Load in rasters
      tmax_list[[i]] <- terra::rast(tmax_files)
      tmin_list[[i]] <- terra::rast(tmin_files)
    }

    # Prepare final rasters
    tmax_rast <- terra::rast(tmax_list)
    terra::time(tmax_rast) <- years
    tmin_rast <- terra::rast(tmin_list)
    terra::time(tmin_rast) <- years

    # Return list of final rasters
    return(list(tmax = tmax_rast, tmin = tmin_rast))
  } else {
    # Read in files
    t_files <- list.files(filepath, full.names = TRUE)
    tmax_files <- t_files[stringr::str_detect(files, "tmax.*bil$")]
    tmin_files <- t_files[stringr::str_detect(files, "tmin.*bil$")]

    # Extract years from file names
    tmax_years <- as.integer(stringr::str_extract(tmax_files, "[[:digit:]]{4}"))
    tmax_unique_years <- sort(unique(tmax_years))
    tmin_years <- as.integer(stringr::str_extract(tmin_files, "[[:digit:]]{4}"))
    tmin_unique_years <- sort(unique(tmin_years))
    years <- intersect(tmax_unique_years, tmin_unique_years)

    # For each year...
    for (i in seq_along(years)) {
      tmax_iyear <- tmax_files[tmax_years == years[i]]
      tmin_iyear <- tmin_files[tmin_years == years[i]]

      # Skip if tmax and tmin have different amount of files
      if (length(tmax_iyear) != length(tmin_iyear)) {
        warning(paste0("Different amount of files between tmax and tmin for year ", years[i]))
        next
      }

      # TODO: Check that tmax and tmin files have exact same dates
    }
  }
}
