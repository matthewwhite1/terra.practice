#' @export
get_maple_farms <- function() {
  # Get US state abbreviations
  us_states <- sf::read_sf("Data_Clean/US_State_Lines/cb_2018_us_state_500k.shp") |>
    dplyr::filter(!(STUSPS %in% c("PR", "DC", "AS", "VI", "GU", "MP"))) |>
    dplyr::select(STUSPS) |>
    unclass()
  us_states <- us_states$STUSPS

  # Initialize empty farm name vector
  farms <- c()

  # Define base url
  base_url <- "https://www.maplesyrupfarms.org/"

  # For each state...
  for (state in us_states) {
    # Define state url and read it
    state_url <- paste0(base_url, state, ".php")
    page <- rvest::read_html(state_url)

    # Get state regions
    state_links <- rvest::html_elements(page, "a") |>
      rvest::html_attr("href")
    state_links <- state_links[stringr::str_detect(state_links, paste0("^", state, ".*\\.php$"))]
    state_links <- state_links[!is.na(state_links)]

    # For each region...
    for (region_link in state_links) {
      # Get maple farms
      region_url <- paste0(base_url, region_link)
      if (!RCurl::url.exists(region_url)) {
        warning(paste0("URL ", region_url, " does not exist."))
        next
      }
      region_page <- rvest::read_html(region_url)
      farm_names <- rvest::html_elements(region_page, ".farm") |>
        rvest::html_text2()

      # Add to farms vector
      farms <- c(farms, farm_names)
    }
  }

  # Return farms vector
  farms[farms != ""]
}
