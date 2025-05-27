#' @export
download_loca2 <- function(model, out_dir = paste0(getwd())) {
  # Increase timeout boundary for large files
  options(timeout = 21600) # 6 hours, just in case

  # Create directories
  model_dir <- paste0(out_dir, "/", model)
  dir.create(model_dir)

  # Get list of directories inside model directory
  base_url <- "https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/"
  model_url <- paste0(base_url, model, "/0p0625deg/r1i1p1f1/")

  # Both historical and future...
  for (period in c("historical", "ssp585")) {
    # For each variable...
    for (var_name in c("pr/", "tasmax/", "tasmin/")) {
      # Create variable directory
      var_dir <- paste0(model_dir, "/0p0625deg/r1i1p1f1/", period, "/", var_name)
      dir.create(var_dir, recursive = TRUE)

      # Find desired files
      data_url <- paste0(model_url, period, "/", var_name)
      data_page <- httr::GET(data_url)
      data_pagehtml <- XML::htmlParse(data_page)
      data_nodes <- XML::getNodeSet(data_pagehtml, "//table")
      data_table <- XML::readHTMLTable(data_nodes[[1]])
      data_table <- data_table[, -1] |>
        dplyr::filter(!is.na(Name) & Name != "Parent Directory" &
                        !stringr::str_detect(Name, "monthly") &
                        !stringr::str_detect(Name, "yearly") &
                        stringr::str_detect(Size, "G"))

      # Download desired files
      for (file_name in data_table$Name) {
        file_url <- paste0(data_url, file_name)
        output_file <- paste0(var_dir, file_name)
        utils::download.file(file_url, output_file)
        my_message <- paste0("Downloaded file to ", output_file)
        print(my_message)
      }
    }
  }
}
