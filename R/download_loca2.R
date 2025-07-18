#' TODO: Where else could this fail? If the servers are down?

#' Download LOCA2 netCDF files
#'
#' This function can be used to download daily netCDF files from LOCA2.
#'
#' @param model A character vector of models. These must exactly
#'   match the LOCA2 model names (Ex: "ACCESS-CM2").
#' @param run An integer vector of model runs. A run of 1 corresponds to the
#'   first run of the model (r1i1p1f1), a run of 2 corresponds to the second
#'   run of the model, and so on.
#' @param scenario A character vector of future climate scenarios. Each value
#'   in this vector must be either historical, ssp245, ssp370, or ssp585.
#' @param var A character vector of variables to download. Each value in this
#'   vector must be either pr (precipitation), tasmax (maximum temperature),
#'   tasmin (minimum temperature), or DTR (difference between maximum and
#'   minimum temperature).
#' @param out_dir The name of the output directory that will store the
#'   downloaded files.
#'
#' @details When filtering for files to download, this function excludes files
#'   that include "yearly" or "monthly", and it only downloads files that are
#'   at least 1 gigabyte in size. Keep in mind that even downloading the files
#'   for just one run of one model can take multiple hours, as these models
#'   contain some large files.
#'
#' @seealso [get_loca2_model_names()]
#'
#' @source \url{https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/}
#'
#' @examples
#' # Download all files from first LOCA2 model
#' # download_loca2(model = "ACCESS-CM2",
#' #                run = 1,
#' #                scenario = "ssp585",
#' #                var = c("pr", "tasmax", "tasmin"))
#'
#' # Download all files with multiple models, runs, and scenarios
#' # download_loca2(model = c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR"),
#' #                run = c(1, 2, 3),
#' #                scenario = c("ssp245", "ssp370", "ssp585"))
#' @export
download_loca2 <- function(model,
                           run = 1,
                           scenario = c("historical", "ssp585"),
                           var = c("pr", "tasmax", "tasmin"),
                           out_dir = paste0(getwd())) {
  # Get all model names for error checking
  model_names <- get_loca2_model_names()

  # Argument error checking
  if (!is.numeric(run) || !all(run %% 1 == 0)) {
    stop("The run argument must be a positive integer.")
  } else if (!all(scenario %in% c("historical", "ssp245", "ssp370", "ssp585"))) {
    stop("Each scenario must be historical, ssp245, ssp370, or ssp585.")
  } else if (!all(model %in% model_names)) {
    stop("Invalid model name (see get_loca2_model_names()).")
  } else if (!dir.exists(out_dir)) {
    stop("Out directory does not exist.")
  } else if (!all(var %in% c("pr", "tasmax", "tasmin", "DTR"))) {
    stop("Each variable must be pr, tasmax, tasmin, or DTR.")
  }

  # Increase timeout boundary for large files
  options(timeout = 21600) # 6 hours, just in case

  # Define base url
  base_url <- "https://cirrus.ucsd.edu/~pierce/LOCA2/NAmer/"

  # For each given model...
  for (given_model in model) {
    # Create model directory
    model_dir <- file.path(out_dir, given_model)
    dir.create(model_dir, recursive = TRUE)

    # For each given run...
    for (given_run in run) {
      # Create run URL
      run_full <- paste0("r", given_run, "i1p1f1/")
      model_url <- paste0(base_url, given_model, "/0p0625deg/", run_full)

      # Throw an error if run doesn't exist
      if (!RCurl::url.exists(model_url)) {
        warning(paste0("Run number ", given_run, " for model ", given_model, " does not exist."))
        next
      }

      # For both historical and future...
      for (period in scenario) {
        # Create scenario URL
        period_url <- paste0(model_url, period)

        # Throw an error if scenario doesn't exist
        if (!RCurl::url.exists(period_url)) {
          warning(paste0("Climate scenario ", period, " for model ", given_model, " for run ", given_run, " does not exist."))
          next
        }

        # For each variable...
        for (var_name in var) {
          # Create variable directory
          var_dir <- file.path(model_dir, "0p0625deg", run_full, period, var_name)
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
            file_url <- paste0(data_url, "/", file_name)
            output_file <- file.path(var_dir, file_name)
            utils::download.file(file_url, output_file, mode = "wb")
            message(paste0("Downloaded file to ", output_file))
          }
        }
      }
    }
  }
}
