model_runs <- vector("list", 3)
k_upper <- 2.2 + 273.15
k_lower <- -1.1 + 273.15
for (i in 1:3) {
  path <- "D:/Data/LOCA2/ACCESS-CM2/0p0625deg/r"
  loca_rast <- loca_t_rast(paste0(path, i, "i1p1f1"))
  model_runs[[i]] <- sap_day(loca_rast$tmax, loca_rast$tmin, k_upper, k_lower)
}

# Export rasters
for (i in 1:3) {
  propname <- paste0("ACCESS-CM2_run_", i, "_proportion.tif")
  terra::writeRaster(model_runs[[i]]$proportion, propname)
  sumname <- paste0("ACCESS-CM2_run_", i, "_sum.tif")
  terra::writeRaster(model_runs[[i]]$sum, sumname)
}

# Import rasters
model_runs_read <- vector("list", 3)
for (i in 1:3) {
  propname <- paste0("ACCESS-CM2_run_", i, "_proportion.tif")
  sumname <- paste0("ACCESS-CM2_run_", i, "_sum.tif")
  model_runs_read[[i]] <- list(proportion = terra::rast(propname), sum = terra::rast(sumname))
}
