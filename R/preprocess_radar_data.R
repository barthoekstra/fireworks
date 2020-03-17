library(tools)

source("R/remove_rays.R")
source("R/calculate_dpr.R")
source("R/remove_precipitation.R")
source("R/remove_groundclutter.R")
source("R/calculate_distance_to_radar.R")

preprocess_radar_data <- function(pvol_path, ei_rays, pvol_dynamic_groundclutter, pvol_static_groundclutter, azim_limits = NULL) {
  pvol <- read_pvolfile(pvol_path, param = "all")
  
  # Filter clutter
  pvol <- remove_rays(pvol, rays = ei_rays)
  pvol <- suppressWarnings(calculate_dpr(pvol))
  pvol <- remove_precipitation(pvol)
  pvol <- remove_groundclutter(pvol, pvol_dynamic_groundclutter)
  pvol <- remove_groundclutter(pvol, pvol_static_groundclutter)
  
  # Calculate VPs
  if (is.null(azim_limits)) {
    vp <- calculate_vp(file = pvol_path, vpfile = paste("data/processed/vp/", basename(pvol_path), sep = ""), verbose = FALSE)
  } else {
    vp <- calculate_vp(file = pvol_path, vpfile = paste("data/processed/vp/", basename(pvol_path), sep = ""), verbose = FALSE,
                       azim_min = azim_limits[1], azim_max = azim_limits[2])
  }
  
  # Apply range-bias correction
  corrected_ppi <- integrate_to_ppi(pvol, vp, res = 500, xlim = c(-150000, 150000), ylim = c(-150000, 150000))
  
  # Calculate distance to radar for all PPI pixels
  corrected_ppi <- calculate_distance_to_radar(corrected_ppi)
  
  # Calculate coordinates for all PPI pixels
  coords <- coordinates(corrected_ppi$data)
  corrected_ppi$data$x <- coords[, 1]
  corrected_ppi$data$y <- coords[, 2]
  
  # Save resultant PPI
  saveRDS(corrected_ppi, file = paste("data/processed/corrected-ppis/", basename(file_path_sans_ext(pvol_path)), ".RDS", sep = ""))
}
