library(tools)

source("R/remove_rays.R")
source("R/calculate_dpr.R")
source("R/remove_precipitation.R")
source("R/remove_groundclutter.R")
source("R/calculate_distance_to_radar.R")

preprocess_radar_data <- function(pvol_path, ei_rays, pvol_dynamic_groundclutter, pvol_static_groundclutter, azim_limits = NULL, overwrite = FALSE,
                                  res = 500) {
  pvol <- read_pvolfile(pvol_path, param = "all")
  
  # Filter clutter
  if (!is.null(ei_rays)) {
    pvol <- remove_rays(pvol, rays = ei_rays)
  }
  
  pvol <- suppressWarnings(calculate_dpr(pvol))
  pvol <- remove_precipitation(pvol)
  
  if (!is.null(pvol_dynamic_groundclutter)) {
    pvol <- remove_groundclutter(pvol, pvol_dynamic_groundclutter)
  }
  
  if (!is.null(pvol_static_groundclutter)) {
    pvol <- remove_groundclutter(pvol, pvol_static_groundclutter)
  }
  
  # Calculate VPs
  vp_out <- paste("data/processed/vp/", basename(pvol_path), sep = "")
  if (!file.exists(vp_out) || overwrite) {
    if (is.null(azim_limits)) {
      vp <- calculate_vp(file = pvol_path, vpfile = vp_out, verbose = FALSE)
    } else {
      vp <- calculate_vp(file = pvol_path, vpfile = vp_out, verbose = FALSE,
                         azim_min = azim_limits[1], azim_max = azim_limits[2])
    }
  } else {
    vp <- read_vpfiles(vp_out)
  }
  
  # Apply range-bias correction
  corrected_ppi <- integrate_to_ppi(pvol, vp, res = res, xlim = c(-150000, 150000), ylim = c(-150000, 150000))
  
  # Calculate distance to radar for all PPI pixels
  corrected_ppi <- calculate_distance_to_radar(corrected_ppi)
  
  # Calculate coordinates for all PPI pixels
  coords <- coordinates(corrected_ppi$data)
  corrected_ppi$data$x <- coords[, 1]
  corrected_ppi$data$y <- coords[, 2]
  
  # Save resultant PPI
  ppi_out <- paste("data/processed/corrected-ppis/", basename(file_path_sans_ext(pvol_path)), ".RDS", sep = "")
  saveRDS(corrected_ppi, file = ppi_out)
  
  print(ppi_out)
}
