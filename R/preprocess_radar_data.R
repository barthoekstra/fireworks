source("R/remove_rays.R")
source("R/calculate_dpr.R")
source("R/remove_precipitation.R")
source("R/remove_groundclutter.R")

preprocess_radar_data <- function(pvol_path, ei_rays, pvol_dynamic_groundclutter, pvol_static_groundclutter) {
  pvol <- read_pvolfile(pvol_path, param = "all")
  pvol <- remove_rays(pvol, rays = ei_rays)
  pvol <- suppressWarnings(calculate_dpr(pvol))
  pvol <- remove_precipitation(pvol)
  pvol <- remove_groundclutter(remove_groundclutter(pvol, pvol_dynamic_groundclutter), pvol_static_groundclutter)
}