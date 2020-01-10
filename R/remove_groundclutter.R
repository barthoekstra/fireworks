remove_groundclutter <- function(pvol, pvol_clutter, dbzh_threshold = -10) {
  for (scan in seq_along(pvol$scans)) {
    for (param in pvol$scans[[scan]]$params) {
      if (attributes(param)$param == "BIOLD" || attributes(param)$param == "BIOLR") {
        next()
      }
      DBZH_avg <- pvol_clutter$scans[[scan]]$params[["DBZH_AVG"]]
      pvol$scans[[scan]]$params[[attributes(param)$param]][DBZH_avg > dbzh_threshold] <- NA
    }
  }
  return(pvol)
}