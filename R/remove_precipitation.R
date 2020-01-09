remove_precipitation <- function(pvol) {
  for (scan in seq_along(pvol$scans)) {
    for (param in pvol$scans[[scan]]$params) {
      if (attributes(param)$param == "BIOLD" || attributes(param)$param == "BIOLR") {
        next()
      }
      classification <- pvol$scans[[scan]]$params[["BIOLD"]]
      pvol$scans[[scan]]$params[[attributes(param)$param]][classification == 0] <- NA
      pvol$scans[[scan]]$params[[attributes(param)$param]][is.na(classification)] <- NA
    }
  }
  return(pvol)
}