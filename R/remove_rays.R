remove_rays <- function(pvol, rays, value = NA) {
  for (scan_id in as.integer(names(rays))) {
    for (param in pvol$scans[[scan_id]]$params) {
      pvol$scans[[scan_id]]$params[[attributes(param)$param]][, as.integer(rays[[as.character(scan_id)]])] <- value
    }
  }
  return(pvol)
}