stack_rainfree_reflectivities <- function(pvols, outputfile) {
  pvol_stack <- read_pvolfile(pvols[1])
  
  cat("Total # of scans to be processed: ", length(pvols), "\n")
  
  for (file in seq_along(pvols)) {
    if (file %% 5 == 0) {
      cat(file, ".. ")
    }
    
    # Read pvol
    pvol_in <- read_pvolfile(file = pvols[file], param = c("DBZH", "DBZV", "RHOHV"))  # The params we need to calculate DPR
    
    # Calculate DPR so we can filter precipitation
    pvol_in <- suppressWarnings(calculate_dpr(pvol_in))
    
    # Now remove precipitation
    pvol_in <- remove_precipitation(pvol_in)
    
    for (scan in seq_along(pvol_in$scans)) {
      # Check if DBZH is already a 3D array
      if (length(dim(pvol_stack$scans[[scan]]$params$DBZH)) != 3) {
        # Apparently DBZH is no 3D array yet, so let's replace DBZH by a populated one
        dims <- dim(pvol_stack$scans[[scan]]$params$DBZH)
        pvol_stack$scans[[scan]]$params$DBZH <- array(numeric(), c(length(pvols), dims[1], dims[2]))
      }
      pvol_stack$scans[[scan]]$params$DBZH[file, , ] <- pvol_in$scans[[scan]]$params$DBZH
    }
  }
  cat("\n")
  
  saveRDS(pvol_stack, file = outputfile)
}