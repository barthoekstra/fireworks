source("R/despeckle_scan_logical.R")
source("R/window_coords.R")

calculate_dpr <- function(pvol){
  for (i in seq_along(pvol$scans)) {
    # Calculate ZDR as ZDR = DBZH - DBZV
    pvol$scans[[i]]$params$ZDR <- pvol$scans[[i]]$params$DBZH - pvol$scans[[i]]$params$DBZV
    attributes(pvol$scans[[i]]$params$ZDR)$param <- "ZDR"
    
    # Calculate depolarization ratio
    zdr_linear <- 10 ** (pvol$scans[[i]]$params$ZDR / 10)
    dpr_linear <- (zdr_linear + 1 - 2 * sqrt(zdr_linear) * pvol$scans[[i]]$params$RHOHV) / 
      (zdr_linear + 1 + 2 * sqrt(zdr_linear) * pvol$scans[[i]]$params$RHOHV)
    pvol$scans[[i]]$params$DPR <- 10 * log10(dpr_linear)
    attributes(pvol$scans[[i]]$params$DPR)$param <- "DPR"
    
    # Classify based on depolarization ratio
    biology <- (pvol$scans[[i]]$params$DPR > -12) * 1  # multiply by 1 to convert TRUE/FALSE to 1/0
    class(biology) <- c("param", "matrix")
    attributes(biology) <- attributes(pvol$scans[[i]]$params$DPR)  # copy attributes from DPR
    attributes(biology)$param <- "BIOLR"
    pvol$scans[[i]]$params$BIOLR <- biology
    
    # Despeckle biology classification
    pvol$scans[[i]]$params$BIOLD <- pvol$scans[[i]]$params$BIOLR
    pvol$scans[[i]]$params$BIOLD <- despeckle_scan_logical(pvol$scans[[i]]$params$BIOLD)
    attributes(pvol$scans[[i]]$params$BIOLD)$param <- "BIOLD"
  }
  
  pvol
}