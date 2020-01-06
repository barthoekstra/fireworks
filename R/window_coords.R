window_coords <- function(r, a, rwidth, awidth, rnum){
  # Azimuths
  amin <- (a - awidth) %% 360
  amax <- (a + awidth) %% 360
  if(amin <= 0){ amin <- amin + 360 }
  if(amax <= 0){ amax <- amax + 360 }
  aret <- c(amin:amax)
  if(amin > amax){
    aret <- c(c(amin:360), c(1:amax))
  }
  
  # Ranges
  rmin <- r - rwidth
  rmax <- r + rwidth
  if(rmax > rnum) { rmax <- rnum }
  if(rmin > rnum) { rmin <- rnum }
  if(rmin <= 1){ rmin <- 1 }
  rret <- c(rmin:rmax)
  
  return(list(rret, aret))
}