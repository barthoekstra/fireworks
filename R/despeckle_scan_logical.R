despeckle_scan_logical <- function(log_arr){
  despeckled <- log_arr
  
  for(r in 1:nrow(log_arr)){
    for(a in 1:ncol(log_arr)){
      if(is.na(log_arr[r, a])){
        next
      }
      window <- window_coords(r, a, 1, 1, nrow(log_arr))
      sum_logical <- sum(log_arr[window[[1]], window[[2]]], na.rm = TRUE)
      nr_cells <- length(window[[1]]) * length(window[[2]])
      
      if(sum_logical > (nr_cells / 2)){
        out <- TRUE
      }
      else {
        out <- FALSE
      }
      despeckled[r, a] <- out
    }
  }
  return(despeckled)
}