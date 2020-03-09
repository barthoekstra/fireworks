calculate_distance_to_radar <- function(integrated_ppi) {
  dist <- raster(integrated_ppi$data["VIR"]) * 0  # Create matrix of size VIR with zeros
  
  center <- cellFromXY(dist, c(0, 0))
  suppressWarnings(dist[dist == 0] <- NaN)
  dist[center] <- 0
  
  dist <- distance(dist)
  
  dist_values <- as.data.frame(dist@data@values)
  integrated_ppi$data$dist_radar <- as.numeric(unlist(dist_values))
  
  integrated_ppi
}
