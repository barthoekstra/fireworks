side_by_side_ppi <- function(pvol1, pvol2, title1 = "", title2 = "", params = "DBZH", scan_id = 1, range_max = 100000, basemap = FALSE, 
                             alpha = 0.5, radar_size = 0, zlim = NULL) {
  ppi1 <- project_as_ppi(pvol1$scans[[scan_id]], range_max = range_max)
  ppi2 <- project_as_ppi(pvol2$scans[[scan_id]], range_max = range_max)
  
  for (param in params) {
    if (length(params) > 1) {
      zlim <- bioRad:::get_zlim(param, NULL)  # @ TODO: Change in bioRad?: second argument is unused.
      
      if (param %in% c("BIOLR", "BIOLD")) {
        zlim <- c(0, 1)
      }
      if (param == "DPR") {
        zlim <- c(-11, 25)
      }
    }
    
    if (basemap) {
      basemap1 <- download_basemap(ppi1, source = "osm", verbose = FALSE)
      basemap2 <- download_basemap(ppi2, source = "osm", verbose = FALSE)
      
      p1 <- map(ppi1, map = basemap1, param = param, zlim = zlim, alpha = alpha, radar_size = radar_size) + labs(title = paste(title1, ": ", param, sep = ""))
      p2 <- map(ppi2, map = basemap2, param = param, zlim = zlim, alpha = alpha, radar_size = radar_size) + labs(title = paste(title2, ": ", param, sep = ""))
    } else {
      p1 <- plot(ppi1, param = param, zlim = zlim) + labs(title = paste(title1, ": ", param, sep = ""))
      p2 <- plot(ppi2, param = param, zlim = zlim) + labs(title = paste(title2, ": ", param, sep = ""))
    }
    
    p <- ggarrange(p1, p2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
    print(p)
  }
}