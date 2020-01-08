side_by_side_ppi <- function(pvol1, pvol2, title1 = "", title2 = "", params = "DBZH", scan_id = 1, range_max = 100000) {
  ppi1 <- project_as_ppi(pvol1$scans[[scan_id]], range_max = range_max)
  ppi2 <- project_as_ppi(pvol2$scans[[scan_id]], range_max = range_max)
  
  for (param in params) {
    zlim <- bioRad:::get_zlim(param, NULL)  # @ TODO: Change in bioRad?: second argument is unused.
    if (param %in% c("BIOLR", "BIOLD")) {
      zlim <- c(0, 1)
    }
    
    p1 <- plot(ppi1, param = param, zlim = zlim) + labs(title = paste(title1, ": ", param, sep = ""))
    p2 <- plot(ppi2, param = param, zlim = zlim) + labs(title = paste(title2, ": ", param, sep = ""))
    
    p <- ggarrange(p1, p2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
    print(p)
  }
}