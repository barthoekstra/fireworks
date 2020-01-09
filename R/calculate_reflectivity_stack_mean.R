calculate_reflectivity_stack_mean <- function(stack, mds) {
  for (i in seq_along(stack$scans)) {
    stack$scans[[i]]$params$DBZH[is.na(stack$scans[[i]]$params$DBZH)] <- mds
    
    DBZH_mean <- apply(stack$scans[[i]]$params$DBZH, c(2, 3), mean)
    
    stack$scans[[i]]$params$DBZH_AVG <- DBZH_mean
    class(stack$scans[[i]]$params$DBZH_AVG) <- c("param", "matrix")
    attributes(stack$scans[[i]]$params$DBZH_AVG) <- attributes(stack$scans[[i]]$params$VRADH)
    attributes(stack$scans[[i]]$params$DBZH_AVG)$param <- "DBZH_AVG"
    
    stack$scans[[i]]$params$DBZH <- NULL
  }
  
  return(stack)
}