clip <- function(data, bounds = c(0.05, 0.99)){
  data[data == 0] <- NA
  quantiles = stats::quantile(data, c(bounds[1], bounds[2]), na.rm = TRUE)
  data[data < quantiles[1]] = quantiles[1]
  data[data > quantiles[2]] = quantiles[2]
  data[is.na(data)] <- 0
  return(data)
}