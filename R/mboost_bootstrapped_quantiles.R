mboost_bootstrapped_quantiles <- function(modelci, level, which = NULL, raw = FALSE) {
  pred_id <- modelci$model$which(which, usedonly = FALSE)
  preds <- sapply(modelci$boot_pred, function(p) p[[pred_id]])
  # preds <- do.call("cbind", preds)
  if (!raw) {
    quantiles <- c((1 - level)/2, 1 - (1 - level)/2)
    preds <- apply(preds, 1, FUN = quantile, probs = quantiles)
  }
  return(preds)
}