library(mboost)

confint.mboost_adjusted <- function(object, parm = NULL, level = 0.95,
                           B = 1000, B.mstop = 25, newdata = NULL,
                           which = parm, outerfolds = NULL,
                           papply = ifelse(B.mstop == 0, mclapply, lapply),
                           cvrisk_options = list(), ...) {
  
  which <- object$which(which, usedonly = FALSE)
  if (!all(which %in% object$which(NULL, usedonly = FALSE)))
    stop(sQuote("which"), " is wrongly specified")
  
  if (!is.list(cvrisk_options))
    stop(sQuote("cvrisk_options"), " must be a named list")
  if (length(cvrisk_options) > 0 && is.null(names(cvrisk_options)))
    stop(sQuote("cvrisk_options"), " must be a named list")
  if ("folds" %in% names(cvrisk_options))
    stop("One cannot modify the folds of the inner bootstrap")
  if ("object" %in% names(cvrisk_options))
    stop("One cannot specify the model (object) of the inner bootstrap")
  
  ## create new data and/or restructure data
  newdata <- .create_newdata(object, newdata, which)
  
  if (is.null(outerfolds)) {
    outer.folds <- cv(model.weights(object), B = B)
  } else {
    outer.folds <- as.matrix(outerfolds)
  }
  
  cat("Start computing bootstrap confidence intervals... \n")

  do_update <- function(i) {
    #for (i in 1:B) {
    cat("\rB =", i)
    ## update model
    mod <- update(object, weights = outer.folds[, i],
                  risk = "inbag", trace = FALSE)
    if (B.mstop > 0) {
      ## <FIXME> are the weights handled correctly?
      cvr <- do.call("cvrisk",
                     args = c(list(object = mod,
                                   folds = cv(model.weights(mod), B = B.mstop)),
                              cvrisk_options))
      mod[mstop(cvr)]
    }
    list(.predict_confint(mod, newdata = newdata, which = which), mod)
  }
  predictions <- papply(1:B, do_update, ...)
  cat("\n")

  ## prepare returned object
  res <- list(level = level, boot_pred = predictions[[1]][[1]], data = newdata, varimp = as.data.frame(varimp(predictions[[1]][[2]])))
  attr(res, "which") <- which
  class(res) <- "mboost.ci"
  return(res)
}

## FIXME: Aditionally needed: Does multivariate bols base-learners work correctly?
.create_newdata <- function(object, newdata = NULL, which, ...) {
  if (is.null(newdata)) {
    data <- newdata <- model.frame(object, which = which)
    nms <- names(object$baselearner)[which]
    
    for (w in which) {
      ## get data from w-th base-learner
      tmp <- data[[w]][rep(1, 1000), , drop = FALSE]
      
      ## are there varying coefficients (i.e. argument by)
      get_vary <- object$baselearner[[w]]$get_vary
      vary <- ""
      if (!is.null(get_vary)) vary <- get_vary()
      if (vary != "") {
        if (is.factor(tmp[[vary]])) {
          if (nlevels(tmp[[vary]]) > 2)
            stop("Atomatic data creation for ", sQuote("by"),
                 " variables with more than two levels is",
                 " currently not supported;",
                 " Specify ", sQuote("newdata"), " instead.")
          data[[w]][[vary]] <- factor(levels(data[[w]][[vary]])[-1],
                                      levels = levels(data[[w]][[vary]]))
        }
        if (is.numeric(tmp[[vary]]))
          data[[w]][[vary]] <- 1
      }
      
      ## now make grid
      grid <- function(x) {
        if (is.numeric(x)) {
          return(seq(min(x), max(x), length = 1000))
        } else {
          return(rep(unique(x), length.out = 1000))
        }
      }
      
      for (j in 1:ncol(data[[w]]))
        tmp[, colnames(data[[w]])[j]] <- grid(data[[w]][,j])
      
      ## FIXME: what about btree and bmrf?
      
      ## check if any base-learner is a bivariate smooth effect, i.e. if
      ## base-learner is multivariate and bbs, bspatial or brad
      which.vary <- colnames(tmp) == vary
      multivar <- grepl("bbs|bspatial|brad", nms[w]) &
        ncol(tmp[!which.vary]) >= 2
      if (multivar) {
        ## make grid
        egrid <- expand.grid(tmp[!which.vary])
        if (vary != "") {
          x.vary <- tmp[which.vary]
          rownames(x.vary) <- NULL
          tmp <- cbind(egrid, x.vary)
        } else {
          tmp <- egrid
        }
      }
      ## reset rownames
      rownames(tmp) <- NULL
      newdata[[w]] <- tmp
    }
  } else {
    ## restructure new data
    data <- model.frame(object, which = which)
    nms <- lapply(data, colnames)
    tmp <- lapply(nms, function(x) newdata[, x, drop = FALSE])
    newdata <- tmp
  }
  return(newdata)
}

## special prediction function for the construction of confidence intervals:
.predict_confint <- function(object, newdata = NULL, which, ...) {
  nrows <- sapply(newdata, nrow)
  predictions <- sapply(which, function(w)
    matrix(predict(object, newdata[[w]], which = w),
           ncol = 1, nrow = nrows[w]))
  if (is.matrix(predictions))
    predictions <- as.data.frame(predictions)
  names(predictions) <- names(newdata[which])
  return(predictions)
}

data_cleaned <- readRDS("data/models/data_cleaned.RDS")
model <- readRDS("data/models/model.RDS")

# folds <- cv(model.weights(model), B = 1000)
# saveRDS(folds, file = "data/models/confints/folds.RDS")
folds <- readRDS("data/models/confints/folds.RDS")

for (i in 401:600) {
  if (i == 1) {
    ci <- confint.mboost(model, parm = NULL, level = 0.9, B = 1, B.mstop = 0)
  } else {
    ci <- confint.mboost_adjusted(model, parm = NULL, level = 0.9, B = 1, B.mstop = 0, outerfolds = folds[, i])
  }
  saveRDS(ci, file = paste0("data/models/confints/modelci_", i, ".RDS"))
  rm(ci)
}

# cis <- lapply(paste0("data/models/confints/", list.files("data/models/confints/")[4:361]), readRDS)
# ci_boot_pred <- lapply(cis, function(x) x$boot_pred[[1]])
# modelci <- readRDS("~/fireworks/data/models/confints/modelci_1.RDS")
# ci_boot_pred[359] <- modelci$boot_pred
# modelci$boot_pred <- ci_boot_pred

