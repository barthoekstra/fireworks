bootstrap <- function (gbm.object, predictor = NULL, n.plots = length(pred.names), 
          list.4.preds = NULL, booted.preds = NULL, nrow = NULL, ncol = NULL, 
          col.line = "darkorange", cex.line = 0.5, type.ci = "lines", 
          col.ci = "grey80", cex.ci = 0.3, lty.ci = 2, alpha.ci = 0.5, 
          smooth = FALSE, col.smooth = "blue", cex.smooth = 0.3, span = 0.3, 
          rug = FALSE, rug.pos = "t", common.scale = TRUE, cis = c(0.025, 0.975), 
          y.label = "Fitted function", x.label = NULL, scale = TRUE, cisn = c(95, 80),
          ...) 
{
  gbm.call <- gbm.object$gbm.call
  pred.names <- gbm.call$predictor.names
  ggPD_boot.plots <- function(gbm.object) {
    if (!requireNamespace("gbm")) {
      stop("you need to install the gbm package to run this function")
    }
    if (is.null(booted.preds)) {
      stop("you need to set booted.preds as the array from the bootstrap run\n           (eg testboot$function.preds using testboot<-gbm.bootstrap.functions())")
    }
    # if (is.null(list.4.preds)) {
    #   stop("you need to set list.4.preds as the result of plot.gbm.4list()")
    # }
    requireNamespace("splines")
    gbm.x <- gbm.call$gbm.x
    response.name <- gbm.call$response.name
    nt <- gbm.object$n.trees
    data <- gbm.call$dataframe
    max.vars <- length(gbm.object$contributions$var)
    if (n.plots > max.vars) {
      n.plots <- max.vars
      warning("reducing no of plotted predictors to maximum available (", 
              max.vars, ")")
    }
    predictors <- list(rep(NA, n.plots))
    responses <- list(rep(NA, n.plots))
    responses.ci <- list(rep(NA, n.plots))
    responses.mean <- list(rep(NA, n.plots))
    # responses.lower <- list(rep(NA, n.plots))
    # responses.upper <- list(rep(NA, n.plots))
    
    for (j in c(1:max.vars)) {
      k <- match(gbm.object$contributions$var[j], pred.names)
      if (is.null(x.label)) {
        var.name <- gbm.call$predictor.names[k]
      }
      else {
        var.name <- x.label
      }
      pred.data <- data[, gbm.call$gbm.x[k]]
      response.matrix <- gbm::plot.gbm(gbm.object, i.var = k, 
                                       n.trees = nt, return.grid = TRUE, ...)
      predictors[[j]] <- response.matrix[, 1]
      if (is.factor(data[, gbm.call$gbm.x[k]])) {
        predictors[[j]] <- factor(predictors[[j]], levels = levels(data[, 
                                                                        gbm.call$gbm.x[k]]))
      }
      num.values <- nrow(response.matrix)
      responses[[j]] <- response.matrix[, 2]
      
      # temp <- apply(booted.preds[, k, ], 1, function(x) {
      #   quantile(x, cis[1], na.rm = T)
      # })
      # responses.lower[[j]] <- temp[1:num.values]
      # 
      # temp <- apply(booted.preds[, k, ], 1, function(x) {
      #   quantile(x, cis[2], na.rm = T)
      # })
      # responses.upper[[j]] <- temp[1:num.values]
      
      temp <- apply(booted.preds[, k, ], 1, function(x) {
        cis <- cisn / 100
        cis <- c(cis, 1 - cis)
        quantile(x, cis, na.rm = T, names = TRUE)
      })
      
      responses.ci[[j]] <- temp
      
      temp <- apply(booted.preds[, k, ], 1, function(x) {
        mean(x, na.rm = TRUE)
      })
      
      responses.mean[[j]] <- temp
      
      if (scale) {
        responses[[j]] <- responses[[j]] - mean(responses[[j]])
        responses.ci[[j]] <- responses.ci[[j]] - mean(responses.ci[[j]])
        responses.mean[[j]] <- responses.mean[[j]] - mean(responses.mean[[j]])
      }
      
      # if (scale) {
      #   shift <- mean(responses[[j]])
      #   responses[[j]] <- responses[[j]] - shift
      #   responses.lower[[j]] <- responses.lower[[j]] - shift
      #   responses.upper[[j]] <- responses.upper[[j]] - shift
      # }
      
      # if (scale) {
      #   temp <- temp - mean(temp)
      # }
      
      if (j == 1) {
        ymin = min(responses[[j]])
        ymax = max(responses[[j]])
        dat <- data.frame(pred.data)
      }
      else {
        ymin = min(ymin, min(responses[[j]]))
        ymax = max(ymax, max(responses[[j]]))
        dat <- data.frame(dat, pred.data)
      }
    }
    if (is.null(predictor)) {
      fittedFunc <- list()
      fittedFunc.lower <- list()
      fittedFunc.upper <- list()
      fittedVal <- list()
      ribbon <- list()
      ggPD <- list()
      for (i in 1:n.plots) {
        k <- match(gbm.object$contributions$var[i], 
                   pred.names)
        var.name <- gbm.call$predictor.names[k]
        fittedFunc[[i]] <- data.frame(predictors[i], 
                                      responses[i])
        colnames(fittedFunc[[i]]) <- c("x", "y")
        fittedFunc.lower[[i]] <- data.frame(predictors[i], 
                                            responses.lower[i])
        colnames(fittedFunc.lower[[i]]) <- c("x", "y")
        fittedFunc.upper[[i]] <- data.frame(predictors[i], 
                                            responses.upper[i])
        colnames(fittedFunc.upper[[i]]) <- c("x", "y")
        fittedVal[[i]] <- data.frame(gbm.object$fitted, 
                                     dat[i])
        colnames(fittedVal[[i]]) <- c("y", "x")
        ribbon[[i]] <- data.frame(x = fittedFunc.lower[[i]]$x, 
                                  ylow = fittedFunc.lower[[i]]$y, yup = fittedFunc.upper[[i]]$y)
        if (is.factor(fittedFunc[[i]]$x)) {
          ggPD[[i]] <- ggplot(fittedFunc[[i]], aes(x = x, 
                                                   y = y)) + geom_boxplot(group = 1, color = col.line, 
                                                                          size = cex.line) + geom_boxplot(data = fittedFunc.lower[[i]], 
                                                                                                          aes(x = x, y = y), color = col.ci, group = 1) + geom_boxplot(data = fittedFunc.upper[[i]], 
                                                                                                                                                            aes(x = x, y = y), color = col.ci, group = 1) + ylab(y.label) + 
            xlab(paste(var.name, "  (", round(gbm.object$contributions[i, 
                                                                       2], 1), "%)", sep = "")) + theme_bw() + 
            theme(panel.grid.minor = element_line(linetype = "blank"), 
                  panel.grid.major = element_line(linetype = "blank"), 
                  axis.text.x = element_text(size = 6), 
                  axis.title.x = element_text(size = 10), 
                  axis.line.y = element_line(size = 0.1), 
                  axis.line.x = element_line(size = 0.1))
          if (common.scale == T) {
            ggPD[[i]] <- ggPD[[i]] + ylim(c(ymin, ymax))
          }
        }
        if (type.ci == "lines") {
          ggPD[[i]] <- ggplot(fittedFunc[[i]], aes(x = x, 
                                                   y = y)) + geom_line(color = col.line, size = cex.line) + 
            geom_line(data = fittedFunc.lower[[i]], 
                      aes(x = x, y = y), size = cex.ci, color = col.ci, 
                      linetype = lty.ci) + geom_line(data = fittedFunc.upper[[i]], 
                                                     aes(x = x, y = y), size = cex.ci, color = col.ci, 
                                                     linetype = lty.ci) + ylab(y.label) + xlab(paste(var.name, 
                                                                                                     "  (", round(gbm.object$contributions[i, 
                                                                                                                                           2], 1), "%)", sep = "")) + theme_bw() + 
            theme(panel.grid.minor = element_line(linetype = "blank"), 
                  panel.grid.major = element_line(linetype = "blank"), 
                  axis.title.x = element_text(size = 10), 
                  axis.line.y = element_line(size = 0.1), 
                  axis.line.x = element_line(size = 0.1))
          if (smooth == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_smooth(span = span, 
                                                 size = 0.3, color = col.smooth, se = F, 
                                                 linetype = 2)
          }
          if (rug == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_rug(data = fittedVal[[i]], 
                                              aes(x = x, y = y), sides = rug.pos, position = "jitter", 
                                              color = "#EBEBEB")
          }
          if (common.scale == T) {
            ggPD[[i]] <- ggPD[[i]] + ylim(c(ymin, ymax))
          }
        }
        if (type.ci == "ribbon") {
          ggPD[[i]] <- ggplot() + geom_ribbon(data = ribbon[[i]], 
                                              aes(x = x, ymin = ylow, ymax = yup), fill = col.ci, 
                                              alpha = alpha.ci) + geom_line(data = fittedFunc[[i]], 
                                                                            aes(x = x, y = y), color = col.line, size = cex.line) + 
            ylab(y.label) + xlab(paste(var.name, "  (", 
                                       round(gbm.object$contributions[i, 2], 1), 
                                       "%)", sep = "")) + theme_bw() + theme(panel.grid.minor = element_line(linetype = "blank"), 
                                                                             panel.grid.major = element_line(linetype = "blank"), 
                                                                             axis.title.x = element_text(size = 10), 
                                                                             axis.line.y = element_line(size = 0.1), 
                                                                             axis.line.x = element_line(size = 0.1))
          if (smooth == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_smooth(data = fittedFunc[[i]], 
                                                 aes(x = x, y = y), span = span, size = 0.3, 
                                                 color = col.smooth, se = F, linetype = 2)
          }
          if (rug == T) {
            ggPD[[i]] <- ggPD[[i]] + geom_rug(data = fittedVal[[i]], 
                                              aes(x = x, y = y), sides = rug.pos, position = "jitter", 
                                              color = "#EBEBEB")
          }
          if (common.scale == T) {
            ggPD[[i]] <- ggPD[[i]] + ylim(c(ymin, ymax))
          }
        }
      }
      list(ggPD = ggPD)
    }
    else {
      if (is.character(predictor)) {
        predictor <- match(predictor, gbm.object$contributions$var)
      }
      k <- match(gbm.object$contributions$var[predictor], 
                 pred.names)
      var.name <- gbm.call$predictor.names[k]
      fittedFunc <- data.frame(predictors[predictor], 
                               responses[predictor])
      colnames(fittedFunc) <- c("x", "y")
      
      if (!is.factor(fittedFunc$x)) {
        fittedFunc.ci <- data.frame(predictors[predictor], 
                                    t(as.data.frame(responses.ci[predictor])))
        colnames(fittedFunc.ci) <- c("x", rownames(responses.ci[[predictor]]))
        
        fittedFunc.mean <- data.frame(predictors[predictor],
                                      as.data.frame(responses.mean[predictor]))
        colnames(fittedFunc.mean) <- c("x", "y")
        
        max_cols <- paste(cisn, "%", sep = "")
        min_cols <- paste(c(100 - cisn), "%", sep = "")
        
        fittedFunc.ci %>%
          select(x, all_of(min_cols)) %>%
          pivot_longer(cols = ends_with("%")) %>%
          rename("percentile" = "name", "ymin" = "value") %>%
          mutate(percentile = gsub("%", "", percentile),
                 percentile = as.numeric(percentile),
                 percentile = 100 - percentile) -> fittedFunc.ci.lower
        
        fittedFunc.ci %>%
          select(x, all_of(max_cols)) %>%
          pivot_longer(cols = ends_with("%")) %>%
          rename("percentile" = "name", "ymax" = "value") %>%
          mutate(percentile = gsub("%", "", percentile),
                 percentile = as.numeric(percentile)) %>%
          left_join(fittedFunc.ci.lower, by = c("x" = "x", "percentile" = "percentile")) -> ribbon
        
        data_quantiles <- quantile(dat[predictor], seq(0, 1, 0.1), na.rm = TRUE)
        fittedVal <- data.frame(data_quantiles)
        colnames(fittedVal) <- c("x_10")
        fittedVal$x_05 <- quantile(dat[predictor], c(NA, seq(0.05, 0.95, 0.1)), na.rm = TRUE)
        fittedVal$y <- 1
      }
      
      # fittedFunc.lower <- data.frame(predictors[predictor], 
      #                                responses.lower[predictor])
      # colnames(fittedFunc.lower) <- c("x", "y")
      # fittedFunc.upper <- data.frame(predictors[predictor], 
      #                                responses.upper[predictor])
      # colnames(fittedFunc.upper) <- c("x", "y")
      
      # Modifications to plotting
      
      
      # ribbon <- data.frame(x = fittedFunc.lower$x, ylow = fittedFunc.lower$y, 
      #                      yup = fittedFunc.upper$y)
      
      # fittedVal <- data.frame(gbm.object$fitted, dat[predictor])
      # colnames(fittedVal) <- c("y", "x")
      # if (scale) {
      #   # fittedVal$y <- fittedVal$y - mean(fittedVal$y)
      #   # fittedVal$y <- (fittedVal$y - min(fittedVal$y, na.rm = TRUE)) /
      #   #   (max(fittedVal$y, na.rm = TRUE) - min(fittedVal$y, na.rm = TRUE))
      #   fittedVal$
      # }
      
      if (is.factor(fittedFunc$x)) {
        d <- as.data.frame(t(booted.preds[, k, ]))
        as.data.frame(booted.preds[, k, ]) %>%
          drop_na() %>%
          t() -> d
        
        colnames(d) <- droplevels(predictors[predictor][[1]])
        
        d %>%
          as.data.frame() %>%
          # mutate(index = 1:nrow(d)) %>%
          pivot_longer(cols = everything(), names_to = "factor") %>%
          mutate(value = value - mean(value, na.rm = TRUE)) -> d
          
        
        
        # colnames(d) <- droplevels(predictors[predictor][[1]])
        # d %>% select_if(!starts_with("")) %>%
        #   pivot_longer(cols = everything()) %>%
        #   mutate(value = value - mean(value, na.rm = TRUE)) -> d
        
        ggPD <- ggplot(d, aes(value, factor)) +
          geom_boxplot() +
          ylab(y.label) +
          xlab(paste(var.name, "  (", round(gbm.object$contributions[predictor, 2], 1), "%)", sep = "")) + theme_bw() +
          theme(panel.grid.minor = element_line(linetype = "blank"),
               panel.grid.major = element_line(linetype = "blank"),
               axis.text.x = element_text(size = 6), axis.title.x = element_text(size = 10),
               axis.line.y = element_line(size = 0.1),
               axis.line.x = element_line(size = 0.1))
        
        # ggPD <- ggplot(fittedFunc, aes(x = x, y = y)) + 
        #   geom_boxplot(color = col.line, size = cex.line) + 
        #   geom_boxplot(data = fittedFunc.lower, aes(x = x, 
        #                                             y = y), color = col.ci) + geom_boxplot(data = fittedFunc.upper, 
        #                                                                                    aes(x = x, y = y), color = col.ci) + ylab(y.label) + 
        #   xlab(paste(var.name, "  (", round(gbm.object$contributions[predictor, 
        #                                                              2], 1), "%)", sep = "")) + theme_bw() + 
        #   theme(panel.grid.minor = element_line(linetype = "blank"), 
        #         panel.grid.major = element_line(linetype = "blank"), 
        #         axis.text.x = element_text(size = 6), axis.title.x = element_text(size = 10), 
        #         axis.line.y = element_line(size = 0.1), 
        #         axis.line.x = element_line(size = 0.1))
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      if (type.ci == "lines" & !is.factor(fittedFunc$x)) {
        ggPD <- ggplot(fittedFunc, aes(x = x, y = y)) + 
          geom_line(color = col.line, size = cex.line) + 
          geom_line(data = fittedFunc.lower, aes(x = x, 
                                                 y = y), size = cex.ci, color = col.ci, linetype = lty.ci) + 
          geom_line(data = fittedFunc.upper, aes(x = x, 
                                                 y = y), size = cex.ci, color = col.ci, linetype = lty.ci) + 
          ylab(y.label) + xlab(paste(var.name, "  (", 
                                     round(gbm.object$contributions[predictor, 
                                                                    2], 1), "%)", sep = "")) + theme_bw() + 
          theme(panel.grid.minor = element_line(linetype = "blank"), 
                panel.grid.major = element_line(linetype = "blank"), 
                axis.title.x = element_text(size = 10), 
                axis.line.y = element_line(size = 0.1), 
                axis.line.x = element_line(size = 0.1))
        if (smooth == T) {
          ggPD <- ggPD + geom_smooth(span = span, size = 0.3, 
                                     color = col.smooth, se = F, linetype = 2)
        }
        if (rug == T) {
          ggPD <- ggPD + geom_rug(data = fittedVal, 
                                  aes(x = x, y = y), sides = rug.pos, position = "jitter", 
                                  color = "#EBEBEB")
        }
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      if (type.ci == "ribbon" & !is.factor(fittedFunc$x)) {
        ggPD <- ggplot()
        
        for (i in sort(unique(ribbon$percentile), decreasing = TRUE)) {
          d <- filter(ribbon, percentile == i)
          d$ymax_smooth <- predict(loess(ymax ~ x, data = d, span = 0.4))
          d$ymin_smooth <- predict(loess(ymin ~ x, data = d, span = 0.4))
          ggPD <- ggPD +
            # geom_ribbon(data = d, aes(x = x, ymin = ymin, ymax = ymax, group = percentile, fill = percentile), alpha = 0.3) +
            geom_ribbon(data = d, aes(x = x, ymin = ymin_smooth, ymax = ymax_smooth, fill = factor(percentile)))
            # geom_ribbon(data = d, aes(x = x, ymin = ymin, ymax = ymax, fill = factor(percentile)))
        }
        fittedFunc.mean$y_smooth <- predict(loess(y ~ x, data = fittedFunc.mean, span = 0.4))
        ggPD <- ggPD + 
          # geom_line(data = fittedFunc, aes(x = x, y = y), color = "grey60", size = 1, linetype = "dotted") +
          geom_line(data = fittedFunc.mean, aes(x = x, y = y_smooth), color = "black", size = 1.0) +
          # geom_line(data = fittedFunc.mean, aes(x = x, y = y), color = "black", size = 1.0) +
          # scale_fill_continuous(type = "viridis", trans = "reverse") +
          # scale_alpha_continuous(range = c(0.2, 0.3)) +
          scale_fill_manual(values = c("#3182bd", "#9ecae1"), guide = guide_legend(title = "Conf.int")) +
          scale_x_continuous(expand = c(0, 0)) +
          ylab(y.label) + 
          xlab(paste(var.name, "  (", round(gbm.object$contributions[predictor, 2], 1), "%)", sep = "")) + 
          theme_bw() + 
          theme(panel.grid.minor = element_line(linetype = "blank"), 
                panel.grid.major = element_line(linetype = "blank"), 
                axis.title.x = element_text(size = 10), 
                axis.line.y = element_line(size = 0.1), 
                axis.line.x = element_line(size = 0.1))
        
        # ggPD <- ggPD + geom_ribbon(data = ribbon, 
        #                                aes(x = x, ymin = ylow, ymax = yup), fill = col.ci, 
        #                                alpha = alpha.ci) + geom_line(data = fittedFunc, 
        #                                                              aes(x = x, y = y), color = col.line, size = cex.line) + 
        #   ylab(y.label) + xlab(paste(var.name, "  (", 
        #                              round(gbm.object$contributions[predictor, 
        #                                                             2], 1), "%)", sep = "")) + theme_bw() + 
        #   theme(panel.grid.minor = element_line(linetype = "blank"), 
        #         panel.grid.major = element_line(linetype = "blank"), 
        #         axis.title.x = element_text(size = 10), 
        #         axis.line.y = element_line(size = 0.1), 
        #         axis.line.x = element_line(size = 0.1))
        if (smooth == T) {
          ggPD <- ggPD + geom_smooth(data = fittedFunc, 
                                     aes(x = x, y = y), span = span, size = 0.3, 
                                     color = col.smooth, se = F, linetype = 2)
        }
        if (rug == T) {
          # ggPD <- ggPD + geom_rug(data = fittedVal, 
          #                         aes(x = x, y = y), sides = rug.pos, position = "jitter", 
          #                         color = "#EBEBEB")
          ggPD <- ggPD + geom_rug(data = fittedVal,
                                  aes(x = x_10), sides = rug.pos, position = "identity",
                                  color = "#636363") +
            geom_rug(data = fittedVal, aes(x = x_05), sides = rug.pos, position = "identity",
                     color = "#EBEBEB", length = unit(0.02, "npc"))
          # ggPD <- ggPD + geom_rug(data )
        }
        if (common.scale == T) {
          ggPD <- ggPD + ylim(c(ymin, ymax))
        }
      }
      list(ggPD = ggPD)
    }
  }
  plot <- ggPD_boot.plots(gbm.object)
  if (is.null(predictor)) {
    # do.call(grid.arrange, c(plot$ggPD, list(nrow = nrow, 
    #                                         ncol = ncol)))
    do.call(wrap_plots, c(plot$ggPD, list(nrow = nrow, ncol = ncol)))
  }
  else grid.draw(plot$ggPD)
}

