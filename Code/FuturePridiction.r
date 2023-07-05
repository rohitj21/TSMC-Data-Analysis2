Y <- log10(timeseries$Revenue)
bestModelPrid <-forecast(Arima(ts(Y, start = 1999, frequency = 12),
      order = c(4, 1, 1),
      seasonal = c(1, 0, 2)), h = 3)
bestModelPrid$mean <- 10^bestModelPrid$mean
bestModelPrid$lower <- 10^bestModelPrid$lower
bestModelPrid$upper <- 10^bestModelPrid$upper
prid <- cbind(bestModelPrid$lower[,2], bestModelPrid$mean, bestModelPrid$upper[,2])
colnames(prid) <- c("Lower 90%", "Point Estimate", "Upper 90%")
prid


