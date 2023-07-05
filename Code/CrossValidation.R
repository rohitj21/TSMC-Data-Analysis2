p<-3
cutoffs <- seq.default(from = 216, to  = 288-p, by = p)
logrev <- ts(log10(timeseries$Revenue) , start = 1999, frequency = 12)
iters <- length(cutoffs)
sarima.pred<-NULL
hw.pred <- NULL
prophet.pred <- NULL
for(endpoint in cutoffs){
  print(paste(iters, "iterations left"))
  # Sarima
  dat <- ts(logrev[1:endpoint],start = 1999, frequency = 12)
  m1 <- Arima(dat,order=c(4,1,1),seasonal=c(1,0,2))
  sarima_forecast <- forecast(m1,h=p)
  sarima.pred = c(sarima.pred, sarima_forecast$mean)

  # Holt Winter
  Zt <- diff(dat)
  m2 <- hw(Zt, h = p, seasonal = "additive")
  hwYt <- dat[endpoint] + diffinv(m2$mean)[-1]
  hw.pred = c(hw.pred, hwYt)

  # Prophet

  Zt <- diff(logrev)
  df <- data.frame(ds = seq(as.Date('1999-02-01'), as.Date('2023-04-01'), by = 'm'),y= Zt)

  m3 <-prophet(df[1:(endpoint-1),], growth = 'linear',
             yearly.seasonality = 'auto',
             weekly.seasonality = F,
             daily.seasonality = F,
             seasonality.mode = 'additive',
  )
  future <- make_future_dataframe(m3, freq='month', periods=12)
  forecast <- predict(m3, future)
  pYt <- logrev[endpoint] + diffinv(forecast[endpoint-1 + (1:p),16])[-1]
  prophet.pred <- c(prophet.pred, pYt)

  iters <- iters - 1
}

RevenuePridiction <- data.frame(
  "Time" = timeseries$Date[217:288],
  "RealValue" = 10^logrev[217:288],
  "HW" = 10^hw.pred,
  "Prophet" = 10^prophet.pred,
  "SARIMA"= 10^sarima.pred
)

err <- RevenuePridiction[3:5] - RevenuePridiction$RealValue

#################################################
RMSE <- NULL
MAE <- NULL
MASE <- NULL
MAPE <- NULL
for(i in 3:5){
  RMSE = c(RMSE, rmse(RevenuePridiction$RealValue, RevenuePridiction[, i]))
  MAE = c(MAE, mae(RevenuePridiction$RealValue, RevenuePridiction[, i]))
  MAPE = c(MAPE, mape(RevenuePridiction$RealValue, RevenuePridiction[, i]))
  MASE = c(MASE, mase(RevenuePridiction$RealValue, RevenuePridiction[, i], step_size = 12))
}

CV_determinants <- data.frame(RMSE, MAE, MAPE, MASE, row.names = colnames(RevenuePridiction)[3:5])

#--------------------------------------#
#exploratory plots
t.xaxis <- seq.default(2017, 2023-1/12, by = 1/12)

errtable <- cbind('Model' = c("Holt-Winters", "Prophet", "Sarima"), round(CV_determinants, digits = 4))
p0 <- ggplot()+ theme_classic()+annotate(geom = 'table', x=0, y=0, label = list(errtable))+
    theme_void()+
  labs(caption = "Revenue is in the units of 1 billion NTD      \n\n Black line denotes the actual revenue       \n\n\n\n")
p0

CV.dat <- data.frame("time" = t.xaxis, RevenuePridiction[, c(2, 3, 4, 5)])

p1 <- ggplot(CV.dat, aes(x = time, y = HW))+
  geom_line(aes(y=RealValue))+
  geom_line(linewidth = 1, col = "darkred")+theme_light()+
  labs(title = "Holt-Winters Model", x = '', y='')+
  scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x),
                     labels = label_number(accuracy = 1))

p2 <- ggplot(CV.dat, aes(x = time, y = Prophet))+
  geom_line(aes(y=RealValue))+
  geom_line(linewidth = 1, col = 'darkgreen')+theme_light()+
  labs(title = "Prophet Model", x = '', y = '')+
  scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x),
                     labels = label_number(accuracy = 1))

p3 <- ggplot(CV.dat, aes(x = time, y = SARIMA))+
  geom_line(linewidth = 1, col = "darkblue")+theme_light()+
  geom_line(aes(y=RealValue))+
  labs(title = "Sarima Model", x= '', y = '')+
  scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x),
                     labels = label_number(accuracy = 1))
p4 <- grid.arrange(p1,p2,p3,p0, top=textGrob("Cross Validation Model Comparision"))
ggsave("Final Report/Plots/CrossValidation.png",plot = p4, height = 720, width = 1080, scale = 2, units = 'px')


##---------##
# This is the code to linearly combine the three models
#
# combinedmodel <- lm(formula =
#                       RealValue~ HW*Prophet*SARIMA ,
#                     data = RevenuePridiction)
# summary(combinedmodel)
# checkresiduals(combinedmodel$residuals)
# mape(RevenuePridiction$RealValue, combinedmodel$fitted.values)
# plot(1:72, RevenuePridiction$RealValue, type = "l")
# lines(1:72, combinedmodel$fitted.values, col = "red", lwd=2)

