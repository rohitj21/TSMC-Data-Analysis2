
#-----------------------------------#
p<-3
cutoffs <- seq.default(from = 216, to  = 288-p, by = p)
logrev <- ts(log10(timeseries$Revenue) , start = 1999, frequency = 12)

arima.pred<-NULL
for(endpoint in cutoffs){
  dat <- ts(logrev[1:endpoint],start = 1999, frequency = 12)
  m <- forecast(Arima(dat,order=c(4,1,1),seasonal=c(1,0,2)),h=p)
  arima.pred = c(arima.pred, m$mean)
}
arima.pred = data.frame("Time" = timeseries$Date[217:288], "Prediction" = arima.pred)

hw.pred <- NULL

for(endpoint in cutoffs){
    dat <- ts(logrev[1:endpoint],start = 1999, frequency = 12)
    Zt <- diff(dat)
    m <- hw(Zt, h = p, seasonal = "additive")
    Yt <- dat[endpoint] + diffinv(m$mean)[-1]
    hw.pred = c(hw.pred, Yt)
}
hw.pred = data.frame("Time" = timeseries$Date[217:288], "Prediction" = hw.pred)


#--------------------------------#
prophet.pred <- NULL
for(endpoint in cutoffs){
    Zt <- diff(logrev)
    df <- data.frame(ds = seq(as.Date('1999-02-01'), as.Date('2023-04-01'), by = 'm'),y= Zt)

    m<-prophet(df[1:endpoint,], growth = 'linear',
               yearly.seasonality = 'auto',
               weekly.seasonality = F,
               daily.seasonality = F,
               seasonality.mode = 'additive',
    )
    future <- make_future_dataframe(m, freq='month', periods=12)
    forecast <- predict(m, future[])[endpoint + (1:p),16]
    Yt <- logrev[endpoint] + diffinv(forecast)[-1]
    prophet.pred <- c(prophet.pred, Yt)
}
prophet.pred <- data.frame("Time" = timeseries$Date[217:288], "Prediction" = prophet.pred)


plot(1:72, logrev[217:288], type = 'l')
lines(1:72, arima.pred$Prediction, col  = 'red')
lines(1:72, hw.pred$Prediction, col  = 'blue')


#--------------------------------#

pridictions <- data.frame(
  "Time" = hw.pred$Time,
  "RealValue" = 10^logrev[216 + (1:72)],
  "HW" = 10^hw.pred$Prediction,
  "Prophet" = 10^prophet.pred$Prediction,
  "ARIMA"= 10^arima.pred$Prediction
)

RevenuePridiction <- cbind("Time" = pridictions[,1], (pridictions[,2:5]))

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

p3 <- ggplot(CV.dat, aes(x = time, y = ARIMA))+
  geom_line(linewidth = 1, col = "darkblue")+theme_light()+
  geom_line(aes(y=RealValue))+
  labs(title = "Sarima Model", x= '', y = '')+
  scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x),
                     labels = label_number(accuracy = 1))
p4 <- grid.arrange(p1,p2,p3,p0, top=textGrob("Cross Validation Model Comparision"))
ggsave("Plots/CrossValidation.png",plot = p4, height = 720, width = 1080, scale = 2, units = 'px')



