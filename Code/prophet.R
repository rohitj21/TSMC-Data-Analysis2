Y <- log10(timeseries$Revenue)
Z <- diff(Y)
df <- data.frame(ds = seq(as.Date('1999-02-01'), as.Date('2023-04-01'), by = 'm'),y= Z)
m<-prophet(df, growth = 'linear',
           yearly.seasonality = 'auto',
           weekly.seasonality = F,
           daily.seasonality = F,
           seasonality.mode = 'additive',
)
future <- make_future_dataframe(m,freq='month', periods=1)
forecast <- predict(m, future)[1:291,]
y_hat <- c(Y[1], Y[1:291]+ forecast$yhat)

dat.prophetmodel <- data.frame("Year"=timeseries$Time, "Fitted" = 10^y_hat, "Actual"=10^Y)
dat.prophetmodel <- melt(dat.prophetmodel, id.vars = "Year", variable.name = "Series", value.name = "Revenue")
fittedprophet <- ggplot(dat.prophetmodel, aes(x = Year, y=Revenue, col = Series)) +
    geom_line() + theme_light() + labs(title = "Prophet Model",
                                       caption = "Revenue is in the units of 1 billion NTD")+
    scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x),
                       labels = label_number(accuracy = 1))+
    xlab("Time (years)")
fittedprophet
ggsave("Final Report/Plots/fittedprophetplot.png", fittedprophet, width = 900, height = 350, units = 'px', scale = 3)


















