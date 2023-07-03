
Y <- ts(log10(timeseries$Revenue), start = 1999, frequency = 12)
Z <- diff(Y)
hwadd <- hw(Z, seasonal = "additive")
hwadd$model
hwfittedvalue <- c(Y[1], Y[1:291] + hwadd$fitted)

dat.hwaddmodel <- data.frame("Year"=timeseries$Time, "Fitted" = 10^hwfittedvalue, "Actual"=10^Y)
dat.hwaddmodel <- melt(dat.hwaddmodel, id.vars = "Year", variable.name = "Series", value.name = "Revenue")
fittedhwadd <- ggplot(dat.hwaddmodel, aes(x = Year, y=Revenue, col = Series)) +
    geom_line() + theme_light() + labs(title = "Holt Winters Model",
                                       caption = "Revenue is in the units of 1 billion NTD")+
    scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x),
                       labels = label_number(accuracy = 1))+
    xlab("Time (years)")
fittedhwadd
ggsave("Plots/fittedhwaddplot.png", fittedhwadd, width = 900, height = 350, units = 'px', scale = 3)
