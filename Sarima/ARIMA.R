Y <- log10(timeseries$Revenue)
Z <- diff(Y, differences = 1)
plot_z <- ggplot(data.frame(Z),aes(x = 1:291, y = Z)) + geom_line(col = "darkred")+
        theme_light()+ xlab("t") + ylab(expression(~Z[t]) ) +
        labs(title = expression(~Z[t]~{'vs t plot'}),
             caption = expression(~{"Plot of "}~Z[t]~{"="}~Y[t]~{"-"}~Y[t-1]~{", "} ~Y[t]~{"="}~log[10]~(R[t])))
plot_z
ggsave("Plots/Ztplot.png", plot_z, width = 900, height = 350, units = 'px', scale = 2.5)

png('Plots/acfZ.png', height = 300, width = 600)
Acf(Z)
dev.off()
png('Plots/pacfZ.png', height = 300, width = 600)
Pacf(Z)
dev.off()

pValues <- c(1, 3, 4)
qValues <- c(1, 3, 4)
QValues <- c(0, 1, 2)
m = 12
P = 1
d = 1
D = 0
arimaComp <- matrix(0, nrow = 3*3*3, ncol = 4)
colnames(arimaComp) <- c("p", "q", "Q", "AICc value")
i = 1
for(p in pValues){
    for(q in qValues){
        for(Q in QValues){
            Amodel <- Arima(ts(Y, start = 1999, frequency = m),order = c(p, d, q), seasonal = c(P, D, Q))
            arimaComp[i,] <- c(p, q, Q, Amodel$aicc)
            i <- i+1
        }
    }
}
arimaComp <- data.frame(arimaComp)
bestvalues <-  as.numeric(arimaComp[which.min(arimaComp$AICc.value),][1:3])
bestmodel <- Arima(ts(Y, start = 1999, frequency = 12),
                   order = c(bestvalues[1], d, bestvalues[2]),
                   seasonal = c(P, D, bestvalues[3]))

dat.armodel <- data.frame("Year"=timeseries$Time, "Fitted" = 10^bestmodel$fitted, "Actual"=10^Y)
dat.armodel <- melt(dat.armodel, id.vars = "Year", variable.name = "Series", value.name = "Revenue")
fittedarima <- ggplot(dat.armodel, aes(x = Year, y=Revenue, col = Series)) +
    geom_line() + theme_light() + labs(title = "ARIMA(4,1,1)(1,0,2)[12]",
                                       caption = "Revenue is in the units of 1 billion NTD")+
    scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x),
                       labels = label_number(accuracy = 1))+
    xlab("Time (years)")
ggsave("Plots/fittedarimaplot.png", fittedarima, width = 900, height = 350, units = 'px', scale = 3)

checkresiduals(bestmodel$residuals, plot = F)$p.value
