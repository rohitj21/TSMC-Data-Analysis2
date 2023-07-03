
# we have all the data in matrix format
# revenue is in the units of billions of New Taiwan Dollars
dataMatrix <- data.frame((read.csv("Data/MonthlyData.csv")))
# changeing the row names to be year for convinience
dataMatrix[,2:14] <- dataMatrix[,2:14]/1000
rownames(dataMatrix) <- dataMatrix[,1]

#---------------------------------------------------------#
# making a different dataframe for yearly revenue
yearly.revenue <- data.frame("Year" = dataMatrix[,1],'Revenue' = dataMatrix[,14])
rownames(yearly.revenue) <- rownames(dataMatrix)
# we don't have the total revenue for year 2023 so we omit it
yearly.revenue <- na.omit(yearly.revenue)
yearly.revenue$Year <- as.factor(yearly.revenue$Year)
#-----------------------------------------------------------------------#
# for time series analyasis we need monthly revenue in a single vector

timeseries <- melt(dataMatrix[,-14], id.vars = "Year", variable.name = "Month", value.name = "Revenue")
timeseries <- timeseries[order(timeseries$Year, timeseries$Month),]
# omiting the 2023(may to dec) data as we don't have it
timeseries <- na.omit(timeseries)
rownames(timeseries) <- 1:292
timeseries$Year <- as.factor(timeseries$Year)
timeseries <- cbind(timeseries, 'Time' = seq(from = 1999, to =2023.25, by = 1/12))


Dates <- rep("", 300)
i <- 1
for(year in 1999:2023){
    for(month in 1:12){
        Dates[i] <- paste(year, month, '1', sep = '-')
        i <- i+1
    }
}

Dates
timeseries <- cbind.data.frame("Date"  = Dates[1:292], timeseries)
rownames(timeseries) <- as.Date(timeseries$Date)

#------------------------------------------#
HistoricalData <- data.frame(t(read.csv("Data/HistoricalData.csv"))) %>% remove_rownames()
QuaterlyTime <- seq.default(1999, 2023, by = 0.25)
HistoricalData[2:98,1] <- QuaterlyTime
HistoricalData[1,1] <- "Time"

rev.by.chipsize <- HistoricalData[2:98, c(1,10:22)] %>% `colnames<-`(HistoricalData[1,c(1,10:22)])
rev.by.chipsize <-  apply(rev.by.chipsize, c(1, 2),as.numeric )
rev.by.chipsize <-  apply(rev.by.chipsize, c(1, 2),function(x){
    if(is.na(x)){
        return(0)
    }
    return(x)
} )
rev.by.chipsize <- data.frame(matrix(rev.by.chipsize,
                  nrow = dim(rev.by.chipsize)[1], ncol = dim(rev.by.chipsize)[2]))%>%
                  `colnames<-`(HistoricalData[1,c(1,10:22)])

rev.by.application <- HistoricalData[2:98, c(1,25:28)] %>% `colnames<-`(HistoricalData[1,c(1,25:28)])
rev.by.application <-  apply(rev.by.application, c(1, 2),as.numeric )
rev.by.application <-  apply(rev.by.application, c(1, 2),function(x){
    if(is.na(x)){
        return(0)
    }
    return(x)
} )
rev.by.application <- data.frame(matrix(rev.by.application,
                                     nrow = dim(rev.by.application)[1], ncol = dim(rev.by.application)[2]))%>%
  `colnames<-`(HistoricalData[1,c(1,25:28)])

rev.by.platform <- HistoricalData[2:98,c(1,31:36)] %>% `colnames<-`(HistoricalData[1,c(1,31:36)])
rev.by.platform <-  apply(rev.by.platform, c(1, 2),as.numeric )
rev.by.platform <-  apply(rev.by.platform, c(1, 2),function(x){
    if(is.na(x)){
        return(0)
    }
    return(x)
} )
rev.by.platform <- data.frame(matrix(rev.by.platform,
                    nrow = dim(rev.by.platform)[1], ncol = dim(rev.by.platform)[2]))%>%
                    `colnames<-`(HistoricalData[1,c(1,31:36)])


rev.by.geography <- HistoricalData[2:98,c(1,39:43)] %>% `colnames<-`(HistoricalData[1,c(1,39:43)])
rev.by.geography <-  apply(rev.by.geography, c(1, 2),as.numeric )
rev.by.geography <-  apply(rev.by.geography, c(1, 2),function(x){
    if(is.na(x)){
        return(0)
    }
    return(x)
} )
rev.by.geography <- data.frame(matrix(rev.by.geography,
                                     nrow = dim(rev.by.geography)[1], ncol = dim(rev.by.geography)[2]))%>%
  `colnames<-`(HistoricalData[1,c(1,39:43)])


rev.by.application <- rev.by.application[1:81,]
rev.by.platform <- rev.by.platform[77:97,]
temp <- NULL
for(i in 1999:2023){
  subd <- rev.by.chipsize[which(rev.by.chipsize$Time>=i &  rev.by.chipsize$Time<i+1), ]
  temp <- rbind(temp,colMeans(subd))
}
temp[,1] <- round(temp[,1])
rev.by.chipsize <- temp

temp <- NULL
for(i in 1999:2023){
  subd <- rev.by.application[which(rev.by.application$Time>=i &  rev.by.application$Time<i+1), ]
  temp <- rbind(temp,colMeans(subd))
}
temp[,1] <- round(temp[,1])
rev.by.application <- temp

temp <- NULL
for(i in 1999:2023){
  subd <- rev.by.geography[which(rev.by.geography$Time>=i &  rev.by.geography$Time<i+1), ]
  temp <- rbind(temp,colMeans(subd))
}
temp[,1] <- round(temp[,1])
rev.by.geography <- temp


temp <- NULL
for(i in 1999:2023){
  subd <- rev.by.platform[which(rev.by.platform$Time>=i &  rev.by.platform$Time<i+1), ]
  temp <- rbind(temp,colMeans(subd))
}
temp[,1] <- round(temp[,1])
rev.by.platform <- temp

rev.by.application <- data.frame(na.omit(rev.by.application))
rev.by.chipsize <- data.frame(na.omit(rev.by.chipsize))
rev.by.geography <- data.frame(na.omit(rev.by.geography))
rev.by.platform <- data.frame(na.omit(rev.by.platform))

colnames(rev.by.application) <- HistoricalData[1,c(1,25:28)]
colnames(rev.by.chipsize) <- HistoricalData[1,c(1,10:22)]
colnames(rev.by.geography) <- HistoricalData[1,c(1,39:43)]
colnames(rev.by.platform) <- HistoricalData[1,c(1,31:36)]

save(rev.by.application, rev.by.chipsize,
      rev.by.geography, rev.by.platform, timeseries ,file = "Data/TSMC.RData")
