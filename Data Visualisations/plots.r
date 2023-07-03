# Revenue plot
p1  <- ggplot(data=timeseries, aes(x = Time, y = Revenue))+ geom_line(linewidth = 0.5, col = "darkgreen")+
    labs(title = "TSMC's Monthly Revenue", caption = "Revenue is in the units of 1 billion NTD") +
    xlab("Time (years)") + theme_light() +
    scale_y_continuous(trans = log10_trans(), breaks = trans_breaks("log10", function(x) 10^x),labels = label_number(accuracy = 1) )
p1

ggsave("Plots/RevenuePlot.png", plot = p1, width = 900, height = 400, units = 'px', scale = 2.5)


dat.geography <- melt(rev.by.geography, id.vars = "Time", value.name = "Revenue Share", variable.name = "Region" )

geography_plot <- ggplot(dat.geography, aes(x = Time, y = 100*`Revenue Share`, fill = Region, col = Region) )+
    geom_line(linewidth = 1)+
    labs(title = "Geography")+xlab("")+
    ylab("")+
    theme_light()
geography_plot


dat.chipsize <- melt(rev.by.chipsize, id.vars = "Time", value.name = "Revenue Share", variable.name = "Chip Size" )

chipsize_plot <- ggplot(dat.chipsize[which(dat.chipsize$`Revenue Share` >0.01),], aes(x = Time, y = 100*`Revenue Share`, col = `Chip Size`) )+
  geom_line(linewidth = 0.8)+
  labs(title = "Chipsize")+xlab("")+
  ylab("")+
  theme_light()


dat.platform <- melt(rev.by.platform, id.vars = "Time", value.name = "Revenue Share", variable.name = "Platform" )
platform_plot <- ggplot(dat.platform, aes(x = Time, y = 100*`Revenue Share`, fill = `Platform`, col = `Platform`) )+
    geom_line(linewidth = 0.7)+
    labs(title = "Platform")+xlab("")+
    ylab("")+
    theme_light()


dat.application <- melt(rev.by.application, id.vars = "Time", value.name = "Revenue Share", variable.name = "Application" )
app_plot <- ggplot(dat.application, aes(x = Time, y = 100*`Revenue Share`, fill = `Application`, col = `Application`) )+
    geom_line(linewidth  = 0.8)+
    labs(title = "Application")+xlab("")+
    ylab("")+
    theme_light()

voidplot <- ggplot()+theme_void()+
    labs(title = "Scale", subtitle = "x-axis: Time (years)\ny-axis: Percentage Share in Annual Revenue")
rev_breakdown <- grid.arrange(app_plot,geography_plot,platform_plot,voidplot, top=textGrob("Annual Revenue Breakdown"))
ggsave("Plots/RevenueBreakdown.png",plot = rev_breakdown, height = 720, width = 1480, scale = 2.6, units = 'px')

