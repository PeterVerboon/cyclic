
require(gtools);  # for invalid function

# plot the predicted values of the model

require(ggplot2)


x <- seq(0, 10,by=0.1)
y <-  a0 + b1*cos(2*pi/P*(x - b2)) + b3*x 

# y1 <-  a0 + b1*cos(2*pi/P*(x1 - b2))    # cyclic only

ylin <-  a0 + b3*x         # linear model



y <- exp(y)/(1+exp(y))   # from log odds to probabilities


g <- ggplot()                   
#  g <- g + geom_point(aes(x=x1,y=y1))
# g <- g +geom_hline(yintercept=a0, colour="blue")  
# g <- g +geom_vline(xintercept=(b2), colour="red") 

g <- g + geom_line(aes(x=x, y=y, width = 6)) 
 g <- g + geom_line(aes(x=x, y=ylin, width = 6), lty = "dashed") 
g <- g + labs(x = "assessment numbers", y = "Hunger")
# g <- g + ylim(0.2, 0.8)
g <- g + ylim(-0.5, 0.5)
g <- g + theme(axis.text = element_text(size = 12, colour="black"))
g


####

x <- as.numeric(dat2[,"Group.2"])
x <- as.numeric(dat[,"beepnr"])
y <- as.numeric(dat2[,"PAc"])

g <- ggplot(dat, aes(xvar,y))   # plot gefitte waarden
g <- g + stat_smooth(span=1.0)


g <- ggplot(dat, aes(xvar,yvar))   # plot data
g <- g + stat_smooth(span=1.0)


g <- g + geom_line(aes(x=xvar, y=y, width = 6)) 


g 






ExportPlot <- function(gplot, filename, width=2, height=1.5) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  print(gplot)
  dev.off()
  png(file = paste(filename, '_.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}

ExportPlot(g,"plot_Intimacy",width = 5, height = 3)
ExportPlot(g,"plot_Sexual_Desire-LogOdds_both",width = 5, height = 3)