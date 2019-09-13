final_df <- read.csv ('D:/Work/MyGitHub/pskem_balance/assets/data/mainassets.csv')
## Plot rainfall first
par(xaxs="i", yaxs="i", mar=c(5,5,5,5))
dates <- as.Date(final_df$date)
rain <- final_df$P
flow <- final_df$Q
plot(dates, rain, type="h", ylim=c(max(rain)*1.5,0),
     axes=FALSE, xlab=NA, ylab=NA, col="blue",
     lwd=3, lend="round", main = 'Rainfall-runoff graph for river Pskem')
axis(4)
mtext("Rainfall (mm)", side=4, line=3)

## Plot flow on top
par(new=TRUE)
plot(dates, flow, type="l", lwd=2, ylim=c(0, max(flow)*1.5), ylab  = 'Flow (m3/s)', xlab = 'Dates')