library (trend)
library (dplyr)
library (pracma)
final_df <- read.csv ('D:/Work/MyGitHub/pskem_balance/assets/data/mainassets.csv')

## Plot rainfall first
par(xaxs="i", yaxs="i", mar=c(5,5,5,5))
dates <- as.Date(final_df$date)
rain <- final_df$P
flow <- final_df$Q
temp <- final_df$T

#Mann-Kendall chart, average monthly values
#no noticeable trends for monthly air temperature values
temp_mon <- final_df %>% group_by(final_df$year, final_df$month) %>% summarize(mean(T))
dat.ts_T <- ts (temp_mon$`mean(T)`, start = 2004, end = 2017, frequency = 12)
res_T <- smk.test(dat.ts_T)
summary (res_T)

#significant trends @10% significance, decreasing precipitation in February and increasing precipitation in June
prcp_mon <- final_df %>% group_by(final_df$year, final_df$month) %>% summarize(mean(P))
dat.ts_P <- ts (prcp_mon$`mean(P)`, start = 2004, end = 2017, frequency = 12)
res_P <- smk.test(dat.ts_P)
summary (res_P)

#significant trends @5% significance, decreasing trend of flow in May and August
flow_mon <- final_df %>% group_by(final_df$year, final_df$month) %>% summarize(mean(Q))
dat.ts_Q <- ts (flow_mon$`mean(Q)`, start = 2004, end = 2017, frequency = 12)
res_Q <- smk.test(dat.ts_Q)
summary (res_Q)

#EMA, MAV of precipitation and flow values
#subset month where significant trends observed (February, June - precipitation), (May, August - river flow)
precipitationFebruary <- final_df[final_df$month == 2, ]
precipitationJune <- final_df[final_df$month == 6, ]
dischargeMay <- final_df[final_df$month == 5, ]
dischargeAugust <- final_df[final_df$month == 8, ]

precipitationFebruary <- precipitationFebruary %>% group_by (year) %>% summarize (mean(P))
precipitationJune <- precipitationJune %>% group_by (year) %>% summarize (mean(P))
dischargeMay <- dischargeMay %>% group_by (year) %>% summarize (mean(Q))
dischargeAugust <- dischargeAugust %>% group_by (year) %>% summarize (mean(Q))

par(mfrow=c(2,2), mar = c(2,2,4,2))
plot (precipitationFebruary, type = 'l', main = 'Precipitation in February (mm), Pskem', xaxt = 'n')
y <- movavg(precipitationFebruary$`mean(P)`, 3, "s"); lines(data.frame(precipitationFebruary$year, y), lty = 3,lwd = 0.5, col = 2)
y <- movavg(precipitationFebruary$`mean(P)`, 3, "w"); lines(data.frame(precipitationFebruary$year, y),lty = 3,lwd = 0.5, col = 4)
y <- movavg(precipitationFebruary$`mean(P)`, 3, "e"); lines(data.frame(precipitationFebruary$year, y),lty = 3,lwd = 0.5, col = 6)
abline(lm(precipitationFebruary$`mean(P)`~precipitationFebruary$year), col="red")
grid()

plot (precipitationJune, type = 'l', main = 'Precipitation in June (mm), Pskem', xaxt = 'n')
y <- movavg(precipitationJune$`mean(P)`, 3, "s"); lines(data.frame(precipitationJune$year, y),lty = 3,lwd = 0.5, col = 2)
y <- movavg(precipitationJune$`mean(P)`, 3, "w"); lines(data.frame(precipitationJune$year, y),lty = 3,lwd = 0.5, col = 4)
y <- movavg(precipitationJune$`mean(P)`, 3, "e"); lines(data.frame(precipitationJune$year, y),lty = 3,lwd = 0.5, col = 6)
abline(lm(precipitationJune$`mean(P)`~precipitationFebruary$year), col="red")
grid()

plot (dischargeMay, type = 'l', main = 'Discharge in May (cms), Pskem')
y <- movavg(dischargeMay$`mean(Q)`, 3, "s"); lines(data.frame(dischargeMay$year, y),lty = 3,lwd = 0.5, col = 2)
y <- movavg(dischargeMay$`mean(Q)`, 3, "w"); lines(data.frame(dischargeMay$year, y),lty = 3,lwd = 0.5, col = 4)
y <- movavg(dischargeMay$`mean(Q)`, 3, "e"); lines(data.frame(dischargeMay$year, y),lty = 3,lwd = 0.5, col = 6)
abline(lm(dischargeMay$`mean(Q)`~precipitationFebruary$year), col="red")
grid()

plot (dischargeAugust, type = 'l',  main = 'Discharge in August (cms), Pskem')
y <- movavg(dischargeAugust$`mean(Q)`, 3, "s"); lines(data.frame(dischargeAugust$year, y),lty = 3,lwd = 0.5, col = 2)
y <- movavg(dischargeAugust$`mean(Q)`, 3, "w"); lines(data.frame(dischargeAugust$year, y),lty = 3,lwd = 0.5, col = 4)
y <- movavg(dischargeAugust$`mean(Q)`, 3, "e"); lines(data.frame(dischargeAugust$year, y),lty = 3,lwd = 0.5, col = 6)
abline(lm(dischargeAugust$`mean(Q)`~precipitationFebruary$year), col="red")
grid()
legend(2012, 125, c("original data", "simple",  "weighted",
                  "exponential"),
       col = c(1,2,4,6), lty = c(1,3,3,3), lwd = 0.5, box.col = "gray", bg = "white")
text (2014, 100, 'Moving window N = 3')

par (mfrow = c(1,1))
#plot of rainfall runoff
plot(dates, rain, type="h", ylim=c(max(rain)*1.5,0),
     axes=FALSE, xlab=NA, ylab=NA, col="blue",
     lwd=3, lend="round", main = 'Rainfall-runoff graph for river Pskem')
axis(4)
mtext("Rainfall (mm)", side=4, line=3)

## Plot flow on top
par(new=TRUE)
plot(dates, flow, type="l", lwd=2, ylim=c(0, max(flow)*1.5), ylab  = 'Flow (m3/s)', xlab = 'Dates')