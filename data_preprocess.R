#Review of consistency for discharge observations

setwd("D:/Work/MyGitHub/pskem_balance/assets/Pskem_Q")
library (dplyr)
#to convert all xlsx files into csv file we used current lines
#library("rio")
#xls <- dir(pattern = "xlsx")
#created <- mapply(convert, xls, gsub("xlsx", "csv", xls))
#unlink(xls) # delete xlsx files

pskem_q <- list.files (pattern = '.csv')
convertto_ts <- function (x) {
  datas <- read.csv (x)
  doctag_numeric <- as.numeric(gsub("[^\\d]+", "", x, perl=TRUE))
  yearto_date <- seq(as.Date(paste0(doctag_numeric, "/1/1")), as.Date(paste0(doctag_numeric, "/12/31")), "days")
  datas <- na.omit(as.numeric (as.matrix(datas)[,-1])) #remove first column
  doc_ts <- data.frame (yearto_date, datas)
  return (doc_ts)
}

paligned_q <- NULL
for (i in pskem_q){
  res <- convertto_ts (i)
  paligned_q <- rbind (paligned_q, res)
}
df <- data.frame(date = paligned_q$yearto_date,
                 year = as.numeric(format(paligned_q$yearto_date, format = "%Y")),
                 month = as.numeric(format(paligned_q$yearto_date, format = "%m")),
                 day = as.numeric(format(paligned_q$yearto_date, format = "%d")),
                 Q = paligned_q$datas)


pquality_assurance <- df %>% group_by(year, month) %>% summarize(max(Q), min(Q), mean (Q))
pquality_assurance <- as.data.frame(pquality_assurance)
boxplot (pquality_assurance$`min(Q)` ~ pquality_assurance$month, xlab = 'Month', ylab = 'Discharge', main = 'Pskem river monthly minimum discharge values, m3/s (2004-2017y.)')



df$Q[which (df$year %in% 2017 & df$month %in% 4 & df$Q %in% 0.49)] <- NA
df$Q[which (df$year %in% 2017 & df$month %in% 7 & df$Q %in% 1.47)] <- NA
df$Q[which (df$year %in% 2016 & df$month %in% 7 & df$Q %in% 16.50)] <- NA
df$Q[which (df$year %in% 2015 & df$month %in% 7 & df$Q %in% 26.00)] <- NA
df$Q[which (df$year %in% 2017 & df$month %in% 9 & df$Q %in% 4.50)] <- NA
#which (is.na(df$Q))

library (zoo)
df$Q <- na.approx(df$Q)
pquality_assurance <- df %>% group_by(year, month) %>% summarize(max(Q), min(Q), mean (Q))
pquality_assurance <- as.data.frame(pquality_assurance)
boxplot (pquality_assurance$`min(Q)` ~ pquality_assurance$month, xlab = 'Month', ylab = 'Discharge', main = 'Pskem river monthly minimum discharge values, m3/s (2004-2017y.)')

#potential figures for publication
#ggplot(mt, aes(x=factor(cyl), y=mpg, fill = med_mpg)) +
#stat_summary(fun.data = custom_quantile, geom = "boxplot")

#Review of consistency for climate observations
setwd("D:/Work/MyGitHub/pskem_balance/assets/Pskem_T_P")
pskem_climate <-read.csv('Pskem_T_P.csv')
pskem_climate$P_mm[is.na(pskem_climate$P_mm)]<-0


#check date values
yearto_date <- seq(as.Date(paste0(2004, "/1/1")), as.Date(paste0(2018, "/12/31")), "days")
df_climate <- data.frame(date = yearto_date,
                 year = as.numeric(format(yearto_date, format = "%Y")),
                 month = as.numeric(format(yearto_date, format = "%m")),
                 day = as.numeric(format(yearto_date, format = "%d")))

any(df_climate$year != pskem_climate$Year)
any(df_climate$month != pskem_climate$Month)
any(df_climate$day != pskem_climate$Day)
which (df_climate$day != pskem_climate$Day)
pskem_climate[1060,]
df_climate[1060,]
pskem_climate[2243,]
df_climate[2243,]
pskem_climate$Day[1060] <- 25
pskem_climate$Day[2243] <- 20

#check air temperature values
any (pskem_climate$T_C < -30)
any (pskem_climate$T_C > 40)

#check precipitation values
any (pskem_climate$P_mm < 0 )

pskem_climate <- pskem_climate[1:5114,]

final_df <- cbind (df,T=pskem_climate$T_C, P = pskem_climate$P_mm)
write.csv (final_df, 'mainassets.csv', row.names = F)
