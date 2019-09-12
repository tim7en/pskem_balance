setwd("E:/Maxim/assets/Pskem_Q")
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
boxplot (pquality_assurance$`min(Q)` ~ pquality_assurance$month, xlab = 'Month', ylab = 'Discharge', main = '?????????????????????? ???????????? ???????? ???????? ??????????, 2004 - 2017 ??????')

#?????? ????????????
#ggplot(mt, aes(x=factor(cyl), y=mpg, fill = med_mpg)) +
#stat_summary(fun.data = custom_quantile, geom = "boxplot")
