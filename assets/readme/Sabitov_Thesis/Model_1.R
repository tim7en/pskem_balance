##################
##################
##################
# clean up the data
rm(list = ls(all = T))
start.time <- Sys.time()

# Watershed area same as in HW - 7
A <- 2540
K <- 0.45

# read in file
x <- read.csv(file = "Met.csv")
y <- read.csv(file = "flow.csv")

# load function
source("Func_final.R")

# read in the data
Data <- preproc(x)

y <- preproc_d_f(y)
y_dim <- dim(y)
y <- y[which(y$WaterYear == min(Data$WaterYear))[1]:y_dim[1], ]
y$Flow_cms <- y$Flow * 0.0283



# define errors
ERR <- which(Data$TMAX < Data$TMIN)
ERR3 <- which(Data$PRCP < 0)
Data$TMAX[ERR] <- NA
Data$PRCP[ERR3] <- NA

# define dimentions of the data
n <- dim(Data)

library(zoo)

Data[2:n[2]] <- na.approx(Data[2:n[2]])
f <- which(Data$Month == 10, Data$Day == 1)[1]
Data <- Data[f:n[1], ]
n <- dim(Data)

# Convert temp into C
Data$TMAX <- (Data$TMAX - 32) * 5 / 9 - (2770 - 1251) / 1000 * 6.5
Data$TMIN <- (Data$TMIN - 32) * 5 / 9 - (2770 - 1251) / 1000 * 6.5

# Define avg temperature and convert into C
Data$T_avg <- ((Data$TMAX + Data$TMIN) / 2)

# convert precip from inch to cm
Data$PRCP <- (Data$PRCP * 2.54)

# Field capacity of bucket is at 10 mm up to 10cm
# FCAP=0.01#3.411

# Base_flow const
SAT <- 6.4
# Base_flow const
K_B <- 0.99
# j=j+0.001
# Column for snow
Data$Snow <- 0
# Snow Melt
Data$Mt <- 0
# Define Rain
Data$Rain <- 0
# Define SnowPack
Data$SN <- 0
# Saturated storage is on 0
Data$Sat <- 0
# Surface runoff
Data$SRn <- 0
Data$PERC <- 0
# If temp < 0, rain turns into snow

# define location where T_av<0
loc_1 <- which(Data$T_avg <= 0)
loc_2 <- which(Data$T_avg > 0)
# Define snow
Data$Snow[loc_1] <- Data$PRCP[loc_1]
# Define rain
Data$Rain[loc_2] <- Data$PRCP[loc_2]

for (i in seq(1, n[1] - 1)) {
  if (Data$T_avg[i] <= 0) {
    Data$SN[i + 1] <- Data$SN[i] + Data$Snow[i]
  }
  else {
    Data$Mt[i] <- min(Data$SN[i], Data$T_avg[i] * K)
    Data$SN[i + 1] <- Data$SN[i] - Data$Mt[i]
  }

  Data$PERC[i] <- Data$Mt[i] + Data$Rain[i]
}

# define sat conditions for the first day as 0.265
Data$Sat[1] <- SAT

# define SDt
Data$SDt <- 0

for (i in seq(1, n[1] - 1)) {
  # SDt
  Data$SDt[i] <- (1 - K_B) * Data$Sat[i]
  # Sat
  Data$Sat[i + 1] <- Data$Sat[i] + Data$PERC[i] - Data$SDt[i]
}

Data$Q <- (Data$SDt) / (1000 * 100) * A / 86400 * 10^9

Data$Q_obs <- y$Flow_cms

B <- Bias(Data$Q[which(Data$Date=="2013-10-01"):which(Data$Date=="2015-09-30")]/35.31, Data$Q_obs[which(Data$Date=="2013-10-01"):which(Data$Date=="2015-09-30")]/35.31, length(Data$Q[which(Data$Date=="2013-10-01"):which(Data$Date=="2015-09-30")]))
N <- Nash(Data$Q[which(Data$Date=="2013-10-01"):which(Data$Date=="2015-09-30")]/35.31, Data$Q_obs[which(Data$Date=="2013-10-01"):which(Data$Date=="2015-09-30")]/35.31)
R <- cor(Data$Q[which(Data$Date=="2013-10-01"):which(Data$Date=="2015-09-30")]/35.31, Data$Q_obs[which(Data$Date=="2013-10-01"):which(Data$Date=="2015-09-30")]/35.31)


monthly_mod <- (aggregate(x = Data$Q, by = list(Data$Month, Data$WaterYear), FUN = mean))
monthly_measured <- (aggregate(x = Data$Q_obs, by = list(Data$Month, Data$WaterYear), FUN = mean))

x11()
par(mfrow = c(2, 1))
par(family = "Times")
par(mar = c(5, 6, 4, 1))
plot(Data$Q_obs[1:1094] ~ Data$Date[1:1094], pch = NA_integer_, ylab = "Discharge, cms", xlab = "Date", ylim = c(0, 250), cex.lab = 2, cex.axis = 1.6)
lines(Data$Q_obs[1:1094] ~ Data$Date[1:1094], pch = 17, lty = 2, col = "blue", lwd = 2)
lines(Data$Q[1:1094] ~ Data$Date[1:1094], pch = 14, col = "red", lwd = 2)
legend("topleft", legend = c("Measured", "Modeled"), lty = c(2, 1), lwd = c(2, 2), col = c("Blue", "Red"), cex = 1.8, bty = "n", inset = c(0, 0))
cor (Data$Q,Data$Q_obs)

par(family = "Times")
par(mar = c(5, 6, 4, 1))
plot(monthly_measured$x, pch = NA_integer_, ylab = "Discharge, cms", xlab = "Months", ylim = c(0, 200), cex.lab = 2, cex.axis = 1.6)
points(monthly_measured$x, pch = 17, col = "blue")
points(monthly_mod$x, pch = 14, col = "red")
legend("topleft", legend = c("Measured", "Modeled"), pch = c(17, 14), col = c("Blue", "Red"), cex = 1.8, bty = "n", inset = c(0, 0))
cor (monthly_measured$x,monthly_mod$x)

qqplot(monthly_measured$x, monthly_mod$x, ylim = c(0, 180), xlim = c(0, 180))
lines(monthly_measured$x, monthly_measured$x)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

