##################
##################
##################
# clean up the data
rm(list = ls(all = T))
source ('modelFunctions.R')
drainageArea <- 2540

basinData <- read.csv ("D:\\Work\\MyGitHub\\pskem_balance\\assets\\mainassets.csv")
basinData$P <- basinData$P/10 #mm to cm
basinData$wateryear <- 0
for (i in seq (1, nrow (basinData))){
  if (basinData$month[i] < 10){basinData$wateryear[i] <- basinData$year[i]-1} else {basinData$wateryear[i] <- basinData$year[i]}
}

# Lapse rate 6.5 degree per 1000 meters
basinData$Lapse <- 6.5

#convert temperature according to lapse rate to the average elevation of the basin
basinData$T <- basinData$T - ((2770 - 1251)/1000 * basinData$Lapse)

#find pet using solar radiation (cm)
basinData$ETO <- Evp (basinData, basinData$T, basinData$T)

#set model parameters, # snowmelt coefficient
basinData$Snow <- 0
basinData$Mt <- 0
basinData$Rn <- 0
basinData$SN <- 0
K <- 0.45
SAT <- 0.265
K_B <- 0.989
CN_2 <- 45
FCAP <- 0.2 #field capacity 
UNSAT <- 5 #unsaturated storage
n <- nrow (basinData)
basinData$SN[1] <- SAT


# define accumulation of amount of snow at day t+1; #define melt

# define location where T_av<0
belowZero <- which(basinData$T <= 0)

# Define snow
basinData$Snow[belowZero] <- basinData$P[belowZero]

for (i in seq(1, n[1] - 1)) {
  if (basinData$T[i] <= 0) {
    basinData$SN[i + 1] <- basinData$SN[i] + basinData$Snow[i]
  }
  else {
    basinData$Mt[i] <- min(basinData$SN[i], basinData$T[i] * K)
    basinData$SN[i + 1] <- basinData$SN[i] - basinData$Mt[i]
  }
}

if (basinData$T[n] <= 0) {
  basinData$Snow[n] <- basinData$P[n]
} else {
  basinData$Mt[n] <- min(basinData$SN[n], basinData$T[n] * K)
}


#Units in cm (P, Mt, Snow, PET)

##################################################################################################################

# Define rain
basinData$Rn <- basinData$P - basinData$Snow

# Define AM5
basinData$AM5 <- 0

# counter
j <- 1
# set k to  1
k <- 1

# define AM5 Melt
# for the first 5 days
for (k in seq(1, 5)) {
  basinData$AM5[k + 1] <- sum(basinData$Mt[1:j] + basinData$Rn[1:j])
  j <- j + 1
}

# for the entire set
for (i in seq(j, n)) {
  basinData$AM5[i] <- sum(basinData$Mt[(i - 5):(i - 1)] + basinData$Rn[(i - 5):(i - 1)])
}

basinData$AM5_comp <- 0


# Define CN_1
CN_1 <- CN_2 / (2.334 - 0.01334 * CN_2)
# Define CN
CN_3 <- CN_2 / (0.4036 + 0.0059 * CN_2)

# my boundaries
AM1_D <- 1.3
AM2_D <- 2.8

# my boundaries
AM1_G <- 3.6
AM2_G <- 5.3

# define growing season or dormant season
basinData$Season <- 0
growingMonth <- which(basinData$month < 11 & basinData$month > 4)
basinData$Season[growingMonth] <- basinData$Season[growingMonth] + 1


# main curve number
basinData$CN <- 0

cond_1 <- which(basinData$Season == 0 & basinData$AM5 > AM2_D)
basinData$CN[cond_1] <- CN_3

cond_2 <- which(basinData$Season == 0 & basinData$AM5 > AM1_D & basinData$AM5 < AM2_D)
basinData$CN[cond_2] <- (CN_2 + ((CN_3 - CN_2) / (AM2_D - AM1_D)) * (basinData$AM5[cond_2] - AM1_D))

cond_3 <- which(basinData$Season == 0 & AM1_D > basinData$AM5)
basinData$CN[cond_3] <- (CN_1 + ((CN_2 - CN_1) / (AM1_D - 0)) * basinData$AM5[cond_3])

cond_4 <- which(basinData$Season == 1 & basinData$AM5 > AM2_G)
basinData$CN[cond_4] <- CN_3

cond_5 <- which(basinData$Season == 1 & basinData$AM5 > AM1_G & basinData$AM5 < AM2_G)
basinData$CN[cond_5] <- (CN_2 + ((CN_3 - CN_2) / (AM2_G - AM1_G)) * (basinData$AM5[cond_5] - AM1_G))

cond_6 <- which(basinData$Season == 1 & AM1_G > basinData$AM5)
basinData$CN[cond_6] <- (CN_1 + ((CN_2 - CN_1) / (AM1_G - 0)) * basinData$AM5[cond_6])


# use which outside of loop
cond_7 <- which(basinData$Mt > 0)

# else use loc_0 to assign CN_3
# CN for snowmelt
basinData$CN[cond_7] <- CN_3

# S in Si units (Potential retention)
basinData$S <- (drainageArea / basinData$CN) - 25.4

# numerator (P-0.2S)^2
basinData$num <- 0

# Estimate numerator for surface runoff
cond_8 <- which((basinData$Rn + basinData$Mt) < (0.2 * basinData$S))
basinData$num <- (basinData$Rn + basinData$Mt - 0.2 * basinData$S)^2
basinData$num[cond_8] <- 0

# Define denum to plug into equation for surface runoff
basinData$denum <- basinData$Rn + basinData$Mt + 0.8 * basinData$S

# Define surface runoff
basinData$SRt <- basinData$num / basinData$denum

# Define infiltration
basinData$I <- basinData$Rn + basinData$Mt - basinData$SRt

# Create dataframe to store crop koefficients
KU <- data.frame(c(0.78, 0.82, 0.82, 0.79, 0.89, 0.91, 0.93, 0.98, 1.03, 0.97, 0.72, 0.61))
names(KU) <- NULL
names(KU) <- ("C_Koef")
KU$month <- seq(1, 12)
d <- dim(KU)

basinData$C_Koef <- 0 # create column to store coefficients

KK <- merge(basinData, KU, (by <- "month"), all = F) # Merge into another df two basinData frames to assign KK
KK <- KK[with(KK, order(KK$date)), ]

basinData$C_Koef <- KK$C_Koef.y

basinData$UNSAT <- 0 # Create column for unsaturated storage

basinData$UNSAT[1] <- UNSAT # Soil saturation on day 1

basinData$PE <- 0 # create column for adjusted potential evapotranspiration

basinData$PERC <- 0 # create column for percolation

# Loop define Evapotranspiration as function of UNSAT on day t and infiltration
# Define UNSAT at the next day


for (i in seq(1, n[1] - 1)) {
  basinData$PE[i] <- min(basinData$C_Koef[i] * basinData$ETO[i], basinData$UNSAT[i]) # adjust evapotranspiration
  
  basinData$UNSAT[i + 1] <- basinData$UNSAT[i] + basinData$I[i] - basinData$PE[i] # estimate UNSAT for day 2
  
  if (basinData$UNSAT[i + 1] <= FCAP) {
    basinData$PERC[i] <- 0
  } else {
    basinData$PERC[i] <- basinData$UNSAT[i + 1] - FCAP
    basinData$UNSAT[i + 1] <- FCAP
  }
}

# Adjust evapotranspiration for the last part
k <- max(n[1])
basinData$PE[k] <- min(basinData$C_Koef[k] * basinData$ETO[k], basinData$UNSAT[k])

#################################################################################################################################################################################
################################################################################################################################################################################

# define column Saturated as 0
basinData$Sat <- 0

# define sat conditions for the first day as 0.265
basinData$Sat[1] <- SAT

# define SDt
basinData$SDt <- 0

for (i in seq(1, n[1] - 1)) {
  # SDt
  basinData$SDt[i] <- (1 - K_B) * basinData$Sat[i]
  # Sat
  basinData$Sat[i + 1] <- basinData$Sat[i] + basinData$PERC[i] - basinData$SDt[i]
}

# Define SDt
basinData$SDt[k] <- (1 - K_B) * basinData$Sat[k]


basinData$Q_est <- 0

# Discharge (cm)
basinData$Q_est <- (basinData$SRt + basinData$SDt)
basinData$Q_cm <- (basinData$SRt + basinData$SDt)

# result
basinData$Q_est <- basinData$Q_est / (1000 * 100) * drainageArea / 86400 * 10^9

plot (basinData$Q ~ basinData$Q_est)


# Tavg <- (basinData$T * 9/5)+32
# Tprcp <- basinData$P / 2.54
# Tdate <- basinData$date
# Flow <- basinData$Q * 35.314666212661
# output <- data.frame (Tdate,basinData$month, basinData$day, Tavg, Tprcp, Flow)
# 
# write.csv (output, 'output.csv')
