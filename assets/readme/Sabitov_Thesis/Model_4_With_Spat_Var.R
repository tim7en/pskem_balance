# Hydrologic Modelling
# Instructor (Chuck Kroll)
# Student: Timur Sabitov


# clean up the data
rm(list = ls(all = T))
ptm <- proc.time()
# install.packages("styler")


# Watershed area same as in HW - 7
DA <- 680

# PARAMETERS
AM5 <- 0 # Anticident moisture
K <- 0.45 # coefficient
CN_2 <- 91
G_area <- 91 # area of glaciers

# read in file
x <- read.csv(file = "Met.csv")
y <- read.csv(file = "flow.csv")

# load function
source("Func_final.R")

# read in the data
Data <- preproc(x)

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

# Lapse rate
Data$Lapse <- 0

Data$Lapse[which(Data$Month == 3)] <- 6.7
Data$Lapse[which(Data$Month == 4)] <- 6.7
Data$Lapse[which(Data$Month == 5)] <- 6.7
Data$Lapse[which(Data$Month == 6)] <- 6.5
Data$Lapse[which(Data$Month == 7)] <- 6.5
Data$Lapse[which(Data$Month == 8)] <- 6.5
Data$Lapse[which(Data$Month == 9)] <- 6
Data$Lapse[which(Data$Month == 10)] <- 6
Data$Lapse[which(Data$Month == 11)] <- 6
Data$Lapse[which(Data$Month == 12)] <- 6
Data$Lapse[which(Data$Month == 1)] <- 6
Data$Lapse[which(Data$Month == 2)] <- 6

# convert precip from inch to cm
Data$PRCP <- (Data$PRCP * 2.54)


Data_original <- Data

# Define average air temperature at the elevation of glaciers
Data$Glac_Tavg <- (((Data$TMIN - 32) * 5 / 9 - (3680 - 1251) / 1000 * Data$Lapse) + ((Data$TMAX - 32) * 5 / 9 - (3680 - 1251) / 1000 * Data$Lapse)) / 2

# Tmax and Tmin on average elevation of the upper part of the watershed; #Define avg temperature and convert into C. Note the air temperature is averaged for the entire watershed

Data$TMAX <- (Data$TMAX - 32) * 5 / 9 - (3800 - 1251) / 1000 * Data$Lapse
Data$TMIN <- (Data$TMIN - 32) * 5 / 9 - (3800 - 1251) / 1000 * Data$Lapse
Data$T_avg <- (Data$TMAX + Data$TMIN) / 2


# define Ablation from glaciers using empirical equation
Data$Ab <- (9.5 + Data$Glac_Tavg)^3

# define Glac discharge using Ablation and the area of glaciation
Data$Glac_Q <- 1 / (31.5 * 10^3) * Data$Ab * G_area

# Adjust for glacial discharge, since we can't have negative discharge without snow
Data$Glac_Q_adj <- 0
Data$Glac_Q_adj[which(Data$Glac_Q > 0)] <- Data$Glac_Q[which(Data$Glac_Q > 0)]

# Define parameters for the data frame
Data$Snow <- 0
Data$Mt <- 0
Data$Rn <- 0
Data$SN <- 0
Data$SN[1] <- 0

# define location where T_av<0
loc_1 <- which(Data$T_avg <= 0)
Data$Snow[loc_1] <- Data$PRCP[loc_1]

# Define snow cover
for (i in seq(1, n[1] - 1)) {
  if (Data$T_avg[i] <= 0) {
    Data$SN[i + 1] <- Data$SN[i] + Data$Snow[i]
  }
  else {
    Data$Mt[i] <- min(Data$SN[i], Data$T_avg[i] * K)
    Data$SN[i + 1] <- Data$SN[i] - Data$Mt[i]
  }
}

# Adjust for the last day
k <- max(n[1])
if (Data$T_avg[k] <= 0) {
  Data$Snow[k] <- Data$PRCP[k]
} else {
  Data$Mt[k] <- min(Data$SN[k], Data$T_avg[k] * K)
}

##################################################################################################################

# Define rain
Data$Rn <- Data$PRCP - Data$Snow

# Define AM5
Data$AM5 <- 0

# counter
j <- 1
# set k to  1
k <- 1

# define AM5 Melt
# for the first 5 days
for (k in seq(1, 5)) {
  Data$AM5[k + 1] <- sum(Data$Mt[1:j] + Data$Rn[1:j])
  j <- j + 1
}

# for the entire set
for (i in seq(j, n[1])) {
  Data$AM5[i] <- sum(Data$Mt[(i - 5):(i - 1)] + Data$Rn[(i - 5):(i - 1)])
}

Data$AM5_comp <- 0

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
Data$Season <- 0

loc_2 <- which(Data$Month < 11 & Data$Month > 4)
Data$Season[loc_2] <- Data$Season[loc_2] + 1

# main curve number
Data$CN <- 0

loc_3 <- which(Data$Season == 0 & Data$AM5 > AM2_D)
Data$CN[loc_3] <- CN_3

loc_4 <- which(Data$Season == 0 & Data$AM5 > AM1_D & Data$AM5 < AM2_D)
Data$CN[loc_4] <- (CN_2 + ((CN_3 - CN_2) / (AM2_D - AM1_D)) * (Data$AM5[loc_4] - AM1_D))

loc_5 <- which(Data$Season == 0 & AM1_D > Data$AM5)
Data$CN[loc_5] <- (CN_1 + ((CN_2 - CN_1) / (AM1_D - 0)) * Data$AM5[loc_5])

loc_6 <- which(Data$Season == 1 & Data$AM5 > AM2_G)
Data$CN[loc_6] <- CN_3

loc_7 <- which(Data$Season == 1 & Data$AM5 > AM1_G & Data$AM5 < AM2_G)
Data$CN[loc_7] <- (CN_2 + ((CN_3 - CN_2) / (AM2_G - AM1_G)) * (Data$AM5[loc_7] - AM1_G))

loc_8 <- which(Data$Season == 1 & AM1_G > Data$AM5)
Data$CN[loc_8] <- (CN_1 + ((CN_2 - CN_1) / (AM1_G - 0)) * Data$AM5[loc_8])


# use which outside of loop
loc_0 <- which(Data$Mt > 0)

# else use loc_0 to assign CN_3
# CN for snowmelt
Data$CN[loc_0] <- CN_3

# S in Si units (Potential retention)
Data$S <- (2540 / Data$CN) - 25.4

# numerator (P-0.2S)^2
Data$num <- 0

# Estimate numerator for surface runoff
loc_9 <- which(Data$Rn + Data$Mt < .2 * Data$S)
Data$num <- Data$Rn + Data$Mt - 0.2 * Data$S
Data$num[loc_9] <- 0

# Define denum to plug into equation for surface runoff
Data$denum <- Data$Rn + Data$Mt + 0.8 * Data$S

# Define surface runoff
Data$SRt <- Data$num / Data$denum + Data$Glac_Q_adj / (DA * 10^6) * 86400 / 100
# Define infiltration
# Data$I<-Data$Rn+Data$Mt-Data$SRt
Data$PERC <- Data$Rn + Data$Mt - Data$SRt
Data$Sat <- 0


# Loop to find best SATURATION zone 1.
q <- 5

while (q < 10) {
  SAT <- q # Initial saturation zone
  q <- q + 0.1
  print(SAT)
  print("First while run")
  # define sat conditions for the first day as 0.265
  Data$Sat[1] <- SAT


  # Loop to find best baseflow constant for zone 1.
  q1 <- 0.998

  while (q1 < 1) {
    K_B <- q1
    q1 <- q1 + 0.001
    # print (K_B)

    # define SDt
    Data$SDt <- 0

    # Subsurface discharge
    for (i in seq(1, n[1] - 1)) {
      # SDt
      Data$SDt[i] <- (1 - K_B) * Data$Sat[i]
      # Sat
      Data$Sat[i + 1] <- Data$Sat[i] + Data$PERC[i] - Data$SDt[i]
    }

    # Define SDt
    Data$SDt[k] <- (1 - K_B) * Data$Sat[k]


    Data$Q <- 0
    Data$Q <- Data$SRt + Data$SDt
    Data$Q <- Data$Q / (1000 * 100) * DA / 86400 * 10^9 * 35.3146667

    ######################### Layer 2 of the watershed#######################
    ######################### Layer 2 of the watershed#######################
    ######################### Layer 2 of the watershed#######################
    ######################### Layer 2 of the watershed#######################
    ######################### Layer 2 of the watershed#######################

    DA_2 <- 1200
    CN_2 <- 50


    Data_2 <- Data_original
    Data_2$TMAX <- (Data_2$TMAX - 32) * 5 / 9 - (2800 - 1251) / 1000 * Data$Lapse
    Data_2$TMIN <- (Data_2$TMIN - 32) * 5 / 9 - (2800 - 1251) / 1000 * Data$Lapse
    Data_2$T_avg <- (Data_2$TMAX + Data_2$TMIN) / 2


    # Convert back temperature into Kelvin, averaged for the entire watershed.
    Data_2$TMAX <- Data_2$TMAX * 9 / 5 + 32
    Data_2$TMIN <- Data_2$TMIN * 9 / 5 + 32

    # Estimate potential ET
    Data_2$PET <- Evp(Data_2, Data_2$TMAX, Data_2$TMIN)

    # Define parameters for the data frame
    Data_2$Snow <- 0
    Data_2$Mt <- 0
    Data_2$Rn <- 0
    Data_2$SN <- 0
    Data_2$SN[1] <- 0

    # define location where T_av<0
    loc_1 <- which(Data_2$T_avg <= 0)
    Data_2$Snow[loc_1] <- Data_2$PRCP[loc_1]

    # Define snow cover
    for (i in seq(1, n[1] - 1)) {
      if (Data_2$T_avg[i] <= 0) {
        Data_2$SN[i + 1] <- Data_2$SN[i] + Data_2$Snow[i]
      }
      else {
        Data_2$Mt[i] <- min(Data_2$SN[i], Data_2$T_avg[i] * K)
        Data_2$SN[i + 1] <- Data_2$SN[i] - Data_2$Mt[i]
      }
    }

    # Adjust for the last day
    k <- max(n[1])
    if (Data_2$T_avg[k] <= 0) {
      Data_2$Snow[k] <- Data_2$PRCP[k]
    } else {
      Data_2$Mt[k] <- min(Data_2$SN[k], Data_2$T_avg[k] * K)
    }


    # Define AM5
    Data_2$AM5 <- 0

    # counter
    j <- 1
    # set k to  1
    k <- 1

    # define AM5 Melt
    # for the first 5 days
    for (k in seq(1, 5)) {
      Data_2$AM5[k + 1] <- sum(Data_2$Mt[1:j] + Data_2$Rn[1:j])
      j <- j + 1
    }

    # for the entire set
    for (i in seq(j, n[1])) {
      Data_2$AM5[i] <- sum(Data_2$Mt[(i - 5):(i - 1)] + Data_2$Rn[(i - 5):(i - 1)])
    }

    Data_2$AM5_comp <- 0

    # Define CN_1
    CN_1 <- CN_2 / (2.334 - 0.01334 * CN_2)
    # Define CN
    CN_3 <- CN_2 / (0.4036 + 0.0059 * CN_2)


    # define growing season or dormant season
    Data_2$Season <- 0

    loc_2 <- which(Data_2$Month < 11 & Data_2$Month > 4)
    Data_2$Season[loc_2] <- Data_2$Season[loc_2] + 1

    # main curve number
    Data_2$CN <- 0

    loc_3 <- which(Data_2$Season == 0 & Data_2$AM5 > AM2_D)
    Data_2$CN[loc_3] <- CN_3

    loc_4 <- which(Data_2$Season == 0 & Data_2$AM5 > AM1_D & Data_2$AM5 < AM2_D)
    Data_2$CN[loc_4] <- (CN_2 + ((CN_3 - CN_2) / (AM2_D - AM1_D)) * (Data_2$AM5[loc_4] - AM1_D))

    loc_5 <- which(Data_2$Season == 0 & AM1_D > Data$AM5)
    Data_2$CN[loc_5] <- (CN_1 + ((CN_2 - CN_1) / (AM1_D - 0)) * Data_2$AM5[loc_5])

    loc_6 <- which(Data_2$Season == 1 & Data_2$AM5 > AM2_G)
    Data_2$CN[loc_6] <- CN_3

    loc_7 <- which(Data_2$Season == 1 & Data_2$AM5 > AM1_G & Data_2$AM5 < AM2_G)
    Data_2$CN[loc_7] <- (CN_2 + ((CN_3 - CN_2) / (AM2_G - AM1_G)) * (Data_2$AM5[loc_7] - AM1_G))

    loc_8 <- which(Data_2$Season == 1 & AM1_G > Data_2$AM5)
    Data_2$CN[loc_8] <- (CN_1 + ((CN_2 - CN_1) / (AM1_G - 0)) * Data_2$AM5[loc_8])


    # use which outside of loop
    loc_0 <- which(Data_2$Mt > 0)

    # else use loc_0 to assign CN_3
    # CN for snowmelt
    Data_2$CN[loc_0] <- CN_3

    # S in Si units (Potential retention)
    Data_2$S <- (2540 / Data_2$CN) - 25.4

    # numerator (P-0.2S)^2
    Data_2$num <- 0

    # Estimate numerator for surface runoff
    loc_9 <- which(Data_2$Rn + Data_2$Mt < .2 * Data_2$S)
    Data_2$num <- Data_2$Rn + Data_2$Mt - 0.2 * Data_2$S
    Data_2$num[loc_9] <- 0

    # Define denum to plug into equation for surface runoff
    Data_2$denum <- Data_2$Rn + Data_2$Mt + 0.8 * Data_2$S

    # Define surface runoff
    Data_2$SRt <- Data_2$num / Data_2$denum

    # Define infiltration
    Data_2$I <- Data_2$Rn + Data_2$Mt - Data_2$SRt


    # Create dataframe to store crop koefficients
    KU <- data.frame(c(0.78, 0.82, 0.82, 0.79, 0.89, 0.91, 0.93, 0.98, 1.03, 0.97, 0.72, 0.61))
    names(KU) <- NULL
    names(KU) <- ("C_Koef")
    KU$Month <- seq(1, 12)
    d <- dim(KU)

    Data_2$C_Koef <- 0 # create column to store coefficients

    KK <- merge(Data_2, KU, (by <- "Month"), all = F) # Merge into another df two data frames to assign KK
    KK <- KK[with(KK, order(KK$Date)), ]

    Data_2$C_Koef <- KK$C_Koef.y

    Data_2$UNSAT <- 0 # Create column for unsaturated storage

    # Iterators
    q3 <- 0


    # while (q3 < 5) {
    FCAP <- q3
    # print(FCAP)
    # print ("Third while loop")
    q4 <- 0
    q3 <- q3 + 0.5

    # while (q4 < FCAP) {
    UNSAT <- q4
    # print (UNSAT)
    q4 <- q4 + 0.2

    Data_2$UNSAT[1] <- UNSAT # Soil saturation on day 1
    Data_2$PE <- 0 # create column for adjusted potential evapotranspiration
    Data_2$PERC <- 0 # create column for percolation

    # Loop define Evapotranspiration as function of UNSAT on day t and infiltration
    # Define UNSAT at the next day

    ET_coef <- 0.5 # Since only 50% of ET in the watershed due to the high slopes

    for (i in seq(1, n[1] - 1)) {
      Data_2$PE[i] <- min(ET_coef * (Data_2$C_Koef[i] * Data_2$PET[i]), Data_2$UNSAT[i]) # adjust evapotranspiration

      # Data_2$UNSAT[i+1]<-Data_2$UNSAT[i]+Data_2$I[i]-Data_2$PE[i] #estimate UNSAT for day 2

      Data_2$UNSAT[i + 1] <- Data_2$UNSAT[i] + Data_2$I[i] - Data_2$PE[i]

      if (Data_2$UNSAT[i + 1] <= FCAP) {
        Data_2$PERC[i] <- 0
      } else {
        Data_2$PERC[i] <- Data_2$UNSAT[i + 1] - FCAP
        Data_2$UNSAT[i + 1] <- FCAP
      }
    }

    # Adjust evapotranspiration for the last part
    k <- max(n[1])
    Data_2$PE[k] <- min(Data_2$C_Koef[k] * Data_2$PET[k], Data_2$UNSAT[k])

    #################################################################################################################################################################################
    ################################################################################################################################################################################

    # define column Saturated as 0
    Data_2$Sat <- 0

    # print (K_B)
    # define sat conditions for the first day as 0.265
    Data_2$Sat[1] <- SAT

    # define SDt
    Data_2$SDt <- 0

    for (i in seq(1, n[1] - 1)) {
      # SDt
      Data_2$SDt[i] <- (1 - K_B) * Data_2$Sat[i]
      # Sat
      Data_2$Sat[i + 1] <- Data_2$Sat[i] + Data_2$PERC[i] - Data_2$SDt[i]
    }

    # Define SDt
    Data_2$SDt[k] <- (1 - K_B) * Data_2$Sat[k]


    Data_2$Q <- Data_2$SRt + Data_2$SDt
    Data_2$Q <- Data_2$Q / (1000 * 100) * DA_2 / 86400 * 10^9 * 35.3146667
    ##################################################################################
    ##################################################################################
    ##################################################################################
    ##################################################################################
    ##################################################################################
    ##################################################################################
    ##################################################################################

    DA_3 <- 660
    CN_2 <- 35

    Data_3 <- Data_original
    Data_3$TMAX <- (Data_3$TMAX - 32) * 5 / 9 - (1800 - 1251) / 1000 * Data$Lapse
    Data_3$TMIN <- (Data_3$TMIN - 32) * 5 / 9 - (1800 - 1251) / 1000 * Data$Lapse
    Data_3$T_avg <- (Data_3$TMAX + Data_2$TMIN) / 2


    # Convert back temperature into Kelvin, averaged for the entire watershed.
    Data_3$TMAX <- Data_2$TMAX * 9 / 5 + 32
    Data_3$TMIN <- Data_2$TMIN * 9 / 5 + 32

    # Estimate potential ET
    Data_3$PET <- Evp(Data_3, Data_3$TMAX, Data_3$TMIN)

    # Define parameters for the data frame
    Data_3$Snow <- 0
    Data_3$Mt <- 0
    Data_3$Rn <- 0
    Data_3$SN <- 0
    Data_3$SN[1] <- 0

    # define location where T_av<0
    loc_1 <- which(Data_3$T_avg <= 0)
    Data_3$Snow[loc_1] <- Data_3$PRCP[loc_1]

    # Define snow cover
    for (i in seq(1, n[1] - 1)) {
      if (Data_3$T_avg[i] <= 0) {
        Data_3$SN[i + 1] <- Data_3$SN[i] + Data_3$Snow[i]
      }
      else {
        Data_3$Mt[i] <- min(Data_3$SN[i], Data_3$T_avg[i] * K)
        Data_3$SN[i + 1] <- Data_3$SN[i] - Data_3$Mt[i]
      }
    }

    # Adjust for the last day
    k <- max(n[1])
    if (Data_3$T_avg[k] <= 0) {
      Data_3$Snow[k] <- Data_3$PRCP[k]
    } else {
      Data_3$Mt[k] <- min(Data_3$SN[k], Data_3$T_avg[k] * K)
    }


    # Define AM5
    Data_3$AM5 <- 0

    # counter
    j <- 1
    # set k to  1
    k <- 1

    # define AM5 Melt
    # for the first 5 days
    for (k in seq(1, 5)) {
      Data_3$AM5[k + 1] <- sum(Data_3$Mt[1:j] + Data_3$Rn[1:j])
      j <- j + 1
    }

    # for the entire set
    for (i in seq(j, n[1])) {
      Data_3$AM5[i] <- sum(Data_3$Mt[(i - 5):(i - 1)] + Data_3$Rn[(i - 5):(i - 1)])
    }

    Data_3$AM5_comp <- 0

    # Define CN_1
    CN_1 <- CN_2 / (2.334 - 0.01334 * CN_2)
    # Define CN
    CN_3 <- CN_2 / (0.4036 + 0.0059 * CN_2)


    # define growing season or dormant season
    Data_3$Season <- 0

    loc_2 <- which(Data_3$Month < 11 & Data_3$Month > 4)
    Data_3$Season[loc_2] <- Data_3$Season[loc_2] + 1

    # main curve number
    Data_3$CN <- 0

    loc_3 <- which(Data_3$Season == 0 & Data_3$AM5 > AM2_D)
    Data_3$CN[loc_3] <- CN_3

    loc_4 <- which(Data_3$Season == 0 & Data_3$AM5 > AM1_D & Data_3$AM5 < AM2_D)
    Data_3$CN[loc_4] <- (CN_2 + ((CN_3 - CN_2) / (AM2_D - AM1_D)) * (Data_3$AM5[loc_4] - AM1_D))

    loc_5 <- which(Data_3$Season == 0 & AM1_D > Data$AM5)
    Data_3$CN[loc_5] <- (CN_1 + ((CN_2 - CN_1) / (AM1_D - 0)) * Data_3$AM5[loc_5])

    loc_6 <- which(Data_3$Season == 1 & Data_3$AM5 > AM2_G)
    Data_3$CN[loc_6] <- CN_3

    loc_7 <- which(Data_3$Season == 1 & Data_3$AM5 > AM1_G & Data_3$AM5 < AM2_G)
    Data_3$CN[loc_7] <- (CN_2 + ((CN_3 - CN_2) / (AM2_G - AM1_G)) * (Data_3$AM5[loc_7] - AM1_G))

    loc_8 <- which(Data_3$Season == 1 & AM1_G > Data_3$AM5)
    Data_3$CN[loc_8] <- (CN_1 + ((CN_2 - CN_1) / (AM1_G - 0)) * Data_3$AM5[loc_8])


    # use which outside of loop
    loc_0 <- which(Data_3$Mt > 0)

    # else use loc_0 to assign CN_3
    # CN for snowmelt
    Data_3$CN[loc_0] <- CN_3

    # S in Si units (Potential retention)
    Data_3$S <- (2540 / Data_3$CN) - 25.4

    # numerator (P-0.2S)^2
    Data_3$num <- 0

    # Estimate numerator for surface runoff
    loc_9 <- which(Data_3$Rn + Data_3$Mt < .2 * Data_3$S)
    Data_3$num <- Data_3$Rn + Data_3$Mt - 0.2 * Data_3$S
    Data_3$num[loc_9] <- 0

    # Define denum to plug into equation for surface runoff
    Data_3$denum <- Data_3$Rn + Data_3$Mt + 0.8 * Data_3$S

    # Define surface runoff
    Data_3$SRt <- Data_3$num / Data_3$denum

    # Define infiltration
    Data_3$I <- Data_3$Rn + Data_3$Mt - Data_3$SRt


    # Create dataframe to store crop koefficients
    KU <- data.frame(c(0.78, 0.82, 0.82, 0.79, 0.89, 0.91, 0.93, 0.98, 1.03, 0.97, 0.72, 0.61))
    names(KU) <- NULL
    names(KU) <- ("C_Koef")
    KU$Month <- seq(1, 12)
    d <- dim(KU)

    Data_3$C_Koef <- 0 # create column to store coefficients

    KK <- merge(Data_3, KU, (by <- "Month"), all = F) # Merge into another df two data frames to assign KK
    KK <- KK[with(KK, order(KK$Date)), ]

    Data_3$C_Koef <- KK$C_Koef.y

    Data_3$UNSAT <- 0 # Create column for unsaturated storage



    Data_3$UNSAT[1] <- UNSAT # Soil saturation on day 1

    Data_3$PE <- 0 # create column for adjusted potential evapotranspiration

    Data_3$PERC <- 0 # create column for percolation

    # Loop define Evapotranspiration as function of UNSAT on day t and infiltration
    # Define UNSAT at the next day

    ET_coef <- 0.56 # Since only 50% of ET in the watershed due to the high slopes

    for (i in seq(1, n[1] - 1)) {
      Data_3$PE[i] <- min(ET_coef * (Data_3$C_Koef[i] * Data_3$PET[i]), Data_3$UNSAT[i]) # adjust evapotranspiration

      # Data_3$UNSAT[i+1]<-Data_3$UNSAT[i]+Data_3$I[i]-Data_3$PE[i] #estimate UNSAT for day 2

      Data_3$UNSAT[i + 1] <- Data_3$UNSAT[i] + Data_3$I[i] - Data_3$PE[i]

      if (Data_3$UNSAT[i + 1] <= FCAP) {
        Data_3$PERC[i] <- 0
      } else {
        Data_3$PERC[i] <- Data_3$UNSAT[i + 1] - FCAP
        Data_3$UNSAT[i + 1] <- FCAP
      }
    }

    # Adjust evapotranspiration for the last part
    k <- max(n[1])
    Data_3$PE[k] <- min(Data_3$C_Koef[k] * Data_3$PET[k], Data_3$UNSAT[k])

    #################################################################################################################################################################################
    ################################################################################################################################################################################

    # define column Saturated as 0
    Data_3$Sat <- 0


    # print (SAT)

    Data_3$Sat[1] <- SAT

    # define SDt
    Data_3$SDt <- 0


    for (i in seq(1, n[1] - 1)) {
      # SDt
      Data_3$SDt[i] <- (1 - K_B) * Data_3$Sat[i]
      # Sat
      Data_3$Sat[i + 1] <- Data_3$Sat[i] + Data_3$PERC[i] - Data_3$SDt[i]
    }

    # Define SDt
    Data_3$SDt[k] <- (1 - K_B) * Data_3$Sat[k]

    # Discharge (cm)
    Data_3$Q <- (Data_3$SRt + Data_3$SDt)

    # Convert into cfs
    Data_3$Q <- Data_3$Q / (1000 * 100) * DA_3 / 86400 * 10^9 * 35.3146667

    # Final flow
    Data_3$Q_final <- 0
    Data_3$Q_final <- Data$Q + Data_2$Q + Data_3$Q

    N0 <- -20
    B0 <- -1600
    R0 <- 0
    R <- cor(Data_3$Q, y$Flow)
    N <- Nash(Data_3$Q, y$Flow)
    B <- Bias(Data_3$Q / 35.31, y$Flow / 35.31, length(Data_3$Q))


    if ((abs(B) < abs(B0)) && N > N0) {
      B0 <- B
      N0 <- N
      R_0 <- R

      SAT0_1 <- SAT
      KB0_1 <- K_B
      UNSAT0_1 <- UNSAT
      FCAP0 <- FCAP
    }
  }
}
plot (y$Flow)
lines (Data_3$Q_new)
# }
# }





#################################################################################################################################################################################
##########################################################################################################################






proc.time() - ptm


# #######################################################################################################################
# #######################################################################################################################

# # Plots
#
# monthly_mod <- (aggregate(x = Data$Q, by = list(Data$Month, Data$WaterYear), FUN = mean))
#
# monthly_measured <- (aggregate(x = y$Flow, by = list(y$Month, y$WaterYear), FUN = mean))
#
#
# monthly_mod_y1 <- monthly_mod[which(monthly_mod$Group.2 == min(monthly_mod$Group.2)), ]
# monthly_mod_last <- monthly_mod[which(monthly_mod$Group.2 == max(monthly_mod$Group.2)), ]
#
# monthly_measured_y1 <- monthly_measured[which(monthly_measured$Group.2 == min(monthly_measured$Group.2)), ]
# monthly_measured_last <- monthly_measured[which(monthly_measured$Group.2 == max(monthly_measured$Group.2)), ]
#
#
# plot(Data$Q ~ Data$Date, pch = NA_integer_, ylab = "Discharge, cfs", xlab = "Date", main = "Daily streamflow at Pskem river")
# lines(Data$Q ~ Data$Date, col = "red", lty = 2)
# lines(y$Flow ~ Data$Date, col = "blue", lty = 1)
# legend("topleft", c("Model", "Actual"), lty = c(2, 1), bty = "n", col = c("red", "blue"))
#
#
#
# # END
#
# ## NOTE runoff from glaciers is empirical and already includes evapotranspiration!!!!
#
#
#
# # define axes
# limy_1 <- max(monthly_mod_y1$x, monthly_measured_y1$x)
# #################################################################
# pdf("Figure1.pdf") # #print as pdf file
#
# plot(monthly_measured_y1$x ~ monthly_measured_y1$Group.1, pch = 17, col = "red", main = "Plot of observed and modelled monthly average flows for the first year of observations", ylab = "Flow, cfs", xlab = "Month", ylim = c(0, limy_1 + 20), cex.main = 0.8)
# points(monthly_mod_y1$x ~ monthly_mod_y1$Group.1, pch = 18, col = "blue")
# legend("topleft", legend = c("Measured", "Modeled"), pch = c(17, 18), col = c("Red", "Blue"), cex = 1, bty = "n")
#
#
#
# limy_2 <- max(monthly_mod_last$x, monthly_measured_last$x)
# #################################################################
# pdf("Figure2.pdf") # #print as pdf file
#
# plot(monthly_measured_last$x ~ monthly_measured_last$Group.1, pch = 17, col = "red", main = "Plot of observed and modelled monthly average flows for the last year of observations", ylab = "Flow, cfs", xlab = "Month", ylim = c(0, limy_2 + 20), cex.main = 0.8)
# points(monthly_mod_last$x ~ monthly_mod_last$Group.1, pch = 18, col = "blue")
# legend("topleft", legend = c("Measured", "Modeled"), pch = c(17, 18), col = c("Red", "Blue"), cex = 1, bty = "n")
#
#
# remove(KK, KU, x, A, AM1_D, AM1_G, AM2_G, AM2_D, AM5, CN_1, CN_2, CN_3, by, CN2, ERR, ERR3, FCAP, i, j, K, k, K_B, loc_0, loc_1, loc_2, loc_3, loc_4, loc_5, loc_6, loc_7, loc_8, loc_9, n, SAT, Fin_EVP, Fin_Ground, Fin_P, Fin_Surf, Fin_Stream, Month_abv, Peak, Average, d, f, limy_1, limy_2, UNSAT, y_dim)
#
# #################################################################
#
# plot(Data$Q / 35.31 ~ Data$Date, pch = NA_integer_)
# lines(Data$Q / 35.31 ~ Data$Date, col = "red", lty = 2)
# lines(y$Flow / 35.31, col = "blue")
# legend("topright", c("Model", "Actual"))
#
# par(mar = c(5.1, 4.3, 4.1, 2.1))
# plot(monthly_measured$x / 35.31, pch = 17, main = "Average monthly streamflow at Pskem river between 2013-2015 water year", ylab = "Discharge, cms", xlab = "Month", col = "blue", cex = 1.7, cex.main = 2.2, cex.lab = 2, y.intersp = 0.5, ylim = c(0, 200))
# # lines (monthly_measured$x, pch = 17, col = "blue")
# points(monthly_mod$x / 35.31, pch = 14, col = "red")
# legend("topleft", c("Actual", "Model"), pch = c(17, 14), col = c("blue", "red"), bty = "n", inset = c(0, 0), y.intersp = 0.5, cex = 1.2)
# # END
#
# par(mar = c(5.1, 4.0, 4.1, 2.1))
#
# library(EcoHydRology)
# hydro <- data.frame(array(0, 1095))
# hydro$Date <- Data$Date
# hydro$Precip <- Data$PRCP * 25.4
# hydro$Streamflow <- Data$Q / 35.31
# hydro <- hydro[, 2:4]
# hydrograph(hydro, streamflow2 = y$Flow / 35.31, S1.col = "red", S2.col = "blue") # ylab = "Streamflow, cms", xlab = "Date", cex = 1.3)
# legend(50, 200, lty = c(2, 1), c("Actual", "Model"), col = c("blue", "red"), bty = "n", cex = 1.2)
#
# plot(Data$T_av ~ Data$Date)
#
# par(mfrow = c(1, 1))
# # x11()
# # background color
# mycol <- rgb(226, 240, 217, max = 255)
# par(bg = "white")
#
# library(TTR)
# library(spatstat)
# # exponentially weighted mean
# plot(Data$T_av ~ Data$Date, main = "Exponentially-weighted mean of daily air temperatures at Pskem watershed", ylim = c(-30, 30), pch = NA_integer_, ylab = "Tair, C", xlab = "Date", cex.main = 1.8, cex.axis = 1.3, cex.lab = 1.3)
# lines(Data$Date, EMA(Data$T_av, n = 5, wilder = F), col = "red", lty = 1)
# lines(lowess(Data$Date, Data$T_av, f = .2, iter = 3), col = "blue", lty = 2)
# legend("topleft", c("Daily average air temperature", "Lowess line, f = 1/5"), lty = c(1, 2), col = c("red", "blue"), bty = "n", cex = 1.3)
#
# Nash <- function(mod, obs) {
#   N_S <- 1 - sum((mod - obs)^2) / sum((obs - mean(obs))^2)
#   return(N_S)
# }
#
# # compute Bias
# Bias <- function(mod, obs, N) {
#   Bias <- sum(mod - obs) / N
#   return(Bias)
# }
#
# Bias(Data$Q, y$Flow, 1095)
# Nash(Data$Q, y$Flow)
# Nash(monthly_mod$x, monthly_measured$x)
#
#
# reg_1 <- lm(log(y$Flow) ~ log(Data$Q))
# # Test 1. Assumption 1. Linear relationship between dependent and independent variables
#
# # Plot of observed vs predicted values
#
# plot(y$Flow, Data$Q, main = "Plot of Observed vs Predicted values", xlab = "PredictedQ100", ylab = "ObservedQ100")
# lines(y$Flow, y$Flow, col = "orange")
#
# # Set resreg equal to model final residuals
#
# residQ <- residuals(reg_1)
#
# # plot residuals vs model predictions
#
# plot(predict(reg_1), residQ, main = "Plot of Residuals vs Predicted values") # plot regression predictions vs final model residuals
# lines(lowess(predict(reg_1), residQ))
#
# # Conclusion: Assumption not violated.
#
# sum(y$Flow) / sum(Data$Glac_Q_adj)
# cor(monthly_mod$x, monthly_measured$x)
# cor(Data$Q, y$Flow)
# ## NOTE runoff from glaciers is empirical and already includes evapotranspiration!!!!
