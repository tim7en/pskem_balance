# load function
source("Func_final.R")

Curve_numb<-function (x,elv,st_elv,DA,K,CN_2, K_B, UNSAT, SAT,FCAP){
  # load function
  source("Func_final.R")
  Data<-preproc (x)
  
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
  
  l<-length(Data$PRCP)
  
  Month<-array(Data$Month,l)
  PRCP<-array(Data$PRCP,l)
  Lapse<-array(Data$Lapse,l)
  
  TMAX<-array(Data$TMAX,l)
  TMIN<-array(Data$TMIN,l)

  
  TMAX <- (TMAX - 32) * 5 / 9 - (elv - st_elv) / 1000 * Lapse
  TMIN <- (TMIN - 32) * 5 / 9 - (elv - st_elv) / 1000 * Lapse
  T_avg <- (TMAX + TMIN) / 2
  
  # Convert back temperature into Kelvin, averaged for the entire watershed.
  TMAX <- TMAX * 9 / 5 + 32
  TMIN <- TMIN * 9 / 5 + 32
  
  # Estimate potential ET
  PET <- Evp(Data, TMAX, TMIN)
  
  
  # define location where T_av<0
  Snow <- array(0,l)
  loc_1 <- which(T_avg <= 0)
  Snow [loc_1] <- PRCP[loc_1]
  Mt <- array (0,l)
  SN <- array (0,l)
  
  #Snow cover at day 1 is 0
  SN[1]<-0
  
  # Define snow cover
  for (i in seq(1, n[1] )) {
    if (T_avg[i] <= 0) {
      SN[i + 1] <- SN[i] + Snow[i]
    }
    else {
      Mt[i] <- min(SN[i], T_avg[i] * K)
      SN[i + 1] <- SN[i] - Mt[i]
    }
  }
  
  Rn<-PRCP-Snow
  # Define AM5
  AM5 <- array(0,l)
  
  # counter
  j <- 1
  # set k to  1
  k <- 1
  
  # define AM5 Melt
  # for the first 5 days
  for (k in seq(1, 5)) {
    AM5[k + 1] <- sum(Mt[1:j] + Rn[1:j])
    j <- j + 1
  }
  
  # for the entire set
  for (i in seq(j, n[1])) {
    AM5[i] <- sum(Mt[(i - 5):(i - 1)] + Rn[(i - 5):(i - 1)])
  }
  
  AM5_comp <- 0
  
  # my boundaries
  AM1_D <- 1.3
  AM2_D <- 2.8
  
  # my boundaries
  AM1_G <- 3.6
  AM2_G <- 5.3
  
  # Define CN_1
  CN_1 <- CN_2 / (2.334 - 0.01334 * CN_2)
  # Define CN
  CN_3 <- CN_2 / (0.4036 + 0.0059 * CN_2)
  
  
  # define growing season or dormant season
  Season <- array(0,l)
  
  loc_2 <- which(Month < 11 & Month > 4)
  Season[loc_2] <- Season[loc_2] + 1
  
  # main curve number
  CN <- 0
  
  loc_3 <- which(Season == 0 & AM5 > AM2_D)
  CN[loc_3] <- CN_3
  
  loc_4 <- which(Season == 0 & AM5 > AM1_D & AM5 < AM2_D)
  CN[loc_4] <- (CN_2 + ((CN_3 - CN_2) / (AM2_D - AM1_D)) * (AM5[loc_4] - AM1_D))
  
  loc_5 <- which(Season == 0 & AM1_D > AM5)
  CN[loc_5] <- (CN_1 + ((CN_2 - CN_1) / (AM1_D - 0)) * AM5[loc_5])
  
  loc_6 <- which(Season == 1 & AM5 > AM2_G)
  CN[loc_6] <- CN_3
  
  loc_7 <- which(Season == 1 & AM5 > AM1_G & AM5 < AM2_G)
  CN[loc_7] <- (CN_2 + ((CN_3 - CN_2) / (AM2_G - AM1_G)) * (AM5[loc_7] - AM1_G))
  
  loc_8 <- which(Season == 1 & AM1_G > AM5)
  CN[loc_8] <- (CN_1 + ((CN_2 - CN_1) / (AM1_G - 0)) * AM5[loc_8])
  
  
  # use which outside of loop
  loc_0 <- which(Mt > 0)
  
  # else use loc_0 to assign CN_3
  # CN for snowmelt
  CN[loc_0] <- CN_3
  
  # S in Si units (Potential retention)
  S <- (2540 / CN) - 25.4
  
  # numerator (P-0.2S)^2
  num <- array(0,l)
  
  # Estimate numerator for surface runoff
  loc_9 <- which(Rn + Mt < .2 * S)
  num <- Rn + Mt - 0.2 * S
  num[loc_9] <- 0
  
  # Define denum to plug into equation for surface runoff
  denum <- Rn + Mt + 0.8 * S
  
  # Define surface runoff
  SRt <- num / denum
  
  # Define infiltration
  I <- Rn + Mt - SRt
  
  
  # Create dataframe to store crop koefficients
  KU <- data.frame(c(0.78, 0.82, 0.82, 0.79, 0.89, 0.91, 0.93, 0.98, 1.03, 0.97, 0.72, 0.61))
  names(KU) <- NULL
  names(KU) <- ("C_Koef")
  KU$Month <- seq(1, 12)
  d <- dim(KU)
  
  C_Koef <- array(0,l) # create column to store coefficients
  
  KK <- merge(Data, KU, (by <- "Month"), all = F) # Merge into another df two data frames to assign KK
  KK <- KK[with(KK, order(KK$Date)), ]
  
  C_Koef <- KK$C_Koef.y
  
  UNSAT <- array(0,l) # Create column for unsaturated storage
  
  FCAP <- FCAP

  UNSAT[1]<-Unsat
  PE <- array(0,l) # create column for adjusted potential evapotranspiration
  PERC <- array(0,l) # create column for percolation
  
  # Loop define Evapotranspiration as function of UNSAT on day t and infiltration
  # Define UNSAT at the next day
  
  ET_coef <- 1 # Since only 50% of ET in the watershed due to the high slopes
  
  for (i in seq(1, n[1] )) {
    PE[i] <- min(ET_coef * (C_Koef[i] * PET[i]), UNSAT[i]) # adjust evapotranspiration
    
    # UNSAT[i+1]<-UNSAT[i]+I[i]-PE[i] #estimate UNSAT for day 2
    
    UNSAT[i + 1] <- UNSAT[i] + I[i] - PE[i]
    
    if (UNSAT[i + 1] <= FCAP) {
      PERC[i] <- 0
    } else {
      PERC[i] <- UNSAT[i + 1] - FCAP
      UNSAT[i + 1] <- FCAP
    }
  }
  

  #################################################################################################################################################################################
  ################################################################################################################################################################################
  
  # define column Saturated as 0
  Sat <- array(0,l)
  
  Sat[1] <- SAT
  
  # define SDt
  SDt <- array(0,l)
  
  for (i in seq(1, n[1] )) {
    # SDt
    SDt[i] <- (1 - K_B) * Sat[i]
    # Sat
    Sat[i + 1] <- Sat[i] + PERC[i] - SDt[i]
  }
  
  # Define SDt
  SDt[k] <- (1 - K_B) * Sat[k]
  
  
  Result<-as.list(0)
  Q <- SRt + SDt
  Q <- Q / (1000 * 100) * DA / 86400 * 10^9 * 35.3146667
  

  #Result<-as.data.frame(cbind(Q,as.POSIXlt(Data$Date)))
  #x2<-ts(Q,start= c(Data$Year[1],Data$Month[1],Data$Day[1]),frequency = 366)
  Result<-as.data.frame(cbind(Q,Data$Day,Data$Month,Data$Year))

  return (Result) 
  }


  