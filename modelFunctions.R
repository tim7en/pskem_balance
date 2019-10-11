# define wateryear (required inputs - year, month (october first corresponds to the beginning of the wateryear))
wateryear <- function(y, m) {
  if (length(y) < 2 | length(m) < 2 | length(m) != length(y) | !is.numeric(y) | !is.numeric(m)) {
    return("inputs do not satisfy requirements")
  } else {
    y[which(m < 10)] <- y[which(m < 10)] - 1
    return(y)
  }
}

# hamonds method to find potential evapotranspiration using solar radiation method, air temperature inputs required in Celsius
hamondsevp <- function(x, tmax, tmin) {
  if (!is.data.frame(x) | !"month" %in% names(x) | !"date" %in% names(x)) {
    return("data frame required")
  } else if (length(tmax) < 2 | length(tmin) < 2 | length(tmin) != length(tmax) | !is.numeric(tmax) | !is.numeric(tmin)) {
    return("inputs do not satisfy requirements")
  } else {
    n <- dim(x)
    x$TMAX <- tmax
    x$TMIN <- tmin
    N <- c(9.3, 10.4, 11.7, 13.1, 14.3, 15.0, 14.6, 13.6, 12.3, 10.9, 9.7, 9.0) # number of daylight hours per day in each month (beginning from January)
    month <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    Coef <- data.frame(cbind(N, month)) # assign coefficient N to data frame of our values
    Co <- merge(x, Coef, (by <- "month"), all = T)
    Co <- Co[with(Co, order(Co$date)), ]
    x$N <- Co$N
    x$T <- (x$TMAX + x$TMIN) / 2
    x$Est <- 6.108 * exp(17.27 * x$T / (237.3 + x$T))
    x$E <- 0
    loc_f <- which(x$T < 0)
    x$E <- 0.021 * ((x$N)^2) * x$Est / (x$T + 273)
    x$E[loc_f] <- 0
    return(x$E)
  }
}

# Physical process based snowmelt method, data frame with valid columns of air temperature - T, precipitation - P required. Metric units in cm.
snowmelt <- function(x, sat, k) {
  if (!"SN" %in% names(x) | !"SP" %in% names(x) | !"T" %in% names(x) | !"Mt" %in% names(x) | !"P" %in% names(x)) {
    return("inputs are invalid 1 ")
  } else if (!is.data.frame(x) | length(sat) > 1 | length(k) > 1) {
    return("inputs are invalid 2 ")
  } else {
    basinData <- x
    basinData$SN[1] <- sat
    belowZero <- which(basinData$T <= 0) # define location where T_av<0 # define accumulation of amount of snow at day t+1; #define melt
    basinData$SN[belowZero] <- basinData$P[belowZero] + basinData$SN[belowZero] # Define snow, since all of the days are set to 0, except first day, we just add that array
    for (i in seq(1, n[1] - 1)) {
      if (basinData$T[i] <= 0) {
        basinData$SP[i + 1] <- basinData$SP[i] + basinData$SN[i] # snowpack accumulation
      } else {
        basinData$Mt[i] <- min(basinData$SP[i], basinData$T[i] * k) # otherwise snowpack is melting
        basinData$SP[i + 1] <- basinData$SP[i] - basinData$Mt[i]
      }
    }
    if (basinData$T[n] <= 0) {
      basinData$SP[n] <- basinData$SP[n - 1] + basinData$SN[n]
    } else {
      basinData$Mt[n] <- min(basinData$SP[n], basinData$T[n] * k)
    }
    return(basinData)
  }
}

# 5 day anticident moisture conditions
am5runoff <- function(x) {
  if (!is.data.frame(x) | !"AM5" %in% names(x) | !"Rn" %in% names(x) | !"Mt" %in% names(x)) {
    return("inputs are invalid")
  } else {
    basinData <- x
    for (k in seq(1, 5)) {
      basinData$AM5[k + 1] <- sum(basinData$Mt[1:j] + basinData$Rn[1:j]) # define AM5 Melt # for the first 5 days
      j <- j + 1
    }
    for (i in seq(j, n)) {
      basinData$AM5[i] <- sum(basinData$Mt[(i - 5):(i - 1)] + basinData$Rn[(i - 5):(i - 1)]) # for the entire set
    }
    return(basinData)
  }
}

# curvenumber method tied up to surfacerunoff
curvenumber <- function(x, CN_2) {
  if (!is.data.frame(x) | !"AM5" %in% names(x) | !"month" %in% names(x) | !"Mt" %in% names(x) | !is.numeric(CN_2)) {
    return("inputs are invalid")
  } else {
    basinData <- x
    CN_1 <- CN_2 / (2.334 - 0.01334 * CN_2)
    CN_3 <- CN_2 / (0.4036 + 0.0059 * CN_2)
    AM1_D <- 1.3
    AM2_D <- 2.8
    AM1_G <- 3.6
    AM2_G <- 5.3
    basinData$Season <- 0 # define growing season or dormant season
    growingMonth <- which(basinData$month < 11 & basinData$month > 4)
    basinData$Season[growingMonth] <- basinData$Season[growingMonth] + 1
    basinData$CN <- 0 # main curve number
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
    cond_7 <- which(basinData$Mt > 0) # use which outside of loop     # else use loc_0 to assign CN_3
    basinData$CN[cond_7] <- CN_3 # CN for snowmelt
    return(basinData)
  }
}

# surfacerunoff, continuation of curvenumber method
surfacerunoff <- function(x) {
  if (!is.data.frame(x) | !"CN" %in% names(x) | !"Rn" %in% names(x) | !"Mt" %in% names(x)) {
    return("inputs are invalid")
  } else {
    basinData <- x
    basinData$S <- (drainageArea / basinData$CN) - 25.4 # S in Si units (Potential retention)
    basinData$num <- 0 # numerator (P-0.2S)^2
    cond_8 <- which((basinData$Rn + basinData$Mt) < (0.2 * basinData$S)) # Estimate numerator for surface runoff
    basinData$num <- (basinData$Rn + basinData$Mt - 0.2 * basinData$S)^2
    basinData$num[cond_8] <- 0
    basinData$denum <- basinData$Rn + basinData$Mt + 0.8 * basinData$S # Define denum to plug into equation for surface runoff
    basinData$SRt <- basinData$num / basinData$denum # Define surface runoff
    return(basinData)
  }
}

# crop coefficient adjusted actual evapotrapiration
actualevp <- function(x, UNSAT, FCAP) {
  if (!is.data.frame(x) | !"month" %in% names(x) | !"date" %in% names(x) | !"ETO" %in% names(x) | !"I" %in% names(x) | !"Mt" %in% names(x) | !is.numeric(CN_2)) {
    return("inputs are invalid")
  } else {
    basinData <- x
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
    for (i in seq(1, n[1] - 1)) { # Loop define Evapotranspiration as function of UNSAT on day t and infiltration # Define UNSAT at the next day
      basinData$PE[i] <- min(basinData$C_Koef[i] * basinData$ETO[i], basinData$UNSAT[i]) # adjust evapotranspiration
      basinData$UNSAT[i + 1] <- basinData$UNSAT[i] + basinData$I[i] - basinData$PE[i] # estimate UNSAT for day 2
      if (basinData$UNSAT[i + 1] <= FCAP) {
        basinData$PERC[i] <- 0
      } else {
        basinData$PERC[i] <- basinData$UNSAT[i + 1] - FCAP
        basinData$UNSAT[i + 1] <- FCAP
      }
    }
    basinData$PE[k] <- min(basinData$C_Koef[n] * basinData$ETO[n], basinData$UNSAT[n])
    return(basinData)
  }
}

# final model runoff estimate
modelrunoff <- function(x, SAT, K_B) {
  if (!is.data.frame(x) | !"month" %in% names(x) | !"date" %in% names(x) | !"ETO" %in% names(x) | !"I" %in% names(x) | !"Mt" %in% names(x) | !is.numeric(SAT)) {
    return("inputs are invalid")
  } else {
    basinData <- x
    basinData$Sat <- 0 # define column Saturated as 0
    basinData$Sat[1] <- SAT # define sat conditions for the first day
    basinData$SDt <- 0 # define SDt
    for (i in seq(1, n[1] - 1)) {
      basinData$SDt[i] <- (1 - K_B) * basinData$Sat[i] # SDt
      basinData$Sat[i + 1] <- basinData$Sat[i] + basinData$PERC[i] - basinData$SDt[i] # Sat
    }
    basinData$SDt[k] <- (1 - K_B) * basinData$Sat[k] # Define SDt
    basinData$Q_est <- 0
    basinData$Q_est <- (basinData$SRt + basinData$SDt) # Discharge (cm)
    basinData$Q_est <- basinData$Q_est / (1000 * 100) * drainageArea / 86400 * 10^9 # result
    return(basinData)
  }
}


# Compute Nash-Sutcliff efficiency
Nash <- function(mod, obs) {
  if (length(mod) < 2 | length(obs) < 2 | length(mod) != length(obs) | !is.numeric(mod) | !is.numeric(obs)) {
    return("inputs do not satisfy requirements")
  } else {
    N_S <- 1 - sum((mod - obs)^2) / sum((obs - mean(obs))^2)
    return(N_S)
  }
}

# compute Bias
Bias <- function(mod, obs) {
  if (length(mod) < 2 | length(obs) < 2 | length(mod) != length(obs) | !is.numeric(mod) | !is.numeric(obs)) {
    return("inputs do not satisfy requirements")
  } else {
    Bias <- sum(mod - obs) / length(mod)
    return(Bias)
  }
}
