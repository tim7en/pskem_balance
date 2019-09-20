#Function for FDC

#create FDC


FDC <- function (x, p) {
  n <- length(x)
  #sort data
  Flow <- sort(x, decreasing = T)
  
  #two ways of ranking
  rank <- rank(-Flow)
  
  #weibull plotting position
  Weibull <- rank / (1 + n)
  
  I_est <- floor((n + 1) * (p))
  
  
  theta <- ((n + 1) * (p) - I_est)
  
  Q <- array(0, length(p))
  
  #Estimate discharge values for flow duration curve
  for (i in seq(1, length(p))) {
    Q[i] <-
      (1 - theta[i]) * Flow[I_est[i]] + theta[i] * Flow[I_est[i] + 1]
  }
  return (Q)
}


FDC_plot <- function (x, p) {
  n <- length(x)
  #sort data
  Flow <- sort(x, decreasing = T)
  
  #two ways of ranking
  rank <- rank(-Flow)
  
  #weibull plotting position
  Weibull <- rank / (1 + n)
  
  I_est <- floor((n + 1) * (p))
  
  
  theta <- ((n + 1) * (p) - I_est)
  
  Q <- array(0, length(p))
  
  #Estimate discharge values for flow duration curve
  for (i in seq(1, length(p))) {
    Q[i] <-
      (1 - theta[i]) * Flow[I_est[i]] + theta[i] * Flow[I_est[i] + 1]
  }
  #plot flow vs exceedance probability
  plot (Weibull, Flow, log = "y", pch = NA_integer_)
  lines (Weibull, Flow)
  points (p, Q)
  return (Q)
}


FDC_Weib <- function (x) {
  n <- length(x)
  
  #sort data
  Flow <- sort(x, decreasing = T)
  
  #two ways of ranking
  rank <- rank(-Flow)
  
  #weibull plotting position
  Weibull <- rank / (1 + n)
  
  res <- cbind (Weibull, Flow)
  
  return (as.data.frame(res))
}


#add a function count (count <- function(x, n){ length((which(x == n))) })
(count <- function(x, n) {
  length((which(x == n)))
})


#Function to preproc data
#Timur Sabitov

#LOOP 1 - assign water years for the data frame

#create table with months
preproc <- function (x) {
  n <- dim(x)
  
  #change missing values to NA
  
  is.na(x)<-x== -9999
  is.na(x)<-x== c("-9999")
  
  
  x$DATE<-as.character(x$DATE)
  
  #Convert formate to posixlt 
  x$Date<-as.POSIXlt(x$DATE, format="%Y%m%d")
  
  #create table with days
  x$Day<-x$Date$mday
  
  #create table with months
  x$Month<-x$Date$mon+1
  
  #create table with years
  x$Year<-x$Date$year+1900

  #create table for water years
  x$WaterYear <- 0
  
  #loop throught the data to define water years
  
  for (i in seq(1, n[1])) {
    if (x$Month[i] >= 10) {
      x$WaterYear[i] = (x$Year + 1)[i]
    } else {
      x$WaterYear[i] = x$Year[i]
    }
  }
  return (x)
}


Div <- function (x) {
  dif <- max(x$WaterYear) - min(x$WaterYear) + 1
  
  #LOOP 2 - count water years
  
  c <- array(0, dif)
  
  
  
  for (j in seq(1, dif)) {
    w <- seq(min(x$WaterYear), max(x$WaterYear))[j]
    c[j] <- count(x$WaterYear, w)
    
  }
  
  #save output as list flow
  flow <- as.list(NA)
  
  #find cumulative
  b <- cumsum(c)
  
  #for the first year from 0 to b[1]
  flow[[1]] <- x$Flow[0:b[1]]
  
  #LOOP 3 - it should define cumulative borders for intervals and extract values into list
  
  for (k in seq(2, dif)) {
    flow[[k]] <- x$Flow[(b[k - 1] + 1):b[k]]
    
  }
  
  return (flow)
}
