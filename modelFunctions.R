Evp<-function (x, tmax, tmin){
  
  #define dimentions of data frame
  n<-dim(x)
  x$TMAX<-tmax
  x$TMIN<-tmin
  
  #number of daylight hours per day in each month (beginning from January)
  N<-c(9.3,10.4,11.7,13.1,14.3,15.0,14.6,13.6,12.3,10.9,9.7,9.0)
  month<-c(1,2,3,4,5,6,7,8,9,10,11,12)
  
  #assign coefficient N to data frame of our values
  Coef<-data.frame(cbind (N,month))
  Co<-merge(x,Coef, (by = "month"), all = T)
  Co<-Co[with(Co,order(Co$date)),]
  x$N<-Co$N
  
  #if in kelvin
  #Define average T in C*
  #x$T<-(((x$TMAX+x$TMIN)/2)-32)/1.8
  
  #Define Est
  #Est= 6.108 exp(17.27 T/(237.3 + T))
  
  #else
  x$T <- (x$TMAX+x$TMIN)/2
  #=6.108*EXP(17.27*H8/(237.3+H8))
  x$Est <- 6.108*exp(17.27*x$T/(237.3 + x$T))
  
  #Populate my E
  x$E<-0
  
  #check which row has negative temperatures
  
  loc_f<-which (x$T<0)
  
  x$E<-0.021* ((x$N)^2)* x$Est/(x$T + 273)
  x$E[loc_f]<-0
  
  return (x$E)
}