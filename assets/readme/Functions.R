#Functions

Proc<-function (x,a,d){
  
  #define first year of the data
  oury <- x$Years
  
  a<-as.data.frame(dfw)
  names(a)<-NULL
  names(a)<-c("DATE")
  
  #Convert formate to posixlt 
  a$Date<-as.POSIXlt(a$DATE, format="%Y%m%d")
  
  
  #create table with months
  a$Month<-a$Date$mon+1
  
  #create table with years
  a$Year<-a$Date$year+1900
  
  #convert as Date
  a$Date<-as.Date(as.POSIXlt(a$DATE, format="%Y%m%d"))
  
  y<-t(y)
  #define dimentions
  d_a<-dim(a)
  d_x<-dim(y)
  
  #counter
  j<-1
  
  #loop throught the data, 
  for (i in seq (1, d_x[1])){
    for (k in seq (1, d_x[2])){
      
      a$Discharge[j]<-y[i,k]
      if (is.na(a$Discharge[j])==T) (k<-k+1) else {}
      j<-j+1
      
    }
  }
  
  a<-na.omit(a)
  Data<-data.frame(date <- seq(as.Date(d),length = (366), by = "day"))
  if (length(a$Discharge)==366) {Data$Discharge<-a$Discharge} else {Data<-data.frame(Data[1:365,]);Data$Discharge<-a$Discharge}
  
  return (Data)
}  





Cr_cor<-function (x,y,A,Ax){

colnames (x)<-c("Date","Flow")
#Area of watershed 
Precip_1<-x$Flow/(A*10^6)
Precip_2<-x$Flow/(A*10^6)

#Area of watershed 1
G_1<-y$Flow-Precip_1*(Ax*10^6)

#hydrograph of y1
x11()
cor<-cor (G_1,y$Flow)
plot (y$Flow~y$Day, pch = NA_integer_, ylim = c(0,130))
lines (y$Flow~y$Day, col = "blue")
lines (G_1, col = "red", lty = 2 )

return (cor)

}

LG2<-function (x,perc){
  Y<-log(x)
  My<-mean(Y)
  sd<-sd(Y)
  #define percentile
  zp<-qnorm(perc)
  
  #For E=0
  Q<-round((exp(My+zp*sd)),2)
  
  print ("7 day 10 year low flow, 2 parameter LGnorm")
  
  return(Q)
}




