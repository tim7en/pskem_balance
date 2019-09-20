#Function to define overall statistics from HW-1,2,3
#Sabitov Timur


#Variables

#Variables
#Input is my data
#x$Month - months
#x$WaterYear - Water Year
#x$Year - Calendar Year
#x$posixlt- proper data format
#c - number of days in 20 years for each water year separatly (considers leap day)
#b - cumulative number of days in 20 water years
#flow list of 20 water years and flow values as lists
#p and d are intervals of 7 days for moving average
#data - data frame of 20 years means for each year
#Q100 - 100 year flood
#Q710 - 7 day low flow 10 year
#n - dimentions of the entire data set
#minq - minimums of moving averages of 7 day low flow for 20 water years
#Average - mean for 20 water years
#Peak - max for 20 water years
#sdmean - standard deviation of the mean flow  
#sdmax - standard deviation of the maximum flow out of each water year
#sd7min - standart deviation of 7 day minimum
#Qmean - average water year flow for 20 years
#Qmaxmean - average of maximum flow for 20 years
#Q7mean - average of 7 day minimum 
#result <- result data frame that includes peak, average, 7day minimum
#stat <- statistics of our data (std, mean)


#add a function write stats

#!IMPORTANT
#to apply this function you need appropriate names for colums as input for input file, column of Year

#column of WaterYear, Flow

#add a function count (count <- function(x, n){ length((which(x == n))) })
(count <- function(x, n){ length((which(x == n))) })

#add a function write stats
#to apply this function you need appropriate names for colums as input for input file, column of Year
#column of WaterYear etc.

overall<-function(input){
  
  #convert format of dates
  input$Posixlt<-as.POSIXlt(input$Date, format="%m/%d/%Y")
  
  #create table with months
  input$Month<-input$Posixlt$mon+1
  
  #create table with years
  input$Year<-input$Posixlt$year+1900
  
  #create table for water years
  input$WaterYear<-0
  
  
  #future data variable where counting of days in each year will be saved
  c<-NA
  
  #our 7 day intervals inside of loop
  p<-NA
  d<-NA
  
  n<-dim(input)
  #loop1
  
  for (i in seq(1,n[1])){
    
    if (input$Month[i]>=10) 
    {input$WaterYear[i]=(input$Year+1)[i]} 
    else 
    {input$WaterYear[i]=input$Year[i]}
    
  }
  
  #get the difference between maximum and minimum water year
  dif<-((max(input$WaterYear)-min(input$WaterYear))+1)
  
  #add count function inside of another function !!! mind...blo...g
  (count <- function(x, n){ length((which(x == n))) })
  
  for (j in seq(1,dif)){
    
    w<-seq(min(input$WaterYear),max(input$WaterYear))[j]
    c[j]<-count(input$WaterYear,w)
    
  }
  
  #save output as list flow
  flow<-as.list(NA)
  
  #find cumulative
  b<-cumsum(c) 
  
  #LOOP  - it should define cumulative borders for intervals and extract values into list "flow"
  
  #for the first year from 0 to b[1]
  flow[[1]]<-input$Flow[0:b[1]]
  
  for (k in seq(2,dif)){
    
    flow[[k]]<-input$Flow[(b[k-1]+1):b[k]]
    
  }
  
  #subset possible command
  Q7min<-array(0,dif)
  
  #array that will be used for 
  days<-array(0,dif)
  
  #matrix where means for each water year will be saved
  data<-matrix(NA,360,dif)
  
  for (y in seq (1,max(c))){
    
    for (u in seq (1,dif)){
      ##days[u]<-length(flow[[u]])
      if (c[u]>365)
      {
        ##N[u]<-366
        p<-seq(1,366)[y]
        d<-seq(7,366)[y]
        
        #options(error = expression(NULL))[y]#try to remove error mes
        
        #finally removing error mess (silent= T with function try)
        try(data[,u][y]<-mean(flow[[u]][p:d]),silent=T)
        
        
        Q7min[u]<-min(data[,u])
        
      }else{
        
        p<-seq(1,366)[y] #used for selecting from 1 to 7 if in seq 2:8 3:9 etc... (by 7)
        d<-seq(7,366)[y]
        
        #options(error = expression(NULL))[y]#try to remove error mes
        
        try(data[,u][y]<-mean(flow[[u]][p:d]),silent=T)
        
        
        Q7min[u]<-min(na.omit(data[,u]))
      }
    }
  }
  
  Peak<-round((aggregate(x = input$Flow, by = list(input$WaterYear), FUN = max)),2)
  Average<-round((aggregate(x = input$Flow, by = list(input$WaterYear), FUN = mean)),2)
  Minimums<-round((aggregate(x = input$Flow, by = list(input$WaterYear), FUN = min)),2)
  Q7min<-round(cbind(seq(min(input$WaterYear),max(input$WaterYear)),Q7min),2)
  
  result<-data.frame(Peak,Average,Minimums,Q7min)
  names(result)<-NULL
  names(result)<-c("Year","Peak_Flow","Year","Avg_Flow","Year","Min_Flow","Year","710Min_Flow")
  #write output file Res.csv
  #write.csv(result,"Res.csv")
  return (result)
}


#Variables
#LP3 - function for Log Pearson type III
#LG - function for 2 or 3 parameter log distribution
#n - length of the data
#lx - log space of the data
#avg - mean of the data
#std - standard deviation of the data
#zp - percentile of the data from normal distribution
#skew - define unbiased skew of the data
#kp - define adjustment koefficient of the data
#Q - value of predicted discharge in cfs
#Y - data in log space
#My - mean of data in log space


#Function will apply Log-Pearson type III distribution to define possible flood
LP3<- function (x,percentile){
  
  #determine length of data
  n<-length(x)
  
  #convert into log space
  lx<-log(x)
  
  #define mean and standard deviation
  avg<-mean(lx)
  std<-sd(lx)
  
  #define percentile
  zp<-qnorm(percentile)
  
  #define skew of the data
  skew<- (1+(6/n))*(n*(sum((lx-avg)^3)/((n-1)*(n-2)*std^3)))
  
  #define kp coefficient
  kp<- 2/skew*(1+(skew*(zp))/6-(skew^2)/36)^3-2/skew
  
  #Define discharge
  Q<- round((exp(avg+kp*std)),2)
  print("100 year peak flow. Units in cfs")
  return(Q)
  
}

#Function will apply 2 and 3 parameter log-normal distribution

LG<-function(x,percentile){
  
  #perform calculations and define value of 
  E<-(min(x)*max(x)-(median(x))^2)/(min(x)+max(x)-2*median(x))
  
  if (((min(x)+max(x))>(2*median(x))) & (min(x)*max(x))<(median(x)^2))
  {
    Y<-log(x-E)
    My<-mean(Y)
    sd<-sd(Y)
    
    #define percentile p.29 Note y=ln(E-x) is normally distributed, and:
    #Xp=E-exp(My+Z(1-p)*sd)
    zp<-qnorm(1-percentile)
    
    #for E in upper bound
    Q<-round((E-exp(My+zp*sd)),2)
    
    print ("7 day 10 year low flow, E in upper bound, 3 parameter LGnorm")
    
    return(Q)
  }
  if (((min(x)+max(x))>(2*median(x))) & (min(x)*max(x))>(median(x)^2))
  {
    Y<-log(x-E)
    My<-mean(Y)
    sd<-sd(Y)
    
    #define percentile
    zp<-qnorm(percentile)
    
    #For E in lower bound
    Q<-round((E+exp(My+zp*sd)),2)
    
    print ("7 day 10 year low flow, E in lower bound, 3 parameter LGnorm. Units in cfs")
    
    return(Q)
    
  }else{
    
    Y<-log(x)
    My<-mean(Y)
    sd<-sd(Y)
    #define percentile
    zp<-qnorm(percentile)
    
    #For E=0
    Q<-round((exp(My+zp*sd)),2)
    
    print ("7 day 10 year low flow, 2 parameter LGnorm")
    
    return(Q)
  }  
}



#Define all necessuary statistics

stat<-function (input){
  
  St_d<-sd(input)

  Ma_x<-max(input)

  Mi_n<-min(input)

  Mea_n<-mean(input)
  
  results<-cbind(St_d,Ma_x,Mi_n,Mea_n)
  
  return (results)
}
  
#End

#Hydrologic Modelling
#Function to define evapotranspiration
#Timur Sabitov
#Instuctor: Chuck Kroll
#Date : 3/11/2017

#func

#Main variables
# x<- my input file for model
# y<- my input file to compare
# A<- my watershed area

# n<- dimentions of original data frame
# oury<- first year 
# y<- last day and year of original data
# ndate <- corresponds to new data frame with appropriate dates
# ERR <- changes errors in data with condition to NA
# ERR1 <- changes errors in data with condition to NA
# ERR2 <- changes errors in data with condition to NA
# DF_new <- new data frame with NA

# dif<- number of years in data frame
# N <- average number of daylight hours in a month
# DF_new_final$Q<- estimated discharge in cfs from our model

# DF_new_final <- final data frame without NA's, with all results
# BIAS_1 <- Bias of average yearly estimated values
# BIAS_2 <- Bias of average monthly estimated values
# NASH1<- Nash-Sutcliff efficiency for average yearly values
# NASH2<- Nash-Sutcliff efficiency for average monthly values


ET_0<-function (x,y, A){
  
  #define dimentions of data frame
  
  n<-dim(x)
  
  #change missing values to NA
  
  is.na(x)<-x== -9999
  is.na(x)<-x== c("-9999")
  
  
  x$DATE<-as.character(x$DATE)
  
  
  x$Date<-as.POSIXlt(x$DATE, format="%Y%m%d")
  
  #create table with days
  
  x$Day<-x$Date$mday
  
  #create table with months
  
  x$Month<-x$Date$mon+1
  
  #create table with years
  
  x$Year<-x$Date$year+1900
  
  #convert as Date
  
  x$Date<-as.Date(as.POSIXlt(x$DATE, format="%Y%m%d"))
  
  #Define first year 
  
  oury<-x$Date[1]
  
  d<-data.frame(date <- seq(as.Date(oury), length = n[1]+365, by = "day"))
  
  
  #Define row that corresponds for the last day and year of the origin data
  
  y1<-which (d$date....seq.as.Date.oury...length...n.1....365..by....day.. == max(x$Date))
  
  #Cut new data to and preserve last first and last day of the origin data with
  #appropriate number of days and years
  
  ndate<-d$date....seq.as.Date.oury...length...n.1....365..by....day..[1:y1]
  
  
  d<-data.frame(ndate)
  
  names (d)<- c("Date")
  
  #merge original frame with new data.frame
  
  ne<-merge(x,d,by = c("Date"),all = T)
  
  
  #create new data frame 
  
  DF_new<-ne
  
  #Define erros
  
  ERR<-which (DF_new$TMAX<DF_new$TMIN)
  
  ERR2<- which (DF_new$SNOw>DF_new$PRCP)
  
  ERR3<- which (DF_new$PRCP<0)
  
  DF_new$TMAX[ERR]<-NA
  DF_new$SNOW[ERR2]<-NA
  DF_new$PRCP[ERR3]<-NA
  
  DF_new_final<-cbind (DF_new[1], DF_new[which( colnames(DF_new)=="PRCP" )],DF_new[which( colnames(DF_new)=="TMAX")],DF_new[which( colnames(DF_new)=="TMIN")])
  
  library (zoo)
  
  n<-dim(DF_new_final)
  
  DF_new_final[2:n[2]]<-round(na.approx(DF_new_final[2:n[2]]),2)
  
  x<-DF_new_final
  
  #convert format of dates
  x$Posixlt<-as.POSIXlt(x$Date, format="%m/%d/%Y")
  
  #create table with days
  x$Day<-x$Posixlt$mday
  
  #create table with months
  x$Month<-x$Posixlt$mon+1
  
  #create table with years
  x$Year<-x$Posixlt$year+1900
  
  #create table for water years
  x$WaterYear<-0
  
  #define dimentions of data frame
  n<-dim(x)
  
  #properly describe variables, include iterations as interations in variables
  #intermediate variables that will be used in future compt
  
  #LOOP 1 - assign water years for the data frame
  
  for (i in seq(1,n[1])){
    
    if (x$Month[i]>=10) {x$WaterYear[i]=(x$Year+1)[i]} else {x$WaterYear[i]=x$Year[i]}
    
  }
  
  
  #number of daylight hours per day in each month (beginning from January)
  
  N<-rbind(9.3,10.4,11.7,13.1,14.3,15.0,14.6,13.6,12.3,10.9,9.7,9.0)
  
  #Loop matrix multiplication of N
  #define monthly averages for 20 years
  
  dif<-max(x$Year)-min(x$Year)
  
  mon_count<-matrix(0,12,dif)
  
  n2<-dim (mon_count)
  
  for (k in seq (1,n2[2])){
    mon_count[,k]<-N
  }
  
  #create data frame
  
  mon_count<- as.data.frame(cbind(seq(1,12),mon_count))
  
  #Define average T in C*
  
  x$T<-(((x$TMAX+x$TMIN)/2)-32)/1.8
  
  #Populate DF for each month with number of daylight in the month
  
  x$N<-0
  
  for (i in seq (1,n[1])){
    for (k in seq (1,12)){
      
      if (x$Month[i]==mon_count$V1[k])
      {x$N[i]<-mon_count$V2[k]} else { }
    }
  }
  
  #Define Est
  #Est= 6.108 exp(17.27 T/(237.3 + T))
  
  x$Est <- 6.108*exp(17.27*x$T/(237.3 + x$T))
  
  #Populate my E
  
  x$E<-0
  
  #check which row has negative temperatures
  
  which (x$T<0)
  
  
  for (i in seq (1,n[1])){
    
    if (x$T[i]<0) 
    {
      
      x$E[i]=0
      
    } else {  #E = 0.021 N^2*E/(T + 273)
      
      x$E[i]= 0.021* ((x$N)^2)[i]* x$Est[i]/(x$T + 273)[i]
      
    }
    
  }
  
  #Estimated stream flow from the model
  
  #We need to convert our potential evapotranspiration to cfs.
  #Evapotranspiration is given in cm/d.
  
  #1. Convert it into km, where 1 km = 10^5 = 1000*100
  #2. Obtain volume per area loading, which is in km^3/d
  #3. Obtain volume per area loading per second #2/86400
  #4. Convert km^3 into m^3 where 1 km^3 = 10^9 m3, as a result we have m3s
  #5. Multiply by coefficient to convert into cfs
  
  x$E_0<-(x$E/(1000*100)*A)/86400*(10^9)*35.3147
  
  #Convert precipitation to cfs
  #1. Convert inches to cm
  #2. Obtain volume per area loading, in km^3/d
  #4. Convert km^3 into m^3 where 1 km^3 = 10^9 m3, as a result we have m3s
  #5. Multiply by coefficient to convert into cfs
  
  x$P<-(x$PRCP*2.54/(1000*100)*A)/86400*(10^9)*35.3147
  
  #Get final values for our flow
  
  x$Q<-x$P-x$E_0
  
  #convert format of dates
  y$Posixlt<-as.POSIXlt(y$Date, format="%m/%d/%Y")
  
  #create table with days
  y$Day<-y$Posixlt$mday
  
  #create table with months
  y$Month<-y$Posixlt$mon+1
  
  #create table with years
  y$Year<-y$Posixlt$year+1900
  
  #create table for water years
  y$WaterYear<-0
  
  #define dimentions of data frame
  n<-dim(x)
  
  #properly describe variables, include iterations as interations in variables
  #intermediate variables that will be used in future compt
  
  #LOOP 1 - assign water years for the data frame
  
  for (i in seq(1,n[1])){
    
    if (y$Month[i]>=10) {y$WaterYear[i]=(y$Year+1)[i]} else {y$WaterYear[i]=y$Year[i]}
    
  }
  
  #Define means for model and means for input
  
  m_av20_model<-round((aggregate(x = x$Q, by = list(x$Month,x$WaterYear), FUN = mean)),2)
  
  
  #y - input of original data for comparisom
  
  m_av20<-round((aggregate(x = y$Flow, by = list(y$Month,x$WaterYear), FUN = mean)),2)
  
  
  #Define means for model and means for input
  
  y_av20_model<-round((aggregate(x = x$Q, by = list(x$WaterYear), FUN = mean)),2)
  
  #y - input of original data for comparisom
  
  y_av20<-round((aggregate(x = y$Flow, by = list(y$WaterYear), FUN = mean)),2)
  
  
  #Bias
  
  BIAS_1<-sum(y_av20_model$x-y_av20$x)/dif
  
  BIAS_2<-sum(m_av20_model$x-m_av20$x)/(dif*12)
  
  #Nash Sutcliffe Efficiency
  
  z1<-mean(y_av20$x)
  z2<-mean(m_av20$x)
  
  NASH_1<-1-sum((y_av20_model$x-y_av20$x)^2)/sum((y_av20$x-z1)^2)
  
  NASH_2<-1-sum((m_av20_model$x-m_av20$x)^2)/sum((m_av20$x-z2)^2)
  
  print ("Bias yearly averages")
  print (BIAS_1)#Bias yearly averages
  
  print ("Bias monthly averages")
  print (BIAS_2)
  
  print ("NSE yearly averages")
  print (NASH_1)
  
  print ("NSE monthly averages")
  print (NASH_2)
  
}


#Compute Nash-Sutcliff efficiency

#x- modeled
#y- measured

Nash<-function (mod,obs){
  N_S<-1-sum((mod-obs)^2)/sum((obs-mean(obs))^2)
  return (N_S)
}

#compute Bias
Bias<-function (mod,obs,N){
  Bias<-sum(mod-obs)/N
  return (Bias)
}


#Preproc
preproc<-function (x){
  
  #define dimentions of data frame
  
  n<-dim(x)
  
  #change missing values to NA
  
  is.na(x)<-x== -9999
  is.na(x)<-x== c("-9999")
  
  
  x$DATE<-as.character(x$DATE)
  
  x$Date<-as.POSIXlt(x$DATE, format="%Y%m%d")
  
  #create table with days
  
  x$Day<-x$Date$mday
  
  #create table with months
  
  x$Month<-x$Date$mon+1
  
  #create table with years
  
  x$Year<-x$Date$year+1900
  
  #convert as Date
  
  x$Date<-as.Date(as.POSIXlt(x$DATE, format="%Y%m%d"))
  
  #Define first year 
  
  oury<-x$Date[1]
  
  d<-data.frame(date <- seq(as.Date(oury), length = n[1]+365, by = "day"))
  #create table for water years
  x$WaterYear<-0
  
  #define dimentions of data frame
  n<-dim(x)
  
  #properly describe variables, include iterations as interations in variables
  #intermediate variables that will be used in future compt
  
  #LOOP 1 - assign water years for the data frame
  
  for (i in seq(1,n[1])){
    
    if (x$Month[i]>=10) {x$WaterYear[i]=(x$Year+1)[i]} else {x$WaterYear[i]=x$Year[i]}
    
  }
  
  #Define row that corresponds for the last day and year of the origin data
  
  y1<-which (d$date....seq.as.Date.oury...length...n.1....365..by....day.. == max(x$Date))
  
  #Cut new data to and preserve last first and last day of the origin data with
  #appropriate number of days and years
  
  ndate<-d$date....seq.as.Date.oury...length...n.1....365..by....day..[1:y1]
  
  d<-data.frame(ndate)
  names (d)<- c("Date")
  
  #merge original frame with new data.frame
  ne<-merge(x,d,by = c("Date"),all = T)
  
  #create new data frame 
  DF_new<-ne
  DF_new_final<-cbind (DF_new[1],DF_new[which( colnames(DF_new)=="Month")],DF_new[which( colnames(DF_new)=="Tavg")], DF_new[which( colnames(DF_new)=="Day" )], DF_new[which( colnames(DF_new)=="PRCP" )],DF_new[which( colnames(DF_new)=="TMAX")],DF_new[which( colnames(DF_new)=="TMIN")], DF_new[which(colnames(DF_new) == "WaterYear")],DF_new[which(colnames(DF_new) == "Year")])
  
  return (DF_new_final)
}


Evp<-function (x, A){
    
    #define dimentions of data frame
    
    n<-dim(x)
    
    #number of daylight hours per day in each month (beginning from January)
    
    N<-rbind(9.3,10.4,11.7,13.1,14.3,15.0,14.6,13.6,12.3,10.9,9.7,9.0)
    
    #Loop matrix multiplication of N
    #define monthly averages for 20 years
    
    dif<-max(x$Year)-min(x$Year)
    
    mon_count<-matrix(0,12,dif)
    
    n2<-dim (mon_count)
    
    for (k in seq (1,n2[2])){
      mon_count[,k]<-N
    }
    
    #create data frame
    
    mon_count<- as.data.frame(cbind(seq(1,12),mon_count))
    
    
    #Define average T in C*
    x$T<-(((x$TMAX+x$TMIN)/2)-32)/1.8
    
    #Populate DF for each month with number of daylight in the month
    
    x$N<-0
    
    for (i in seq (1,n[1])){
      for (k in seq (1,12)){
        
        if (x$Month[i]==mon_count$V1[k])
        {x$N[i]<-mon_count$V2[k]} else { }
      }
    }
    
    #Define Est
    #Est= 6.108 exp(17.27 T/(237.3 + T))
    
    x$Est <- 6.108*exp(17.27*x$T/(237.3 + x$T))
    
    #Populate my E
    
    x$E<-0
    
    #check which row has negative temperatures
    
    which (x$T<0)
    
    
    for (i in seq (1,n[1])){
      
      if (x$T[i]<0) 
      {
        
        x$E[i]=0
        
      } else {  #E = 0.021 N^2*E/(T + 273)
        
        x$E[i]= 0.021* ((x$N)^2)[i]* x$Est[i]/(x$T + 273)[i]
        
      }
      
    }
    
    #Estimated stream flow from the model
    
    #We need to convert our potential evapotranspiration to cfs.
    #Evapotranspiration is given in cm/d.
    
    #1. Convert it into km, where 1 km = 10^5 = 1000*100
    #2. Obtain volume per area loading, which is in km^3/d
    #3. Obtain volume per area loading per second #2/86400
    #4. Convert km^3 into m^3 where 1 km^3 = 10^9 m3, as a result we have m3s
    #5. Multiply by coefficient to convert into cfs
    
    #x$E_0<-(x$E/(1000*100)*A)/86400*(10^9)*35.3147
    
    #Convert precipitation to cfs
    #1. Convert inches to cm
    #2. Obtain volume per area loading, in km^3/d
    #4. Convert km^3 into m^3 where 1 km^3 = 10^9 m3, as a result we have m3s
    #5. Multiply by coefficient to convert into cfs
    
    #x$P<-(x$PRCP*2.54/(1000*100)*A)/86400*(10^9) #*35.3147 - use to convert into cfs)
    
    #Get final values for our flow
    
    x$E
    
    return (x$E)
    
}


#Preproc if only data and flow given
preproc_d_f<-function (y){

  #define dimentions of data frame
  n<-dim(y)
  
  y$DATE<-as.character(y$Date)
  
  #convert format of dates
  y$Posixlt<-as.POSIXlt(y$Date, format="%m/%d/%Y")
  
  #create table with days
  y$Day<-y$Posixlt$mday
  
  #create table with months
  y$Month<-y$Posixlt$mon+1
  
  #create table with years
  y$Year<-y$Posixlt$year+1900
  
  #create table for water years
  y$WaterYear<-0
  
  
  y$Date<-as.Date(as.POSIXlt(y$DATE, format="%Y%m%d"))
  
  #define dimentions of data frame
  n<-dim(y)
  
  #Define first year 
  
  f_year<-y$Year[1]
  
  d<-data.frame(date <- seq(as.Date(f_year), length = n[1]+365, by = "day"))

  
  #define dimentions of data frame
  n<-dim(y)
  
  #properly describe variables, include iterations as interations in variables
  #intermediate variables that will be used in future compt
  
  #LOOP 1 - assign water years for the data frame
  
  for (i in seq(1,n[1])){
    
    if (y$Month[i]>=10) {y$WaterYear[i]=(y$Year+1)[i]} else {y$WaterYear[i]=y$Year[i]}
    
  }
  
  return (y)
}




################Funcion to define SST

SST_1<-function (PET,ET,SDt,SS_T1,I,SRt,n,SS_T){

for (c in seq (1,n[1])){
  
  #loop through potential evapotranspiration and if it more than initial storage it is equal to sst else to potential evapotranspiration
  if (PET[c]>SS_T[c]) {ET[c]=SS_T[c]} else {ET[c]=PET[c]}
  
  SDt[c]<-(1-K_B)*(SS_T[c]-ET[c])
  
  SS_T1[c]<-SS_T[c]+I[c]-ET[c]-SDt[c]
  
  SS_T[c+1]<-SS_T1[c]
}
 
  Q<-SRt+SDt
  
  res<-Q/(1000*100)*A/86400*10^9*35.3146667
  
  return (res)
}



