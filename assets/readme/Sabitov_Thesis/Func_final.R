#Function to define overall statistics from HW-1,2,3
#Sabitov Timur


#Variables

#End

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


#Preproc
preproc<-function (x){
  
  #define dimentions of data frame
  n<-dim(x)
  
  #change missing values to NA
  is.na(x)<-x== -9999
  is.na(x)<-x== c("-9999")
  
  x$DATE<-as.character(x$DATE)
  
  x$Date<-as.POSIXlt(x$DATE, format="%m/%d/%Y")
  
  #create table with days
  x$Day<-x$Date$mday
  
  #create table with months
  x$Month<-x$Date$mon+1
  
  #create table with years
  x$Year<-x$Date$year+1900
  
  #convert as Date
  x$Date<-as.Date(as.POSIXlt(x$DATE, format="%m/%d/%Y"))
  
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
  loc_m<-which (x$Month>=10)
  x$WaterYear<-x$Year
  x$WaterYear[loc_m]<-x$Year[loc_m]+1
  
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
  DF_new_final<-cbind (DF_new[1],DF_new[which( colnames(DF_new)=="Month")], DF_new[which( colnames(DF_new)=="Day" )], DF_new[which( colnames(DF_new)=="PRCP" )],DF_new[which( colnames(DF_new)=="TMAX")],DF_new[which( colnames(DF_new)=="TMIN")], DF_new[which(colnames(DF_new) == "WaterYear")],DF_new[which(colnames(DF_new) == "Year")])
  
  return (DF_new_final)
}


Evp<-function (x, tmax, tmin){
    
    #define dimentions of data frame
    n<-dim(x)
    x$TMAX<-tmax
    x$TMIN<-tmin
    #number of daylight hours per day in each month (beginning from January)
    N<-c(9.3,10.4,11.7,13.1,14.3,15.0,14.6,13.6,12.3,10.9,9.7,9.0)
    Month<-c(1,2,3,4,5,6,7,8,9,10,11,12)
    
    #assign coefficient N to data frame of our values
    Coef<-data.frame(cbind (N,Month))
    
    Co<-merge(x,Coef, (by = "Month"), all = T)
    Co<-Co[with(Co,order(Co$Date)),]
    
    x$N<-Co$N

    #Define average T in C*
    x$T<-(((x$TMAX+x$TMIN)/2)-32)/1.8

    #Define Est
    #Est= 6.108 exp(17.27 T/(237.3 + T))
    
    x$Est <- 6.108*exp(17.27*x$T/(237.3 + x$T))
    
    #Populate my E
    x$E<-0
    
    #check which row has negative temperatures
    
    loc_f<-which (x$T<0)
    
    x$E<-0.021* ((x$N)^2)* x$Est/(x$T + 273)
    x$E[loc_f]<-0
    
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
  if (y$Posixlt$year[1]<0) {y$Year<-y$Posixlt$year+3800} else {y$Year<-y$Posixlt$year+1900}
  
  
  #create table for water years
  y$WaterYear<-0
  
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
  
  loc_m<-which (y$Month>=10)
  y$WaterYear<-y$Year
  y$WaterYear[loc_m]<-y$Year[loc_m]+1
  
  return (y)
}


###########################################################################
count <- function(x, n){ length((which(x == n))) }

Q710<-function(x){

input<-x

#get the difference between maximum and minimum water year
dif<-((max(input$WaterYear)-min(input$WaterYear))+1)

count <- function(x, n){ length((which(x == n))) }

#add count function inside of another function !!! mind...blo...g
#future data variable where counting of days in each year will be saved
c<-NA

#our 7 day intervals inside of loop
p<-NA
d<-NA

n<-dim(input)

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

if (dif == 1){
for (k in seq(2,dif)){
  
  flow[[k]]<-input$Flow
  
  }
}
else 
{for (k in seq(2,dif)){
  
  flow[[k]]<-input$Flow[(b[k-1]+1):b[k]]
  
}
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
return (Q7min)
}




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


#Compute Nash-Sutcliff efficiency
Nash<-function (mod,obs){
  N_S<-1-sum((mod-obs)^2)/sum((obs-mean(obs))^2)
  return (N_S)
}

#compute Bias
Bias<-function (mod,obs){
  Bias<-sum(mod-obs)/length (mod)
  return (Bias)
}