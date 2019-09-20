#Hydrologic Modelling
#Final exam
#Instructor (Chuck Kroll)
#Student: Timur Sabitov
#Date 5/3/2017

#Variables

#Data - main df

#monthly_mod<-modelled monthly average streamflow across entire period
#monthly_measured<-measured monthly average streamflow across entire period
#monthly_mod/measured_y1/last - modelled/measured average monthly streamflows for first/last year of observations

#Result - table of modelled average monthly evapotranspiration, precipitation, groundwater discharge, surface flow, stream flow
#results - table of measured Average flow, peak flow, Q710

#clean up the data
rm(list=ls(all=T))
R_0=0.1

#Watershed area same as in HW - 7 
A<-2540
K=0.45
p=3.4
R=0
FCAP_0=0
K_B=0.1
KB_0=0

#read in file 
x<- read.csv(file = "Met.csv")
y<- read.csv (file = "flow.csv")

#load function
source ("Func_final.R")

#read in the data
Data<-preproc (x)

#define errors
ERR<-which (Data$TMAX<Data$TMIN)
ERR3<- which (Data$PRCP<0)
Data$TMAX[ERR]<-NA
Data$PRCP[ERR3]<- NA

#define dimentions of the data
n<-dim(Data)

library (zoo)

Data[2:n[2]]<-na.approx(Data[2:n[2]])
f<-which(Data$Month==10,Data$Day==1)[1]
Data<-Data[f:n[1],]
n<-dim(Data)

#Convert temp into C
Data$TMAX<-(Data$TMAX-32)*5/9-(2770-1251)/1000*6.5
Data$TMIN<-(Data$TMIN-32)*5/9-(2770-1251)/1000*6.5

#Define avg temperature and convert into C
Data$T_avg<-((Data$TMAX+Data$TMIN)/2)

#convert precip from inch to cm
Data$PRCP<-(Data$PRCP*2.54)

y<-preproc_d_f(y)
y_dim<-dim(y)
y<-y[which(y$WaterYear==min(Data$WaterYear))[1]:y_dim[1],]
y$Flow_cms<-y$Flow*0.0283


while (p<3.419){
  
#Field capacity of bucket is at 10 mm up to 10cm
FCAP=p
p=p+0.001
print (FCAP)
j=0.98
while (j<1){
  #Base_flow const
  K_B=j
  #j=j+0.001
  j=j+0.001
  #print (K_B)
  #Saturated zone
  SAT=0.1
  #Column for snow
  Data$Snow<-0
  #Snow Melt
  Data$Mt<-0
  #Define Rain
  Data$Rain<-0
  #Define SnowPack
  Data$SN<-0
  #Storage
  Data$UNSAT=0
  #Saturated storage is on 0
  Data$Sat=0
  #Surface runoff
  Data$SRn=0
  Data$PERC=0
  #If temp < 0, rain turns into snow
  
  #define location where T_av<0
  loc_1<-which (Data$T_avg<=0)
  loc_2<-which (Data$T_avg>0)
  #Define snow
  Data$Snow[loc_1]<-Data$PRCP[loc_1]
  #Define rain
  Data$Rain[loc_2]<-Data$PRCP[loc_2]
  
  for (i in seq (1,n[1]-1)){
    if (Data$T_avg[i]<=0) {Data$SN[i+1]<-Data$SN[i]+Data$Snow[i]}
    else
    {Data$Mt[i]<-min(Data$SN[i],Data$T_avg[i]*K); Data$SN[i+1]<-Data$SN[i]-Data$Mt[i]}
    
    Data$UNSAT[i]=Data$UNSAT[i]+Data$Mt[i]+Data$Rain[i]
    
    if (Data$UNSAT[i]<=FCAP){Data$UNSAT[i+1]=Data$UNSAT[i]}
    else {Data$PERC[i]=Data$UNSAT[i]-FCAP; Data$UNSAT[i]=FCAP} #It becomes runoff;
  }
  
  
  #define sat conditions for the first day as 0.265
  Data$Sat[1]<-SAT
  
  #define SDt
  Data$SDt<-0
  
  for (i in seq (1,n[1]-1)){
    #SDt
    Data$SDt[i]<-(1-K_B)*Data$Sat[i]
    #Sat
    Data$Sat[i+1]<-Data$Sat[i]+Data$PERC[i]-Data$SDt[i]
  }
  
  Data$Q=(Data$SDt)/(1000*100)*A/86400*10^9
  

  
  Data$Q_obs=y$Flow_cms
  #plot (Data$Q)
  #lines (Data$Q_obs)
  R=cor (Data$Q,Data$Q_obs)
  
  if (abs(R)>abs(R_0)) {R_0 = R; FCAP_0=FCAP; KB_0 = K_B}
  }
}

