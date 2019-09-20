##################
##################
##################
#clean up the data
rm(list=ls(all=T))
#Watershed area same as in HW - 7 
A<-2540
K=0.45

#read in file 
x<- read.csv(file = "Met.csv")
y<- read.csv (file = "flow.csv")

y<-preproc_d_f(y)
y_dim<-dim(y)
y<-y[which(y$WaterYear==min(Data$WaterYear))[1]:y_dim[1],]
y$Flow_cms<-y$Flow*0.0283

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

#Field capacity of bucket is at 10 mm up to 10cm
#FCAP=0.01#3.411

#Base_flow const
KB0<-0
FCAP0<-0
B0<-15
N0<--20
R0<-0
SAT0<-0
h<-6.3


#Saturated zone
while (h<6.5){
  SAT<-h
  print (h)
  h<-h+0.1
  p<-0
  while (p<1){
    #Field capacity of bucket is at 10 mm up to 10cm
    FCAP=p
    p=p+0.3
    j=0.97
    while (j<1){
      #Base_flow const
      K_B=j
      #j=j+0.001
      j=j+0.001
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
      
      B<-Bias (Data$Q,Data$Q_obs,length (Data$Q))
      N<- Nash (Data$Q ,Data$Q_obs)
      
      R<-cor (Data$Q,Data$Q_obs)
      print (R)
      
      if (((abs(B)<abs(B0))) && (N>N0))
      {FCAP0<-FCAP; B0<-B; N0<-N; KB0<-K_B; R0<-R; SAT0<-SAT}
      
      #plot (Data$Q)
      #lines (Data$Q_obs)
    }
  }
}

monthly_mod<-(aggregate(x = Data$Q, by = list(Data$Month, Data$WaterYear), FUN = mean))
monthly_measured<-(aggregate(x = Data$Q_obs, by = list(Data$Month, Data$WaterYear), FUN = mean))

plot (monthly_measured$x)
lines (monthly_mod$x)





