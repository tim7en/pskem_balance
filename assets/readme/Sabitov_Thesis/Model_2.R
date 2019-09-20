#clean up the data
rm(list=ls(all=T))
#Watershed area same as in HW - 7 
A<-2540
G_area<-91
K=0.45
#Saturated zone
#load function
source ("Func_final.R")
#read in file 
x<- read.csv(file = "Met.csv")
y<- read.csv (file = "flow.csv")


#read in the data
Data<-preproc (x)

y<-preproc_d_f(y)
y_dim<-dim(y)
y<-y[which(y$WaterYear==min(Data$WaterYear))[1]:y_dim[1],]
y$Flow_cms<-y$Flow*0.0283 #cms

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

Data$Lapse<-0
#Adjusted lapse rates
Data$Lapse[which(Data$Month==3)]<-6.7
Data$Lapse[which(Data$Month==4)]<-6.7
Data$Lapse[which(Data$Month==5)]<-6.7
Data$Lapse[which(Data$Month==6)]<-6.5
Data$Lapse[which(Data$Month==7)]<-6.5
Data$Lapse[which(Data$Month==8)]<-6.5
Data$Lapse[which(Data$Month==9)]<-6
Data$Lapse[which(Data$Month==10)]<-6
Data$Lapse[which(Data$Month==11)]<-6
Data$Lapse[which(Data$Month==12)]<-6
Data$Lapse[which(Data$Month==1)]<-6
Data$Lapse[which(Data$Month==2)]<-6

#Watershed Coefficients
WC_1<-0.1
WC_2<-0.8
WC_3<-0.1




#Convert temp into C
Data$TMAX_gl<-(Data$TMAX-32)*5/9
Data$TMIN_gl<-(Data$TMIN-32)*5/9
#Define avg temperature
Data$T_avg_gl<-(Data$TMAX_gl+Data$TMIN_gl)/2
Data$Glac_Tavg<-Data$T_avg_gl-(3680-1251)/1000*Data$Lapse
#define Ab
Data$Ab<-(9.5+Data$Glac_Tavg)^3
#glac discharge
Data$Glac_Q<-1/(31.5*10^3)*Data$Ab*G_area #cms
Data$Glac_Q_adj<-0
Data$Glac_Q_adj[which(Data$Glac_Q>0)]<-Data$Glac_Q[which(Data$Glac_Q>0)]

# #Convert temp into C
# Data$TMAX<-(Data$TMAX-32)*5/9-(2770-1251)/1000*6.5
# Data$TMIN<-(Data$TMIN-32)*5/9-(2770-1251)/1000*6.5
# 
# #Define avg temperature and convert into C
# Data$T_avg<-((Data$TMAX+Data$TMIN)/2)

Data$T_avg<-((Data$TMAX-32)*5/9+(Data$TMIN-32)*5/9)/2

#convert precip from inch to cm
Data$PRCP<-(Data$PRCP*2.54)

Data$Snow<-0
Data$Rain<-0
Data$WT_C<-0

#Saturation layer
R_0<-0
KB_0<-0
FCAP_0<-0
#Define initial bias as -30 and initial NSE -15
B0<--60
N0<--15
SAT_0<-0 #Saturation layer
p<-0 #Loop counter for the Field Capacity Storage
j<-0 #Loop counter for the K_B, base flow constant

    Data$Snow_top<-0
    Data$Snow_mid<-0
    Data$Snow_low<-0
    Data$Snow_low<-0
    
    Data$SN_top<-0
    Data$SN_mid<-0
    Data$SN_low<-0
    
    Data$T_top<-0
    Data$T_mid<-0
    Data$T_low<-0
    Data$T_low<-0
    #Storage
    Data$UNSAT<-0
    #Saturated storage is on 0
    Data$Sat<-0
    #Surface runoff
    Data$SRn<-0
    Data$PERC<-0
    #If temp < 0, rain turns into snow
    Data$Mt_top<-0
    Data$Mt_mid<-0
    Data$Mt_low<-0

    #Tlower will be average between ((2300-1251)/2+1251)/1000*6.5
    #Tmid will be average T at this elevation Data$T_low-2800/1000*6.5
    #Ttop will be average T at this elevation
    Data$T_low<-Data$T_avg-((2300-1251)/2)/1000*Data$Lapse
    Data$T_mid<-Data$T_avg-(((3300-2300)/2+2300)-1251)/1000*Data$Lapse
    Data$T_top<-Data$T_avg-(((4300-3300)/2+3300)-1251)/1000*Data$Lapse
    
    #Define potential evapotranspiration using Hamon's method, where input TMAX and TMIN is in Kelvin!
    T_low<-Data$T_low*1.8+32
    T_mid<-Data$T_mid*1.8+32
    T_top<-Data$T_top*1.8+32
    
    Data$PET_low<-Evp(Data,T_low,T_low)
    Data$PET_mid<-Evp (Data,T_mid,T_mid)
    Data$PET_top<-Evp (Data,T_top,T_top)
    
    #Create dataframe to store crop koefficients
    KU<-data.frame(c(0.78,0.82,0.82,0.79,0.89,0.91,0.93,0.98,1.03,0.97,0.72,0.61))
    names (KU)<-NULL
    names (KU)<-("C_Koef")
    KU$Month<-seq(1,12)
    d<-dim(KU)
    Data$C_Koef<-0 #create column to store coefficients
    KK<-merge (Data,KU,(by = "Month"),all=F) #Merge into another df two data frames to assign KK
    KK<-KK[with(KK,order(KK$Date)),]
    Data$C_Koef<-KK$C_Koef.y

    for (i in seq (1,n[1]-1)){
      if (Data$T_top[i]<=0) {Data$SN_top[i+1]<-WC_1*Data$PRCP[i];Data$Snow_top[i]<-WC_1*Data$PRCP[i]}
      else {Data$Mt_top[i]<-min(Data$SN_top[i],Data$T_top[i]*K); Data$SN_top[i+1]<-Data$SN_top[i]-Data$Mt_top[i];}
    }


    for (i in seq (1,n[1]-1)){
      if ((Data$T_mid[i]<=0)) {Data$SN_mid[i+1]=Data$SN_mid[i]+WC_2*Data$PRCP[i];Data$Snow_mid[i]<-WC_2*Data$PRCP[i]}
      else {Data$Mt_mid[i]<-min (Data$SN_mid[i],Data$T_mid[i]*K); Data$SN_mid[i+1]<-Data$SN_mid[i]-Data$Mt_mid[i]; }
    }


    for (i in seq (1,n[1]-1)){
      if ((Data$T_low[i]<=0)) {Data$SN_low[i+1]<-WC_3*Data$PRCP[i];Data$Snow_low[i]<-WC_3*Data$PRCP[i]}
      else {Data$Mt_low[i]<-min (Data$SN_low[i],Data$T_low[i]*K); Data$SN_low[i+1]<-Data$SN_low[i]-Data$Mt_low[i];}
    }

    Data$Rain<-Data$PRCP-Data$Snow_low-Data$Snow_mid-Data$Snow_top
    Data$Rain[which(Data$Rain<0)]<-0
    Data$SRn<-Data$Mt_mid+Data$Mt_top+Data$Mt_low+Data$Rain
    
    
    UNSAT<-0.93
    Data$UNSAT[1]<-UNSAT #Soil saturation on day 1
    Data$PE_top<-0 #create column for adjusted potential evapotranspiration
    Data$PE_mid<-0 #create column for adjusted potential evapotranspiration
    Data$PE_low<-0 #create column for adjusted potential evapotranspiration
    
    Data$PERC<-0 #create column for percolation
    
    #Loop define Evapotranspiration as function of UNSAT on day t and infiltration
    #Define UNSAT at the next day
    
    for (i in seq (1,n[1]-1)){
      
      Data$PE_top[i]<-min(Data$C_Koef[i]*Data$PET_top[i], Data$UNSAT[i]) #adjust evapotranspiration
      
      Data$UNSAT[i+1]<-Data$UNSAT[i]+Data$SRn[i]-Data$PE[i] #estimate UNSAT for day 2
      
      if (Data$UNSAT[i+1]<=FCAP) {Data$PERC[i]<-0} else 
      {Data$PERC[i]<-Data$UNSAT[i+1]-FCAP; Data$UNSAT[i+1]=FCAP}
    }
    
  
    #Data$SRn<-Data$SRn-Data$PET_low-Data$PET_mid-Data$PET_top
    #Data$PERC<-Data$SRn
    
    #Iterator for calibration
    f<-0
    #Find a field capacity of unsaturated zone;
    while (f<5){
    FCAP<-f
    f<-f+0.5
    for (i in seq (1,n[1]-1)){
      Data$UNSAT[i]<-Data$UNSAT[i]+Data$SRn[i]
      if (Data$UNSAT[i]<=FCAP){Data$UNSAT[i+1]=Data$UNSAT[i]}
      else {Data$PERC[i]=Data$UNSAT[i]-FCAP; Data$UNSAT[i]=FCAP} #It becomes runoff;
    }
    
    #define SDt
    Data$SDt<-0
    #Iterator for calibration
    h=4
    while (h<8){
      SAT<-h
      Data$Sat[1]<-SAT
      h<-h+0.5
      #print (h)
      j<-0.95
    while (j<1){
      #Base_flow const
      K_B=j
      j=j+0.01
    for (i in seq (1,n[1]-1)){
      #SDt
      Data$SDt[i]<-(1-K_B)*Data$Sat[i]
      #Sat
      Data$Sat[i+1]<-Data$Sat[i]+Data$PERC[i]-Data$SDt[i]
    }
    
    Data$Q<-(Data$SDt-(Data$PET_low+Data$PET_mid+Data$PET_top))/(1000*100)*A/86400*10^9+Data$Glac_Q_adj
    Data$Q_obs<-y$Flow_cms
    R=cor (Data$Q,Data$Q_obs)
    B<-Bias (Data$Q,Data$Q_obs,length (Data$Q))
    N<- Nash (Data$Q ,Data$Q_obs)

    if ((abs(B)<abs(B0))&& N>N0)
    {B0<-B; N0<-N;R_0<-R; SAT_0<-SAT; KB_0<-K_B;}
    }
  }
}

monthly_mod<-(aggregate(x = Data$Q, by = list(Data$Month, Data$WaterYear), FUN = mean))
monthly_measured<-(aggregate(x = Data$Q_obs, by = list(Data$Month, Data$WaterYear), FUN = mean))
plot (monthly_measured$x)
lines (monthly_mod$x)


