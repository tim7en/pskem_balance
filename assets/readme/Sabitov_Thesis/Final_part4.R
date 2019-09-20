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

#Watershed area same as in HW - 7 
A<-2540

#PARAMETERS
K_B<-0.989876534 #CN2<-65 NS 0.6561178 original input #changed from 0.924
FCAP<-12.42591 #Field capacity #changed to 0
AM5<-0 #Anticident moisture
SAT<-6.775272384 #Initial saturation zone
K<-0.45 #coefficient
CN_2<-36.05177556
UNSAT<-0.93585302 #changed to 0
G_area<-92 #area of glaciers

# #assume we have 100 km2 of glaciers with 5 meters of ice over 2540 km2 of area
# koef<-5/1000*100/2540*1000
# 

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

#Lapse rate
Data$Lapse<-0

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

Data$Lapse<-6.5

Data$Glac_Tavg<-(((Data$TMIN-32)*5/9-(3680-1251)/1000*Data$Lapse)+((Data$TMAX-32)*5/9-(3680-1251)/1000*Data$Lapse))/2
Data$TMAX<-(Data$TMAX-32)*5/9-(2770-1251)/1000*Data$Lapse
Data$TMIN<-(Data$TMIN-32)*5/9-(2770-1251)/1000*Data$Lapse


#define Ab
Data$Ab<-(9.5+Data$Glac_Tavg)^3

#glac discharge
Data$Glac_Q<-1/(31.5*10^3)*Data$Ab*G_area*35.3146662127

Data$Glac_Q_adj<-0
Data$Glac_Q_adj[which(Data$Glac_Q>0)]<-Data$Glac_Q[which(Data$Glac_Q>0)]
#if glac is <0 then it ads up as snow

write.csv (Data, file = "input.csv")
#Estimate potential ET
Data$PET<-Evp(Data)

#Define avg temperature and convert into C
Data$T_avg<-((Data$TMAX+Data$TMIN)/2)

#convert precip from inch to cm
Data$PRCP<-(Data$PRCP*2.54)

#Column for snow
Data$Snow<-0

#Melt
Data$Mt<-0

#Define Rn
Data$Rn<-0

#Define SN
Data$SN<-0

#################################################### INITIAL CONDITIONS ON DAY 1 ########################################
#Define initial conditions
Data$SN[1]<-0

#define accumulation of amount of snow at day t+1; #define melt

#define location where T_av<0
loc_1<-which (Data$T_avg<=0)

#Define snow
Data$Snow[loc_1]<-Data$PRCP[loc_1]

for (i in seq (1,n[1]-1)){
  if (Data$T_avg[i]<=0) {Data$SN[i+1]<-Data$SN[i]+Data$Snow[i]}
  else
  {Data$Mt[i]<-min(Data$SN[i],Data$T_avg[i]*K); Data$SN[i+1]<-Data$SN[i]-Data$Mt[i]}
}

k<-max(n[1])

if (Data$T_avg[k]<=0) {Data$Snow[k]<-Data$PRCP[k]} else {Data$Mt[k]<-min(Data$SN[k],Data$T_avg[k]*K)}

##################################################################################################################

#Define rain
Data$Rn<-Data$PRCP-Data$Sn

#Define AM5
Data$AM5<-0

#counter
j<-1
#set k to  1
k<-1

#define AM5 Melt
#for the first 5 days
for (k in seq (1,5)){
  Data$AM5[k+1]<-sum(Data$Mt[1:j]+Data$Rn[1:j])
  j<-j+1
}

#for the entire set
for (i in seq (j, n[1])){
  Data$AM5[i]<-sum(Data$Mt[(i-5):(i-1)]+Data$Rn[(i-5):(i-1)])
}

Data$AM5_comp<-0

#Define CN_1
CN_1<-CN_2/(2.334-0.01334*CN_2)
#Define CN
CN_3<-CN_2/(0.4036+0.0059*CN_2)

#my boundaries
AM1_D<-1.3
AM2_D<-2.8

#my boundaries
AM1_G<-3.6
AM2_G<-5.3

#define growing season or dormant season
Data$Season<-0

loc_2<-which (Data$Month<11 & Data$Month>4)
Data$Season[loc_2]<-Data$Season[loc_2]+1

#main curve number
Data$CN<-0

loc_3<-which (Data$Season==0 & Data$AM5>AM2_D)
Data$CN[loc_3]<-CN_3

loc_4<-which (Data$Season==0 & Data$AM5>AM1_D & Data$AM5<AM2_D)
Data$CN[loc_4]<-(CN_2+((CN_3-CN_2)/(AM2_D-AM1_D))*(Data$AM5[loc_4]-AM1_D))

loc_5<-which (Data$Season==0 & AM1_D>Data$AM5)
Data$CN[loc_5]<-(CN_1+((CN_2-CN_1)/(AM1_D-0))*Data$AM5[loc_5])

loc_6<-which (Data$Season==1 & Data$AM5>AM2_G)
Data$CN[loc_6]<-CN_3

loc_7<-which (Data$Season==1 & Data$AM5>AM1_G & Data$AM5<AM2_G)
Data$CN[loc_7]<-(CN_2+((CN_3-CN_2)/(AM2_G-AM1_G))*(Data$AM5[loc_7]-AM1_G))

loc_8<-which (Data$Season==1 & AM1_G>Data$AM5)
Data$CN[loc_8]<-(CN_1+((CN_2-CN_1)/(AM1_G-0))*Data$AM5[loc_8])


#use which outside of loop
loc_0<-which (Data$Mt>0)

#else use loc_0 to assign CN_3
#CN for snowmelt
Data$CN[loc_0]<-CN_3

#S in Si units (Potential retention)
Data$S<-(2540/Data$CN)-25.4

#numerator (P-0.2S)^2
Data$num<-0

#Estimate numerator for surface runoff
loc_9<-which (Data$Rn+Data$Mt<.2*Data$S)
Data$num<-Data$Rn+Data$Mt-0.2*Data$S
Data$num[loc_9]<-0

#Define denum to plug into equation for surface runoff
Data$denum<-Data$Rn+Data$Mt+0.8*Data$S

#Define surface runoff
Data$SRt<-Data$num/Data$denum

#Define infiltration
Data$I<-Data$Rn+Data$Mt-Data$SRt

# #Thesis research part
# Data$Q<-(Data$SRt+Data$I)/(10*100)*(2540*10^6)/86400
# 
# Data$EVP<-Data$PET/(10*100)*(2540*10^6)/86400
# 
# Data$Q<-Data$Q-Data$EVP
# 
# plot (Data$Q~Data$Date, pch = NA_integer_)
# lines (Data$Q~Data$Date, col = "blue")



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

Data$UNSAT<-0 #Create column for unsaturated storage

Data$UNSAT[1]<-UNSAT #Soil saturation on day 1

Data$PE<-0 #create column for adjusted potential evapotranspiration

Data$PERC<-0 #create column for percolation

#Loop define Evapotranspiration as function of UNSAT on day t and infiltration
#Define UNSAT at the next day


for (i in seq (1,n[1]-1)){
  
  Data$PE[i]<-min(Data$C_Koef[i]*Data$PET[i], Data$UNSAT[i]) #adjust evapotranspiration
  
  Data$UNSAT[i+1]<-Data$UNSAT[i]+Data$I[i]-Data$PE[i] #estimate UNSAT for day 2
  
  if (Data$UNSAT[i+1]<=FCAP) {Data$PERC[i]<-0} else 
  {Data$PERC[i]<-Data$UNSAT[i+1]-FCAP; Data$UNSAT[i+1]=FCAP}
}

#Adjust evapotranspiration for the last part
k<-max(n[1])
Data$PE[k]<-min(Data$C_Koef[k]*Data$PET[k],Data$UNSAT[k])

#################################################################################################################################################################################
################################################################################################################################################################################

#define column Saturated as 0
Data$Sat<-0

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

#Define SDt
Data$SDt[k]<-(1-K_B)*Data$Sat[k]


Data$Q<-0

#Discharge (cm)
Data$Q<-(Data$SRt+Data$SDt)
Data$Q_cm<-(Data$SRt+Data$SDt)

# Convert into cfs
Data$Q<-Data$Q/(1000*100)*A/86400*10^9*35.3146667+Data$Glac_Q_adj


#################################################################################################################################################################################
##########################################################################################################################


y<-preproc_d_f(y)

y_dim<-dim(y)

y<-y[which(y$WaterYear==min(Data$WaterYear))[1]:y_dim[1],]


cor (Data$Q, y$Flow)
#Define evaporation
#Estimate daily evapotranspiration in cm/d
Data$EVP<-(((y$Flow-Data$Q)*0.028316846592)/(A*10^6))*100*86400+Data$PET

# Remove all evaporation values below 0 if we consider only actual evapotranspiration ?
# Data$EVP[which(Data$EVP<0)]<-0

#########################################################################################################################

Q710_mod<-data.frame(Q710(y))

Peak<-(aggregate(x = y$Flow, by = list(y$WaterYear), FUN = max))
Average<-(aggregate(x = y$Flow, by = list(y$WaterYear), FUN = mean))
results<-data.frame(cbind(Peak$Group.1,Peak$x,Average$x,Q710_mod))
colnames(results)<-c("WaterYear","Peak","Average","Q710")

write.csv (results, file = "Table1.csv")

#create data frame to fill in abbreviations of monthes

Month_abv<-data.frame(c("January","February","March","April","May","June","July","August","September","October","November","December"))
colnames(Month_abv)<-("Month_name")
Month_abv$Month<-seq(1,12)

Fin_P<-(aggregate(x = Data$PRCP, by = list(Data$Month), FUN = mean))
Fin_EVP<-(aggregate(x = Data$EVP, by = list(Data$Month), FUN = mean))
Fin_Ground<-(aggregate(x = Data$SDt, by = list(Data$Month), FUN = mean))
Fin_Surf<-(aggregate(x = Data$SRt, by = list(Data$Month), FUN = mean))
Fin_Stream<-(aggregate(x = Data$Q_cm, by = list(Data$Month), FUN = mean))

Result<-data.frame(cbind(Fin_P$Group.1,Fin_P$x,Fin_EVP$x,Fin_Ground$x,Fin_Surf$x,Fin_Stream$x))
colnames (Result)<-c("Month","Precip","Evaporation","Ground_disch","Surf_flow","Stream_Flow")

Result<-merge (Result,Month_abv, by = "Month")
Result$Month<-Result$Month_name
n<-dim(Result)
Result<-Result[1:n[2]-1]

write.csv(Result, file = "Table2.csv")


# #######################################################################################################################
# #######################################################################################################################

#Plots

monthly_mod<-(aggregate(x = Data$Q, by = list(Data$Month, Data$WaterYear), FUN = mean))

monthly_measured<-(aggregate(x = y$Flow, by = list(y$Month, y$WaterYear), FUN = mean))


monthly_mod_y1<-monthly_mod[which(monthly_mod$Group.2==min(monthly_mod$Group.2)),]
monthly_mod_last<-monthly_mod[which(monthly_mod$Group.2==max(monthly_mod$Group.2)),]

monthly_measured_y1<-monthly_measured[which(monthly_measured$Group.2==min(monthly_measured$Group.2)),]
monthly_measured_last<-monthly_measured[which(monthly_measured$Group.2==max(monthly_measured$Group.2)),]


plot (Data$Q~Data$Date, pch = NA_integer_, ylab = "Discharge, cfs", xlab = "Date", main = "Daily streamflow at Pskem river")
lines (Data$Q~Data$Date, col = "red", lty = 2 )
lines (y$Flow~Data$Date,col = "blue", lty = 1)
legend ("topleft", c("Model","Actual"), lty = c(2,1), bty = "n", col = c("red", "blue"))



#END

##NOTE runoff from glaciers is empirical and already includes evapotranspiration!!!!



#define axes 
limy_1<-max (monthly_mod_y1$x,monthly_measured_y1$x)
#################################################################
pdf("Figure1.pdf") #print as pdf file

plot (monthly_measured_y1$x~monthly_measured_y1$Group.1, pch = 17,col = "red",main = "Plot of observed and modelled monthly average flows for the first year of observations", ylab = "Flow, cfs", xlab ="Month", ylim = c(0,limy_1+20), cex.main = 0.8)
points (monthly_mod_y1$x~monthly_mod_y1$Group.1, pch = 18, col = "blue")
legend("topleft", legend=c("Measured","Modeled"), pch = c(17,18), col = c("Red", "Blue"),cex = 1, bty = "n")



limy_2<-max(monthly_mod_last$x,monthly_measured_last$x)
#################################################################
pdf("Figure2.pdf") #print as pdf file

plot (monthly_measured_last$x~monthly_measured_last$Group.1, pch = 17,col = "red",main = "Plot of observed and modelled monthly average flows for the last year of observations", ylab = "Flow, cfs", xlab ="Month",ylim = c(0,limy_2+20),cex.main = 0.8)
points (monthly_mod_last$x~monthly_mod_last$Group.1, pch = 18, col = "blue")
legend("topleft", legend=c("Measured","Modeled"), pch = c(17,18), col = c("Red", "Blue"), cex = 1, bty = "n")


remove (KK,KU,x,A,AM1_D,AM1_G,AM2_G,AM2_D,AM5,CN_1,CN_2,CN_3,by,CN2,ERR,ERR3,FCAP,i,j,K,k,K_B,loc_0,loc_1,loc_2,loc_3,loc_4,loc_5,loc_6,loc_7,loc_8,loc_9,n,SAT, Fin_EVP,Fin_Ground,Fin_P,Fin_Surf,Fin_Stream,Month_abv,Peak,Average,d,f,limy_1,limy_2,UNSAT,y_dim)

#################################################################

plot (Data$Q/35.31~Data$Date, pch = NA_integer_)
lines (Data$Q/35.31~Data$Date, col = "red", lty = 2 )
lines (y$Flow/35.31,col = "blue")
legend ("topright", c("Model","Actual"))

par(mar=c(5.1,4.3,4.1,2.1))
plot (monthly_measured$x/35.31, pch = 17, main  = "Average monthly streamflow at Pskem river between 2013-2015 water year", ylab = "Discharge, cms", xlab = "Month", col = "blue",cex = 1.7, cex.main = 2.2, cex.lab = 2,y.intersp=0.5, ylim = c(0,200))
#lines (monthly_measured$x, pch = 17, col = "blue")
points (monthly_mod$x/35.31, pch = 14, col = "red")
legend ("topleft", c("Actual","Model"), pch = c(17,14), col = c("blue", "red"), bty = "n",inset = c(0,0), y.intersp=0.5, cex = 1.2)
#END

par(mar=c(5.1,4.0,4.1,2.1))

library (EcoHydRology)
hydro<-data.frame(array(0,1095))
hydro$Date<-Data$Date
hydro$Precip<-Data$PRCP*25.4
hydro$Streamflow<-Data$Q/35.31
hydro<-hydro[,2:4]
hydrograph (hydro,streamflow2 = y$Flow/35.31,S1.col = "red", S2.col = "blue")# ylab = "Streamflow, cms", xlab = "Date", cex = 1.3)
legend (50,200,lty = c(2,1), c("Actual", "Model"),col = c("blue", "red"),bty = "n" , cex = 1.2)

plot (Data$T_av~Data$Date)

par( mfrow = c(1, 1))
# x11()
#background color
mycol <- rgb(226, 240, 217, max = 255)
par(bg = "white")

library (TTR)
library (spatstat)
#exponentially weighted mean
plot (Data$T_av~Data$Date, main = "Exponentially-weighted mean of daily air temperatures at Pskem watershed", ylim = c(-30,30),pch = NA_integer_, ylab = "Tair, C",xlab = "Date", cex.main = 1.8, cex.axis = 1.3, cex.lab = 1.3)
lines (Data$Date,EMA(Data$T_av, n=5, wilder = F), col= "red", lty = 1)
lines (lowess(Data$Date,Data$T_av,f=.2,iter = 3),col="blue", lty = 2)
legend ("topleft", c("Daily average air temperature", "Lowess line, f = 1/5"), lty = c(1,2), col = c("red", "blue"), bty = "n", cex = 1.3)

Nash<-function (mod,obs){
  N_S<-1-sum((mod-obs)^2)/sum((obs-mean(obs))^2)
  return (N_S)
}

#compute Bias
Bias<-function (mod,obs,N){
  Bias<-sum(mod-obs)/N
  return (Bias)
}

Bias (Data$Q,y$Flow,1095)
Nash (Data$Q,y$Flow)
Nash (monthly_mod$x,monthly_measured$x)


reg_1<-lm(log(y$Flow)~log(Data$Q))
#Test 1. Assumption 1. Linear relationship between dependent and independent variables

#Plot of observed vs predicted values

plot (y$Flow,Data$Q, main = "Plot of Observed vs Predicted values", xlab = "PredictedQ100", ylab = "ObservedQ100")
lines (y$Flow,y$Flow, col = "orange")

# Set resreg equal to model final residuals

residQ<- residuals(reg_1)

# plot residuals vs model predictions

plot(predict(reg_1),residQ, main = "Plot of Residuals vs Predicted values")   #plot regression predictions vs final model residuals
lines(lowess(predict(reg_1),residQ))

#Conclusion: Assumption not violated.

sum (y$Flow)/sum (Data$Glac_Q_adj)
cor (monthly_mod$x,monthly_measured$x)
cor (Data$Q, y$Flow)
##NOTE runoff from glaciers is empirical and already includes evapotranspiration!!!!
