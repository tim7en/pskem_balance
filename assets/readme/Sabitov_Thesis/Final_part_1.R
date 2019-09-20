#Final exam
#Hydrologic Modelling
#Timur Sabitov
#5/3/2017


#Read in file

#Variables

#x<-original input
#y<-interpolated input
#alph<-value of rejection region for hypothesis testing
#b<-estimated streamflow value for 10.5 inch gage height
#rcor<-test statistics for hypothesis testing of residuals
#eq_1<-ols used to estimate value of the flow at the gage height
#eq_2<-original model used to estimate flow at gage

#clean work space
rm(list=ls(all=T))

#read in file
x<-read.csv (file="peak.csv")

#remove NA
y<-na.omit(x)

#extract coefficient for the OLS regression model for the gage height
eq_1<-lm(y$gage_ht~y$peak_va)

#estimate gage height for the missing day as function of peak flow
x$gage_ht[which (is.na(x$gage_ht))]<-eq_1$coefficients[[1]]+eq_1$coefficients[[2]]*x$peak_va[which (is.na(x$gage_ht))]


#Rating curve
plot (x$gage_ht~x$peak_va,xlab = "Flow, cfs", ylab = "Gage height, ft", col = "blue", main = "Rating Curve", cex.lab = 1.4, cex.axis = 1.4, cex.main = 2)

#Convert everything into log space
x$peak_va<-log(x$peak_va)
x$gage_ht<-log(x$gage_ht)

#OLS for the peak flow without missing gage height
eq_2<-lm(x$peak_va~x$gage_ht)


#Test 1. Assumption 1. Linear relationship between dependent and independent variables

#plot of observed vs predicted values
plot (predict(eq_2),x$peak_va, main = "Plot of Observed vs Predicted values", xlab = "Predicted Peaks", ylab = "Observed Peaks")
lines (x$peak_va,x$peak_va)

#Define model residuals
resid_Peaks<- residuals(eq_2)

# plot residuals vs model predictions
plot(predict(eq_2),resid_Peaks, main = "Plot of Predictions vs Model residuals", xlab = "Predicted flow", ylab = "Prediction Residuals")   #plot regression predictions vs final model residuals
lines(lowess(predict(eq_2),resid_Peaks))


#Test 2. Assumption 2. Variance of the residuals is constant
plot (x$gage_ht ,resid_Peaks, main = "Plot of model residuals for Gage Height", ylab = "Prediction Residuals", xlab = "Gage HT")
lines(lowess(x$gage_ht,resid_Peaks))

#convert dates as character
x$peak_dt<-as.character(x$peak_dt)

#convert format of dates
x$Posixlt<-as.POSIXlt(x$peak_dt, format="%m/%d/%Y")

#create table with years
x$Year<-x$Posixlt$year+1900

plot (resid_Peaks~x$Year, xlab = "Date", ylab = "Model Residuals", main = "Plot of Model residuals vs Time step", pch = 17)

#Detectable shape, assumption is violated


#Test 3. Assumption 3. Residuals are independent.

#Autocorrelation plot of residuals

#par(mfrow=c(1,1))
acf (resid_Peaks, main = "Model residuals")

#Conclusion: Assumption not violated, variance within the intervals.


#Test 4. Assumption 4. Residuals are normally distributed, therefore variables normally distributed.

#H0 - Null hypothesis claims that log space distribution of discharge appear to be normally distributed, 
#hence it was generated from normal distribution

#Ha - Alternative hypothesis claims that it is was not generated from normally distributed data

# Calculate Test Statistic for Normal PPCC Hypothesis Test

zp <- qnorm((rank(resid_Peaks)-(3/8))/(length(resid_Peaks)+(1/4)))
rcor<-cor(resid_Peaks,zp)
rcor

#our alpha at significance of 5%
#Rejection region, r should be less or equal to a

alph<-c(((0.9807-0.9767)/15)*6+0.9767) #Linear interpolation -> from table of critical values for (r)
alph

#As T.S. rcor<alpha, reject H0 residuals  are not normally distributed.

#part 1 - b
#Flow at the gage stage of 10.5
g_height<-log(10.5)

b<-exp(eq_2$coefficients[[1]]+eq_2$coefficients[[2]]*g_height)

#END
