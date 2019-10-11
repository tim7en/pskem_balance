# clean up the data
rm(list = ls(all = T))
source("modelFunctions.R")
drainageArea <- 2540
basinData <- read.csv("D:\\Work\\MyGitHub\\pskem_balance\\assets\\mainassets.csv")
basinData$P <- basinData$P / 10 # mm to cm
basinData$wateryear <- 0
basinData$wateryear <- wateryear(basinData$year, basinData$month)
basinData$Lapse <- 6.5 # Lapse rate 6.5 degree per 1000 meters
basinData$T <- basinData$T - ((2770 - 1251) / 1000 * basinData$Lapse) # convert temperature according to lapse rate to the average elevation of the basin
basinData$ETO <- hamondsevp(basinData, basinData$T, basinData$T) # find pet using solar radiation (cm)
K <- 0.45 # snowmelt coefficient
K_B <- 0.988
CN_2 <- 45
FCAP <- 6 # field capacity
UNSAT <- 4 # saturated layer of unsaturated storage (infiltrated)
n <- nrow(basinData)
SAT <- 1 # lets assume 1 cm of water is on the ground from previous day (fully saturated but not infiltrated yet)
globpars <- expand.grid(FCAP = seq ( 2,0, -0.05), K_B = seq ( 0.9889,0.9880,-2.5e-04), CN_2 = seq (60, 45, -5))
modelB <- -30 #start values for model parameterization
modelNash <- -200
for (i in seq (1, nrow(globpars))){
  basinData$SN <- 0 # set model parameters
  basinData$Mt <- 0
  basinData$Rn <- 0
  basinData$SP <- 0
  pars <- globpars[i,]
  basinData <- snowmelt (basinData, SAT, K)
  basinData$Rn <- basinData$P - basinData$SN # Define rain as precip that is not a snow
  basinData$AM5 <- 0 # Define AM5, 5 day anticident moisture conditions
  j <- 1 # counters
  k <- 1 # set k to  1 
  basinData <- am5runoff (basinData) #find 5 day anticident moisture conditions
  basinData <- curvenumber (basinData, globpars$CN_2[i])
  basinData <- surfacerunoff (basinData)
  basinData$I <- basinData$Rn + basinData$Mt - basinData$SRt # Define infiltration
  basinData <- actualevp (basinData, UNSAT, globpars$FCAP[i]) #define actual evapotraspiration
  basinData <- modelrunoff (basinData, SAT, globpars$K_B[i])
  B <- Bias (basinData$Q_est, basinData$Q) #perfect bias is around 0
  N <- Nash (basinData$Q_est, basinData$Q) #perfect nash is around 1
  if ( N > modelNash) {   #abs(B) < abs(modelB) &&
    modelB <- B
    modelNash <- N
    modelPars <- pars
    print (paste0('Updated, Left:', nrow(globpars) - i))
  }
}

#plot(basinData$Q ~ basinData$Q_est)