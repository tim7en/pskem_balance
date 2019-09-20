
# clean up the data
rm(list = ls(all = T))
source("Curve_func.R")

ptm <- proc.time()
# install.packages("styler")

# read in file
x <- read.csv(file = "Met.csv")
y <- read.csv(file = "flow.csv")

# read in the data
Data <- preproc(x)

# define errors
ERR <- which(Data$TMAX < Data$TMIN)
ERR3 <- which(Data$PRCP < 0)
Data$TMAX[ERR] <- NA
Data$PRCP[ERR3] <- NA

# define dimentions of the data
n <- dim(Data)

library(zoo)

Data[2:n[2]] <- na.approx(Data[2:n[2]])
f <- which(Data$Month == 10, Data$Day == 1)[1]
Data <- Data[f:n[1], ]
n <- dim(Data)

# Lapse rate
Data$Lapse <- 0

Data$Lapse[which(Data$Month == 3)] <- 6.7
Data$Lapse[which(Data$Month == 4)] <- 6.7
Data$Lapse[which(Data$Month == 5)] <- 6.7
Data$Lapse[which(Data$Month == 6)] <- 6.5
Data$Lapse[which(Data$Month == 7)] <- 6.5
Data$Lapse[which(Data$Month == 8)] <- 6.5
Data$Lapse[which(Data$Month == 9)] <- 6
Data$Lapse[which(Data$Month == 10)] <- 6
Data$Lapse[which(Data$Month == 11)] <- 6
Data$Lapse[which(Data$Month == 12)] <- 6
Data$Lapse[which(Data$Month == 1)] <- 6
Data$Lapse[which(Data$Month == 2)] <- 6

Lapse <- Data$Lapse
# convert precip from inch to cm
PRCP <- (Data$PRCP * 2.54)
TMAX <- Data$TMAX
TMIN <- Data$TMIN

# Define average air temperature at the elevation of glaciers
Glac_Tavg <- (((TMIN - 32) * 5 / 9 - (3680 - 1251) / 1000 * Lapse) + ((TMAX - 32) * 5 / 9 - (3680 - 1251) / 1000 * Lapse)) / 2

# Tmax and Tmin on average elevation of the upper part of the watershed; #Define avg temperature and convert into C. Note the air temperature is averaged for the entire watershed

TMAX <- (TMAX - 32) * 5 / 9 - (3800 - 1251) / 1000 * Lapse
TMIN <- (TMIN - 32) * 5 / 9 - (3800 - 1251) / 1000 * Lapse
T_avg <- (TMAX + TMIN) / 2


# define Ablation from glaciers using empirical equation
Ab <- (9.5 + Glac_Tavg)^3

G_area <- 91
# define Glac discharge using Ablation and the area of glaciation
Glac_Q <- 1 / (31.5 * 10^3) * Ab * G_area
Glac_Q_adj <- array(0,length(Ab))
Glac_Q_adj[which(Glac_Q > 0)] <- Glac_Q[which(Glac_Q > 0)]



y <- preproc_d_f(y)
y_dim <- dim(y)
y <- y[which(y$WaterYear == min(Data$WaterYear))[1]:y_dim[1], ]
Q_obs <- y$Flow



################# 33

K <- 0.45
st_elv <- 1251

FCAP <- 0
Unsat <- 0
SAT <- 0
K_B <- 0

f1 <- 0
while (f1 < 5) {
  FCAP <- f1
  f1 <- f1 + 0.5
  print (f1)
  "Most outer loop, +1 element"
  f2 <- 0
  while (f2 < FCAP) {
    Unsat <- f2
    f2 <- f2 + 0.4

    f3 <- 2
    while (f3 < 5) {
      SAT <- f3
      f3 <- f3 + 0.5

      f4 <- 0.96
      while (f4 < 1) {
        K_B <- f4
        f4 <- K_B + 0.002

        Q1 <- Curve_numb(x, 3800, st_elv, 680, K, 91, K_B, Unsat, SAT, FCAP)
        names(Q1) <- c("Q", "Day", "Month", "Year")
        


        f5 <- 0
        while (f5 < 5) {
          FCAP <- f5
          f5 <- f5 + 0.5

          f6 <- 0
          while (f6 < FCAP) {
            Unsat <- f6
            f6 <- f6 + 0.4
          
            f7 <- 5
            while (f7 < 10) {
              SAT <- f7
              f7 <- f7 + 0.5

              f8 <- 0.96
              while (f8 < 1) {
                K_B <- f8
                f8 <- f8 + 0.002
                
                Q2 <- Curve_numb(x, 2800, st_elv, 1200, K, 50, K_B, Unsat, SAT, FCAP)
                names(Q2) <- c("Q", "Day", "Month", "Year")
                print (f8)
                

                f9 <- 0
                while (f9 < 5) {
                  FCAP <- f9
                  f9 <- f9 + 0.5

                  f10 <- 0
                  while (f10 < FCAP) {
                    Unsat <- f10
                    f10 <- f10 + 0.4

                    f11 <- 5
                    while (f11 < 20) {
                      SAT <- f11
                      f11 <- f11 + 0.5

                      
                      f12 <- 0.96
                      while (f12 < 1) {
                        K_B <- f12
                        f12 <- f12 + 0.002
                        
      
                        Q3 <- Curve_numb(x, 1800, st_elv, 660, K, 35, K_B, Unsat, SAT, FCAP)
                        names(Q3) <- c("Q", "Day", "Month", "Year")

                        Q <- Q1$Q + Q2$Q + Q3$Q+Glac_Q_adj
                        N <- Nash(Q, Q_obs)
                        B <- Bias(Q / 35.31, Q_obs / 35.31, length(Q))

                        B0 <- -60
                        N0 <- -50
                        
                        SAT0_1 <- -50
                        KB0_1 <- -50
                        UNSAT0_1 <- -50
                        FCAP0_1 <- -50
                        
                        SAT0_2 <- -50
                        KB0_2 <- -50
                        UNSAT0_2 <- -50
                        FCAP0_2 <- -50
                        
                        
                        SAT0_3 <- -50
                        KB0_3 <- -50
                        UNSAT0_3 <- -50
                        FCAP0_3 <- -50
                        

                        if ((abs(B) < abs(B0)) && N > N0) {
                          B0 <- B
                          N0 <- N
                          
                          SAT0_1 <- f3
                          SAT0_2<-f7
                          SAT0_3<-f11
                          
                          KB0_1<-f4
                          KB0_2<-f8
                          KB0_3<-f12
                            
                          UNSAT0_1<-f2
                          UNSAT0_2<-f6
                          UNSAT0_3<-f10
                          
                          FCAP0_1<-f1
                          FCAP0_2<-f5
                          FCAP0_3<-f9
                          
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
