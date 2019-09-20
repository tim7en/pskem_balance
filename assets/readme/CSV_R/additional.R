

#Divide by degree and direction and calculate cumulatives number of lakes
N<- sum(a<22.5)
NE<- (sum(a<67.5)-N)
E <- (sum(a<112.5)-sum(a<67.5))
SE <- (sum(a<157.5)-sum(a<112.5))
S <-(sum(a<202.5)-sum(a<157.5))
SW <- (sum(a<247.5)-sum(a<202.5))
W <- (sum(a<292.5)-sum(a<247.5))
NW <- (sum(a<337.5)-sum(a<292.5))
N<- ((sum(a<360)-sum(a<337.5))+N)
cum<- c(N,NE,E,SE,S,SW,W,NW)
cumsum<- sum(N,NE,E,SE,S,SW,W,NW)
