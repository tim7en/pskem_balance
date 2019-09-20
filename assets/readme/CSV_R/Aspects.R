#Timur Sabitov
#Subject, Master thesis plots Ele (Plot of aspects_rose diagram)
#Date, 1/8/2017

#Input Data
a1 <- read.csv(file= "Ele_Kg.csv",header=T,sep = ",")
a <- as.data.frame(a1$Aspect)

#density of lakes by elevation in Kirgizstan
z<- density(a1$Z)
plot (z)
polygon(z, col="red", border="blue")

#density of lakes slope in Kirgizstan
s<- density (a1$Slope)
plot (s)
polygon(s, col="red", border="blue")

#Create rose diagram using ggplot2
library(ggplot2)
rose <- ggplot(mapping = aes(x = a))+ 
  stat_bin(breaks = (0:8 - 0.5)/8 * 360) +
  scale_x_continuous(breaks = 0:7/8*360, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
  coord_polar(start=-pi/8)
rose

b1 <- read.csv(file= "Glazirin_Kg.csv",header=T,sep = ",")
b <- as.data.frame(b1$Aspect)

#density of lakes by elevation in Kirgizstan
z<- density(b1$Z)
plot (z)
polygon(z, col="red", border="blue")

#density of lakes slope in Kirgizstan
s<- density (b1$Slope)
plot (s)
polygon(s, col="red", border="blue")

#Create rose diagram using ggplot2
library(ggplot2)
rose <- ggplot(mapping = aes(x = b))+ 
  stat_bin(breaks = (0:8 - 0.5)/8 * 360) +
  scale_x_continuous(breaks = 0:7/8*360, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
  coord_polar(start=-pi/8)
rose

