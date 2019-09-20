#Timur Sabitov
#Subject, Master thesis plots Ele (Plot of aspects_rose diagram)
#Date, 1/8/2017

#Input Data

a1 <- read.csv(file= "Ele_Kg.csv",header=T,sep = ",")
a <- as.data.frame(a1$Aspect)

#Create frame for 2 diagrams
par (mfrow = c(2,1))

#density of lakes by elevation in Kirgizstan
z<- density(a1$Z)
plot (z, main = "Lakes elevation in Kirgyz range")
polygon(z, col="red", border="blue")

#density of lakes slope in Kirgizstan
s<- density (a1$Slope)
plot (s, main = "Lakes elevation in Kirgyz range")
polygon(s, col="red", border="blue")

#Load lmoments and define statistics 
library (Lmoments)
l1<-Lmoments(a1$Slope)

#Create rose diagram using ggplot2
library(ggplot2)

rose <- ggplot(mapping = aes(x = a))+ 
  stat_bin(breaks = (0:8 - 0.5)/8 * 360) +
  scale_x_continuous(breaks = 0:7/8*360, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
  coord_polar(start=-pi/8)
rose


#Input Data
b1 <- read.csv(file= "Timur_Turkestan.csv",header=T,sep = ",")
b <- as.data.frame(b1$Aspect)

#frame for diagrams
par (mfrow = c(2,1))
#density of lakes by elevation in Kirgizstan Turkestan range
z2<- density(b1$Z)
plot (z2, main = "Lakes elevation in Turkestan range")
polygon(z2, col="red", border="blue")

#density of lakes slope in Kirgizstan Turkestan range
s2<- density (b1$Slope)
plot (s2, main = "Lakes slope in Turkestan range")
polygon(s2, col="red", border="blue")

#define lmoments
l2<-Lmoments(b1$Slope)

#Create rose diagram using ggplot2
rose <- ggplot(mapping = aes(x = b))+ 
  stat_bin(breaks = (0:8 - 0.5)/8 * 360) +
  scale_x_continuous(breaks = 0:7/8*360, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
  coord_polar(start=-pi/8)
rose

#Input data
c1 <- read.csv(file= "Timur_Tashkent.csv",header=T,sep = ",")
c <- as.data.frame(c1$Aspect)

#frame for diagrams
par (mfrow = c(2,1))

#density of lakes by elevation in Tashkent
z3<- density(c1$Z)
plot (z3, main = "Lakes elevation in Pskem range")
polygon(z3, col="red", border="blue")

#density of lakes slope in Tashkent
s3<- density (c1$Slope)
plot (s3, main = "Lakes slope in Pskem range")
polygon(s3, col="red", border="blue")

#define lmomnets
l3<-Lmoments(c1$Slope)

#rose diagram
rose <- ggplot(mapping = aes(x = c))+ 
  stat_bin(breaks = (0:8 - 0.5)/8 * 360) +
  scale_x_continuous(breaks = 0:7/8*360, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
  coord_polar(start=-pi/8)
rose

#Input data
d1 <- read.csv(file= "Timur_Kashkadarya.csv",header=T,sep = ",")
d <- as.data.frame(d1$Aspect)

#frame for diagrams
par (mfrow = c(2,1))

#density of lakes by elevation in Kashkadarya
z4<- density(d1$Z)
plot (z4, main = "Lakes elevation in Hissar range")
polygon(z4, col="red", border="blue")

#density of lakes slope in Kashkadarya
s4<- density (d1$Slope)
plot (s4, main = "Lakes slope in Hissar range")
polygon(s4, col="red", border="blue")

#define lmomnets
l3<-Lmoments(d1$Slope)

#rose diagram
rose <- ggplot(mapping = aes(x = d))+ 
  stat_bin(breaks = (0:8 - 0.5)/8 * 360) +
  scale_x_continuous(breaks = 0:7/8*360, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
  coord_polar(start=-pi/8)
rose

#Compare density of 2 or more groups using sm package
par (mfrow = c(1,1))
#slope
Slope<- c(as.data.frame(a1$Slope),as.data.frame(b1$Slope),as.data.frame(c1$Slope),as.data.frame(d1$Slope))






#density of lakes slope in Kirgizstan
s<- density (a1$Slope)
plot (s, main = "Lakes slope in mountain regions of Central Asia", xlim = c(0, 30), ylim = c(0, .15))
col2rgb ("deeppink")
pink <- rgb(255, 20, 147, max = 255, alpha = 80, names = "pink")
polygon(s, col=pink, border="red")

#density of lakes slope in Tashkent
s3<- density (c1$Slope)
lines (s3, main = "Lakes slope in mountain regions of Central Asia", xlim = c(0, 30), ylim = c(0, .15))
blue <- rgb(173, 216, 230, max = 255, alpha = 80, names = "blue")
polygon(s3, col=blue, border="blue")


#density of lakes slope in Kirgizstan
s2<- density (b1$Slope)
lines (s2, main = "Lakes slope in Turkestan range")
col2rgb ("darkkhaki")
khaki <- rgb(189, 183, 107, max = 255, alpha = 80, names = "khaki")
polygon(s2, col=khaki, border="yellow")


#density of lakes slope in Kashkadarya
s4<- density (d1$Slope)
lines (s4, main = "Lakes slope in Hissar range")## Transparent colors
col2rgb ("darkslategray4")
gray <- rgb(82, 139, 139, max = 255, alpha = 80, names = "gray")
polygon (s4, col=gray, border="black", legend("topright", legend=c("Pskem","Kirgyz","Turkestan","Hissar"), fill = c(blue, pink, khaki, gray)))


