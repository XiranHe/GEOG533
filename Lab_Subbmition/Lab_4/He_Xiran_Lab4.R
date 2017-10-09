# Question_1
## 1-a
pnorm(20,32,7)
curve(dnorm(x, mean = 32,sd = 7),from = 11,to = 53)
cord.x <- c(11, seq(11,20, by = 0.01),20)
cord.y <- c(0, dnorm(seq(11,20,0.01), mean = 32, sd = 7),0)
polygon(cord.x,cord.y,col = "blue")

## 1-b
pnorm(35,32,7,lower.tail = F)
curve(dnorm(x, mean = 32,sd = 7),from = 11,to = 53)
cord.x <- c(35, seq(35,53, by = 0.01),53)
cord.y <- c(0, dnorm(seq(35,53,0.01), mean = 32, sd = 7),0)
polygon(cord.x,cord.y,col = "blue")

## 1-c
pnorm(40,32,7)-pnorm(20,32,7)
curve(dnorm(x, mean = 32,sd = 7),from = 11,to = 53)
cord.x <- c(20, seq(20,40, by = 0.01),40)
cord.y <- c(0, dnorm(seq(20,40,0.01), mean = 32, sd = 7),0)
polygon(cord.x,cord.y,col = "blue")

# Question_2
## 2-a
pexp(3, rate = 0.1)

### Plot the density function
curve(dexp(x,0.1),from = 0, to = 55,lwd=2)
cord.x <- c(0,seq(0,3,by = 0.01),3)
cord.y <- c(0,dexp(seq(0,3,0.01),0.1),0)
polygon(cord.x, cord.y,col = "yellow")

## 2-b
pexp(20,rate = 0.1,lower.tail = F)

### Plot the density function
curve(dexp(x,0.1),from = 0, to = 55,lwd=2)
cord.x <- c(20,seq(20,55,by = 0.01),55)
cord.y <- c(0,dexp(seq(20,55,0.01),0.1),0)
polygon(cord.x, cord.y,col = "yellow")

## 2-c
pexp(10,rate = 0.1)-pexp(5,rate = 0.1)

### Plot the density function
curve(dexp(x,0.1),from = 0, to = 55,lwd=2)
cord.x <- c(5,seq(5,10,by = 0.01),10)
cord.y <- c(0,dexp(seq(5,10,0.01),0.1),0)
polygon(cord.x, cord.y,col = "yellow")

# Question_3
## 3-a
library(raster) 
ras1 <- raster(nrows=30,ncols=30,xmn=0,xmx=30,ymn=0,ymx=30) 
ras1 
ras1[] <- runif(ncell(ras1)) 
### Uniform Distribution Plot
plot(ras1) 
### Uniform Distribution Hist
hist(ras1) 

## 3-b
library(raster) 
ras2 <- raster(nrows=30,ncols=30,xmn=0,xmx=30,ymn=0,ymx=30) 
ras2 
ras2[] <- rnorm(ncell(ras2)) 
### Normal Distribution Plot
plot(ras2)
### Normal Distribution Hist
hist(ras2)

## 3-c
library(raster) 
ras3 <- raster(nrows=30,ncols=30,xmn=0,xmx=30,ymn=0,ymx=30) 
ras3 
ras3[] <- ras1[,] + ras2[,]
### Addition plot
plot(ras3)
### Addition hist
hist(ras3)

## 3-d
m <- cellStats(ras3,"mean") 
m
ras[ras3 <= m] <- 0 
ras[ras3 > m] <- 1 
### Mean plot
plot(ras)
### Mean hist
hist(ras)
### TIFF image
writeRaster(ras,filename = "test.tif",overwrite = T)

