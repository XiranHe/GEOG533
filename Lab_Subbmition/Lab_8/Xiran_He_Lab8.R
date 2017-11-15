## Q1-1
### 1a
n <- 24
residual <- 1.7
total <- 2.3
regression <- total - residual

df.regression <- 1
df.residual <- n-2
df.total <- n-1

mean.regression <- regression/df.regression
mean.residual <- 1.7 / df.residual
f <- mean.regression/mean.residual

regression
df.residual
df.total
mean.regression
mean.residual
f

### 1b
a <- 0.46
b <- 0.19
x <- 50000/1000
p <- a + b*x
round(p,digits = 1)

### 1c

### 1d
f.critical <- qf(0.95, df1 = 1, df2 = n-2)
if(f < f.critical){
  print("we accept the null hypothdsis: the regression coefficient is different from zero")
}else {
  print("we reject the null hypothesis: the regression coefficient is not different from zero")
}

### 1e
R2 <- regression/total
R <- sqrt(R2)
R



## Q2-6
### 2a
snowfall <- c(36, 78, 11,45)
elevation <- c(400,800,200,675)
snowfall.m <- mean(snowfall)
elevation.m <- mean(elevation)
m <- lm(snowfall ~ elevation)

Stop <- sum((snowfall-snowfall.m)*(elevation-elevation.m)) 
Stop
Strail <- sum((elevation-elevation.m)^2 )
Strail
slope <- Stop/Strail
intercept <- snowfall.m-slope*elevation.m
slope
intercept
summary(m)
print("the intercept and the slope are not significant")


### 2b
n <- 4
m <- lm(snowfall ~ elevation)
result <- summary(m)
top <- sum((result$residuals - mean(result$residuals))^2)
bot <- n-2
sde <- sqrt(top/bot)
sde

### 2c
A <- anova(m)
f <- A$`F value`[1]
f.critical <- qf(0.95, df1 = 1, df2 = n-2)
if (f > f.critical){
  print("we reject the hypothesis that the independent variables is equal to zero")
}else{
  print("we accept the hypothesis that the independent variables is equal to zero")
}

### 2d
R2 <- result$r.squared
R2

### 2e
snowfall <- c(36, 78, 11,45)
predict <- m$fitted.values
residual <- m$residuals
residual
df1 <- data.frame(snowfall, predict, residual)
kable(df1)

### 2f
result <- anova(m)
result
result$Df[[1]]
result$Df[[2]]
result$`Sum Sq`[[1]]
result$`Sum Sq`[[2]]
total <- result$`Sum Sq`[[1]]+result$`Sum Sq`[[2]]
result$`Mean Sq`[[1]]
result$`Mean Sq`[[2]]
total.m <- total/2
result$`F value`[[1]]


### 2g
plot(snowfall ~ elevation)
abline(m, col = "blue", lwd = 2)

## Q3-10
url = "http://spatial.binghamton.edu/geog533/data/Milwaukee_Sales.csv"
df3 <- read.csv(file = url)
y1 <- df3$SalePrice
x1 <- df3$LotSize
m1 <- lm(y1 ~ x1)
m1
plot(y1 ~x1)
abline(m1, col = "red", lwd = 3)
print("y = 92984.25 + 6.443x")

## Q4-11
### 4a
##### ????? conclusion
ur2 = "http://spatial.binghamton.edu/geog533/data/UK_Housing.csv"
df4 <- read.csv(file = ur2)
y2 <- df4$price
x2 <- df4$bedrooms
m2 <- lm(y2 ~ x2)
m2
plot(y2 ~x2)
abline(m2, col = "purple", lwd = 3)
print("y = 11254 + 11893x")


### 4b
###### ?????? conclusion
ur2 = "http://spatial.binghamton.edu/geog533/data/UK_Housing.csv"
df4 <- read.csv(file = ur2)
y3 <- df4$price
x3 <- df4$bathrooms
m <- lm(y3 ~ x3)
m
plot(y3 ~x3)
abline(m, col = "pink", lwd = 3)
print("y = 32729 + 7113x")


## Q5
###### photo & all data
#install.packages("leaflet")
# to install the development version from Github, run
# devtools::install_github("rstudio/leaflet")

library(leaflet)
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=106.7828953, lat=26.622512, popup="The birthplace of Xiran He")
m  # Print the map


