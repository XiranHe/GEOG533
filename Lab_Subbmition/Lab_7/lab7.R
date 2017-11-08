## Q1-1
### 1a
income <- c(30,28,52,40,35)
education <- c(12,13,18,16,17)
result <- cor.test(income, education, method = "pearson")
r <- result$estimate
r

### 1b
if(result$p.value > 0.05){
  print("we accept null hypothesis that p = 0")
}else {
  print("we reject the null hypothesis that p = 0")
}

### 1c
income.rk <- rank(income)
education.rk <- rank(education)
result <- cor.test(income.rk, education.rk, method = "spearman")
result

### 1d
diffzero <- result$estimate - 0
if(diffzero > 0.05){
  print("the value is not significantly different from zero")
}else {
  print("the value is significantly different from zero")
}

## Q2-3
n1 <- 36
n2 <- 80
critical_t1 <-  qt(0.975, df = n1-2)
critical_t2 <- qt(0.975, df = n2-2)
r1 <- critical_t1 / sqrt(n1-2+critical_t1^2)
r1
r2 <- critical_t2 / sqrt(n1-2+critical_t2^2)
r2



## Q3-4
X <- c(2,8,9,7)
Y <- c(6,6,10,4)
result <- cor.test(X, Y, method = "pearson")
r <- result$estimate
r
if(result$p.value > 0.05){
  print("the correlation is equal to 0")
}else {
  print("the correlation is not equal to 0")
}


## Q4-6
###  interprate
Mincome <- c(35165,35778,37027,37256,37512,37997,37343,36054,35593,35241,35486)
race <- c(399,469,429,450,474,598,364,430,433,410,317)
result <- cor.test(Mincome, race, method = "pearson")
r <- result$estimate
r


if(result$p.value > 0.05){
  print("the correlation coefficient is equal to 0")
}else {
  print("the correlation coefficient is not equal to 0")
}
print("the median income is not related to Number of races")

## Q5-7
n = 6
RX <- c(1,2,5,6,11,12)
RY <- c(8,4,12,3,10,7)
result <- cor.test(RX, RY, method = "spearman")
result

# RX.r <- rank(RX)
# RY.r <- rank(RY)
# different <- RX.r-RY.r
# sqdif <- different * different
# Total <- sum(sqdif)
# spear <- 1-(6*Total/(n*n*n-n))
# spear

if(result$p.value > 0.05){
  print("true correlation is equal to 0")
}else {
  print("true correlation is not equal to 0")
}

## Q6-8
X <- c(3.2,2.4,1.6,8.3,7.2,5.1)
Y <- c(6.2,7.3,8.1,2.6,6.3,4.3)
result <- cor.test(X, Y, method = "pearson")
r <- result$estimate
r
if(result$p.value > 0.05){
  print("the correlation coefficient is equal to 0")
}else {
  print("the correlation coefficient is not equal to 0")
}


## Q7-9
### interpreta
url <- "table.csv"
table <- read.csv(url,header = TRUE)
result <- cor.test(table$Bdrms, table$Lotsize, method = "spearman")
r <- result$estimate
r

if(result$p.value > 0.05){
  print("the correlation coefficient is equal to 0")
}else {
  print("the correlation coefficient is not equal to 0")
}


table.1 <- head(table, n = 7)
result.1 <- cor.test(table.1$Bdrms, table.1$Lotsize, method = "spearman")
r.1 <- result.1$estimate
r.1


## Q8-10
### interpreta
url <- "UK.csv"
UK <- read.csv(url,header = TRUE)
result <- cor.test(UK$floorarea, UK$bedrooms, method = "spearman")
r <- result$estimate
r

if(result$p.value > 0.05){
  print("the correlation coefficient is equal to 0")
}else {
  print("the correlation coefficient is not equal to 0")
}

## Q9
#### calculate without computer
library(datasets)
df <- cars
speed <- cars$speed
dist <- cars$dist

### 9a
plot(speed, dist)

### 9b
n <- nrow(df)
n

### 9c
df$x <- speed - mean(speed)
df$y <- dist - mean(dist)
df$diff <- df$x * df$y
total <- sum(df$diff)
total
sd.x <- sd(speed)
sd.y <- sd(dist)
r.pear <- total / ((n-1)*sd.x*sd.y)
r.pear

### 9d
result <- cor.test(speed, dist, method = "pearson")
r.cor <- result$estimate
r.cor

if(r.pear == r.cor){
  print("the two results are equal")
}


### 9e
sped.r <- rank(speed)
dist.r <- rank(dist)
df$dffrank <- sped.r-dist.r
sqrdif <- df$dffrank * df$dffrank
total2 <- sum(sqrdif)
r.s <- 1-6*total2/(n^3-n)
r.s

### 9f
result <- cor.test(sped.r, dist.r, method = "spearman")
r.or <- result$estimate
r.or

if(r.s == r.or){
  print("the two results are equal")
}








