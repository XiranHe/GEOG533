## Q1
q95 <- qnorm(0.975)
sd <- 2.0
std.err <- (0.2/2) / q95
sqrt.n <- sd / std.err
n <- sqrt.n * sqrt.n
round(n,digits = 0)

## Q2-3
library(MASS) 
x <- mvrnorm(n = 50,mu = 18.5,Sigma = 49,empirical = TRUE) 
result <- t.test(x,mu = 16,conf.level = 0.95)
result
if(result$p.value > 0.05){
  print("the tolerable level is within this interval") 
} else  {
    print("the tolerable level is not within this interval")}


## Q3-5
x <- 50 * 0.24
result <- prop.test(x,n = 50,p = 0.165,conf.level = 0.90)
result
if(result$p.value>0.05) {
  print("the town has not a mobility rate that is different from the national average")
} else {
  print("the town has a mobility rate that is different from the national average")
}



## Q4-7
### 7a
library(MASS)
x1 <- mvrnorm(n = 20,mu = 4.1,Sigma = 14.3,empirical = TRUE)
x2 <- mvrnorm(n = 16,mu = 3.1,Sigma = 12.0,empirical = TRUE)
result.a <- t.test(x1,x2,var.equal = T,conf.level = 0.95)
result.a
if(result.a$p.value>0.05) {
  print("there is no difference between the park-going frequencies of whites and nonwhites")
} else {
  print("there is difference between the park-going frequencies of whites and nonwhites")
}


### 7b
library(MASS)
x1 <- mvrnorm(n = 20,mu = 4.1,Sigma = 14.3,empirical = TRUE)
x2 <- mvrnorm(n = 16,mu = 3.1,Sigma = 12.0,empirical = TRUE)
result.b <- t.test(x1,x2,var.equal = F,conf.level = 0.95)
result.b
if(result.b$p.value>0.05) {
  print("there is no difference between the park-going frequencies of whites and nonwhites")
} else {
  print("there is difference between the park-going frequencies of whites and nonwhites")
}


### 7c
result.a$p.value
result.b$p.value

### 7d
result.a$conf.int

### 7e
library(MASS)
x1 <- mvrnorm(n = 24,mu = 4.1,Sigma = 14.3,empirical = TRUE)
x2 <- mvrnorm(n = 12,mu = 3.1,Sigma = 12.0,empirical = TRUE)
result.e1 <- t.test(x1,x2,var.equal = T,conf.level = 0.95)
result.e1
if(result.a$p.value>0.05) {
  print("there is no difference between the park-going frequencies of whites and nonwhites")
} else {
  print("there is difference between the park-going frequencies of whites and nonwhites")
}


library(MASS)
x1 <- mvrnorm(n = 24,mu = 4.1,Sigma = 14.3,empirical = TRUE)
x2 <- mvrnorm(n = 12,mu = 3.1,Sigma = 12.0,empirical = TRUE)
result.e2 <- t.test(x1,x2,var.equal = F,conf.level = 0.95)
result.e2
if(result.b$p.value>0.05) {
  print("there is no difference between the park-going frequencies of whites and nonwhites")
} else {
  print("there is difference between the park-going frequencies of whites and nonwhites")
}

result.e1$p.value
result.e2$p.value

result.e1$conf.int

## Q5-9
library(MASS) 
x <- mvrnorm(n = 17,mu = 6.4,Sigma = 19.36,empirical = TRUE)
result <- t.test(x,mu = 4.2) 
result
if(result$p.value > 0.05){ 
  print("the stream's pollutant level does not exceeds the allowable limit") 
} else { 
    print("the stream's pollutant level exceeds the allowable limit")}

result$p.value


## Q6-13
library(MASS)
x1 <- mvrnorm(n = 52,mu = 3.4,Sigma = 1.21,empirical = TRUE)
x2 <- mvrnorm(n = 62,mu = 2.8,Sigma = 0.64,empirical = TRUE)
result <- t.test(x1,x2,conf.level = 0.95)
result
if(result.a$p.value>0.05) {
  print("We cannot reject the null hypothesis ")
} else {
  print("We accept the alternative hypothesis")
}

curve(dnorm(x,3.4,1.1),xlim = c(0,7),main="rejection regions")
a <- qnorm(0.025,3.4,1.1)
b <- qnorm(0.975,3.4,1.1)
cord.x <- c(0,seq(0,a,0.01),a)
cord.y <- c(0,dnorm(seq(0,a,0.01),3.4,1.1),0)
polygon(cord.x,cord.y,col = 'blue')
cord.x1 <- c(b,seq(b,7,0.01),7)
cord.y1 <- c(0,dnorm(seq(b,7,0.01),3.4,1.1),0)
polygon(cord.x1,cord.y1,col = 'blue')

## Q7-15
d <- c(100,426,322,466,112,388,1155,234,324,556,221,18,133,177,441)
t.test(d, conf.level = 0.90)$conf.int[1:2]
t.test(d, conf.level = 0.95)$conf.int[1:2]

## Q8-16
s.move <- 50*0.3
a.move <- 40*0.22
result.95 <- prop.test(x=c(s.move,a.move), n=c(50,40),alternative = "two.sided")
result.95
result.95$conf.int
result.95$p.value
if(result$p.value>0.10) {
  print("mobility rates in the two communities is not different ")
} else {
  print("mobility rates in the two communities is different")
}


s.move <- 50*0.3
a.move <- 40*0.22
result.90 <- prop.test(x=c(s.move,a.move), n=c(50,40),alternative = "two.sided",conf.level = 0.90)
result.90$conf.int
result.90$p.value
if(result$p.value>0.10) {
  print("mobility rates in the two communities is not different")
} else {
  print("mobility rates in the two communities is different")
}

## Q9-17
### Q9-a
library(MASS)
x1 <- mvrnorm(n = 15,mu = 12.4,Sigma = 9,empirical = TRUE)
x2 <- mvrnorm(n = 15,mu = 14.4,Sigma = 16,empirical = TRUE)
result.b <- t.test(x1,x2,var.equal = F,conf.level = 0.95)
result.b
if(result.b$p.value>0.05) {
  print("there is not a difference in education between the two towns")
} else {
  print("there is a difference in education between the two towns")
}

### Q9-b
library(MASS)
x1 <- mvrnorm(n = 15,mu = 12.4,Sigma = 9,empirical = TRUE)
x2 <- mvrnorm(n = 15,mu = 14.4,Sigma = 16,empirical = TRUE)
result.b <- t.test(x1,x2,var.equal = T,conf.level = 0.95)
result.b
if(result.b$p.value>0.05) {
  print("there is not a difference in education between the two towns")
} else {
  print("there is a difference in education between the two towns")
}

## Q10-20
x <- 50 * 0.15
result <- prop.test(x,n = 50,p = 0.10)
result
if(result$p.value>0.05) {
  print("there is not different from the statewide average of 0.1")
} else {
  print("there is different from the statewide average of 0.1")
}
