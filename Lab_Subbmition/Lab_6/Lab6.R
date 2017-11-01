## Q1-2


stress <- c(x1,x2,x3,x4,x5) 
group <- c(rep("A",10),rep("B",10),rep("C",10),rep("D",10),rep("E",10)) 
df <- data.frame(stress,group)
m <- aov(stress~group,data = df) 
m 
summary(m)


## Q2-6
### a
library(reshape2)
library(car)
A <- c(5,7,9,11,13,8,10,34,17,50,17,25)
B <- c(25,24,8,2,11,10,10,66,113,1,3,5)
stress <- c(A,B) 
group <- c(rep("A",12),rep("B",12)) 
df <- data.frame(stress,group)
m <- aov(stress~group,data = df) 
result <- summary(m)
result
qf(0.9,df1 = 1,df2 = 22)
if(result[[1]][1,5]> 0.1){
  print("There are not differences in the two population means")
}else{
  print("There are differences in the two population means")
}
           

## Q3-9
A <- rnorm(12, mean = 43.2, sd = 36.2)
B <- rnorm(10, mean = 34.3, sd = 20.3)
C <- rnorm(8, mean = 27.2, sd = 21.4)

stress <- c(A,B,C) 
group <- c(rep("A",12),rep("B",10),rep("C",8)) 
df <- data.frame(stress,group)
m <- aov(stress~group,data = df) 
result <- summary(m)
result
if(result[[1]][1,5]> 0.05){
  print("There are not differences within these three groups")
}else{
  print("There are differences in these three groups")
}

## Q4-10
library(graphics)
clo1 <- c(23.1,13.3,15.6,1.2)
clo2 <- c(43.1,10.2,16.2,0.2)
clo3 <- c(56.5,32.1,43.3,24.4)
clo4 <- c(10002.3,54.4,8.7,54.4)

stress <- c(clo1,clo2,clo3,clo4) 
group <- c(rep("A",4),rep("B",4),rep("C",4),rep("D",4)) 
df <- data.frame(stress,group)
result <- kruskal.test(stress ~ group, data = df)
result

if(result$p.value > 0.05)
{
  print("the means of the four columns of data are equal") 
  
} else  {
  print("the means of the four columns of data are not equal")}


## Q5-12
### a
A <- c(5,4,1,2,3,10,6,6,4,12,11)
B <- c(10,10,8,6,5,3,16,20,7,3,2)
C <- c(8,11,15,19,21,7,7,4,3,17,18)

stress <- c(A,B,C) 
group <- c(rep("A",11),rep("B",11),rep("c",11)) 
df <- data.frame(stress,group)
m <- aov(stress~group,data = df) 
m 
result <- summary(m)
result
qf(0.95,df1 = 2,df2 = 30)
print("the commuting distance do vary by income according to Anova")

### b
library(reshape2)
library(car)
result.b <- leveneTest(value ~, data = df)
if(result.b $`Pr(>F)`[1]> 0.05){
      print("commuting distances do not vary by income")
}else{
      print("commuting distances vary by income")
      }


## Q6-13
A <- rnorm(10, mean = 1.5, sd = 1.0)
B <- rnorm(15, mean = 2.6, sd = 1.1)
C <- rnorm(15, mean = 1.2, sd = 1.2)

stress <- c(A,B,C) 
group <- c(rep("A",10),rep("B",15),rep("C",15)) 
df <- data.frame(stress,group)
m <- aov(stress~group,data = df) 
m 
result <- summary(m)
