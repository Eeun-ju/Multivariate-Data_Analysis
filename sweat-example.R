sweat = read.table(file = paste("C:/Users/user/Desktop/sweat.txt"))
summary(sweat)

head(sweat)

#A) Determine the axes of the 90% confidence ellipsoid for M and the lengths of theses axes.
#install.packages("ellipse")
data <- data.frame(ID = sweat$V1,
                   sweat_rate = sweat$V2,
                   sodium = sweat$V3,
                   potassium = sweat$V4)

head(data)

#행렬화
sweat_m <- as.matrix(data[,-1]) 
#S구하기 vari-covari
sweat_var <- var(sweat_m)
sweat_var

p <- nrow(sweat_var)
n <- nrow(data)

#R을 이용한 eigenvalue, eigenvector 구하기
ev <- eigen(sweat_var)
ev

#The directions and lengths of the axes 

#lengths of the axes
sq <- 2*sqrt((ev$values)*(((p*(n-1))/(n*(n-p)))*qf(0.9,3,n-p)))
sq

#방향
eve <- ev$vectors
eve

#B) Construct QQ plots for the observations on sweat rate, sodium content, and potassium content, respectively.Construct the three possible scatter plots for pairs of observations.Does the multivariate normal assumption seem justified in this case?
#QQ plots
#1. sweat rate
qqnorm(data$sweat_rate)

#2. sodium content
qqnorm(data$sodium)


#3. potassium content
qqnorm(data$potassium)


#Scatter plots

plot(data[,c(2:4)])

#숫자로 나타내기 correlation
cor(data[,c(2:4)])

대부분 정규분포를 따른다고 할 수 있다.

#C) Find simultaneous 95% T-square confidence intervals and Bonferroni intervals for Mu1,Mu2 and Mu3. compare the two sets of intervals.
#95% T^2 cofidence intervals
xbar = apply(data[,2:4],2,mean)
upper = xbar+sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p))*sqrt(diag(sweat_var)/n)
lower = xbar-sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p))*sqrt(diag(sweat_var)/n)
upper;lower

#95% Bonferroni cofidence intervals
upper = xbar+qt((1-0.05/2/p),n-1)*sqrt(diag(sweat_var)/n)
lower = xbar-qt((1-0.05/2/p),n-1)*sqrt(diag(sweat_var)/n)
upper;lower

#Bonferroni 구간이 더 좁은것을 확인 할 수 있다. 더 엄격한 구간이다.