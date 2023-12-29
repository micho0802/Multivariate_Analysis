setwd("/Users/nadee/Desktop/Stat 5572_R_examples")

#Problem 1
l_Data=read.table("lumber.dat")
dim(l_Data)
names(l_Data)=c("Stiffness","Bending")
head(l_Data)
attach(l_Data)

#sample mean vector
xbar=colMeans(l_Data)

#sample var-cov matrix
s=var(l_Data)

#Assessing Bivariate Normality of data
par(mfrow=c(1,2))
qqnorm(Stiffness, main="Stiffness"); qqline(Stiffness)
qqnorm(Bending, main="Bending"); qqline(Bending)

#Shapiro-Wilk normality test
shapiro.test(Stiffness)
shapiro.test(Bending)

mu_0=c(1750,8500)
p_0=2
n=30

T2=n*t(xbar-mu_0)%*%solve(s)%*%(xbar-mu_0)
p_value=1-pf((n-p_0)*T2/((n-1)*p_0),p_0,n-p_0)

library(DescTools)
HotellingsT2Test(l_Data, mu=mu_0)

#Bonferroni confidence intervals
qt(0.05/(2*2),n-1,lower.tail=FALSE)

xbar[1]+c(-1,1)*qt(0.05/(2*2),30-1,lower.tail=FALSE)*sqrt(s[1,1]/n)
xbar[2]+c(-1,1)*qt(0.05/(2*2),30-1,lower.tail=FALSE)*sqrt(s[2,2]/n)


#Problem 2
milk_data=read.table("milk.dat")
dim(milk_data)
head(milk_data)
names(milk_data)=c("fuel","repair","capital","Truck")
head(milk_data)
attach(milk_data)

d1=milk_data[Truck=="gasoline",]
d2=milk_data[Truck=="diesel",]

xbar1=colMeans(d1[,1:3])
xbar2=colMeans(d2[,1:3])

s1=var(d1[,1:3])
s2=var(d2[,1:3])

table(Truck)
n1=36
n2=23
n=n1+n2
p=3

sp=((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)

T2=t(xbar1-xbar2)%*%solve(sp*((1/n1)+(1/n2)))%*%(xbar1-xbar2)
F=qf(0.05,p,n1+n2-p-1,lower.tail=FALSE)
p_value=1-pf((n1+n2-p-1)*T2/((n1+n2-2)*p),p,n1+n2-p-1)


library(DescTools)
HotellingsT2Test(d1[, 1:3],d2[, 1:3])

#Bonferroni 99% Simultaneous confidence interval for comparing Trucks in terms of the variable X1.

qt(0.01/(2*p),n-p,lower.tail=FALSE)

#Bonferroni 99% Simultaneous confidence interval for comparing Trucks in terms of the variable X1.
xbar1[1]-xbar2[1]+c(-1,1)*qt(0.01/(2*p),n-2,lower.tail=FALSE)*sqrt((1/n1+1/n2)*sp[1,1])
#Bonferroni 99% Simultaneous confidence interval for comparing Trucks in terms of the variable X2.
xbar1[2]-xbar2[2]+c(-1,1)*qt(0.01/(2*p),n-2,lower.tail=FALSE)*sqrt((1/n1+1/n2)*sp[2,2])
#Bonferroni 99% Simultaneous confidence interval for comparing Trucks in terms of the variable X3.
xbar1[3]-xbar2[3]+c(-1,1)*qt(0.01/(2*p),n-2,lower.tail=FALSE)*sqrt((1/n1+1/n2)*sp[3,3])

#checking normality in data related to gasoline trucks
par(mfrow=c(1,3))
qqnorm(d1$fuel, main="fuel"); qqline(d1$fuel)
qqnorm(d1$repair, main="repair"); qqline(d1$repair)
qqnorm(d1$capital, main="capital"); qqline(d1$capital)

#Shapiro-Wilk normality test
shapiro.test(d1$fuel)
shapiro.test(d1$repair)
shapiro.test(d1$capital)

#checking normality in data related to diesel trucks
par(mfrow=c(1,3))
qqnorm(d2$fuel, main="fuel"); qqline(d2$fuel)
qqnorm(d2$repair, main="repair"); qqline(d2$repair)
qqnorm(d2$capital, main="capital"); qqline(d2$capital)

#Shapiro-Wilk normality test
shapiro.test(d2$fuel)
shapiro.test(d2$repair)
shapiro.test(d2$capital)

#Checking the assumption of common var-cov matrix of populations
#Bartlett's test
c=1-(((2*p^2+3*p-1)/(6*(p+1)))*((1/(n1-1))+(1/(n2-1))-(1/(n1+n2-2))))
testStat_Bartletts=c*((n1+n2-2)*log(det(sp))-(n1-1)*log(det(s1))-(n2-1)*log(det(s2)))
pval_Bartletts=1-pchisq(testStat_Bartletts,(p*(p+1))/2)
