setwd("/Users/nadee/Desktop/Stat 5572_R_examples")

#The dataset 'container.df' in the library "Hotelling" contains the elemental concentration of nine different elements (Titanium,Aluminium,Iron,Manganese,Magnesium,Calcium,
Barium,Strontium, and Zirconium) in specimens of glass taken from two container types.There are ten containers of each type.

library(Hotelling)
#checking the available data in the library Hotelling
data(package="Hotelling")

#load the data
data(container.df)
attach(container.df)
container.df

#We will look at the first container type.
#Is multivariate normality a reasonable assumption?
#We could do univariate assesment of normality

par(mfrow=c(3,3), mar=c(2,2,2,2))
qqnorm(Ti[gp==1], main="Ti"); qqline(Ti[gp==1])
qqnorm(Al[gp==1], main="Al"); qqline(Al[gp==1])
qqnorm(Fe[gp==1], main="Fe"); qqline(Fe[gp==1])
qqnorm(Mn[gp==1], main="Mn"); qqline(Mn[gp==1])
qqnorm(Mg[gp==1], main="Mg"); qqline(Mg[gp==1])
qqnorm(Ca[gp==1], main="Ca"); qqline(Ca[gp==1])
qqnorm(Ba[gp==1], main="Ba"); qqline(Ba[gp==1])
qqnorm(Sr[gp==1], main="Sr"); qqline(Sr[gp==1])
qqnorm(Zr[gp==1], main="Zr"); qqline(Zr[gp==1])


par(mfrow=c(3,3), mar=c(2,2,2,2))
hist(Ti[gp==1], main="Ti")
hist(Al[gp==1], main="Al")
hist(Fe[gp==1], main="Fe")
hist(Mn[gp==1], main="Mn")
hist(Mg[gp==1], main="Mg")
hist(Ca[gp==1], main="Ca")
hist(Ba[gp==1], main="Ba")
hist(Sr[gp==1], main="Sr")
hist(Zr[gp==1], main="Zr")

# We will exclude Mn(Manganese) from the analysis, as there's not much variability in data of that variable. Hence, doesn't provide much information.

#sample mean vector
x_bar_1=colMeans(container.df[1:10,-c(1,5)])

#sample var-cov matrix
s_1=var(container.df[1:10,-c(1,5)])

#Hotelling's T2 test for one mean vector
#Let's consider the elements Ti, Al and Fe

mu_0=c(0.035,0.9,0.15)
p_0=3
n=10

#computing sample mean vector and sample var-cov matrix of those 3 variables
x_bar=colMeans(container.df[1:10, 2:4])
s=var(container.df[1:10,2:4])

T2=n*t(x_bar-mu_0)%*%solve(s)%*%(x_bar-mu_0)
p_value=1-pf((n-p_0)*T2/((n-1)*p_0),p_0,n-p_0)

library(DescTools)
HotellingsT2Test(container.df[1:10, 2:4], mu=mu_0)



#confidence region considering Al and Fe
x_bar1=colMeans(container.df[1:10, 3:4])
s1=var(container.df[1:10,3:4])
eigen(s1)

mu_1=c(0.9,0.15) 
n*t(x_bar1-mu_1)%*%solve(s1)%*%(x_bar1-mu_1)

#simultaneous confidence interval for difference in means of Al and Fe concentration
a1=c(1,-1)
t(a1)%*%x_bar1
t(a1)%*%s1%*%a1