setwd("/Users/nadee/Desktop/Stat 5572_R_examples")
pottery=read.table("pottery.txt")
names(pottery)=c("site", "Al", "Fe", "Mg", "Ca", "Na") 
head(pottery)
attach(pottery)

table(site)
d1=pottery[site=="A",]
d2=pottery[site=="C",]
d3=pottery[site=="I",]
d4=pottery[site=="L",]

n1=nrow(d1)
n2=nrow(d2)
n3=nrow(d3)
n4=nrow(d4)
n=n1+n2+n3+n4
p=5
g=4

#summary statistics
xbar1=colMeans(d1[,2:6])
xbar2=colMeans(d2[,2:6])
xbar3=colMeans(d3[,2:6])
xbar4=colMeans(d4[,2:6])
xbar=(n1*xbar1+n2*xbar2+n3*xbar3+n4*xbar4)/n

s1=var(d1[,2:6])
s2=var(d2[,2:6])
s3=var(d3[,2:6])
s4=var(d4[,2:6])


W=(n1-1)*s1+(n2-1)*s2+(n3-1)*s3+(n4-1)*s4
B=n1*(xbar1-xbar)%*%t(xbar1-xbar)+n2*(xbar2-xbar)%*%t(xbar2-xbar)+n3*(xbar3-xbar)%*%t(xbar3-xbar)+n4*(xbar4-xbar)%*%t(xbar4-xbar)
Lambda=det(W)/det(B+W)
1-pchisq(-(n-1-(p+g)/2)*log(Lambda),p*(g-1))

#MANOVA with built-in function in r
model1=lm(cbind(Al, Fe, Mg, Ca, Na) ~ site, data=pottery)
manova_model=manova(model1,data=pottery)
summary(manova_model, test='Wilks')

#Test for equality of covariance matrices
library(heplots)
boxM(model1, data=pottery)

#95% Bonferroni intervals for comparing mean Al concentration between sites
xbar1[1]-xbar2[1]+c(-1,1)*qt(0.05/(5*4*(4-1)),n-g,lower.tail=FALSE)*sqrt((1/n1+1/n2)*W[1,1]/(n-g))
xbar1[1]-xbar3[1]+c(-1,1)*qt(0.05/(5*4*(4-1)),n-g,lower.tail=FALSE)*sqrt((1/n1+1/n3)*W[1,1]/(n-g))
xbar1[1]-xbar4[1]+c(-1,1)*qt(0.05/(5*4*(4-1)),n-g,lower.tail=FALSE)*sqrt((1/n1+1/n4)*W[1,1]/(n-g))
xbar2[1]-xbar3[1]+c(-1,1)*qt(0.05/(5*4*(4-1)),n-g,lower.tail=FALSE)*sqrt((1/n2+1/n3)*W[1,1]/(n-g))
xbar2[1]-xbar4[1]+c(-1,1)*qt(0.05/(5*4*(4-1)),n-g,lower.tail=FALSE)*sqrt((1/n2+1/n4)*W[1,1]/(n-g))
xbar3[1]-xbar4[1]+c(-1,1)*qt(0.05/(5*4*(4-1)),n-g,lower.tail=FALSE)*sqrt((1/n3+1/n4)*W[1,1]/(n-g))