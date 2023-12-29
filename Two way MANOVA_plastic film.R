setwd("/Users/nadee/Desktop/Stat 5572_R_examples")
X=read.table("plastic_film.dat")
names(X)=c("Factor_1", "Factor_2", "X_1", "X_2", "X_3") 
head(X)
attach(X)
n=5
p=3
g=2
b=2

summary(manova(as.matrix(X[,3:5]) ~ Factor_1 + Factor_2 + Factor_1*Factor_2), test="Wilks")


xbar_l_dot=rbind(colMeans(X[Factor_1==0, 3:5]), colMeans(X[Factor_1==1, 3:5]))
xbar_k_dot=rbind(colMeans(X[Factor_2==0, 3:5]), colMeans(X[Factor_2==1, 3:5]))

#SSP_res was obtained from SAS
SSP_res=matrix(c(1.764,0.02,-3.07,0.02,2.628,-0.552,-3.07,-0.552,64.924), nrow=p)

#95% Bonferroni confidence intervals for factor 1 main effects
m=p*g*(g-1)
nu=g*b*(n-1)

xbar_l_dot[1,1]-xbar_l_dot[2,1]+c(-1,1)*qt(1-0.05/m, nu)* sqrt(2*SSP_res[1,1]/(nu*b*n))
xbar_l_dot[1,2]-xbar_l_dot[2,2]+c(-1,1)*qt(1-0.05/m, nu)* sqrt(2*SSP_res[2,2]/(nu*b*n))
xbar_l_dot[1,3]-xbar_l_dot[2,3]+c(-1,1)*qt(1-0.05/m, nu)* sqrt(2*SSP_res[3,3]/(nu*b*n))

#95% Bonferroni confidence intervals for factor 2 main effects
xbar_k_dot[1,1]-xbar_k_dot[2,1]+c(-1,1)*qt(1-0.05/m, nu)* sqrt(2*SSP_res[1,1]/(nu*b*n))
xbar_k_dot[1,2]-xbar_k_dot[2,2]+c(-1,1)*qt(1-0.05/m, nu)* sqrt(2*SSP_res[2,2]/(nu*b*n))
xbar_k_dot[1,3]-xbar_k_dot[2,3]+c(-1,1)*qt(1-0.05/m, nu)* sqrt(2*SSP_res[3,3]/(nu*b*n))