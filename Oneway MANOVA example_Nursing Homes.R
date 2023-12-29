setwd("/Users/nadee/Desktop/Stat 5572_R_examples")
n1=271
n2=138
n3=107
n=n1+n2+n3
p=4
g=3

#summary statistics
xbar1=c(2.066,0.48,0.082,0.36)
xbar2=c(2.167,0.596,0.124,0.418)
xbar3=c(2.273,0.521,0.125,0.383)
xbar=(n1*xbar1+n2*xbar2+n3*xbar3)/n

s1=matrix(c(0.291,-0.001,0.002,0.01,-0.001,0.011,0,0.003,0.002,0,0.001,0,0.01,0.003,0,0.01),nrow=p)
s2=matrix(c(0.561,0.011,0.001,0.037,0.011,0.025,0.004,0.007,0.001,0.004,0.005,0.002,0.037,0.007,0.002,0.019),nrow=p)
s3=matrix(c(0.261,0.03,0.003,0.018,0.03,0.017,0,0.006,0.003,0,0.004,0.001,0.018,0.006,0.001,0.013),nrow=p)

W=(n1-1)*s1+(n2-1)*s2+(n3-1)*s3
B=n1*(xbar1-xbar)%*%t(xbar1-xbar)+n2*(xbar2-xbar)%*%t(xbar2-xbar)+n3*(xbar3-xbar)%*%t(xbar3-xbar)
Lambda=det(W)/det(B+W)
((n-p-2)/p)*((1-sqrt(Lambda))/sqrt(Lambda))
F=qf(0.01,2*p, 2*(n-p-2),lower.tail=FALSE)
1-pf(((n-p-2)/p)*((1-sqrt(Lambda))/sqrt(Lambda)), 2*p, 2*(n-p-2))
-(n-1-(p+g)/2)*log(Lambda)
chisq_val=qchisq(0.01, p*(g-1),lower.tail=FALSE)
1-pchisq(-(n-1-(p+g)/2)*log(Lambda),p*(g-1))


#Test for equality of covariance matrices
sp=W/(n-g)
M=(n-g)*log(det(sp))-((n1-1)*log(det(s1))+(n2-1)*log(det(s2))+(n3-1)*log(det(s3)))
u=((2*p^2+3*p-1)/(6*(p+1)*(g-1)))*(1/(n1-1)+1/(n2-1)+1/(n3-1)-1/(n-g))
c=(1-u)*M
chisq=qchisq(0.01, p*(p+1)*(g-1)/2,lower.tail=FALSE)
1-pchisq((1-u)*M,p*(p+1)*(g-1)/2)
