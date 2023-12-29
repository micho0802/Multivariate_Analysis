setwd("/Users/nadee/Desktop/Stat 5572_R_examples")
c_data=read.table("coatings.DAT")
head(c_data)
dim(c_data)
names(c_data)=c("D_C1","N_C1","D_C2","N_C2")
attach(c_data)
diff_D=D_C1-D_C2
diff_N=N_C1-N_C2
d=cbind(diff_D,diff_N)

d_bar=colMeans(d)
sd=var(d)

n=dim(c_data)[1]
p=2
#computing the T2 test statistic
T2=n*t(d_bar)%*%solve(sd)%*%d_bar
F=qf(0.05,p,n-p,lower.tail=FALSE)
p_value=1-pf((n-p)*T2/((n-1)*p),p,n-p)


