setwd("/Users/nadee/Desktop/Stat 5572_R_examples")
swiss_data=read.table("swiss3.txt")
head(swiss_data)
dim(swiss_data)
names(swiss_data)=c("type","length","left","right","bottom","top","diag")
head(swiss_data)
attach(swiss_data)

real_notes=swiss_data[type=="real",]
fake_notes=swiss_data[type=="fake",]

x1_bar=colMeans(real_notes[,2:7])
s1=var(real_notes[,2:7])

x2_bar=colMeans(fake_notes[,2:7])
s2=var(fake_notes[,2:7])

n1=100
n2=100
p=6

sp=((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)
round(sp, digits=3)

T2=t(x1_bar-x2_bar)%*%solve(sp*((1/n1)+(1/n2)))%*%(x1_bar-x2_bar)
F=qf(0.05,p,n1+n2-p-1,lower.tail=FALSE)
p_value=1-pf((n1+n2-p-1)*T2/((n1+n2-2)*p),p,n1+n2-p-1)


library(DescTools)
HotellingsT2Test(real_notes[,2:7],fake_notes[,2:7])



#Bartlett's test
c=1-(((2*(p^2)+3*p-1)/(6*(p+1)))*((1/(n1-1))+(1/(n2-1))-(1/(n1+n2-2))))
B_testStat=c*(((n1+n2-2)*log(det(sp)))-((n1-1)*log(det(s1)))-((n2-1)*log(det(s2))))
df=(p*(p+1))/2
p_val=1-pchisq(B_testStat,df)


