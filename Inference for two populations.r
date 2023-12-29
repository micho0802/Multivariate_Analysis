setwd("/Users/nadee/Desktop/Stat 5572_R_examples")

#The dataset 'container.df' in the library "Hotelling" contains the elemental concentration of nine different elements (Titanium,Aluminium,Iron,Manganese,Magnesium,Calcium,
#Barium,Strontium, and Zirconium) in specimens of glass taken from two container types.There are ten containers of each type.

library(Hotelling)
#load the data
data(container.df)
attach(container.df)
container.df

#considering Al and Fe
x1_bar=colMeans(container.df[1:10, 3:4])
s1=var(container.df[1:10,3:4])

x2_bar=colMeans(container.df[11:20, 3:4])
s2=var(container.df[11:20,3:4])

n1=10
n2=10
p=2

sp=((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)

T2=t(x1_bar-x2_bar)%*%solve(sp*((1/n1)+(1/n2)))%*%(x1_bar-x2_bar)
F=qf(0.05,p,n1+n2-p-1,lower.tail=FALSE)
p_value=1-pf((n1+n2-p-1)*T2/((n1+n2-2)*p),p,n1+n2-p-1)


library(DescTools)
HotellingsT2Test(container.df[1:10, 3:4],container.df[11:20, 3:4])

