#Open the data
data <- read.table('C:\\Users\\hoang\\Downloads\\PeanutsData.dat')

#Seperate the data into cluster
d1 <- data[data$V1 == 'P1',]
d2 <- data[data$V1 == 'P2',]
d3 <- data[data$V1 == 'P3',]
#Get number of row
n1 = nrow(d1)
n2 = nrow(d2)
n3 = nrow(d3)
#Total row
n = n1 + n2 + n3
#Number of parameter i.e. V2, V3, V4
p = 3
#Number of group i.e. P1, P2, P3
g = 3

#Summary statistic
x_bar_1 = colMeans(d1[, 2:4])
x_bar_2 = colMeans(d2[, 2:4])
x_bar_3 = colMeans(d3[, 2:4])
x_bar <- (n1*x_bar_1 + n2*x_bar_2 + n3*x_bar_3)/ n

s1 <- var(d1[, 2:4])
s2 <- var(d2[, 2:4])
s3 <- var(d3[, 2:4])

#Calculate W and B
W <- (n1-1)*s1+(n2-1)*s2+(n3-1)*s3
B <- n1*(x_bar_1 - x_bar)%*%t(x_bar_1 - x_bar)+n2*(x_bar_2-x_bar)%*%t(x_bar_2-x_bar)+n3*(x_bar_3 - x_bar)%*%t(x_bar_3 - x_bar)
#Calculate Lambda
Lambda=det(W)/det(B+W)


print(Lambda)

print(1 -pchisq(-(n-1-(p+g)/2)*log(Lambda),p*(g - 1)))


model_1 <- lm(cbind(V2, V3, V4) ~ V1, data = data)
manova_model <- manova(model_1, data = data)
print(summary(manova_model, test = 'Wilk'))

