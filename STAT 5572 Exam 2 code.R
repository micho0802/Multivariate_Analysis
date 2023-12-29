#Exam 2
#1a

#Given
n1 = 45
n2 = 55

x1_bar <- c(204.4, 556.6)
x2_bar <- c(130.0, 355.0)

s1 <- matrix(c(13825.3, 23823.4, 23823.4, 73107.4), nrow = 2, ncol = 2, byrow = TRUE)
s2 <- matrix(c(8632.0, 19616.7, 19616.7, 55964.5), nrow = 2, ncol = 2, byrow = TRUE)

#Calculate the pooled covariance matrix
Sp <- ((n1-1)*s1+(n2-1)*s2)/(n1+n2-2)

#Calculate test statistic T^2
T2 <- t(x1_bar-x2_bar) %*% solve(Sp*((1/n1)+(1/n2))) %*% (x1_bar-x2_bar)

#Degree of freedom
p <- ncol(Sp)
df <- n1 + n2 - p - 1

#Critical value from Hotelling's T^2 distribution
alpha <- 0.05
critical_value <- qf(1 - alpha, p, df)

#Print results 
cat('Test Statistic (T^2:', T2, '\n')
cat('Critical Value:', critical_value, '\n')

if (T2 > critical_value) {
  cat('Reject the null hypothesis: There is evidence of a difference in mean vectors. \n')
} else {
  cat('Fail to reject the null hypothesis: No significant evidence of a difference in mean vectors. \n')
}

#1b
#Calculate Bonferroni correction factor
bonferroni_factor <- sqrt(2 / (n1 + n2))

#Calculate standard errors for differences in means
se_diff <- sqrt(diag(Sp) * (1/n1) + (1/n2))

#Calculate critical value for Bonferroni correction
bonferroni_critical_value <- qt(1 - alpha/(2 * p), df)

#Calculate differences in means
diff_means <- x1_bar - x2_bar

#Calculate confidence intervals for differences in means
lower_ci <- diff_means - bonferroni_critical_value * bonferroni_factor * se_diff
upper_ci <- diff_means + bonferroni_critical_value * bonferroni_factor * se_diff

#Display the result
for (i in 1:p) {
  cat(paste0('\nX', i), 'Difference in means:', diff_means[i], '\n')
  cat(paste0('X', i), '95% Bonferroni C.I:', '[', lower_ci[i], ',', upper_ci[i], ']\n\n')
}

#2a,b
#Set up the dataframe
trt1 <- data.frame(type = 'Treatment 1', matrix(c(6, 7, 5, 9, 8, 6, 4, 9, 7, 9), ncol = 2, byrow = TRUE))
trt2 <- data.frame(type = 'Treatment 2', matrix(c(3, 3, 1, 6, 2, 3), ncol = 2, byrow = TRUE))
trt3 <- data.frame(type = 'Treatment 3', matrix(c(2, 3, 5, 1, 3, 1, 2, 3), ncol = 2, byrow = TRUE))

# Concatenate dataframes
df <- rbind(trt1, trt2, trt3)
print(df)

#Create MANOVA model
based_model <- lm(cbind(X1, X2) ~ type, data = df)
manova_model <- manova(based_model, data = df)
print(summary(manova_model, test = 'Wilk'))

#3

data <- read.table("C:\\Users\\hoang\\Downloads\\coatings.DAT")

#Preprocessing the data so it can fit the MANOVA model
coating_1 <- data.frame(type = 'Coating 1', data[, 1:2])
coating_2 <- data.frame(type = 'Coating 2', data[, 3:4])
names(coating_1) <- c('Type', 'Depth', 'Number')
names(coating_2) <- c('Type', 'Depth', 'Number')
df <- rbind(coating_1, coating_2)
print(df)

#Create MANOVA model
manova_model <- manova(cbind(Depth, Number) ~ Type, data = df)
print(summary(manova_model, test = 'Wilk'))

