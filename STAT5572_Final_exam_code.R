#3

#Upload the data
data_turnips <- read.table("C:\\Users\\hoang\\Downloads\\turnips.DAT")
print(data_turnips)

#Number of row, alpha and p
n = nrow(data)
alpha = 0.05
p = 3

#Calculate x_bar
x_bar <- c(mean(data$V1), mean(data$V2), mean(data$V3))
cat('\n X_bar :', x_bar, '\n')

#Expected mean
mu_0 <- c(15, 6, 2.85)

#Hotelling_T2 function
Hotelling_T2 <- function(n, x_bar, mu_0, data){
  n %*% (t(x_bar - mu_0) %*% (solve(cov(data))) %*% (x_bar - mu_0))
}
Hotelling_T2 <-  Hotelling_T2(n = n, x_bar = x_bar, mu_0 = mu_0, data = data)

cat('\n Hotelling_T2 :', Hotelling_T2, '\n')

#F statistic
cat('\n F_statistc :', qf(p = alpha, df1 = p, df2 = n - p, lower.tail = FALSE), '\n')

#P-value
cat('\n p_value :', 1-pf((n-p) * Hotelling_T2/((n-1) * p), p, n-p), '\n')

#4
#Upload the data
data_peanut <- read.table("C:\\Users\\hoang\\Downloads\\peanuts.dat")

#Re-name the column of the data
names(data_peanut) <- c('Factor_1', 'Factor_2', 'X_1', 'X_2', 'X_3')
print(data_peanut)

#two-way MANOVA model
attach(data_peanut)
print(summary(manova(as.matrix(data_peanut[,3:5]) ~ Factor_1 + Factor_2 + Factor_1*Factor_2), test="Wilks"))

#5

#a
#Load the data
data_hemo <- read.csv("C:\\Users\\hoang\\Downloads\\hematology.csv")
data_hemo <- data_hemo[, c('X1', 'X2', 'X3', 'X4', 'X5', 'X6')]
print(cov(data_hemo))

#b

#Standardize the variable
standardize_data <- scale(data_hemo)
#Correlation matrix
cor_matrix <- cor(standardize_data)
#PCA
pca_result <- princomp(cor_matrix)
#Print the summary
print(summary(pca_result))

#d
#Scree plot
screeplot(pca_result, type = "lines", main = "Scree Plot", cex.axis = 1.2, cex.lab = 1.2)

#e
loadings <- pca_result$loadings

# Display loadings for the selected components
selected_components <- 1:3  # Adjust as needed
loadings_selected <- loadings[, selected_components]

# Print loadings
print(loadings_selected)