#Read and inspect the dataset
setwd("E:\\Analytics\\Training\\Fidelity\\")
data <- read.csv("risk.csv", header = TRUE)
str(data)
attach(data)

#Visualize the data set
boxplot(data)
boxplot(data[c(2,3,4,5)])

plot(data)
plot(data, col= c("red", "green", "blue"))

require(MASS)
parcoord(data [c(2,3,4,5)], col = 1, lty = 1, var.label = TRUE)
#Create new datasets
#Drop the categorical column in the data
myData1 <- data[c(0,1,2,3,4,5)]

#Drop the unique identifer
myData2 <- data[c(2,3,4,5)]

#Effect of unique values
#Build PCA
plot(myData1)
myPca1 <- prcomp(myData1)
#Inspect PCA results
myPca1
summary(myPca1)
plot(myPca1)
biplot(myPca1)
myPca2 <- prcomp(myData2)
myPca2
summary(myPca2)
plot(myPca2)
biplot(myPca2)
#Extract features
#Effect of scaling
#Build PCA
myPca3 <- prcomp(myData2, scale = TRUE)
#Inspect PCA results
myPca3
summary(myPca3)
plot(myPca3)
biplot(myPca3)
#Extract features