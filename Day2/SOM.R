require("kohonen")
#Read and inspect the dataset
setwd("E:\\Analytics\\Training\\Thoughtworks\\data\\")
data <- read.csv("risk.csv", header = TRUE)
#data <- read.csv("telco.csv", header = TRUE)
str(data)
attach(data)

#Create new data set with numeric variables
myData <- data[c(2,3,4,5)]

#Scale the new data
myData.scl <- scale(myData)

#Build the SOM model
set.seed(7)
myData.som <- som(data = myData.scl, grid = somgrid(5, 4, "hexagonal"))

#Analyze the SOM outputs
plot(myData.som, main = "Risk profiles")
plot(myData.som, type = "quality", main = "Risk profiles - Quality")
plot(myData.som, type = "count", main = "Risk profiles - Count")



