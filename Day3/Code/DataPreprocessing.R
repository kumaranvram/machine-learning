#Demonstration of data preprocessing
#Read data
setwd("E:\\Analytics\\Training\\Thoughtworks\\Data\\")
data <- read.csv("risk.csv")
myData <- data
attach(myData)

#Sample
#select some sample
#Make training  and test sample
d = sort(sample(nrow(myData), nrow(myData)*0.6))
train <- myData[d,]
test <- myData[-d,]
#balance the training sample


#Missing values
#Create sample data
n <- sample(nrow(myData), nrow(myData)*0.1)
mySample <- myData[n,]

#Remove a cell value
mySample[14,3]<-NA

#Impute the missing value with mean value of the columns
require(e1071)
fixSample<-impute(mySample[,1:4], what="mean")
fixSample<-impute(mySample[,1:4], what="median")

#Outlier analysis
#Explore the possible variables having outliers
plot(myData)
plot(myData, col = as.integer(myData$Class))

#Explore the outliers
#When the information need drill down
boxplot(myData$Ratio1)
boxplot(myData$Ratio1~myData$Class)

#When the information needs rollup
boxplot(myData$Ratio2)
boxplot(myData$Ratio2~myData$Class)

#Binning
#Equal width
#Equal frequency
#User defined class width


#Feature selection
#Correlation
cor(myData[2:5])
#PCA - To be practiced later
#Entrophy based
#Chi square based
#Best first


#Feature creation
myData$nr <- myData$Ratio1/myData$Ratio2

#Feature bundling

#Feature transformation
#Log transform
myData$ltR1 <- log(myData$Ratio1)

#Scaling
#Decimal scaling
myData
#Max value scaling
max <- max(myData$Ratio1)
myData$mxR1 <- myData$Ratio1/max
#Standardization
myData$sclR1 <- scale(myData$Ratio1)
